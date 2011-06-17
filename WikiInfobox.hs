-- file: WikiInfobox.hs
module WikiInfobox ( 
                     parseData 
                   , isWikiTag 
                   , removeWikiTags 
                   , fromWikiTag
                   , getWikiUrl
                   , getWikiEditUrl
                   , getWiki
                   , getInfo
                   , InfoboxData
                   ) where
{-- 
  Example:
  src <- getWiki "en" "Belarus"
  getInfo src
  --
  (fmap getInfo . getWiki "en") "Belarus"
--}

import Network.HTTP
import Text.HTML.TagSoup
import Data.List
import Data.List.Utils
import Data.Maybe
import Control.Applicative

type InfoboxData = [(Key, Value)]
type Key         = String
type Value       = String
getUrl :: String -> IO String
getUrl ""   = return []
getUrl url  = simpleHTTP (getRequest url) >>= getResponseBody

getInfo :: String -> InfoboxData
getInfo ""  = [("api_status", "no_matches")]
getInfo x   = (parseData . select . parseTags) x

getWiki :: String -> String -> IO String
getWiki _   "" = return []
getWiki lang q = getWikiEditUrl <$> getWikiUrl lang q >>= getUrl

clean, cleverClean, addWs :: String -> String
clean       = unwords . words
cleverClean = cleverUnwords . words
addWs       = (:) ' ' . (++ " ")


cleanData, cutInfo, cleanMap, removeEmpty :: [String] -> [String]
cleanData   = map (clean . tail)
cleanMap    = map clean
cutInfo     = takeWhile (/= "}}") . dropWhile (not . startswith "{{Infobox")
removeEmpty = concatMap (\x -> if null x then [] else [x])

isWikiTag :: String -> Bool
isWikiTag x = any (\y -> startswith y x || endswith y x) wikiTags

punctMarks, wikiTags :: [String]
punctMarks  = [",", ".", "!", "?", ":", ";"]
wikiTags    = ["[[", "]]", "{{", "}}"]

cleverUnwords :: [String] -> String
cleverUnwords = clean . concatMap (\s -> if isPunct s then s else ' ':s)
    where 
      isPunct x = any (`startswith` x) punctMarks
      
select :: [Tag String] -> [String]
select s   = (drop 7 . fromTagText . (!!3) .  head . partitions (~== "<div class=printfooter>")) s : select' s

select' :: [Tag String] -> [String]
select'     = cleanData . removeEmpty . cutInfo . lines . concatMap fromTagText 
             . filter isTagText . parseTags . fromTagText . (!!1) 
             . head . partitions (~== "<textarea name=wpTextbox1>")

removeWikiTags :: String -> String
removeWikiTags = cleverClean . cleverUnwords . map fromWikiTag . splitWikiTags

sidesWith :: String -> String -> String -> Bool
sidesWith l r s = startswith l s && endswith r s

fromWikiTag :: String -> String
fromWikiTag tag | sidesWith "{{" ""   tag = ""
                | sidesWith "[[" "]]" tag = 
                    if isNothing (elemIndex '|' tag) 
                    then (clean . drop 2 . takeWhile (/= ']')) tag 
                    else (clean . tail . takeWhile (/= ']') . dropWhile (/= '|')) tag
                | otherwise = tag
                              
splitWikiTags :: String -> [String]
splitWikiTags src =  mergeSequences $ findSequences zips
    where 
      xs          = (words . indentMap) src 
      zips        = zip (map isWikiTag xs) xs
      
indentMap :: String -> String
indentMap s  = indentMap' s 0
    where 
      indentMap' xs i = if i == lth then xs else indentMap' (indentTag' i xs) (i + 1)
      lth             = length wikiTags
      indentTag' n    = indentTag (wikiTags !! n)

indentTag :: String -> String -> String
indentTag t  = join (addWs t) . split t 

mergeSequences :: (Eq a) => [(a, String)] -> [String]
mergeSequences    = map (unwords . map snd) . groupBy (\x y -> fst x == fst y)

findSequences :: [(Bool, String)] -> [(Int, String)]
findSequences lst = findSeq' lst False 0
    where 
      findSeq' [] _ _    = []
      findSeq' (x:xs) b n = 
          case (fst x, b) of
            (True, False)  -> (n, snd x) : findSeq' xs (not b) n
            (True,  True)  -> (n, snd x) : findSeq' xs (not b) (n + 1)
            (False, True)  -> (n, snd x) : findSeq' xs b n
            (False, False) -> (-1, snd x) : findSeq' xs b n

parseData ::  [String] -> InfoboxData
parseData (x:xs) = prefix ++ (delEmptyEntries . map parseData') xs
    where 
      prefix       = [("api_status", status), ("wiki_url", x), ("wiki_lang", lang)]
      status       = case null xs of
                       True  -> if null x then "no_matches" else "no_infobox"
                       False -> "ok"
      lang         = takeWhile (/= '.') x
      parseData' y = (,) (key y) (value y)
      value        = (!!1) . prepared
      key        y = join "_" . split " " $ if length (splitKey y) < 1 then "" else  last $ splitKey y
      splitKey     = split "|" . (!!0) . prepared 
      prepared     = map removeWikiTags . splitted
      splitted y
        | '=' `elem` y            = split "=" y
        | startswith "{Infobox" y = ["wiki_type", last $ split " " y]
        | otherwise               = ["", ""]                                    

delEmptyEntries :: InfoboxData -> InfoboxData
delEmptyEntries = filter (not . anyNullAL)

anyNullAL :: ([a], [b]) -> Bool
anyNullAL al    = null (fst al) || null (snd al)

test = do
  src <- getUrl "http://en.wikipedia.org/w/index.php?title=Albania&action=edit"
  return  $ parseData $ select $ parseTags src
test1 = do
  src <- getUrl "http://en.wikipedia.org/w/index.php?title=United_States&action=edit"
  return  $ select $ parseTags src

test3 x = do
  s <- (fmap getInfo . getWiki "en") "Belarus"
  return s

test2 x = do
  url <- getWikiUrl "" x
  src <- getUrl $ getWikiEditUrl url
  return $ (parseData . select . parseTags) src
  

--- Bing API

bingAppId = "D356832030978843E16A93423258B2158ED5B43F"

getWikiEditUrl :: String -> String
getWikiEditUrl []   = []
getWikiEditUrl x = beginOfUrl ++ ((!!4) . split "/") x
    where beginOfUrl = (join "/" . take 3 . split "/") x ++ "/w/index.php?action=edit&title="

getWikiUrl :: String -> String -> IO String
getWikiUrl lang query   = do
  searchResults <- getUrl $ apiUrl lang query
  return $ getResult searchResults
      where 
        getResult x = if not (null $ result x) && checkResult x then parseResult x else ""
        result      = partitions (~== "<web:Url>") . parseTags
        parseResult = fromTagText . (!!1) . head . result
        checkResult = startswith ("http://" ++ lang ++ ".wikipedia.org") . parseResult

apiUrl :: String -> String -> String
apiUrl lang query = concat 
                    ["http://api.search.live.net/xml.aspx?Appid="
                      , bingAppId, "&query='"
                      , lang'
                      , ".wikipedia.org'%20"
                      , qq
                      , "&sources=web&web.count=1&web.offset=0"]
    where qq = urlEncode query
          lang' = if null lang then "en" else lang
