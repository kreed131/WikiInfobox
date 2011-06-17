module WikiInfobox.Render (
                           module WikiInfobox
                          , pickRender
                          , wikiToJSON
                          , wikiToXML
                          ) where

import Data.Char
import Text.JSON
import Text.HTML.TagSoup
import WikiInfobox
--import Data.String.Utils --(toLower)

wikiToJSON :: InfoboxData -> String
wikiToJSON   = encode . toJSObject

wikiToXML  :: InfoboxData -> String
wikiToXML    = concatMap renderXML

wikiToHTML :: InfoboxData -> String
wikiToHTML   = ("<html>" ++) . (++ "</html>") . 
               concatMap (\x -> renderTags 
                                [TagOpen "b" [], TagText $ fst x ++ ":", TagClose "b", TagText $ ' ' : snd x ++ "<br>"]) 

pickRender :: String -> (InfoboxData -> String)
pickRender s = case s' of
                 "xml"     -> wikiToXML
                 "haskell" -> show
                 "human"   -> wikiToHTML
                 _         -> wikiToJSON
                 
    where s' = map toLower s
 
renderXML  :: (String, String) -> String
renderXML  x = renderTags [TagOpen (fst x) [], TagText (snd x), TagClose (fst x)]
