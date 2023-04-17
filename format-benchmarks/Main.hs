module Main (main) where

import Data.List.Extra (trim)
import Data.Maybe (mapMaybe)
import Prettyprinter
import Text.Regex.TDFA
import Text.Regex.TDFA.String (compile)
import Text.XML.Light
import Prelude

data Tree = Section String [Tree] | Leaf String [String] deriving (Show)

-- | Reads stdin which should be the benchmark tests XML contents and outputs to stdout
-- | a YAML style formatted report with endpoint limits.
main :: IO ()
main = do
  input <- getContents
  let [_, Elem Element {elContent = [Elem body]}] = parseXML input
  putStr (show $ vsep (fmap formatTree (parseTree body)) <+> line')

reportRegex :: Regex
Right reportRegex =
  compile
    defaultCompOpt {multiline = True}
    defaultExecOpt
    "size[[:space:]]*: [[:digit:]]*\\.[[:digit:]]*\\%[[:space:]]*mem[[:space:]]*: [[:digit:]]*\\.[[:digit:]]*\\%[[:space:]]*steps[[:space:]]*: [[:digit:]]*\\.[[:digit:]]*\\%"

parseTree :: Element -> [Tree]
parseTree Element {elContent = content} =
  mapMaybe
    ( \case
        Elem Element {elAttribs = attr : _, elContent = [Elem Element {elName = QName {qName = "failure"}, elContent = [Text failDat]}]} ->
          case parseReport (cdData failDat) of
            Just a -> Just $ Leaf (attrVal attr) a
            Nothing -> Just $ Leaf (attrVal attr) ["Could not parse"]
        Elem el ->
          if null (elContent el)
            then Nothing
            else case elAttribs el of
              attr : _ -> Just $ Section (attrVal attr) (parseTree el)
              _ -> Nothing
        _ -> Nothing
    )
    content
  where
    parseReport :: String -> Maybe [String]
    parseReport a =
      let allMatches = getAllTextMatches (match reportRegex a)
       in if null allMatches
            then Nothing
            else
              let l = lines $ last allMatches
               in Just $ trim <$> drop (length l - 3) l

formatTree :: Tree -> Doc a
formatTree (Leaf name content) = nest 2 (vsep (pretty <$> ((name <> ":") : content)))
formatTree (Section name contents) = nest 2 $ vsep (pretty (name <> ":") : (formatTree <$> contents))
