module Main where

import Control.Applicative
import Data.Function
import Data.Maybe
import Data.Tree
import Network.FastCGI
import System.Process
import Text.JSON
import Text.JSON.Helpers
import Text.Printf

main = do
  runFastCGIConcurrent 20 . handleErrors $ do
    method <- fromMaybe "commands" <$> getInput "method"
    setHeader "Content-Type" "text/plain"
    case lookup method commands of
      Just runCmd -> runCmd
      Nothing     -> failure "Unknown page."

failure = outObj "failure"
success = outObj "success"
outObj x v = output . encode $ makeObj [(x,showJSON v)]

commands = [listCommands,jbofiheGrammar]
cmd = (,)

listCommands = cmd "commands" $ do
  success $ map fst commands

jbofiheGrammar = cmd "jbofihe-grammar" $ do
  q <- json "q"
  t <- fromMaybe Grammar <$> maybeJson "type"
  os <- fromMaybe [] <$> maybeJson "options"
  out <- jbofihe t os q
  success $ out

data JbofiheType = Grammar
                 | ParseTree
                 | ParseTreeFull
                 | ParseTreeJson
                 | ParseTreeFullJson
                 | Translate
                 | TranslateHtml
                 | TranslateLatex
  deriving (Show,Enum,Eq)

instance JSON JbofiheType where
  readJSON = readJSONEnum "Jbofihe Type"; showJSON = showJSONEnum

data JbofiheOpt = ElidableTerminators
                | RequireTerminatorsAndSeparators
                | OmittableTerminators
                | OmittableTerminatorsVerbose
                | AllowCulturalRafsi
  deriving (Show,Enum,Eq)

instance JSON JbofiheOpt where
  readJSON = readJSONEnum "Jbofihe Opt"; showJSON = showJSONEnum

jbofihe t os q = do
  out <- io $ readProcess "jbofihe" (typ t ++ map opt os) q
  return $ if any (==t) [ParseTreeJson,ParseTreeFullJson]
     then treeToJson $ jbofiheToTree $ out
     else showJSON out
  where typ ParseTree         = ["-t"]
        typ ParseTreeFull     = ["-tf"]
        typ Translate         = ["-x"]
        typ TranslateHtml     = ["-H"]
        typ TranslateLatex    = ["-l"]
        typ ParseTreeJson     = ["-t"]
        typ ParseTreeFullJson = ["-tf"]
        typ Grammar           = []

        opt ElidableTerminators             = "-ie"
        opt RequireTerminatorsAndSeparators = "-re"
        opt OmittableTerminators            = "-se"
        opt OmittableTerminatorsVerbose     = "-sev"
        opt AllowCulturalRafsi              = "-cr"


treeToJson [Node n []] = makeObj [(realName,showJSON rest)] where
    realName = takeWhile (/=' ') n
    rest  = avoid ' ' . avoid ':' . avoid ' ' . dropWhile (/= ' ') $ n
    avoid = dropWhile . (==)
treeToJson nodes = makeObj $ zip names objs where
  names = map (\(Node n _) -> n) nodes
  objs = map (\(Node _ xs) -> treeToJson xs) nodes

jbofiheToTree = tree (on (==) (length . takeWhile (/='+'))) clean
              . filter (/="CHUNKS")
              . reverse
              . lines
  where clean = drop 1 . dropWhile (/='-')

tree cmp f = go where
  go []     = []
  go (n:ns) = case span (not.cmp n) ns of
    (subs,rest) -> Node (f n) (reverse $ go subs) : go rest

io = liftIO

json n = getJson n (error $ printf "Expected parameter `%s'" n)  id

maybeJson n = getJson n Nothing Just

getJson n nothing this = do
  str <- getInput n
  case str of
    Nothing -> return $ nothing
    Just text -> case decode text <|> decode (show text) of
      Ok js -> return $ this js
      Error e -> error $ printf "Error parsing parameter `%s': %s" n e
