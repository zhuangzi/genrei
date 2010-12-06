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
import System.IO
import Control.Monad.Loops

type Camxes = (Handle,Handle,Handle,ProcessHandle)
data State = State { stateCamxesFlat :: Camxes 
                   , stateCamxesNested :: Camxes }

main = do
  camxesFlat <- getCamxes ["-f"]
  camxes <- getCamxes []
  runFastCGIConcurrent 20 . handleErrors $ do
    method <- fromMaybe "commands" <$> getInput "method"
    setHeader "Content-Type" "text/plain"
    case lookup method commands of
      Just runCmd -> runCmd $ State camxesFlat camxes
      Nothing     -> failure "Unknown page."

getCamxes opts = do
  camxes@(inp,out,err,_) <- runInteractiveCommand $ "camxes " ++ unwords opts
  mapM_ (`hSetBinaryMode` False) [inp,out,err]
  mapM_ (const $ hGetLine out) opts
  return camxes

failure = outObj "failure"
success = outObj "success"
outObj x v = output . encode $ makeObj [(x,showJSON v)]

commands = [listCommands,camxesGrammar,jbofiheGrammar]
cmd = (,)

listCommands = cmd "commands" $ \state -> do
  success $ map fst commands

data CamxesType = Flat | Nested
  deriving (Show,Enum,Eq)
  
instance JSON CamxesType where
  readJSON = readJSONEnum "Camxes Type"; showJSON = showJSONEnum

camxesGrammar = cmd "camxes-grammar" $ \state -> do
  q <- json "q"
  t <- fromMaybe Flat <$> maybeJson "type"
  let cmd = case t of
              Flat -> camxesFlatQuery
              Nested -> camxesQueryNested
  reply <- cmd state q
  case reply of
    "" -> failure "Bad parse."
    _ -> success reply

camxesQueryNested state q = do
  let (inp,out,_err,_pid) = stateCamxesNested state
  io $ do hPutStrLn inp $ unwords $ words q
          hFlush inp
          ls <- unfoldM $ do l <- hGetLine out
                             return $ if null l then Nothing else Just l
          return $ unlines ls

camxesFlatQuery state q = do
  let (inp,out,_err,_pid) = stateCamxesFlat state
  io $ do hPutStrLn inp $ unwords $ words q
          hFlush inp
          hGetLine out

jbofiheGrammar = cmd "jbofihe-grammar" $ \state -> do
  q <- json "q"
  t <- fromMaybe Grammar <$> maybeJson "type"
  os <- fromMaybe [] <$> maybeJson "options"
  out <- jbofihe t os q
  case out of
   Left e -> failure e
   Right val -> success val

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
  out <- io $ catch (Right <$> readProcess "jbofihe" (typ t ++ map opt os) q)
                    (\e -> return $ Left $ "Bad parse.")
  case out of
    Left e -> return $ Left e
    Right reply -> return $ Right $ 
      if any (==t) [ParseTreeJson,ParseTreeFullJson]
         then treeToJson $ jbofiheToTree reply
         else showJSON reply
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

cleanName = takeWhile (/=' ')
nodeRest  = avoid ' ' . avoid ':' . avoid ' ' . dropWhile (/= ' ')
avoid = dropWhile . (==)
    
treeToJson [Node n []] = makeObj [(cleanName n,showJSON $ nodeRest n)] where
treeToJson nodes = makeObj $ zip names objs where
  names = map (\(Node n _) -> cleanName n) nodes
  objs = map jsonize nodes
   where jsonize (Node n []) = showJSON $ nodeRest n
         jsonize (Node n xs) = treeToJson xs

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
