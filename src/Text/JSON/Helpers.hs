{-# LANGUAGE MonomorphismRestriction #-}
-- | Helpers for defining JSON instances.

module Text.JSON.Helpers 
    (readFail,matchConsts,maybeToResult,readJSONEnum,showJSONEnum,constDashed
    ,maybeValFromObj,unj)
    where

import Control.Applicative (Applicative(..),Alternative(..),(<$>))
import Data.Char
import Text.JSON           (Result(..),JSValue(..),fromJSString,JSON(..),valFromObj,JSObject)
import Text.Regex

-- | Make a readJSON error string.
readFail :: String -> String -> String
readFail n s = n ++ ".readJSON: " ++ s

-- | Match constructors against a string.
matchConsts :: (String -> String) -> String -> [(String,a)] -> Result a
matchConsts n s = maybeToResult (n "enum match fail") . lookup s

-- | Convert a Maybe to a parse result.
maybeToResult :: String -> Maybe a -> Result a
maybeToResult n a =
    case a of
      Just v  -> return v
      Nothing -> Error n

-- | Read an enum type from JSON.
readJSONEnum :: (Show a,Enum a) => String -> JSValue -> Result a
readJSONEnum n (JSString s) = matchConsts (readFail n) (fromJSString s) xs
    where xs = zip (map constDashed ys) ys
          ys = enumFrom $ toEnum 0
readJSONEnum n _ = Error $ "failed to read enum type: " ++ n

-- | Show an enum type.
showJSONEnum :: (Enum a,Show a) => a -> JSValue
showJSONEnum = showJSON . constDashed

-- | Convert a const value e.g. "FooBar" to "foo-bar".
constDashed :: (Enum a,Show a) => a -> String
constDashed = map toLower . stripDash . upperToDash . show where
  upperToDash = flip (subRegex (mkRegex "_")) "."
              . flip (subRegex (mkRegex "([a-zA-Z0-9])([A-Z])")) "\\1-\\2"
  stripDash = dropWhile (=='-')

maybeValFromObj :: (JSON a) => String -> JSObject JSValue -> Result (Maybe a)
maybeValFromObj n o = (Just <$> valFromObj n o) <|> pure Nothing

unj :: JSON a => String -> JSObject JSValue -> Result a
unj name o = do v <- valFromObj name o
                readJSON v

