#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
import Control.Monad
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Process
import System.Directory 

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { runTests = tests }

tests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
tests args _ _ _ = do
  case args of
    ["nginx",x]   -> nginx x
    ["fastcgi",x] -> fastcgi x
    ["remote",x]  -> remote x
    a             -> badarg a

remote :: String -> IO ()
remote x = do system $ "scripts/remote-" ++ x; return ()

nginx :: String -> IO ()
nginx "start" = npid not $ do
  system "nginx -p`pwd`/httpd/ -c../nginx.conf"
nginx "stop" = npid id $ do
  system $ "kill `cat " ++ ngpath ++ "`"
nginx "restart" = do
  nginx "stop"
  nginx "clean"
  nginx "start"
nginx "clean" = npid id $ do
  system $ "rm " ++ ngpath ++ ""
nginx a = badarg a

fastcgi :: String -> IO ()
fastcgi "start" = fpid not $ do
  system $ "spawn-fcgi dist/build/genrei.fcgi/genrei.fcgi -p 8002 -P " ++ fppath
fastcgi "stop" = fpid id $ do
  system $ "kill `cat " ++ fppath ++ "`"
  fastcgi "clean"
fastcgi "refresh" = do
  system "cabal build && sleep 1 && cabal test fastcgi restart"
  return ()
fastcgi "restart" = do
  system "cabal test fastcgi stop; cabal test fastcgi clean; cabal test fastcgi start" 
  return ()
fastcgi "clean" = fpid id $ do
  system $ "rm " ++ fppath
fastcgi a = badarg a

npid :: (Bool -> Bool) -> IO a -> IO ()
npid = pid ngpath

fpid :: (Bool -> Bool) -> IO a -> IO ()
fpid = pid fppath

fppath :: FilePath
fppath = "httpd/fastcgi.pid"

ngpath :: FilePath
ngpath = "httpd/nginx.pid"

pid :: FilePath -> (Bool -> Bool) -> IO a -> IO ()
pid path p m = do
  e <- doesFileExist path
  when (p e) $ do m; return ()

badarg :: Show a => a -> IO ()
badarg a = error $ "invalid argument(s): " ++ show a
