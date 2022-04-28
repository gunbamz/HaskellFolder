module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity ( IdentityT(..) )
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import qualified MyLib (someFunc)

main :: IO ()
main = do
  putStrLn "hello world"

myBind :: Monad f => f a -> (a -> f b) -> f b
myBind fa afb = afb =<< fa

itBindTryOne :: Functor f => IdentityT f a -> (a -> b) -> f b
itBindTryOne (IdentityT fa) ab = fmap ab fa

iTBind :: Monad f => IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b
iTBind (IdentityT fa) ab = IdentityT ((runIdentityT . ab) =<< fa)

myPrompt :: String -> IO String
myPrompt prompt = do
  putStr prompt
  getLine

getName :: MaybeT IO String
getName = do
  input <- lift $ myPrompt "Name? "
  if input == ""
    then MaybeT $ return Nothing
    else MaybeT $ return $ Just $ "Name: " ++  input ++ "\n"

getNumber :: String -> MaybeT IO String
getNumber str = do
  input <- lift $ myPrompt "Phone number? "
  if length input /= 7
    then MaybeT $ return Nothing
    else MaybeT $ return $ Just $ str ++ "Phone Number: " ++ input ++ "\n"

getStreetName :: String -> MaybeT IO String
getStreetName str = do
  input <- lift $ myPrompt "Street Name? "
  if input == ""
    then MaybeT $ return Nothing
    else MaybeT $ return $ Just $ str ++ "Street Name: " ++ input ++ "\n"

compositionMethod :: MaybeT IO String
compositionMethod = getName >>=
                    getNumber >>=
                    getStreetName

compAlt :: MaybeT IO String
compAlt = do
  a <- getName
  b <- getNumber a
  getStreetName b

allTogetherNow :: IO ()
allTogetherNow = do
  result <- runMaybeT compositionMethod
  case result of
    Just str -> putStrLn str
    Nothing -> putStrLn "Failure!"

aTNv2 :: IO ()
aTNv2 = do
  result <- runMaybeT compAlt
  case result of
    Just str -> putStrLn str
    Nothing -> putStrLn "Failure!"


