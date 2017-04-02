{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map 
import Control.Applicative
{-import Control.Monad.Trans.Either-}
import Control.Monad.Except
import Control.Monad.IO.Class

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show

getToken :: IO (Either LoginError Text)
getToken = do
  T.putStrLn "Enter email address:"
  email <- T.getLine
  return (getDomain email)

getTokenEither :: ExceptT LoginError IO Text
getTokenEither = ExceptT $ do
  T.putStrLn "Enter email address: "
  input <- T.getLine
  return (getDomain input)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
            [name, domain] -> Right domain
            [_] -> Left InvalidEmail

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

{-data EitherT e m a = EitherT { runEitherT :: m (Either e a) }-}
{-newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }-}

{-let userpw = Map.lookup domain users-}
{-lift $ T.putStrLn "Enter password"-}

data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure    = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return  = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)


userLoginEither :: IO (Either LoginError Text)
userLoginEither = runExceptT $ do
     domain <- getTokenEither
     liftIO $ T.putStrLn "Enter password"
     password <- liftIO $ T.getLine
     userpw <- maybe (ExceptT $ return (Left NoSuchUser)) return (Map.lookup domain users)
     if password == userpw
       then return domain
       else ExceptT $ return (Left WrongPassword)

userLogin :: IO (Either LoginError Text)
userLogin = do
  token <- getToken
  case token of
    Right domain ->
      case Map.lookup domain users of
        Just userpw -> do
          T.putStrLn "Enter password:"
          password <- T.getLine

          if userpw == password
             then return token

             else return (Left WrongPassword)
        Nothing -> return (Left NoSuchUser)
    left -> return left

main :: IO ()
main = do
  putStrLn "hello world"
