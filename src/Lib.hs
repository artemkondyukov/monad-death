{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad.Except   (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Data.String            (IsString (..))

class MonadDeath m where
  throwString :: String -> m a

instance IsString e => MonadDeath (Either e) where
  throwString = Left . fromString

instance MonadDeath Maybe where
  throwString = const Nothing

instance (IsString e, Monad m) => MonadDeath (ExceptT e m) where
--  throwString :: String -> ExceptT e m a
  throwString = ExceptT . pure . Left . fromString

polymorphicDeath :: MonadDeath m => m ()
polymorphicDeath = do
  throwString "I have failed :("

maybeDeath :: Maybe ()
maybeDeath = do
  a <- 1 `lookup` [(1, 2)]
  polymorphicDeath

eitherDeath :: Either String ()
eitherDeath = do
  polymorphicDeath

data FancyException = FE1 | FE2 String | FE3 Int deriving Show

instance IsString FancyException where
  fromString "FE1"                      = FE1
  fromString (reads -> [(n :: Int, _)]) = FE3 n
  fromString s                          = FE2 s

exceptTIODeath :: ExceptT FancyException IO ()
exceptTIODeath = do
  line <- liftIO getLine
  liftIO $ print line
  polymorphicDeath

someFunc :: IO ()
someFunc = do
  print maybeDeath
  print eitherDeath
  v <- runExceptT $ exceptTIODeath
  print v
