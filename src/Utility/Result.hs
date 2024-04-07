{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Utility.Result
  ( Result (..),
    fromEither,
    unwrap,
    toMaybe,
    maybeErr,
    mapErr,
    (<!>),
    collect,
  )
where

import Data.Maybe (mapMaybe)

data Result err ok = Err err | Ok ok deriving (Show, Eq)

-- IMPLEMENT -------------------------------------------------------------------

instance Functor (Result err) where
  fmap :: (a -> b) -> Result err a -> Result err b
  fmap _ (Err err) = Err err
  fmap f (Ok ok) = Ok $ f ok

instance Applicative (Result err) where
  pure :: a -> Result err a
  pure = Ok

  liftA2 :: (a -> b -> c) -> Result err a -> Result err b -> Result err c
  liftA2 _ (Err err) _ = Err err
  liftA2 _ _ (Err err) = Err err
  liftA2 op (Ok x) (Ok y) = Ok $ op x y

instance Monad (Result err) where
  (>>=) :: Result err a -> (a -> Result err b) -> Result err b
  (Err err) >>= _ = Err err
  (Ok ok) >>= f = f ok

instance MonadFail (Result String) where
  fail :: String -> Result String a
  fail = Err

-- CONSTRUCT -------------------------------------------------------------------

fromEither :: Either err ok -> Result err ok
fromEither (Left err) = Err err
fromEither (Right ok) = Ok ok

-- DECONSTRUCT -----------------------------------------------------------------

unwrap :: Result a a -> a
unwrap (Err err) = err
unwrap (Ok ok) = ok

toMaybe :: Result err a -> Maybe a
toMaybe (Err _) = Nothing
toMaybe (Ok ok) = Just ok

maybeErr :: Result a ok -> Maybe a
maybeErr (Ok _) = Nothing
maybeErr (Err err) = Just err

-- MODIFY ----------------------------------------------------------------------

mapErr :: (err -> err') -> Result err ok -> Result err' ok
mapErr _ (Ok ok) = Ok ok
mapErr f (Err err) = Err $ f err

infixr 5 <!>

(<!>) :: Result err ok -> (err -> err') -> Result err' ok
(<!>) = flip mapErr

-- COLLECT ---------------------------------------------------------------------

collect :: [Result err ok] -> Result [err] [ok]
collect results = if null errs then Ok oks else Err errs
  where
    errs = mapMaybe maybeErr results
    oks = mapMaybe toMaybe results
