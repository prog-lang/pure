{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Result
  ( Result (..),
    fromEither,
    unwrap,
    mapErr,
    (<!>),
  )
where

data Result err ok = Ok ok | Err err deriving (Show, Eq)

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

-- MODIFY ----------------------------------------------------------------------

mapErr :: (err -> err') -> Result err ok -> Result err' ok
mapErr _ (Ok ok) = Ok ok
mapErr f (Err err) = Err $ f err

infixr 5 <!>

(<!>) :: Result err ok -> (err -> err') -> Result err' ok
(<!>) = flip mapErr