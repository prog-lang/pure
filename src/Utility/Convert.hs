{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utility.Convert (Into (..), TryInto (..)) where

import Utility.Result (Result (Ok))

class Into a b where
  into :: a -> b

instance (Functor f, Into a b) => Into (f a) (f b) where
  into :: f a -> f b
  into = fmap into

class TryInto a err ok where
  tryInto :: a -> Result err ok

instance (Into a b) => TryInto a err b where
  tryInto :: a -> Result err b
  tryInto = Ok . into
