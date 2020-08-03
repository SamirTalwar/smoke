{-# LANGUAGE DeriveFunctor #-}

module Test.Smoke.Types.Assert where

import Data.Default

newtype Assert a = AssertEqual a
  deriving (Eq, Show, Functor)

instance Default a => Default (Assert a) where
  def = AssertEqual def
