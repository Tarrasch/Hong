{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module HongTest (specs) where

import Test.Hspec
import Test.Hspec.HUnit ()

import Prelude hiding (reverse)

specs :: [Spec]
specs = describe "Hong"
  [ it "Test the test framework" $ do return True
  ]
