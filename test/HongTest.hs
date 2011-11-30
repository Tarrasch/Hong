{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module HongTest (specs) where

import Test.Hspec
import Test.Hspec.HUnit ()

import Prelude hiding (reverse)

specs :: [Spec]
specs = describe "Hong"
  [ it "Tests the test framework" True
  ]
