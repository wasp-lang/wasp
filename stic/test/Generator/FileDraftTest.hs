module Generator.FileDraftTest where

import qualified Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Generator.FileDraft

-- TODO: Write some tests? We have to mock IO, take a look at
--   https://stackoverflow.com/a/7374754/1509394 . But in their example they
--   define some small interface from IO that they need, I don't know what I need
--   because I am also calling external libraries as Mustache! How can I go
--   around that?
