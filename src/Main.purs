module Main where
import Parser
import AST

import Prelude

import Data.Either
import Text.Parsing.Parser 
import Data.Identity

import Effect



main :: Effect Unit
main = pure unit

type EngineRecord = Unit

launch :: Effect EngineRecord 
launch = pure unit

parse :: EngineRecord -> String -> Effect String
parse e x = pure $ case (runParser x program) of
    Left err -> show err
    Right x -> show x