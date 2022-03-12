module AST where

import Prelude
import Data.Monoid.Dual
import Data.List
import Data.Show
import Data.Either



data Loop = Loop (Either Int Number) (List Action)

data Action = Action String 


instance showLoop :: Show Loop where
    show (Loop n xs) = "(Every " <> show n <> " " <> show xs <> ")"


instance showAction :: Show Action where
    show (Action x) = "(play. " <> show x <> ")"

