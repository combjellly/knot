module Main where
import Parser
import AST

import Prelude

import Data.Either
import Text.Parsing.Parser 
import Data.Identity
import RenderEngine
import Effect
import Data.List
import Effect.Class.Console
import Effect.Aff
import Data.Time.Duration
import Effect.Class
import Effect.Ref
import Data.DateTime

-- 1. Record type refactor
-- 2. Render standalone (then calls render) functional and callable by js script on html

--let p = LoopElement (Loop 9.0 (Play "dog":Nil)) : Nil
-- new :: forall s. s -> Effect (Ref s)

-- engine record will evolve to be a record w many fields in it
type EngineRecord =  {
  pRef :: Ref Program
} 

main :: Effect Unit 
main = pure unit

launch :: Effect EngineRecord 
launch wStart = do 
  pRef <- new $ LoopElement (Loop 2.0 (Play "cat":Nil)) : Nil
  _ <- launchAff $ crudeScheduler 0.03 pRef
  let er =  
  pure er


-- https://hackage.haskell.org/package/tempi-1.0.2.1/src/Data/Tempo.hs
-- function that calls scheduler. js function that maybe wakes up and provides rendered stuff +engine record. 
-- scheduler (timing mechanism!) prolly should be external. can use 250ms, that's ok. 

-- render :: {- DateTime -> DateTime -> Tempo -> -} EngineRecord -> Effect Unit
 -- render er = do what crudeScheduler JUST EngineRecord. everything (wStart wEnd is stored in our EngineRecord)

-- will be called every 1/4s on js side
-- renderStandalone :: EngineRecord -> Effect Unit
  --  currentTime = what is time now
  --  what's tempo?, or tempo may be hardcoded
  --  what's the time in cycles. combines tempo and current time. according to tempo, how many cycles have passed?
  --  what is last time we rendered. Last windowEnd
  --  do we need to render or not. has enough time passed since last rendering. if there is rendering,
  -- then need to calculate window size. Store it in EngineRecord

--parser is run from js, takes wStart, munches it and makes it enginerecord. then
--it is rendered using the function below. 
render :: EngineRecord -> Effect Unit
render er = do
  let wEnd = wStart + 0.252
  liftEffect $ log $ show er.wStart <> " "<> show wEnd
  p <- liftEffect $ read $ er.pRef
  let es = renderEvents p er.wStart er.wEnd
  liftEffect $ printToConsole es 
  delay $ Milliseconds 252.0
  scheduler er

crudeScheduler :: Cycles -> Ref Program -> Aff Cycles
crudeScheduler wStart pRef = do
-- troubleshoot some more by plugging diff numbers. general. 
  let wEnd = wStart + 0.252
  liftEffect $ log $ show wStart <> " "<> show wEnd
  p <- liftEffect $ read $ pRef
  let es = renderEvents p wStart wEnd
  liftEffect $ printToConsole es 
  -- using delays may result in timing innacuracy. 250.0 is good. maybe implement adaptive tim 
  delay $ Milliseconds 252.0 --:: Aff Unit
  crudeScheduler wEnd p --pRef

printToConsole :: List SampleEvent -> Effect Unit
printToConsole es = log (show es)

everyFunc :: EngineRecord -> Number -> String -> Effect String 
everyFunc pRef x


engineLog :: Ref Program -> Cycles -> Cycles -> EngineRecord
engineLog pRef wStart wEnd = EngineRecord {pRef : pRef, wStart: wStart, wEnd:wEnd}

parse :: EngineRecord -> String -> Effect String
parse pRef x = case (runParser x program) of
    Left err -> pure $ show err 
    Right p -> do 
      write p pRef
      pure $ show "success"

