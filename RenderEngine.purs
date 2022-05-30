module RenderEngine where

import AST
import Parser
import Main

import Prelude

import Data.Int (toNumber)
import Data.List
import Data.Either
import Math
import Control.Plus (empty)


--EventBank for troubleshooting
type EventBank = (List Event) 
data Event = Number

type SampleEvent = {
  sampleBank :: String,
  when :: Number
  }


ic :: Number -> Number -> Number -> EventBank -> EventBank
ic wStart wEnd n _= 
  let 
    l = (Nil)
  in
    snoc l wStart 
    ic wStart wEnd n l  

ic wStart wEnd n l =  
  if wStart <= wEnd then
  let 
    x = wStart + n
  in
    snoc l wStart
    ic x wEnd n l  
  else
    show l