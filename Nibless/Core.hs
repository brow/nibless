module Nibless.Core where

import Data.Map as Map

type Identifier = String

type Style = Map.Map Identifier Value

data Value = NSString String | Expression String deriving Show

data View = View  { subclass :: Maybe Identifier
                  , outlet :: Maybe Identifier
                  , style :: Maybe Style
                  , subviews :: [View]
                  } deriving Show                

