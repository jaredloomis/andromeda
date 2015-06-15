module Andromeda.Lambda.Mesh where

import Shader
import VertexBuffer

data Mesh = Mesh [VertexBuffer]
  deriving (Show, Eq)
