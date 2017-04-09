{-|
Module      : Grid
Description : d-dimentional cubic lattices

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Functions defining d-dimentional cubic lattices by giving adjacent vertices and edges of vertex
following conventional vertex labeling.

- L: linear size
- d: dimensionality (2: square, 3: cubic, >3: hypercubic)
 -}

module Grid
    ( Vertex
    , Edge
    , L
    , D
    , Direction (Forward, Backward)
    , Grid
    , pbcGrid
    , gridN
    , gridVertices
    , pbcEdges
    , pbcNeighbor
    ) where

import           Control.Concurrent     ()
import           Data.Natural

data Direction = Forward | Backward deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Vertex = Int
type Edge = (Vertex, Vertex)

type L = Natural
type D = Natural

gridN :: L -> D -> Int
gridN l d = fromEnum l ^ fromEnum d

gridVertices :: L -> D -> [Vertex]
gridVertices l d = [1 .. (fromEnum l ^ fromEnum d)]

-- | Returns the next vertex of v in the d dimension for a grid of side l
pbcNeighbor :: Vertex -> L -> D -> Direction -> Vertex 
pbcNeighbor v l' d' r  | r == Forward =
                      if not $ isBoundary v l d
                        then v + innerOffset
                        else v + pbcOffset
                    | r == Backward =
                      if not $ isBoundary (v - innerOffset) l d
                        then v - innerOffset
                        else v - pbcOffset
                    where innerOffset = l^(d - 1)
                          pbcOffset = - l^d + l^(d - 1)
                          isBoundary v l d = (l^d) - (l^(d - 1)) - mod (v - 1) (l^d) <= 0
                          l = fromEnum l'
                          d = fromEnum d'

type Grid = L -> D -> Vertex -> [Edge]

-- | Returns tuple giving forward and backward vertices of given vertex on a Toroidal Boundary Conditions (pbc) grid
pbcGrid :: Grid
pbcGrid l d v = (\r d -> case r of Forward ->  (v, pbcNeighbor v l d r)
                                   Backward -> (pbcNeighbor v l d r , v)
  ) <$> [Forward,Backward] <*> [1 .. d]

-- | List of edges of grid with periodic boundary conditions
pbcEdges :: L -> D -> [Edge]
pbcEdges l d = (\v j-> (v, pbcNeighbor v l j Forward)) <$> gridVertices l d <*> [1 .. d]
