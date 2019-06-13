---
toc: 1
numbersections: true
geometry: margin=2.5cm
title: Programming Languages (Project 1)
author: David Aron Hartig Kaan (dakaa16)
date: November 13, 2017
abstract: |
    The goal of this project is ...
---

\newpage

The Kalaha game with parameters $(n,m)$
====

\begin{code}
module Kalaha where
import Data.List

type PitCount   = Int
type StoneCount = Int
data Kalaha     = Kalaha PitCount StoneCount deriving (Show, Read, Eq)

type KPos       = Int
type KState     = [Int]
type Player     = Bool
\end{code}


The function `startStateImpl`
----

\begin{code}
startStateImpl :: Kalaha -> KState
startStateImpl (Kalaha n m) = replicate n m ++ [0] ++ replicate n m ++ [0]
\end{code}


The function `movesImpl`
----
where x = if p == True then
https://stackoverflow.com/questions/33376330/get-the-element-at-index-elemindex-in-a-list
Maybe int ^^^^
\begin{code}
movesImpl :: Kalaha -> Player -> KState -> [KPos]
movesImpl g p s = [elemIndex 3 s]
  where x = [6,0,4,0,1,2,0]
\end{code}

valueImpl (Kalaha 6 6) (startStateImpl(Kalaha 6 6))

The function `valueImpl`
----
\begin{code}
valueImpl :: Kalaha -> KState -> Double
valueImpl g s = fromIntegral(s!!(count*2+1)) - fromIntegral(s!!count)
    where count = (div (length s) 2) - 1


\end{code}

The function `moveImpl`
----


\begin{code}
moveImpl :: Kalaha -> Player -> KState -> KPos -> (Player,KState)
moveImpl g p s xs = undefined
\end{code}

The function `showGameImpl`
----


\begin{code}
showGameImpl :: Kalaha -> KState -> String
showGameImpl g@(Kalaha n m) xs = undefined
\end{code}


Trees
====

\begin{code}
data Tree m v  = Node v [(m,Tree m v)] deriving (Eq, Show)
\end{code}

The function `takeTree`
----

\begin{code}
takeTree :: Int -> Tree m v -> Tree m v
takeTree = undefined
\end{code}

The Minimax algorithm
====
\begin{code}
data Game s m = Game {
    startState    :: s,
    showGame      :: s -> String,
    move          :: Player -> s -> m -> (Player,s),
    moves         :: Player -> s -> [m],
    value         :: Player -> s -> Double}

kalahaGame :: Kalaha -> Game KState KPos
kalahaGame k = Game {
    startState = startStateImpl k,
    showGame   = showGameImpl k,
    move       = moveImpl k,
    moves      = movesImpl k,
    value      = const (valueImpl k)}

startTree :: Game s m -> Player -> Tree m (Player,Double)
startTree g p = tree g (p, startState g)

\end{code}
The function `tree`
----


\begin{code}
tree      :: Game s m -> (Player, s) -> Tree m (Player, Double)
tree = undefined

\end{code}
The function `minimax`
----


\begin{code}
minimax   :: Tree m (Player, Double) -> (Maybe m, Double)
minimax = undefined
\end{code}

The function `minimaxAlphaBeta`
----

\begin{code}
type AlphaBeta = (Double,Double)

minimaxAlphaBeta :: AlphaBeta -> Tree m (Player, Double) -> (Maybe m, Double)
minimaxAlphaBeta = undefined
\end{code}

Testing and sample executions
====
