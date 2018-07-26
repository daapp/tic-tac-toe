module TicTacToe (start, play, Board, Move, GameState, Player(..)) where

import Data.Array

data Player = X | O | Nobody deriving (Eq, Show)
type Move = Int
type Board = (Array Int Player, Player)
type MoveState = (Move, GameTree)
newtype GameTree = Tree (Board, [MoveState])


-- | Maybe Player == Just Nobody -- draw
-- | Maybe Player == Just player -- player is winner
-- | Maybe Player == Nothing -- game not finished
type GameState = (Board, Maybe Player)


indexes = [1..9]

createBoard :: Board
createBoard = (array (1,9) [(i, Nobody) | i <- indexes], X)
 
makeMove :: Board -> Move -> Board
makeMove (board, X) move = (board // [(move, X)], O)
makeMove (board, O) move = (board // [(move, O)], X)

staticEval :: Board -> Player -> Int
staticEval board player
  | wwb == player = 1
  | wwb == Nobody = 0
  | otherwise = -1
  where wwb = winner board

-- | Return list of empty fields
getMoves :: Board -> [Move]
getMoves board@(field, _) =
  if winner board /= Nobody
  then []
  else filter (\i -> field ! i == Nobody) indexes

winner :: Board -> Player
winner (b, t)
  | row [1,2,3] = b!1
  | row [4,5,6] = b!4
  | row [7,8,9] = b!7
  | row [1,4,7] = b!1
  | row [2,5,8] = b!2
  | row [3,6,9] = b!3
  | row [1,5,9] = b!1
  | row [3,5,7] = b!3
  | otherwise = Nobody
  where row [x, y, z] = b!x == b!y && b!y == b!z

createGameTree :: Board -> GameTree
createGameTree board = Tree (board, map (\m -> (m, createGameTree (makeMove board m))) (getMoves board))
 
getBestMove :: Int -> Player -> GameTree -> (Move, Int)
getBestMove d player (Tree (_, moveStates)) = getBest $ map aux moveStates
    where aux (move, gameTree) = (move, getPayOffAB d player (-1000000) 1000000 gameTree)
          getBest (m:ms) = foldl auxcomp m ms
          auxcomp (m1, s1) (m2, s2) = if s1 >= s2 then (m1, s1) else (m2, s2)

getPayOffAB :: Int -> Player -> Int -> Int -> GameTree -> Int
getPayOffAB 0 player _     _    (Tree (board, _))          = staticEval board player
getPayOffAB d player _     _    (Tree (board, []))         = staticEval board player
getPayOffAB d player alpha beta (Tree (board, moveStates)) = searchAB alpha beta moveStates
    where currentTurn = snd board

          searchAB alpha beta [] = if player == currentTurn then alpha else beta
          searchAB alpha beta ((_, gameTree):moveStates) =
            let val = getPayOffAB (d-1) player alpha beta gameTree
            in if player == currentTurn then
                   if val >= beta
                   then val
                   else searchAB (max alpha val) beta moveStates
               else
                   if alpha >= min beta val
                   then val
                   else searchAB alpha (min beta val) moveStates

----------------------------------

start :: GameState
start = (createBoard, Nothing)

play :: Board -> Move -> GameState
play board move =
    let board2 = makeMove board move
        board3 = if null $ getMoves board2
                 then board2
                 else let (best, payoff) = getBestMove 5 O (createGameTree board2)
                      in makeMove board2 best
    in ( board3,
         if null $ getMoves board3
         -- no more moves
         then Just $ winner board3
         -- there are some moves
         else let win = winner board3
              in if win == Nobody
                 then Nothing
                 else Just win
       )
