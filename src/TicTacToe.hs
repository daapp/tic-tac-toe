module TicTacToe (start, play, Board, Move, GameState, Player(..)) where

import           Data.Array
import           Data.List  (maximumBy)

data Player = X | O | Nobody deriving (Eq, Show)
type Move = Int
type Board = (Array Int Player, Player)
type MoveState = (Move, GameTree)
newtype GameTree = Tree (Board, [MoveState])
  deriving Show

-- | Maybe Player == Just Nobody -- ничья
-- | Maybe Player == Just player -- выиграл player
-- | Maybe Player == Nothing -- игра не окончена
type GameState = (Board, Maybe Player)

-- | Список индексов для полей игровой доски
indexes = [1..9]

-- | Создаёт пустую доску. Первый ход за X
createBoard :: Board
createBoard = (array (1,9) [(i, Nobody) | i <- indexes], X)

-- | изменяет состояние игрового поля согласно указанного хода
makeMove :: Board -> Move -> Board
makeMove (board, X) move = (board // [(move, X)], O)
makeMove (board, O) move = (board // [(move, O)], X)

-- кэширование вычисления
staticEval :: Board -> Player -> Int
staticEval board player
  | wwb == player = 1
  | wwb == Nobody = 0
  | otherwise = -1
  where wwb = winner board

-- | Возвращает список клеток доски, доступных для хода
getMoves :: Board -> [Move]
getMoves board@(field, _) =
  if winner board /= Nobody
  then []
  else filter (\i -> field ! i == Nobody) indexes

-- | Проверяет все возможные комбинации 3-в-ряд и возвращает победителя или Nobody, если такового нет.
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

-- | Возвращает дерево перебора всех возможный вариантов партий
createGameTree :: Board -> GameTree
createGameTree board = Tree (board, map (\m -> (m, createGameTree (makeMove board m))) (getMoves board))

-- | Выбор наилучшего хода для игрока player из дерева игры не глубже уровня вложенности depth.
-- | Возвращает (индекс поля, оценка хода)
-- | Оценка хода - чем больше значение, тем лучше ход
getBestMove :: Int -> Player -> GameTree -> (Move, Int)
getBestMove depth player (Tree (_, moveStates)) = getBest $ map aux moveStates
    where
      -- лучший ход тот, у которого максимальная оценка
      getBest = maximumBy (\(_, a) (_, b) -> a `compare` b)
      -- оцениваем ход с помощью альфа-бета отсечения.
      -- -1'000'000 и 1'000'000 - граничные значения \alpha и \beta
      aux (move, gameTree) = (move, getPayOffAB depth player (-1000000) 1000000 gameTree)

-- |
getPayOffAB :: Int -> Player -> Int -> Int -> GameTree -> Int
-- | поиск в глубину закончен - определяем победителя
getPayOffAB 0 player _     _    (Tree (board, _))          = staticEval board player
-- | свободные клетки закончились - определяем победителя
getPayOffAB depth player _     _    (Tree (board, []))         = staticEval board player
-- | рекурсивный поиск в глубину с помощью альфа-бета отсечения
getPayOffAB depth player alpha beta (Tree (board, moveStates)) = searchAB alpha beta moveStates
    where currentTurn = snd board

          searchAB alpha beta [] = if player == currentTurn then alpha else beta
          searchAB alpha beta ((_, gameTree):moveStates) =
            let val = getPayOffAB (depth-1) player alpha beta gameTree
            in if player == currentTurn then
                   if val >= beta
                   then val
                   else searchAB (max alpha val) beta moveStates
               else
                   if alpha >= min beta val
                   then val
                   else searchAB alpha (min beta val) moveStates

----------------------------------

-- | Возращает начальное состояние игры до первого хода.
start :: GameState
start = (createBoard, Nothing)

-- | Игрок X делает ход.
-- | Возвращает состоянии игры после ответного хода игрока O.
-- | Если результат == (_, Just player), то игра окончена (нет свободных полей) победой игрока player
play :: Board -> Move -> GameState
play board move = (board3, gameResult)
  where
    -- состояние доски после хода O. Если нет свободных полей, то возвращается состояние доски после хода X
    board3 = if null $ getMoves board2
             then board2
             -- вычисление наилучшего хода для игрока O после хода X на глубину в 5 ходов.
             else let (best, payoff) = getBestMove 5 O (createGameTree board2)
                  in makeMove board2 best

    board2 = makeMove board move -- состояние доски после хода X

    gameResult
      | null $ getMoves board3 = Just win --  пустых клеток больше нет, вычисляем окончание игры (победитель или ничья)
      | win == Nobody = Nothing -- пустые клетки есть, но победителя нет (Nobody), возвращаем Nothing - можно продолжать игру
      | otherwise = Just win -- возвращаем победителя - игра завершилась до заполнения всего поля.

    win = winner board3
