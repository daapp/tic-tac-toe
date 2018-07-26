import qualified TicTacToe as TTT

main = do
  let state@(board, result) = TTT.start
  print state
  play board

play board = do
  putStrLn "?"
  s <- getLine
  let (board', result') = TTT.play board (read s)
  print board'
  if isNothing result'
  then play board'
  else print result'
