G52AFP Coursework 1 - Connect 4 Game
   
Jizhou Che
scyjc1@nottingham.ac.uk

----------------------------------------------------------------------

Library imports.

> import Data.List
> import Text.Read
> import System.Random
> import System.IO

----------------------------------------------------------------------

For flexibility, we define constants for the row and column size of the
board, length of a winning sequence, and search depth for the game tree:

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'

----------------------------------------------------------------------

Utilities.


This is the initial board of the game.

> blank :: Board
> blank = replicate rows $ replicate cols B


This function checks whether the nth column of the board is full.

> full :: Int -> Board -> Bool
> full n b = head b !! n /= B


This function tries to make a move for player p on the nth column of the board.
TODO: More explanations on the logic.

> move :: Player -> Int -> Board -> Maybe Board
> move p n b | full n b = Nothing
>            | otherwise = Just $ init b1 ++ row ++ b2
>                          where row = [take n (last b1) ++ p : drop (n + 1) (last b1)]
>                                (b1, b2) = break (\row -> row !! n /= B) b


This function gets the rows of the board.

> horizontals :: Board -> [Row]
> horizontals = id


This function gets the columns of the board.

> verticals :: Board -> [Row]
> verticals = transpose


This function gets the diagonals of the board on both directions.
TODO: More explanations on the logic.

> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)

> diagonals1 :: Board -> [Row]
> diagonals1 b = concat $ split (transpose (chop (cols + 1) (concat b))) cols
>                where split [] _ = []
>                      split rs n = (take n (head rs) : chop cols (drop n (head rs))) : split (tail rs) (n - 1)

> diagonals2 :: Board -> [Row]
> diagonals2 b = concat $ split (transpose (chop (cols - 1) (concat b))) 1
>                where split [] _ = []
>                      split rs n = (take n (head rs) : chop cols (drop n (head rs))) : split (tail rs) (n + 1)

> diagonals0 :: Board -> [Row]
> diagonals0 b = diagonals1 b ++ diagonals2 b

> diagonals00 :: Board -> [Row]
> diagonals00 b = concat (split1 (transpose (chop (cols + 1) (concat b))) cols) ++ concat (split2 (transpose (chop (cols - 1) (concat b))) 1)
>                 where split1 [] _ = []
>                       split1 rs n = (take n (head rs) : chop cols (drop n (head rs))) : split1 (tail rs) (n - 1)
>                       split2 [] _ = []
>                       split2 rs n = (take n (head rs) : chop cols (drop n (head rs))) : split2 (tail rs) (n + 1)

http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#g:17
Check this out. LOL.


This function checks whether a row satisfies the winning conditon.

> connected :: Row -> Bool
> connected r = isInfixOf (replicate win X) r || isInfixOf (replicate win O) r


This function checks whether the board satisfies the winning condition.

> winning :: Board -> Bool
> winning b = any connected (horizontals b ++ verticals b ++ diagonals0 b)


This function checks whether the board satisfies the draw condition.

> draw :: Board -> Bool
> draw b = (not . winning) b && and [full n b | n <- [0 .. cols - 1]]


This function checks who is next to play on the board.
It is assumed that X will play the first move.

> turn :: Board -> Player
> turn b | (length . filter (== X)) (concat b) <= (length . filter (== O)) (concat b) = X
>        | otherwise = O

----------------------------------------------------------------------

Minimax algorithm.


The structure of a general game tree.

> data Tree a = Node a [Tree a] deriving Show


This function expands a board to its possible next moves.

> expand :: Board -> [Board]
> expand b | winning b = []
>          | otherwise = map (\(Just b') -> b') $ filter (\mb' -> mb' /= Nothing) [move (turn b) n b | n <- [0 .. cols - 1]]


This function grows the game tree to the next level.

> grow :: Tree Board -> Tree Board
> grow (Node b []) = Node b [Node b' [] | b' <- expand b]
> grow (Node b ts) = Node b $ map grow ts


This function labels a game tree with the potential winners.
TODO: More explanations on the logic.

> label :: Tree Board -> Tree (Player, Board)
> label (Node b []) | winning b = case turn b of X -> Node (O, b) []
>                                                O -> Node (X, b) []
>                   | otherwise = Node (B, b) []
> label (Node b ts) = case turn b of X -> Node (maximum ls, b) lts
>                                    O -> Node (minimum ls, b) lts
>                                    where ls = map (\(Node (l, _) _) -> l) lts
>                                          lts = map label ts


This function gets the labeled options of the next move.

> options :: Board -> [Board]
> options b = map snd $ filter (\(p, _) -> p == (\(Node (pp, _) _) -> pp) lt) neighbours
>             where neighbours = map (\(Node o _) -> o) $ (\(Node _ ts) -> ts) lt
>                   lt = label (iterate grow (Node b []) !! depth)

----------------------------------------------------------------------

The main game.


The entry point of the game.

> main :: IO ()
> main = selectMode


> selectMode :: IO ()
> selectMode = do
>   putStrLn "1: HUMAN - COMPUTER"
>   putStrLn "2: HUMAN - HUMAN"
>   putStr "> "
>   hFlush stdout
>   x <- getLine
>   let mi = readMaybe x :: Maybe Int
>   if mi == Nothing then do
>     putStrLn "Invalid option!"
>     selectMode
>   else do
>     if mi == Just 1 then do
>       putStrLn ""
>       selectPlayer
>     else if mi == Just 2 then do
>       putStrLn ""
>       playHH blank ""
>     else do
>       putStrLn "Invalid option!"
>       selectMode


> selectPlayer :: IO ()
> selectPlayer = do
>   putStrLn "1: PLAY AS X"
>   putStrLn "2: PLAY AS O"
>   putStr "> "
>   hFlush stdout
>   x <- getLine
>   let mi = readMaybe x :: Maybe Int
>   if mi == Nothing then do
>     putStrLn "Invalid option!"
>     selectPlayer
>   else do
>     if mi == Just 1 then do
>       putStrLn ""
>       b <- humanMove blank ""
>       playHC b ""
>     else if mi == Just 2 then do
>       putStrLn ""
>       playHC blank ""
>     else do
>       putStrLn "Invalid option!"
>       selectPlayer


> humanMove :: Board -> String -> IO Board
> humanMove b s = do
>   putStrLn ""
>   showBoard b
>   putStrLn s
>   putStr $ show (turn b) ++ " is next: "
>   hFlush stdout
>   x <- getLine
>   putStrLn ""
>   let mi = readMaybe x :: Maybe Int
>   if mi == Nothing then do
>     humanMove b "Please input a column number!"
>   else do
>     let Just i = mi
>     if i < 0 || i >= cols then do
>       humanMove b "Column number out of bounds!"
>     else do
>       let mb' = move (turn b) i b
>       if mb' == Nothing then do
>         humanMove b "That column is full!"
>       else do
>         let Just b' = mb'
>         return b'


> playHC :: Board -> String -> IO ()
> playHC b s = do
>   showBoard b
>   putStrLn "Computer is thinking..."
>   b' <- cmove b
>   if winning b' then do
>     showBoard b'
>     putStrLn $ show (turn b) ++ " wins!"
>   else if draw b' then do
>     showBoard b'
>     putStrLn "Draw!"
>   else do
>     bb <- humanMove b' ""
>     if winning bb then do
>       showBoard bb
>       putStrLn $ show (turn b') ++ " wins!"
>     else if draw bb then do
>       showBoard bb
>       putStrLn "Draw!"
>     else do
>       playHC bb ""


> playHH :: Board -> String -> IO ()
> playHH b s = do
>   b' <- humanMove b s
>   if winning b' then do
>     showBoard b'
>     putStrLn $ show (turn b) ++ " wins!"
>   else if draw b' then do
>     showBoard b'
>     putStrLn "Draw!"
>   else do
>     playHH b' ""


> cmove :: Board -> IO Board
> cmove b = do
>   let opts = options b
>   r <- randomRIO (0, length opts - 1)
>   let b' = opts !! r
>   return b'
