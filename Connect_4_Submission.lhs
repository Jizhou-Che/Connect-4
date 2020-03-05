G52AFP Coursework 1 - Connect 4 Game
   
Jizhou Che
scyjc1@nottingham.ac.uk

----------------------------------------------------------------------

Library imports.

> import Data.Char
> import Data.List
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


This function tries to make a move for a player on a specified column of the board.

It finds the row the piece will drop to,
inserts the piece at the specified column,
then inserts the row back to the board.
Moves to already-full columns give Nothing.

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


This function chops a list into sized pieces.

> chop :: Int -> [a] -> [[a]]
> chop n [] = []
> chop n xs = take n xs : chop n (drop n xs)


This function gets the diagonals of the board on one single direction.

It works for matrices of any size, given the number of columns.

> diags :: Board -> [Row]
> diags b = concat $ split (transpose (chop (cols + 1) (concat b))) cols
>           where split [] _ = []
>                 split rs n = (take n (head rs) : chop cols (drop n (head rs))) : split (tail rs) (n - 1)

Example:

01 02 03        01 02 03 04        01 05 09 13 17 21        01 05 09 | 13 17 21
04 05 06        05 06 07 08        02 06 10 14 18           02 06 | 10 14 18
07 08 09        09 10 11 12        03 07 11 15 19           03 | 07 11 15 | 19
10 11 12        13 14 15 16        04 08 12 16 20           04 08 12 | 16 20
13 14 15        17 18 19 20
16 17 18        21
19 20 21

   (0)     ->       (1)       ->          (2)          ->           (3)

(0): Original matrix.
(1): Concatenate then chop into pieces of size (cols + 1).
(2): Perform a transpose.
(3): Split each row into pieces of size (cols) with stair-stepping prefixes.


This function gets the diagonals of the board on both directions.

The diagonals on the opposite direction is achieved by reversing the rows first.

> diagonals :: Board -> [Row]
> diagonals b = diags b ++ diags (reverse b)


This function checks whether a row satisfies the winning conditon.

> connected :: Row -> Bool
> connected r = isInfixOf (replicate win X) r || isInfixOf (replicate win O) r


This function checks whether the board satisfies the winning condition.

> winning :: Board -> Bool
> winning b = any connected (horizontals b ++ verticals b ++ diagonals b)


This function checks whether the board satisfies the draw condition.

The game draws if the board is full but nobody wins.

> draw :: Board -> Bool
> draw b = (not . winning) b && and [full n b | n <- [0 .. cols - 1]]


This function checks who is next to play on the board.

It assumes that X will play the first move.

> turn :: Board -> Player
> turn b | (length . filter (== X)) (concat b) <= (length . filter (== O)) (concat b) = X
>        | otherwise = O

----------------------------------------------------------------------

Minimax algorithm.


The structure of a general game tree.

> data Tree a = Node a [Tree a] deriving Show


This function expands a board to its possible next moves.

Winning boards cannot be further expanded.
Invalid moves to already-full columns are eliminated.

> expand :: Board -> [Board]
> expand b | winning b = []
>          | otherwise = map (\(Just b') -> b') $ filter (\mb' -> mb' /= Nothing) [move (turn b) n b | n <- [0 .. cols - 1]]


This function grows the game tree to the next level.

It simply expands all the leaves.

> grow :: Tree Board -> Tree Board
> grow (Node b []) = Node b [Node b' [] | b' <- expand b]
> grow (Node b ts) = Node b $ map grow ts


This function labels a game tree using the minimax algorithm.

Leaves are labelled with their winners if any, blank otherwise.
Internal nodes are labelled recursively, taking alternating min and max.

> label :: Tree Board -> Tree (Player, Board)
> label (Node b []) | winning b = case turn b of X -> Node (O, b) []
>                                                O -> Node (X, b) []
>                   | otherwise = Node (B, b) []
> label (Node b ts) = case turn b of X -> Node (maximum ls, b) lts
>                                    O -> Node (minimum ls, b) lts
>                                    where ls = map (\(Node (l, _) _) -> l) lts
>                                          lts = map label ts


This function gets the best options for the next move.

It grows the game tree to the specified depth, labels it,
selects the labelled neighbours of the root,
then selects the best group of boards possible (i.e. labelled the same as the root).

> options :: Board -> [Board]
> options b = map snd $ filter (\(p, _) -> p == (\(Node (pp, _) _) -> pp) lt) neighbours
>             where neighbours = map (\(Node o _) -> o) $ (\(Node _ ts) -> ts) lt
>                   lt = label (iterate grow (Node b []) !! depth)

----------------------------------------------------------------------

The main game.


The entry point of the game.

> main :: IO ()
> main = play blank


This function plays a round of the game.

> play :: Board -> IO ()
> play b = do
>   b1 <- hmove b ""
>   if winning b1 then do
>     showBoard b1
>     putStrLn $ show (turn b) ++ " wins!"
>   else if draw b1 then do
>     showBoard b1
>     putStrLn "Draw!"
>   else do
>     b2 <- cmove b1
>     if winning b2 then do
>       showBoard b2
>       putStrLn $ show (turn b1) ++ " wins!"
>     else if draw b2 then do
>       showBoard b2
>       putStrLn "Draw!"
>     else do
>       play b2


This function makes a computer move.

> cmove :: Board -> IO Board
> cmove b = do
>   showBoard b
>   putStrLn "Computer is thinking..."
>   let opts = options b
>   r <- randomRIO (0, length opts - 1)
>   return (opts !! r)


This function gets a human move.

> hmove :: Board -> String -> IO Board
> hmove b s = do
>   showBoard b
>   putStrLn s
>   x <- getCol $ show (turn b) ++ " is next: "
>   let mb' = move (turn b) x b
>   if mb' == Nothing then do
>     hmove b "That column is full!"
>   else do
>     let Just b' = mb'
>     return b'


This function gets a valid column number from the user.

> getCol :: String -> IO Int
> getCol s = do
>   putStr s
>   hFlush stdout
>   xs <- getLine
>   if xs == "" || not (all isDigit xs) || read xs < 0 || read xs >= cols then do
>     getCol "Please input a valid column number: "
>   else do
>     return $ read xs
