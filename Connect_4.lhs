G52AFP Coursework 1 - Connect 4 Game
   
Jizhou Che
scyjc1@nottingham.ac.uk

----------------------------------------------------------------------

Library imports.

> import Data.List

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


This function checks who is next to play on the board.
It is assumed that X will play the first move.

> turn :: Board -> Player
> turn b | (length . filter (== X)) (concat b) <= (length . filter (== O)) (concat b) = X
>        | otherwise = O

----------------------------------------------------------------------

Minimax algorithm.


The structure of the game tree.
NOTE: The parameterised type could be Board or (Board, Player), with Player marking the potential winner of this board.

> data Tree a = Node a [Tree a]

----------------------------------------------------------------------

Boards for testing purposes.

> testBoard :: Board
> testBoard = [[X, B, B, O, O, B, B],
>              [X, X, X, O, O, B, B],
>              [O, O, O, X, X, B, B],
>              [O, O, O, X, O, B, B],
>              [X, X, X, O, X, X, B],
>              [X, X, X, O, O, O, B]]
