import           Data.List  (foldl', maximumBy)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (catMaybes, fromMaybe, listToMaybe, fromJust)
import           Data.Ord   (comparing)

testHoriz = M.fromList [((1,0),X),((1,1),X),((1,2),X),((1,3),X)]
testVert = M.fromList [((0,0),O),((1,0),O),((2,0),O),((3,0),O)]
testDiag1 = M.fromList [((0,0),X),((1,1),X),((2,2),X),((3,3),X)]
testDiag2 = M.fromList [((3,3),X),((2,2),X),((1,1),X),((0,0),X)]

horizToList = M.toList testHoriz

allTuples width height = [ ((x1,x2),G) | x1 <- [0..width], x2 <- [0..height]]

empti = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),G),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),G)]

splitXrow = M.fromList [((0,0),X),((0,1),G),((0,2),X),((0,3),X),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),G)]

vertMap = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),X),((1,0),G),((1,1),G),((1,2),G),((1,3),X),((2,0),G),((2,1),G),((2,2),G),((2,3),X),((3,0),G),((3,1),G),((3,2),G),((3,3),X)]

horzMap = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),G),((1,0),G),((1,1),G),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),G),((2,3),G),((3,0),X),((3,1),X),((3,2),X),((3,3),X)]

leftUpRight = M.fromList [((0,0),X),((0,1),G),((0,2),G),((0,3),G),((1,0),G),((1,1),X),((1,2),G),((1,3),G),((2,0),G),((2,1),G),((2,2),X),((2,3),G),((3,0),G),((3,1),G),((3,2),G),((3,3),X)]

leftDownRight = M.fromList [((0,0),G),((0,1),G),((0,2),G),((0,3),X),((1,0),G),((1,1),G),((1,2),X),((1,3),G),((2,0),G),((2,1),X),((2,2),G),((2,3),G),((3,0),X),((3,1),G),((3,2),G),((3,3),G)]


data Dir = L | R
	deriving (Eq, Ord, Enum)
 
data Player = X | O | G 
	deriving (Show, Eq, Ord, Enum)

data Board = Board 
	{ boardRows :: Integer 
	, boardColumns :: Integer 
	, boardTiles :: Map (Integer, Integer) Player 
	}

instance Show Board where
    show board@(Board rows columns _) = unlines $
        [ concat [show i | i <- [0 .. columns - 1]]
        ] ++
        [ [showTile (get row column board) | column <- [0 .. columns - 1]]
        | row <- [0 .. rows - 1]
        ]

showTile :: Maybe Player -> Char
showTile Nothing  = ' '
showTile (Just X) = 'X'
showTile (Just O) = 'O'
showTile (Just G) = 'G'

get :: Integer -> Integer -> Board -> Maybe Player
get row column = M.lookup (row, column) . boardTiles 

emptyBoard :: Integer -> Integer -> Board
emptyBoard rows columns = Board rows columns empti

-- getTiles :: Board -> Map ((Integer,Integer), Player)
getTiles board = (boardTiles board)

-- main :: IO()
-- main = mapM_ ask horizToList

ask :: ((Integer,Integer), Player) -> IO()
ask ((h,v),p) = do putStrLn (show p)

-- filtered :: Map ((Integer,Integer), Player) -> Player -> Map ((Integer,Integer), Player)
filterPlayers board player = M.filter (== player) board

common = M.filter (== O) testHoriz
gimme = M.filterWithKey (\(c,r) _ -> r == 2) empti
possib = M.toList (getRow (filterPlayers (boardTiles (emptyBoard 4 4)) G) 2)
what = M.toList (getColumn (filterPlayers testVert O) 4)

-- getRow :: Board -> Int -> Map ((Integer,Integer), Player)
getColumn board row = M.filterWithKey (\(c,r) _ -> r == row) board

-- getColumn :: Board -> Int -> Map ((Integer,Integer), Player)
getRow board col = M.filterWithKey (\(c,r) _ -> c == col) board


-- Recurse until you find 4, or the list is empty, if breaks with other player recurse again starting at 0 
checkHoriz [] p = False
checkHoriz (((a1,b1),p1):((a2,b2),p2):((a3,b3),p3):((a4,b4),p4):r) p 
	|  p1==p && p2==p && p3==p && p4==p = True
	| otherwise = checkHoriz (((a2,b2),p2):((a3,b3),p3):((a4,b4),p4):r) p 
checkHoriz (x:t) p = False

-- recurse over each row ** CHANGE do not filter to one player 
horizontal _ _ 0 = False
horizontal board player totalRowNum
-- checkHoriz is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkHoriz (M.toList (getRow (filterPlayers (boardTiles board) player) totalRowNum)) player = True
	| otherwise = horizontal board player (totalRowNum - 1)

-- recurse over each row ** Probably have to change the fact that im filtering out one player 
vertical _ _ 0 = False
vertical board player totalColNum
-- checkHoriz is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkHoriz (M.toList (getColumn (filterPlayers (boardTiles board) player) totalColNum)) player = True
	| otherwise = vertical board player (totalColNum - 1)

diagonal _ _ 0 = False
diagonal board player totalRowNum
-- checkHoriz is being passed getRow totalRownum from map given by filtering out player's tiles from given board
	| checkDiag (M.toList (filterPlayers (boardTiles board) player) ) player (boardTiles board) = True
	| otherwise = diagonal board player (totalRowNum - 1)


checkDiag [] _ _ = False
checkDiag (((x,y),z):t) p tiles 
	| x == 0 && verRight (x,y) p tiles = hasDiagonal ((x,y), z) tiles 3 Nothing


hasDiagonal :: ((Integer, Integer), Player) -> Map (Integer, Integer) Player -> Integer -> Maybe Dir -> Bool
hasDiagonal _ _ 0 _ = True
hasDiagonal ((x,y),z) tiles n dir
	| x == 0 && y < 3 && verRight (x,y) z tiles && dir == Nothing = hasDiagonal ((x+1, y+1), z) tiles (n-1) (Just R)
	| x == 3 && y < 3 && verLeft (x,y) z tiles && dir == Nothing = hasDiagonal ((x-1, y+1), z) tiles (n-1) (Just L)
	| dir == (Just R) && verRight (x,y) z tiles = hasDiagonal ((x+1, y+1), z) tiles (n-1) (Just R)
	| dir == (Just L) && verLeft (x,y) z tiles = hasDiagonal ((x-1, y+1), z) tiles (n-1) (Just L)


helper x y tiles p = fromJust (M.lookup (x-1,y+1) tiles) == p


verRight (x,y) p boardTiles
	| fromJust (M.lookup (x+1,y+1) boardTiles) == p = True
	| otherwise = False

verLeft (x,y) p boardTiles
	| fromJust (M.lookup (x-1,y+1) boardTiles) == p = True
	| otherwise = False


check :: Board -> Player -> Bool 
check board player 
	| horizontal board player 4 = True
	| vertical board player 4 = True
	| diagonal board player 4 = True
	| otherwise = False



 










