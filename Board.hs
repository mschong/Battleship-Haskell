module Board where
{-# LANGUAGE ParallelListComp #-}

--Board module created by: Marina Chong, Jessica Dozal, Jose de la Rosa

--Creates a board with zeros of size n
mkBoard :: Int -> [[Int]]
mkBoard n = replicate n (replicate n 0)

--Checks if all ships are sunk
isGameOver :: [[Int]] -> Bool
isGameOver board = checkList (concat board) where 
    checkList [] = True
    checkList (h:t) | h > 0 = False 
                    | otherwise = checkList t
 
--Checks if ship fits in the given coordinates                    
isShipPlaceable :: Int -> Int -> Int -> Bool -> [[Int]] -> Bool
isShipPlaceable n x y dir board | length board < n = False
                            | x < 0 = False 
                            | y < 0 = False 
                            | x > length board = False
                            | y > length board = False
                            | dir == True && (n+x) > length board = False
                            | dir == False && (n+y) > length board = False
                            | otherwise = isOccupied n x y dir board where
                                isOccupied n x y dir board | n == 0 = True
                                                           | (board !! y !! x) > 0 = False
                                                           | dir == True = isOccupied (n-1) (x+1) y dir board 
                                                           | dir == False = isOccupied (n-1) x (y+1) dir board
                                                           | otherwise = False


--Places a ship in the board
placeShip :: Int -> Int -> Int -> Bool -> [[Int]] -> [[Int]]
placeShip n x y dir board = placeShipBackground n x y dir 0 n board

placeShipBackground :: Int -> Int -> Int -> Bool -> Int -> Int -> [[Int]] -> [[Int]]
placeShipBackground n x y dir currY size [] = [[]]
placeShipBackground n x y dir currY size (h:t) | size == 0 = (h:t)
											                         | (currY == y && dir == True) = placeShipBackground n (x+1) y dir currY (size-1) (replaced:t)
						       				                     | (currY == y) = (copyReplace n x 0 h): placeShipBackground n x (y+1) dir (currY+1) (size-1) t
						        			                     | otherwise = h: placeShipBackground n x y dir (currY + 1) (size) (t) 
                                               where replaced = (copyReplace n x 0 h)

--Displays board
boardToStr marker board = mapM_ (putStrLn . marker) board

--Changes square to a string representation. Takes board with hidden ships.
--If a ship is hit, changes the square to an X. If the board is hit, changes to square to O
sqToStr [] = []
sqToStr (h:t) = (showShots h) ++ " " ++ sqToStr t 
showShots n = if n == -1 then id "O" else if n < -1 then id "X" else id "."
 
 --Changes square to a string representation. Takes board with visible ships.
--If a ship is hit, changes the square to an X. If the board is hit, changes to square to O
sqToStrCheat [] = []
sqToStrCheat (h:t)  = (showCheatShots h) ++ " " ++ sqToStrCheat t
showCheatShots n = if n == 0 then id "." else if n == -1 then id "O" else if n < -1 then id "X" else show n

--Takes a list and return a new list with the given symbol inserted at position x
copyReplace :: Int -> Int -> Int -> [Int] -> [Int]
copyReplace symbol x currX [] = []
copyReplace symbol x currX (h:t) | (x==currX) = symbol: copyReplace symbol (x) (currX+1) (t)
							                 | otherwise = h: copyReplace symbol x (currX+1) (t)


--Checks if a square in te board is hit
isHit :: Int -> Int -> [[Int]] -> Bool
isHit x y [] = False
isHit x y board | (board !! y !! x) == -2 = True
				| otherwise = False


--Sends a shot to a square in the board
hitBoard :: Int -> Int -> [[Int]] -> [[Int]]
hitBoard x y board | (x < 0) || (y < 0) = board
				   | (x > length board) || (y > length board) = board
				   | isHit x y board ==  True = board
				   | otherwise = hitBoardBackground x y 0 board


hitBoardBackground :: Int -> Int -> Int -> [[Int]] -> [[Int]]
hitBoardBackground x y currY (h:t) | (currY == y) && (((h:t) !! 0 !! x) > 0) = (copyReplace (-2) x 0 (h)) : t
                                   | (currY == y) && (((h:t) !! 0 !! x) == 0) = (copyReplace (-1) x 0 (h)) : t
								                   | otherwise = h : hitBoardBackground x y (currY+1) (t)
