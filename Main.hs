module Main where
	
import System.IO 
import System.Random
import System.IO.Unsafe
import Board

--Main module created by: Marina Chong, Jessica Dozal, Jose de la Rosa

--Taken from http://stackoverflow.com/questions/16208249/risks-of-using-unsafeperformio-on-randomio
rand :: Bool -> Int
rand x = unsafePerformIO randomIO

--Creates a random number between 0 and n
generateRandom :: Int -> Int
generateRandom n = ((mod (rand True) n))

--Takes a list with size of each ship and places them randomly on the board
placeShips [] board = board
placeShips (h:t) board = do 
						 show x
						 show y
						 show dir
						 if isShipPlaceable h x y (dir == 1) board
						 then placeShips t (placeShip h x y (dir==1) board)
						 else placeShips (h:t) board where
						 {x = generateRandom 10; y = generateRandom 10; dir = generateRandom 2}


--Gets shot coordinates from the user and shoots at the board and checks if the all the ships are sunk
--Modified from Dr. Cheon's sample code
getXY marker board = do
       putStrLn "Enter a positive x value"
       line <- getLine
       let parsed = reads line :: [(Int, String)] in
         if length parsed == 0
         then getX' board
         else let (x, _) = head parsed in
           if x >= 0 && x < length board
           then do
       			putStrLn "Enter a positive y value"
       			line <- getLine
       			let parsed = reads line :: [(Int, String)] in
         		  if length parsed == 0
         		  then getX' board
         		  else let (y, _) = head parsed in
           		  	if y >= 0 && y < length board
           		  	then do 
           		  		boardToStr marker (hitBoard x y board)
           		  		if isGameOver (hitBoard x y board) then putStrLn "Congrats"
           		  		else getXY marker (hitBoard x y board)
           		  	else getX' board
           else getX' board
       where
         getX' board = do
           putStrLn "Invalid input!"
           getXY marker board

--Used to play with a board with hidden ships
main = getXY sqToStr (placeShips [5,4,3,3,2] (mkBoard 10))

--Used to play with a board with visible ships
mainCheat = getXY sqToStrCheat (placeShips [5,4,3,3,2] (mkBoard 10))