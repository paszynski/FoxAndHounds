module Main(main, FoxGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset, scl :: Int
scl = 1 --scale jest już zajęte
width = 256
height = 256
offset = 0

window :: Display
window = InWindow "Fox & hounds" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 60

-- | Dane opisujące stan gry
data FoxGame = FoxGame
	{ foxLoc :: (Integer, Integer)	-- ^ Lokalizacja lisa
	, hound1Loc :: (Integer, Integer)	-- ^ Lokalizacja ogara1
	, hound2Loc :: (Integer, Integer)	-- ^ Lokalizacja ogara2
	, hound3Loc :: (Integer, Integer) -- ^ Lokalizacja ogara3
	, hound4Loc :: (Integer, Integer) -- ^ Lokalizacja ogara4
	} deriving (Show)

initialState :: FoxGame
initialState = FoxGame
	{ foxLoc = (-4,-4)
	, hound1Loc = (-3,4)
	, hound2Loc = (-1,4)
	, hound3Loc = (2,4)
	, hound4Loc = (4,4)
	}
	
-- | Wyświetlenie planszy ukazującej aktualny stan gry
render :: FoxGame -- ^ Stan gry do wyrenderowania
		-> Picture -- ^ Obrazek gry
render game = pictures pics
	where
		squareLight = light $ light yellow
		squareDark = dark black
		squareFox = dark red
		squareHound = dark blue
		squareSize = 32
		squareSizeN = fromIntegral 32
		replicate8 :: [Integer] -> [Integer]
		replicate8 [] = []
		replicate8 (x:xs) = replicate 8 x ++ replicate8 xs
		squareC :: (Integer,Integer) -> Picture
		squareC (px,py) = if ( mod (div px squareSize) 2 ) == ( mod (div py squareSize) 2 )
							then translate (fromIntegral px) (fromIntegral py) $ color (light (light yellow)) $ rectangleSolid squareSizeN squareSizeN
							else translate (fromIntegral px) (fromIntegral py) $ color (dark black) $ rectangleSolid squareSizeN squareSizeN
		getCoord :: Integer -> Float
		getCoord x = 32 * signum( fromIntegral x) * (abs (fromIntegral x) - 0.5)
		pics = map squareC ( zip (take 64 $ cycle [-112, -80 .. 112]) (replicate8 [112, 80 .. -112]) ) 
				++ 	[ translate (getCoord $ fst $ foxLoc initialState)  (getCoord $ snd $ foxLoc initialState) $ color squareFox $ circleSolid 16
					, translate (getCoord $ fst $ hound1Loc initialState) (getCoord $ snd $ hound1Loc initialState) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound2Loc initialState) (getCoord $ snd $ hound2Loc initialState) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound3Loc initialState) (getCoord $ snd $ hound3Loc initialState) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound4Loc initialState) (getCoord $ snd $ hound4Loc initialState) $ color squareHound $ circleSolid 16
					]
-- | Sprawdzanie kolizji ze ścianami i pionami na planszy
checkCollisionLeftDown :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionLeftDown (x,y) game = 
	if x<1 || y>7 
		|| (x-1 == (fst $ hound1Loc game) && y+1 == (snd $ hound1Loc game))
		|| (x-1 == (fst $ hound2Loc game) && y+1 == (snd $ hound2Loc game))
		|| (x-1 == (fst $ hound3Loc game) && y+1 == (snd $ hound3Loc game))
		|| (x-1 == (fst $ hound4Loc game) && y+1 == (snd $ hound4Loc game))
		|| (x-1 == (fst $ foxLoc game) && y+1 == (snd $ foxLoc game))
	then 1
	else 0
	
checkCollisionRightDown :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionRightDown (x,y) game =
	if x>7 || y>7
		|| (x+1 == (fst $ hound1Loc game) && y+1 == (snd $ hound1Loc game))
		|| (x+1 == (fst $ hound2Loc game) && y+1 == (snd $ hound2Loc game))
		|| (x+1 == (fst $ hound3Loc game) && y+1 == (snd $ hound3Loc game))
		|| (x+1 == (fst $ hound4Loc game) && y+1 == (snd $ hound4Loc game))
		|| (x+1 == (fst $ foxLoc game) && y+1 == (snd $ foxLoc game))
	then 1
	else 0
		
checkCollisionLeftUp :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionLeftUp (x,y) game = 
	if x<1 || y<1
		|| (x-1 == (fst $ hound1Loc game) && y-1 == (snd $ hound1Loc game))
		|| (x-1 == (fst $ hound2Loc game) && y-1 == (snd $ hound2Loc game))
		|| (x-1 == (fst $ hound3Loc game) && y-1 == (snd $ hound3Loc game))
		|| (x-1 == (fst $ hound4Loc game) && y-1 == (snd $ hound4Loc game))
		|| (x-1 == (fst $ foxLoc game) && y-1 == (snd $ foxLoc game))
	then 1
	else 0
	
checkCollisionRightUp :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionRightUp (x,y) game =
	if x>7 || y<1
		|| (x+1 == (fst $ hound1Loc game) && y-1 == (snd $ hound1Loc game))
		|| (x+1 == (fst $ hound2Loc game) && y-1 == (snd $ hound2Loc game))
		|| (x+1 == (fst $ hound3Loc game) && y-1 == (snd $ hound3Loc game))
		|| (x+1 == (fst $ hound4Loc game) && y-1 == (snd $ hound4Loc game))
		|| (x+1 == (fst $ foxLoc game) && y-1 == (snd $ foxLoc game))
	then 1
	else 0

-- | Obsługa wciśnięcia klawiszy
handleKeys :: Event -> FoxGame -> FoxGame

--hound1
handleKeys (EventKey (Char 'q') _ _ _) game = 
	if checkCollisionLeftDown (hound1Loc game) game == 0
		then game { hound1Loc = ((fst $ hound1Loc game) - 1, (snd $ hound1Loc game) + 1) }
		else game { hound1Loc = (fst $ hound1Loc game, snd $ hound1Loc game) }
		
handleKeys (EventKey (Char 'w') _ _ _) game =
	if checkCollisionRightDown (hound1Loc game) game == 0
		then game { hound1Loc = ((fst $ hound1Loc game) + 1, (snd $ hound1Loc game) + 1) }
		else game { hound1Loc = (fst $ hound1Loc game, snd $ hound1Loc game) }

--hound2
handleKeys (EventKey (Char 'e') _ _ _) game = 
	if checkCollisionLeftDown (hound1Loc game) game == 0
		then game { hound2Loc = ((fst $ hound2Loc game) - 1, (snd $ hound2Loc game) + 1) }
		else game { hound2Loc = (fst $ hound2Loc game, snd $ hound2Loc game) }
		
handleKeys (EventKey (Char 'r') _ _ _) game =
	if checkCollisionRightDown (hound2Loc game) game == 0
		then game { hound2Loc = ((fst $ hound2Loc game) + 1, (snd $ hound2Loc game) + 1) }
		else game { hound2Loc = (fst $ hound2Loc game, snd $ hound2Loc game) }

--hound3
handleKeys (EventKey (Char 't') _ _ _) game = 
	if checkCollisionLeftDown (hound3Loc game) game == 0
		then game { hound3Loc = ((fst $ hound3Loc game) - 1, (snd $ hound3Loc game) + 1) }
		else game { hound3Loc = (fst $ hound3Loc game, snd $ hound3Loc game) }
		
handleKeys (EventKey (Char 'y') _ _ _) game =
	if checkCollisionRightDown (hound3Loc game) game == 0
		then game { hound3Loc = ((fst $ hound3Loc game) + 1, (snd $ hound3Loc game) + 1) }
		else game { hound3Loc = (fst $ hound3Loc game, snd $ hound3Loc game) }
		
--hound4
handleKeys (EventKey (Char 'u') _ _ _) game = 
	if checkCollisionLeftDown (hound4Loc game) game == 0
		then game { hound4Loc = ((fst $ hound4Loc game) - 1, (snd $ hound4Loc game) + 1) }
		else game { hound4Loc = (fst $ hound4Loc game, snd $ hound4Loc game) }
		
handleKeys (EventKey (Char 'i') _ _ _) game =
	if checkCollisionRightDown (hound4Loc game) game == 0
		then game { hound4Loc = ((fst $ hound4Loc game) + 1, (snd $ hound4Loc game) + 1) }
		else game { hound4Loc = (fst $ hound4Loc game, snd $ hound4Loc game) }

--foxloc
handleKeys (EventKey (Char 'a') _ _ _) game = 
	if checkCollisionLeftUp (foxLoc game) game == 0
		then game { foxLoc = ((fst $ foxLoc game) - 1, (snd $ foxLoc game) - 1) }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }
		
handleKeys (EventKey (Char 's') _ _ _) game =
	if checkCollisionRightUp (foxLoc game) game == 0
		then game { foxLoc = ((fst $ foxLoc game) + 1, (snd $ foxLoc game) - 1) }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }
		
handleKeys (EventKey (Char 'z') _ _ _) game = 
	if checkCollisionLeftDown (foxLoc game) game == 0
		then game { foxLoc = ((fst $ foxLoc game) - 1, (snd $ foxLoc game) + 1) }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }
		
handleKeys (EventKey (Char 'x') _ _ _) game =
	if checkCollisionRightDown (foxLoc game) game == 0
		then game { foxLoc = ((fst $ foxLoc game) + 1, (snd $ foxLoc game) + 1) }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }


handleKeys _ game = game

update :: Float -> FoxGame -> FoxGame
update _ game = game

main :: IO ()
-- main = display window background (render initialState)
main = play window background fps initialState render handleKeys update
