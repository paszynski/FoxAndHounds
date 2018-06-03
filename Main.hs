module Main(main, FoxGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 256
height = 256
offset = 0

minCoord, maxCoord :: Integer
minCoord = (-4)
maxCoord = 3


window :: Display
window = InWindow "Fox & hounds" (width, height) (offset, offset)

background :: Color
background = white

fps :: Int
fps = 1

-- | Dane opisujące stan gry
data FoxGame = FoxGame
	{ foxLoc :: (Integer, Integer)	-- ^ Lokalizacja lisa
	, hound1Loc :: (Integer, Integer)	-- ^ Lokalizacja ogara1
	, hound2Loc :: (Integer, Integer)	-- ^ Lokalizacja ogara2
	, hound3Loc :: (Integer, Integer) -- ^ Lokalizacja ogara3
	, hound4Loc :: (Integer, Integer) -- ^ Lokalizacja ogara4
	, turn :: String
	} deriving (Show)

initialState :: FoxGame
initialState = FoxGame
	{ foxLoc = (maxCoord-3,minCoord)
	, hound1Loc = (maxCoord-6,maxCoord)
	, hound2Loc = (maxCoord-4,maxCoord)
	, hound3Loc = (maxCoord-2,maxCoord)
	, hound4Loc = (maxCoord,maxCoord)
	, turn = "hound"
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
		fontHound = white
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
		getCoord x = fromIntegral (32 * x + 16)
		pics = map squareC ( zip (take 64 $ cycle [-112, -80 .. 112]) (replicate8 [112, 80 .. -112]) ) 
				++ 	[ translate (getCoord $ fst $ foxLoc game)  (getCoord $ snd $ foxLoc game) $ color squareFox $ circleSolid 16
					, translate (getCoord $ fst $ hound1Loc game) (getCoord $ snd $ hound1Loc game) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound1Loc game) (getCoord $ snd $ hound1Loc game) $ scale 0.1 0.1 $ color fontHound $ text "1"
					, translate (getCoord $ fst $ hound2Loc game) (getCoord $ snd $ hound2Loc game) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound2Loc game) (getCoord $ snd $ hound2Loc game) $ scale 0.1 0.1 $ color fontHound $ text "2"
					, translate (getCoord $ fst $ hound3Loc game) (getCoord $ snd $ hound3Loc game) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound3Loc game) (getCoord $ snd $ hound3Loc game) $ scale 0.1 0.1 $ color fontHound $ text "3"
					, translate (getCoord $ fst $ hound4Loc game) (getCoord $ snd $ hound4Loc game) $ color squareHound $ circleSolid 16
					, translate (getCoord $ fst $ hound4Loc game) (getCoord $ snd $ hound4Loc game) $ scale 0.1 0.1 $ color fontHound $ text "4"
					]
-- | Sprawdzanie kolizji ze ścianami i pionami na planszy
checkCollisionLeftDown :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionLeftDown (x,y) game = 
	if x<=minCoord || y<=minCoord
		|| (x-1 == (fst $ hound1Loc game) && y-1 == (snd $ hound1Loc game))
		|| (x-1 == (fst $ hound2Loc game) && y-1 == (snd $ hound2Loc game))
		|| (x-1 == (fst $ hound3Loc game) && y-1 == (snd $ hound3Loc game))
		|| (x-1 == (fst $ hound4Loc game) && y-1 == (snd $ hound4Loc game))
		|| (x-1 == (fst $ foxLoc game) && y-1 == (snd $ foxLoc game))
	then 1
	else 0
	
checkCollisionRightDown :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionRightDown (x,y) game =
	if x>=maxCoord || y<=minCoord
		|| (x+1 == (fst $ hound1Loc game) && y-1 == (snd $ hound1Loc game))
		|| (x+1 == (fst $ hound2Loc game) && y-1 == (snd $ hound2Loc game))
		|| (x+1 == (fst $ hound3Loc game) && y-1 == (snd $ hound3Loc game))
		|| (x+1 == (fst $ hound4Loc game) && y-1 == (snd $ hound4Loc game))
		|| (x+1 == (fst $ foxLoc game) && y-1 == (snd $ foxLoc game))
	then 1
	else 0
		
checkCollisionLeftUp :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionLeftUp (x,y) game = 
	if x<=minCoord || y>=maxCoord
		|| (x-1 == (fst $ hound1Loc game) && y+1 == (snd $ hound1Loc game))
		|| (x-1 == (fst $ hound2Loc game) && y+1 == (snd $ hound2Loc game))
		|| (x-1 == (fst $ hound3Loc game) && y+1 == (snd $ hound3Loc game))
		|| (x-1 == (fst $ hound4Loc game) && y+1 == (snd $ hound4Loc game))
		|| (x-1 == (fst $ foxLoc game) && y+1 == (snd $ foxLoc game))
	then 1
	else 0
	
checkCollisionRightUp :: (Integer,Integer) -> FoxGame -> Integer
checkCollisionRightUp (x,y) game =
	if x>=maxCoord || y>=maxCoord
		|| (x+1 == (fst $ hound1Loc game) && y+1 == (snd $ hound1Loc game))
		|| (x+1 == (fst $ hound2Loc game) && y+1 == (snd $ hound2Loc game))
		|| (x+1 == (fst $ hound3Loc game) && y+1 == (snd $ hound3Loc game))
		|| (x+1 == (fst $ hound4Loc game) && y+1 == (snd $ hound4Loc game))
		|| (x+1 == (fst $ foxLoc game) && y+1 == (snd $ foxLoc game))
	then 1
	else 0

-- | Obsługa wciśnięcia klawiszy
handleKeys :: Event -> FoxGame -> FoxGame

--hound1qw
handleKeys (EventKey (Char 'q') Up _ _) game = 
	if checkCollisionLeftDown (hound1Loc game) game == 0 && turn game == "hound"
		then game { hound1Loc = ((fst $ hound1Loc game) - 1, (snd $ hound1Loc game) - 1), turn = "fox" }
		else game { hound1Loc = (fst $ hound1Loc game, snd $ hound1Loc game) }
		
handleKeys (EventKey (Char 'w') Up _ _) game =
	if checkCollisionRightDown (hound1Loc game) game == 0 && turn game == "hound"
		then game { hound1Loc = ((fst $ hound1Loc game) + 1, (snd $ hound1Loc game) - 1), turn = "fox" }
		else game { hound1Loc = (fst $ hound1Loc game, snd $ hound1Loc game) }

--hound2
handleKeys (EventKey (Char 'e') Up _ _) game = 
	if checkCollisionLeftDown (hound2Loc game) game == 0 && turn game == "hound"
		then game { hound2Loc = ((fst $ hound2Loc game) - 1, (snd $ hound2Loc game) - 1), turn = "fox" }
		else game { hound2Loc = (fst $ hound2Loc game, snd $ hound2Loc game) }
		
handleKeys (EventKey (Char 'r') Up _ _) game =
	if checkCollisionRightDown (hound2Loc game) game == 0 && turn game == "hound"
		then game { hound2Loc = ((fst $ hound2Loc game) + 1, (snd $ hound2Loc game) - 1), turn = "fox" }
		else game { hound2Loc = (fst $ hound2Loc game, snd $ hound2Loc game) }

--hound3
handleKeys (EventKey (Char 't') Up _ _) game = 
	if checkCollisionLeftDown (hound3Loc game) game == 0 && turn game == "hound"
		then game { hound3Loc = ((fst $ hound3Loc game) - 1, (snd $ hound3Loc game) - 1), turn = "fox" }
		else game { hound3Loc = (fst $ hound3Loc game, snd $ hound3Loc game) }
		
handleKeys (EventKey (Char 'y') Up _ _) game =
	if checkCollisionRightDown (hound3Loc game) game == 0 && turn game == "hound"
		then game { hound3Loc = ((fst $ hound3Loc game) + 1, (snd $ hound3Loc game) - 1), turn = "fox" }
		else game { hound3Loc = (fst $ hound3Loc game, snd $ hound3Loc game) }
		
--hound4
handleKeys (EventKey (Char 'u') Up _ _) game = 
	if checkCollisionLeftDown (hound4Loc game) game == 0 && turn game == "hound"
		then game { hound4Loc = ((fst $ hound4Loc game) - 1, (snd $ hound4Loc game) - 1), turn = "fox" }
		else game { hound4Loc = (fst $ hound4Loc game, snd $ hound4Loc game) }
		
handleKeys (EventKey (Char 'i') Up _ _) game =
	if checkCollisionRightDown (hound4Loc game) game == 0 && turn game == "hound"
		then game { hound4Loc = ((fst $ hound4Loc game) + 1, (snd $ hound4Loc game) - 1), turn = "fox" }
		else game { hound4Loc = (fst $ hound4Loc game, snd $ hound4Loc game) }

--foxloc
handleKeys (EventKey (Char 'a') Up _ _) game = 
	if checkCollisionLeftUp (foxLoc game) game == 0 && turn game == "fox"
		then game { foxLoc = ((fst $ foxLoc game) - 1, (snd $ foxLoc game) + 1), turn = "hound" }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }
		
handleKeys (EventKey (Char 's') Up _ _) game =
	if checkCollisionRightUp (foxLoc game) game == 0 && turn game == "fox"
		then game { foxLoc = ((fst $ foxLoc game) + 1, (snd $ foxLoc game) + 1), turn = "hound" }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }
		
handleKeys (EventKey (Char 'z') Up _ _) game = 
	if checkCollisionLeftDown (foxLoc game) game == 0 && turn game == "fox"
		then game { foxLoc = ((fst $ foxLoc game) - 1, (snd $ foxLoc game) - 1), turn = "hound" }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }
		
handleKeys (EventKey (Char 'x') Up _ _) game =
	if checkCollisionRightDown (foxLoc game) game == 0 && turn game == "fox"
		then game { foxLoc = ((fst $ foxLoc game) + 1, (snd $ foxLoc game) - 1), turn = "hound" }
		else game { foxLoc = (fst $ foxLoc game, snd $ foxLoc game) }


handleKeys _ game = game

update :: Float -> FoxGame -> FoxGame
update _ game = game

main :: IO ()
-- main = display window background (render initialState)
main = play window background fps initialState render handleKeys update
