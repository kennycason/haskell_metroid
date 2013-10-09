import Graphics.UI.SDL
import Graphics.UI.SDL.Video

import Data.Word
import Data.Maybe

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Timer


data GameState = GameState {
    screen :: Surface,
    sprites :: Surface,
    samus :: Surface
}

getSpriteSheetOffset :: Int -> Maybe Rect
getSpriteSheetOffset n = Just (Rect offx offy 32 32)
							where 
								offx = mod (n * 32) (32 * 5)
								offy = quot (n * 32) (32 * 5) * 32


drawSprite :: Surface -> Surface -> Int -> Int -> Int -> IO Bool
drawSprite screen sprites n x y = blitSurface 
											sprites (getSpriteSheetOffset n) 
											screen dst
													where dst = Just (Rect x y 32 32)

drawSamus :: Surface -> Surface -> Int -> Int -> IO Bool
drawSamus screen samus x y = blitSurface samus Nothing screen dst
								where --src = Just (Rect 0 0 40 62)
									  dst = Just (Rect x y 40 62)


{-loadImage :: String -> Maybe (Word8, Word8, Word8) -> IO Surface
loadImage filename colorKey = load filename >>= displayFormat >>= setColorKey colorKey -}


drawRoom :: Surface -> Surface -> [[Int]] -> IO()
drawRoom screen sprites room = mapM_ (\r -> drawRow screen sprites r) [0..14]
								where drawRow screen sprites r = 
									mapM_ (\(x, n) -> drawSprite screen sprites n (x * 32) (r * 32)) (coord (room !! r))
										where coord row = map (\i -> (i, row !! i)) [0..19]


emptyWorld :: Int -> Int -> a -> [[a]]
emptyWorld x y = replicate y . replicate x

room = 	[[3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]
		,[3,0,0,0,0,0,0,0,0,0,0,3,3,0,0,0,0,3,0,3]
		,[3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,7,6,6,6,6,6,6,7,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,6,6,7,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,7,6,6,6,6,6,7,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3]
		,[3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,3,3]
		,[3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,3,3]
		]



main :: IO()
main = do
	-- init
	Graphics.UI.SDL.init [InitEverything]
	setVideoMode 640 480 32 []
	setCaption "Metroid" "Metroid"

	screen <- getVideoSurface
	samus <- loadBMP "img/samus.bmp" 
	sprites <- loadBMP "img/spritesheet.bmp"

	--samus2 <- loadImage "img/samus.bmp"  Nothing --Just (0x00, 0x00, 0x00)


	mapM_ (\i -> do
				drawRoom screen sprites room
				drawSamus screen samus (64 + i * 4) 354
				Graphics.UI.SDL.flip screen
				delay 10) [0..5]

	loop 

	quit
	print "done"

	where
		loop = do
			quit <- whileEvents
			unless quit loop

		whileEvents = do
		    event <- pollEvent
		    case event of
		        Quit -> return True
		        KeyUp _ -> return True
		        --MouseEvent -> return True
		        NoEvent -> return True
		        _ -> whileEvents
