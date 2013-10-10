{-# LANGUAGE FlexibleContexts #-}
-- http://stackoverflow.com/questions/10865963/using-the-state-monad-to-hide-explicit-state
import Graphics.UI.SDL
import Graphics.UI.SDL.Video

import Data.Word
import Data.Maybe

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Timer

-- game data
room1 = [[3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3]
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

-- type defines
data Coord = Coord { x :: Int, y :: Int }

data Room = Room { tiles :: [[Int]] }

data GameData = GameData {
	timer :: Timer,
	currentRoom :: Room,
	samusPos :: Coord
}

data GameConfig = GameConfig {
	screen :: Surface,
	samus :: Surface,
	sprites :: Surface
}

type GameState = StateT GameData IO
type GameEnv = ReaderT GameConfig GameState


-- getters/setters 
getSamusPos :: MonadState GameData m => m Coord
getSamusPos = liftM samusPos get

putSamusPos :: MonadState GameData m => Coord -> m ()
putSamusPos t = modify $ \s -> s { samusPos = t }

modifySamusPosM :: MonadState GameData m => (Coord -> m Coord) -> m ()
modifySamusPosM act = getSamusPos >>= act >>= putSamusPos

modifySamusPos :: MonadState GameData m => (Coord -> Coord) -> m ()
modifySamusPos fn = fn `liftM` getSamusPos >>= putSamusPos


getTimer :: MonadState GameData m => m Timer
getTimer = liftM timer get

putTimer :: MonadState GameData m => Timer -> m ()
putTimer t = modify $ \s -> s { timer = t }

modifyTimerM :: MonadState GameData m => (Timer -> m Timer) -> m ()
modifyTimerM act = getTimer >>= act >>= putTimer


getRoom :: MonadState GameData m => m Room
getRoom = liftM currentRoom get

putRoom :: MonadState GameData m => Room -> m ()
putRoom t = modify $ \s -> s { currentRoom = t }

modifyRoomM :: MonadState GameData m => (Room -> m Room) -> m ()
modifyRoomM act = getRoom >>= act >>= putRoom

modifyRoom :: MonadState GameData m => (Room -> Room) -> m ()
modifyRoom fn = fn `liftM` getRoom >>= putRoom


getScreen :: MonadReader GameConfig m => m Surface
getScreen = liftM screen ask

getSamus :: MonadReader GameConfig m => m Surface
getSamus = liftM samus ask

getSprites :: MonadReader GameConfig m => m Surface
getSprites = liftM sprites ask



-- main functions
newGame :: IO (GameConfig, GameData)
newGame = do
	setVideoMode 640 480 32 []
	setCaption "Hacktroid" []
	screen <- getVideoSurface
	samus <- loadBMP "img/samus.bmp" 
	sprites <- loadBMP "img/spritesheet.bmp"
	timer <- start defaultTimer
	return (GameConfig screen samus sprites , GameData timer (Room room1) (Coord 64 352))


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
								where
									src = Just (Rect 0 0 40 62)
									dst = Just (Rect x y 40 62)


drawRoom :: Surface -> Surface -> [[Int]] -> IO()
drawRoom screen sprites room = mapM_ (\r -> drawRow screen sprites r) [0..14]
								where drawRow screen sprites r = 
									mapM_ (\(x, n) -> drawSprite screen sprites n (x * 32) (r * 32)) (coord (room !! r))
										where coord row = map (\i -> (i, row !! i)) [0..19]


emptyWorld :: Int -> Int -> a -> [[a]]
emptyWorld x y = replicate y . replicate x


handleInput :: Event -> Coord -> Coord
handleInput (KeyDown (Keysym SDLK_UP _ _)) c@Coord { x = x, y = y } = c
handleInput (KeyDown (Keysym SDLK_DOWN _ _)) c@Coord { x = x, y = y } = c
handleInput (KeyDown (Keysym SDLK_LEFT _ _)) c@Coord { x = x, y = y } = c
handleInput (KeyDown (Keysym SDLK_RIGHT _ _)) c@Coord { x = x, y = y } = c

handleInput (KeyUp (Keysym SDLK_UP _ _)) c@Coord { x = x, y = y } = c { x = x, y = y - 32 }
handleInput (KeyUp (Keysym SDLK_DOWN _ _)) c@Coord { x = x, y = y } = c { x = x, y = y + 32 }
handleInput (KeyUp (Keysym SDLK_LEFT _ _)) c@Coord { x = x, y = y } = c { x = x - 32, y = y }
handleInput (KeyUp (Keysym SDLK_RIGHT _ _)) c@Coord { x = x, y = y } = c { x = x + 32, y = y }

handleInput _ d = d


loop :: GameEnv ()
loop = do

	modifyTimerM $ liftIO . start
	quit <- whileEvents $ modifySamusPos . handleInput
	

	timer <- getTimer

	screen <- getScreen
	sprites <- getSprites
	samus <- getSamus
	pos <- getSamusPos

	liftIO $ do
		drawRoom screen sprites room1
		drawSamus screen samus (x pos) (y pos)
		Graphics.UI.SDL.flip screen

		ticks <- getTimerTicks timer
		when (ticks < secsPerFrame) $ do
			delay $ secsPerFrame - ticks
	unless quit loop
 where
	framesPerSecond = 30
	secsPerFrame = 1000 `div` framesPerSecond

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
	event <- liftIO pollEvent
	case event of
		Quit -> return True
		NoEvent -> return False
		_ -> do
			act event
			whileEvents act


runLoop :: GameConfig -> GameData -> IO ()
runLoop = evalStateT . runReaderT loop


main = withInit [InitEverything] $ do -- withInit calls quit for us.
	(env, state) <- newGame
	runLoop env state
