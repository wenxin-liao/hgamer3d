-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2011 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- Engine.hs

-- |Base API, Engine Module, Engine Sub-Module.
-- Basic functionality of the HGamer3D engine.


module HGamer3D.APIs.Base.Engine.Engine (

	getUniqueName,
	mapFunctionToTag,
	getFunctionFromTag,
	initCommonSystem,
	
	getTimeMS,
	runMHGamer3D,
	initHGamer3D,
	createEventMap,
	renderLoop
) 

where


import GHC.Ptr

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle
import HGamer3D.Data.HG3DClass

import HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassHG3DMessagePump as MessagePump
import HGamer3D.Bindings.Ogre.ClassRenderWindow as RenderWindow
import HGamer3D.Bindings.Ogre.ClassWindowUtilsHG3D as WindowUtils
import HGamer3D.Bindings.CEGUI.ClassSystem as CEGUISystem
import HGamer3D.Bindings.CEGUI.EnumMouseButton as CEGUIButton
import HGamer3D.Bindings.CEGUI.ClassHG3DEventController as HG3DEventController

import HGamer3D.APIs.Base.Engine.Types
import qualified HGamer3D.APIs.Base.InputSystem.InputSystem as IS

import HGamer3D.APIs.Base.Graphics3D.EngineHelper
import HGamer3D.APIs.Base.GUI.EngineHelper
import HGamer3D.APIs.Base.Network.EngineHelper
import HGamer3D.APIs.Base.Physics.EngineHelper

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent

import System.Win32.Process
import qualified Data.Text as T
import System.Directory
import System.Environment (getArgs)
import qualified Data.Map as Map
import System.Win32.Time
import System.Environment.FindBin


-- identification dll
--
hg3ddllname :: String
hg3ddllname = "HGamer3D-Version-0.2.0-DontDelete.txt"

-- unique identifiers, by appending a unique integer to a prefix

getUniqueName :: String -> MHGamer3D String
getUniqueName prefix = do 

    HG3DEngineState (x:xs) <- lift get
    lift $ put (HG3DEngineState xs)
    return (prefix ++ (show x))
 
-- event function mapping

mapFunctionToTag :: EventFunction worldState -> String -> EventMap worldState -> EventMap worldState
mapFunctionToTag function tag eventMap = Map.insert tag function eventMap
   
getFunctionFromTag :: String -> EventMap worldState -> Maybe (EventFunction worldState)
getFunctionFromTag tag eventMap = Map.lookup tag eventMap

-- function, which finds installation path of HG3D
--

getHG3DPath :: IO (String, String)
getHG3DPath = do
	env <- findExecutable hg3ddllname
	let path = case env of
		Just path -> fst (T.breakOn (T.pack $ "\\bin\\" ++ hg3ddllname) (T.pack path))
		Nothing -> T.pack ""
	progpath <- getProgPath
	return (T.unpack path, progpath)

initCommonSystem :: IO CommonSystem
initCommonSystem = do
	(csHG3DPath, csProgPath) <- getHG3DPath
	return (CommonSystem csHG3DPath csProgPath)

-- Milliseconds Timescale


getTimeMS :: MHGamer3D (TimeMS)
getTimeMS = do 
	fr <- liftIO $ queryPerformanceFrequency
	wt <- liftIO $ queryPerformanceCounter
	return (TimeMS $ fromIntegral (wt * 1000 `div`  fr))
	

runMHGamer3D :: (HG3DReaderState, HG3DEngineState) -> MHGamer3D a -> IO (a, (HG3DReaderState, HG3DEngineState))
runMHGamer3D (readerstate, enginestate) action = do
	(actionResult, newEnginestate) <- runStateT (runReaderT action readerstate ) enginestate
	return (actionResult, (readerstate, newEnginestate))

data MouseState = MouseUp | MouseDown deriving (Eq)

data TimeMouseState = TimeMouseState {
	tmsMouse::MouseState,
	tmsTimeMS::TimeMS
	}
	
initTimeMouseState :: MHGamer3D (TimeMouseState)
initTimeMouseState = do
	tms <- getTimeMS
	leftButton <- IS.isMouseButtonPressed IS.MouseButtonLeft
	let ms = if leftButton then MouseUp else MouseDown
	return (TimeMouseState ms tms)


initHGamer3D :: String -> IO (HG3DReaderState, HG3DEngineState)
initHGamer3D windowName = do

	-- init Commons configs
	cs <- initCommonSystem
	
	-- get flags from program arguments
	let hg3dpath = (csHG3DPath cs) 
	args <- getArgs
	let fConfig = foldl (||) False $ map (\arg -> if arg == "--config" then True else False) args
	let fDX = foldl (||) False $ map (\arg -> if arg == "--directx" then True else False) args
	let fLog = foldl (||) False $ map (\arg -> if arg == "--logging" then True else False) args
	
	-- init Graphics3D configs and engine
	g3s <- initGraphics3D windowName "OctreeSceneManager" hg3dpath fConfig fDX fLog
	
	-- init GUI configs and engine
	gui <- initGUIEngine fLog
	
	-- init Network engine
	network <- initNetworkEngine
	
	-- init Phyiscs engine
	physics <- initPhysicsEngine
	
	
	let enginestate = HG3DEngineState [1..]
	let readerstate = HG3DReaderState cs g3s gui network physics
	
	return (readerstate, enginestate)
	
-- renderStep a :: TimeMS -> a -> MHGamer3D (Bool, a)
-- this function needs to be defined in 

renderInternalStep :: Int -> TimeMouseState -> MHGamer3D (Bool, TimeMouseState)
renderInternalStep frameRate (TimeMouseState lastMouseState (TimeMS lastTime)) = do
	rs <- ask
	let cs = commonSystem rs
	let g3s = graphics3DSystem rs
	let gui = guiSystem rs
	-- adapt to framerate, while still doing messagePump
	(TimeMS time) <- getTimeMS
	let delta = time - lastTime
	let waitTimeMS = (1000.0 / (fromIntegral frameRate) ) - (fromIntegral delta) 
	time <- if waitTimeMS > 0.0 then do
				liftIO $ sleep (round waitTimeMS)
				(TimeMS time) <- getTimeMS
				return (time)
			else
				return (time)
	-- adapt
	let messagePump = g3sMessagePump g3s
	liftIO $ MessagePump.messagePump messagePump
	closed <- liftIO $ RenderWindow.isClosed (g3sRenderWindow g3s)
	if (closed) then
		return (False, (TimeMouseState lastMouseState (TimeMS time)) )
		else do
			-- here comes the things, we need to do each time
			-- 
			let delta2 = (fromIntegral (time - lastTime)) * 1000.0
			-- mouse state injection
			leftButton <- IS.isMouseButtonPressed IS.MouseButtonLeft
			let ms = if leftButton then MouseDown else MouseUp
			if lastMouseState == MouseUp && ms == MouseDown then do
				liftIO $ CEGUISystem.injectMouseButtonDown  (guiGUI gui) CEGUIButton.MouseLeftButton
				return ()
				else do
					return ()
			if lastMouseState == MouseDown && ms == MouseUp then do
				liftIO $ CEGUISystem.injectMouseButtonUp (guiGUI gui) CEGUIButton.MouseLeftButton
				return ()
				else do
					return ()
			-- mouse position injection
			(xm, ym) <- IS.getMousePosition
			leftButton <- IS.isMouseButtonPressed IS.MouseButtonLeft
			(width, height, colorDepth, left, top) <- liftIO $ RenderWindow.getMetrics (g3sRenderWindow g3s)
			(topT, bottomT, leftT, rightT) <- liftIO $ WindowUtils.getWindowTopLeft (g3sRenderWindow g3s) 
			let offLeft = (rightT - leftT - width) `div` 2
			let offRight = bottomT - topT - height - offLeft
			-- key press injection
			keypressInject
			
			let mouseX = xm - left - offLeft
			let mouseY = ym - top - offRight
			liftIO $ CEGUISystem.injectMousePosition (guiGUI gui) (fromIntegral mouseX) (fromIntegral mouseY)
			-- time pulse injection
			liftIO $ CEGUISystem.injectTimePulse (guiGUI gui) delta2
			-- display 3D and GUI
			liftIO $ Root.renderOneFrame (g3sRoot g3s)
			liftIO $ CEGUISystem.renderGUI (guiGUI gui)
			
			return (True, (TimeMouseState ms (TimeMS time)) )


renderInternalLoop :: Int -> TimeMouseState -> gamestateType -> EventMap gamestateType -> (TimeMS -> gamestateType -> MHGamer3D (Bool, gamestateType)) -> MHGamer3D ()
renderInternalLoop frameRate rsold gamestate eventMap renderStep = do
	let (TimeMouseState ms (TimeMS timeold)) = rsold
	gsnew <- getEventsFromGui gamestate eventMap
	(flagStep, rs) <- renderInternalStep frameRate rsold
	let (TimeMouseState ms (TimeMS time)) = rs
	let delta = (TimeMS (time - timeold))
	(flagLoop, gsnew2) <- renderStep delta gsnew
	if (flagStep && flagLoop) then do
		renderInternalLoop frameRate rs gsnew2 eventMap renderStep
		else return ()
			
			
createEventMap :: EventMap worldState
createEventMap = Map.fromList ([]::[(String, EventFunction worldState)])

renderLoop :: Int -> EventMap worldState -> worldState -> (TimeMS -> worldState -> MHGamer3D (Bool, worldState)) -> MHGamer3D ()
renderLoop frameRate eventMap gamestate renderStep = do

	rs <- initTimeMouseState
	renderInternalLoop frameRate rs gamestate eventMap renderStep

-- event loop

getEventsFromGui :: a -> EventMap a -> MHGamer3D a
getEventsFromGui a eventMap = do
	rs <- ask
	let cs = commonSystem rs
	let g3s = graphics3DSystem rs
	let gui = guiSystem rs
	let eventController = guiEventController gui
	processEvents <- liftIO $ HG3DEventController.eventsAvailable eventController
	
	outera <- if processEvents then do
		(name, sender, window) <- liftIO $ HG3DEventController.popEvent eventController
		let evt = GUIEvent name sender window
		let evtfunc = getFunctionFromTag name eventMap
		innera <- case evtfunc of
			Just func -> do
				fa <- func evt a
				return fa
			Nothing -> return a
		loopa <- getEventsFromGui innera eventMap
		return loopa
		else do
			return a
	return outera

