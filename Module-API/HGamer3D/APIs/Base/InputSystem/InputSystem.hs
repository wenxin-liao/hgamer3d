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

-- InputSystem.hs

-- |Base API, InputSystem Module, InputSystem Sub-Module.
-- The input system handles Mouse, Keyboard and Joystick Input.


module HGamer3D.APIs.Base.InputSystem.InputSystem

(
	-- Enums
	--
	module HGamer3D.Bindings.SFML.EnumJoystickAxis,
	module HGamer3D.Bindings.SFML.EnumKey,
	module HGamer3D.Bindings.SFML.EnumMouseButton,
	
	-- Joystick Data
	Joystick (..),
	JoystickButton (..),
	
	-- Joystick Functions
	updateJoystickStatus,
	
	getConnectedJoysticks,
	isJoystickConnected,
	
	getJoystickAxes,
	getJoystickButtons,
	
	isJoystickButtonPressed,
	getJoystickAxisPosition,
	
	-- Keyboard Functions
	isKeyPressed,
	
	-- Mouse Functions
	isMouseButtonPressed,
	getMousePosition,
	
	
	
)

where

import GHC.Ptr

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector

import HGamer3D.Bindings.SFML.ClassPtr
import HGamer3D.Bindings.SFML.Utils

import qualified HGamer3D.Bindings.SFML.ClassJoystick as Joystick
import qualified HGamer3D.Bindings.SFML.ClassKeyboard as Keyboard
import qualified HGamer3D.Bindings.SFML.ClassMouse as Mouse
import qualified HGamer3D.Bindings.SFML.ClassMouseHG3D as Mouse2
import HGamer3D.Bindings.SFML.EnumJoystickAxis
import HGamer3D.Bindings.SFML.EnumKey
import HGamer3D.Bindings.SFML.EnumMouseButton

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad.Trans
import Control.Monad.Reader


-- Joystick Data
data Joystick = Joystick Int deriving (Eq)
data JoystickButton = JoystickButton Int deriving (Eq)

instance Show Joystick where
	show (Joystick j) = show $ "Joystick-" ++ (show j)

instance Show JoystickButton where
	show (JoystickButton jb) = show $ "JoystickButton-" ++ (show jb)

instance Show EnumJoystickAxis where
	show axis = "JoystickAxis-" ++ (case axis of
		JoystickAxisX -> "X"
		JoystickAxisY -> "Y"
		JoystickAxisZ -> "Z"
		JoystickAxisR -> "R"
		JoystickAxisU -> "U"
		JoystickAxisV -> "V"
		JoystickAxisPovX -> "PovX"
		JoystickAxisPovY -> "PoxY"
		)
	

-- Joystick funtions
--

updateJoystickStatus :: MHGamer3D ()
updateJoystickStatus = liftIO Joystick.update

getConnectedJoysticks :: MHGamer3D [Joystick]
getConnectedJoysticks = do
	let ns = [ x | x <- [0..15]]
	jns <- filterM (\jn -> do
		c <- liftIO $ Joystick.isConnected jn
		return (c) ) ns
	let js = map Joystick jns
	return js
	
isJoystickConnected :: Joystick -> MHGamer3D Bool
isJoystickConnected (Joystick jn) = do
	rv <- liftIO $ Joystick.isConnected jn
	return rv
	
getJoystickAxes:: Joystick -> MHGamer3D [EnumJoystickAxis]
getJoystickAxes (Joystick j) = do
	axes <- filterM ( \a -> do
		liftIO $ Joystick.hasAxis j a) [ JoystickAxisX, JoystickAxisY, JoystickAxisZ, 
			JoystickAxisR, JoystickAxisU, JoystickAxisV, 
			JoystickAxisPovX, JoystickAxisPovY ]
	return axes


getJoystickButtons :: Joystick -> MHGamer3D [JoystickButton]
getJoystickButtons (Joystick j) = do
	jn <- liftIO $ Joystick.getButtonCount j
	let btns = map JoystickButton [0..(jn-1)]
	return btns

isJoystickButtonPressed :: Joystick -> JoystickButton -> MHGamer3D Bool
isJoystickButtonPressed (Joystick j) (JoystickButton b) = liftIO $ Joystick.isButtonPressed j b

getJoystickAxisPosition :: Joystick -> EnumJoystickAxis -> MHGamer3D Float
getJoystickAxisPosition (Joystick j) ax = liftIO $ Joystick.getAxisPosition j ax


-- Keyboard functions
--

isKeyPressed :: EnumKey -> MHGamer3D Bool
isKeyPressed key = liftIO $ Keyboard.isKeyPressed key

-- Mouse functions
--

isMouseButtonPressed :: EnumMouseButton -> MHGamer3D Bool
isMouseButtonPressed mb = liftIO $ Mouse.isButtonPressed mb

getMousePosition :: MHGamer3D (Int, Int)
getMousePosition = liftIO Mouse2.getPosition


