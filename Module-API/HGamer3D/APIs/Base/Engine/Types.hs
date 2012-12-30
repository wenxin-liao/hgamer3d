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

-- Types.hs

-- |Base API, Engine Module, Types Sub-Module.
-- Basic types of the  HGamer3D engine.


module HGamer3D.APIs.Base.Engine.Types (

	MHGamer3D,
	Event (..),
	EventFunction,
	EventMap,
	
	CommonSystem (..),
	Graphics3DSystem (..),
	GUISystem (..),
	NetworkSystem (..),
	PhysicsSystem (..),
	
	HG3DReaderState (..),
	HG3DEngineState (..),
	
	TimeMS (..),
)

where 

import HGamer3D.Data.HG3DClass
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map


data CommonSystem = CommonSystem {
	csHG3DPath::String,
	csProgPath::String
}
	
data Graphics3DSystem = Graphics3DSystem {
	g3sRoot::HG3DClass,
	g3sSceneManager::HG3DClass,
	g3sResourceGroupManager::HG3DClass,
	g3sTextureManager::HG3DClass,
	g3sControllerManager::HG3DClass,
	g3sLogManager::HG3DClass,
	g3sCamera::HG3DClass,
	g3sRenderWindow::HG3DClass,
	g3sViewport::HG3DClass,
	g3sMessagePump::HG3DClass
} 

data GUISystem = GUISystem {
	guiRenderer::HG3DClass,
	guiGUI::HG3DClass,
	guiWindowManager::HG3DClass,
	guiWindowManagerHG3D::HG3DClass,
	guiFontManager::HG3DClass,
	guiSchemeManager::HG3DClass,
	guiEventController::HG3DClass
} 

data NetworkSystem = NetworkSystem {
	nsNetwork::HG3DClass
}

data PhysicsSystem = PhysicsSystem {
	psPhysics::HG3DClass,
	psWorld::HG3DClass
}

data HG3DReaderState = HG3DReaderState {
	commonSystem::CommonSystem,
	graphics3DSystem::Graphics3DSystem,
	guiSystem::GUISystem,
	networkSystem::NetworkSystem,
	physicsSystem::PhysicsSystem
}

data Event = GUIEvent String String HG3DClass -- name sender window

type EventFunction worldState = Event -> worldState -> MHGamer3D worldState
type EventMap worldState = Map.Map String (EventFunction worldState)

data HG3DEngineState = HG3DEngineState {
	esUniqueNumbers::[Integer]
}

type MHGamer3D a = (ReaderT HG3DReaderState) (StateT HG3DEngineState IO) a

data TimeMS = TimeMS Int			-- time in milliseconds

instance Show TimeMS where
	show (TimeMS s) = (show s) ++ " Milliseconds"

