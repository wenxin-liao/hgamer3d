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

-- Basic3D.hs

-- |Base API, Graphics3D Module, Basic3D Sub-Module.
-- Basic functionality of the HGamer3D engine.


module HGamer3D.APIs.Base.Graphics3D.Basic3D (

	Position3D (..),
	Scale3D (..),
	translate3D,
	Direction3D (..),
	Orientation3D (..),

	Camera (..),
	
	HGamer3D.APIs.Base.Graphics3D.Basic3D.getCamera,
	cameraLookAt,
	HGamer3D.APIs.Base.Graphics3D.Basic3D.setBackgroundColour,
	
	addResourceLocationMedia,
	addResourceZipfileMedia,
	addResourceLocationGUI,
	finalizeResourceLocations		
)

where 

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle
import HGamer3D.Data.HG3DClass

import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager

import HGamer3D.APIs.Base.Engine.Types

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent


class Position3D t where

	position3D :: t -> MHGamer3D Vec3
	positionTo3D :: t -> Vec3 -> MHGamer3D ()
	
translate3D :: Position3D t => t -> Vec3 -> MHGamer3D ()
translate3D t v = do
	p <- position3D t
	positionTo3D t ( v &+ p )
	return ()

class Scale3D t where	
	
	scale3D :: t -> MHGamer3D Vec3
	scaleTo3D :: t -> Vec3 -> MHGamer3D ()

class Direction3D t where
	direction3D :: t -> MHGamer3D Vec3
	directionTo3D :: t -> Vec3 -> MHGamer3D ()

class Orientation3D t where
	orientation3D :: t -> MHGamer3D UnitQuaternion
	orientationTo3D :: t -> UnitQuaternion -> MHGamer3D ()

-- Camera functions
--

data Camera = Camera HG3DClass

getCamera :: MHGamer3D Camera
getCamera = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cam = Camera (g3sCamera g3s)
	return cam
	
instance Position3D Camera where

	position3D (Camera c) = do
		pos <- liftIO $ Camera.getPosition c
		return (pos)
		
	positionTo3D (Camera c) pos = do
		liftIO $ Camera.setPosition2 c  pos
		return ()
	
instance Direction3D Camera where

	direction3D (Camera c) = do
		d <- liftIO $ Camera.getDirection c
		return d
		
	directionTo3D (Camera c) v = do
		liftIO $ Camera.setDirection2 c v
	
instance Orientation3D Camera where

	orientation3D (Camera c) = do
		q <- liftIO $ Camera.getOrientation c
		let uq = mkNormal q
		return uq
	
	orientationTo3D (Camera c) uq = do
		liftIO $ Camera.setOrientation c (fromNormal uq)
		return ()

cameraLookAt (Camera c) v = do
	liftIO $ Camera.lookAt c v
	return ()


-- specific single function
--

setBackgroundColour :: Colour -> MHGamer3D ()
setBackgroundColour bgColour = do
	rs <- ask
	let g3s = graphics3DSystem rs
	liftIO $ Viewport.setBackgroundColour (g3sViewport g3s) bgColour

-- locations of media in same folder as program resides
-- 

addResourceLocationMedia :: String -> MHGamer3D ()
addResourceLocationMedia path = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let progPath = csProgPath cs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.addResourceLocation rgm (progPath ++ "\\" ++ path) "FileSystem" "General" False

addResourceZipfileMedia :: String -> MHGamer3D ()
addResourceZipfileMedia path = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let progPath = csProgPath cs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.addResourceLocation rgm (progPath ++ "\\" ++ path) "Zip" "General" False

addResourceLocationGUI :: String -> String -> MHGamer3D ()
addResourceLocationGUI path category = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let progPath = csProgPath cs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.addResourceLocation rgm (progPath ++ "\\" ++ path) "FileSystem" category False

finalizeResourceLocations :: MHGamer3D ()	
finalizeResourceLocations = do
	rs <- ask
	let g3s = graphics3DSystem rs
	let cs = commonSystem rs
	let rgm = g3sResourceGroupManager g3s
	liftIO $ ResourceGroupManager.initialiseAllResourceGroups rgm
	
