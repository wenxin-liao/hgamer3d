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

-- Object3D.hs

-- |Base API, Graphics3D Module, Object3D Sub-Module.
-- Creating and managing 3D objects.


module HGamer3D.APIs.Base.Graphics3D.Object3D (

	Object3D (..),
	Material (..),
	
	Mesh (..),
	
	createColouredLineMesh,
	createColouredCubeMesh,
	createRainbowCubeMesh,
	createNamedMesh,
	
	createObject3DFromMesh,
	
	createSphere,
	createCube,
	createPlane,
	createLine,
	createColouredLine,
	createColouredCube,
	createRainbowCube,
	
	loadMesh,
	
	setObjectMaterial,	
	combineObjects,
	
	yaw3D,
	roll3D,
	pitch3D
) 

where


import GHC.Ptr

import HGamer3D.Bindings.Ogre.ClassPtr
import HGamer3D.Bindings.Ogre.Utils

import HGamer3D.Data.Colour
import HGamer3D.Data.Vector
import HGamer3D.Data.Angle

import HGamer3D.Bindings.Ogre.StructColour
import HGamer3D.Bindings.Ogre.StructSharedPtr

import HGamer3D.Bindings.Ogre.EnumSceneType
import HGamer3D.Bindings.Ogre.EnumNodeTransformSpace
import HGamer3D.Bindings.Ogre.EnumLightType

import HGamer3D.Bindings.Ogre.ClassCamera as Camera
import HGamer3D.Bindings.Ogre.ClassRoot as Root
import HGamer3D.Bindings.Ogre.ClassLight as Light
import HGamer3D.Bindings.Ogre.ClassNode as Node
import HGamer3D.Bindings.Ogre.ClassSceneManager as SceneManager
import HGamer3D.Bindings.Ogre.ClassSceneNode as SceneNode
import HGamer3D.Bindings.Ogre.ClassRenderTarget as RenderTarget
import HGamer3D.Bindings.Ogre.ClassRenderWindow as RenderWindow
import HGamer3D.Bindings.Ogre.ClassResourceGroupManager as ResourceGroupManager
import HGamer3D.Bindings.Ogre.ClassTextureManager as TextureManager
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassViewport as Viewport
import HGamer3D.Bindings.Ogre.ClassFrustum as Frustum
import HGamer3D.Bindings.Ogre.ClassAnimationState as AnimationState
import HGamer3D.Bindings.Ogre.ClassEntity as Entity
import HGamer3D.Bindings.Ogre.ClassControllerManager as ControllerManager
import HGamer3D.Bindings.Ogre.ClassWindowEventUtilities as WindowEventUtilities

import HGamer3D.Bindings.Ogre.ClassManualObject as ManualObject
import HGamer3D.Bindings.Ogre.EnumRenderOperationOperationType
import HGamer3D.Bindings.Ogre.StructHG3DClass
import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType

import HGamer3D.Data.HG3DClass

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State

import HGamer3D.APIs.Base.Engine.Types
import HGamer3D.APIs.Base.Engine.Engine
import HGamer3D.APIs.Base.Graphics3D.EngineHelper
import HGamer3D.APIs.Base.Graphics3D.Basic3D


data Object3D = SingleObject3D HG3DClass HG3DClass | -- node entity 
				CombinedObject3D HG3DClass [Object3D] -- node, subnodes
				
getNode :: Object3D -> HG3DClass
getNode (SingleObject3D node entity) = node
getNode (CombinedObject3D node objects) = node

getEntities :: Object3D -> [HG3DClass]
getEntities (SingleObject3D node entity) = [entity]
getEntities (CombinedObject3D node objects) = foldl (++) [] (map getEntities objects)

data Material = NamedMaterial String 

data Mesh = CubeMesh | -- Cube Mesh-Type
			PlaneMesh | -- Plane Mesh-Type
			SphereMesh | -- Sphere Mesh-Type
			NamedMesh String | -- Mesh resource loaded from file
			ManualMesh String  -- Manual Mesh-Type, identified by name

createNamedMesh :: String -> MHGamer3D Mesh
createNamedMesh meshName = do
	return (NamedMesh meshName)
	
createColouredLineMesh :: Vec3 -> Vec3 -> Material -> Colour -> MHGamer3D Mesh
createColouredLineMesh vStart vEnd (NamedMaterial materialName) colour = do
	rs <- ask
	let g3s = graphics3DSystem rs
	lineName <- getUniqueName "Line"
	meshName <- getUniqueName "Mesh"
	mo <- liftIO $ SceneManager.createManualObject (g3sSceneManager g3s) lineName
	liftIO $ ManualObject.begin mo materialName OT_LINE_LIST "General"
	liftIO $ ManualObject.position mo vStart
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position mo vEnd
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.end mo
	liftIO $ ManualObject.convertToMesh mo meshName "General"
	return (ManualMesh meshName)

-- |Creates a cube by using the ManualObject functionality of Ogre
createColouredCubeMesh :: Material -> Colour -- ^The color of the line 
		-> MHGamer3D Mesh -- ^Return value is a mesh
createColouredCubeMesh (NamedMaterial materialName) colour = do
	

	rs <- ask
	let g3s = graphics3DSystem rs
	cubeName <- getUniqueName "Cube"
	meshName <- getUniqueName "Mesh"
	
	mo <- liftIO $ SceneManager.createManualObject (g3sSceneManager g3s) cubeName
	
	-- basic parameters
	let lsize = 1.0
	let cp = 1.0 * lsize
	let cm = -1.0 * lsize
	
	liftIO $ ManualObject.begin mo materialName OT_TRIANGLE_LIST "General"
	
	liftIO $ ManualObject.position2 mo cm cp cm   -- a vertex
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position2 mo cp cp cm   -- a vertex
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position2 mo cp cm cm   -- a vertex
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position2 mo cm cm cm   -- a vertex
	liftIO $ ManualObject.colour mo colour
	
	liftIO $ ManualObject.position2 mo cm cp cp   -- a vertex
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position2 mo cp cp cp   -- a vertex
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position2 mo cp cm cp   -- a vertex
	liftIO $ ManualObject.colour mo colour
	liftIO $ ManualObject.position2 mo cm cm cp   -- a vertex
	liftIO $ ManualObject.colour mo colour
	
	liftIO $ ManualObject.triangle mo 0 1 2
	liftIO $ ManualObject.triangle mo 2 3 0
	liftIO $ ManualObject.triangle mo 4 6 5
	liftIO $ ManualObject.triangle mo 6 4 7
	
	liftIO $ ManualObject.triangle mo 0 4 5 
	liftIO $ ManualObject.triangle mo 5 1 0
	liftIO $ ManualObject.triangle mo 2 6 7
	liftIO $ ManualObject.triangle mo 7 3 2

	liftIO $ ManualObject.triangle mo 0 7 4
	liftIO $ ManualObject.triangle mo 7 0 3
	liftIO $ ManualObject.triangle mo 1 5 6
	liftIO $ ManualObject.triangle mo 6 2 1
	
	liftIO $ ManualObject.end mo
	
	liftIO $ ManualObject.convertToMesh mo meshName "General"
	return (ManualMesh meshName)


-- create a rainbow coloured cube with manual ojbect lib
--
createRainbowCubeMesh :: MHGamer3D Mesh
createRainbowCubeMesh  = do

	rs <- ask
	let g3s = graphics3DSystem rs
	cubeName <- getUniqueName "Cube"
	meshName <- getUniqueName "Mesh"
	
	let materialName = "BaseWhiteNoLighting"
	
	mo <- liftIO $ SceneManager.createManualObject (g3sSceneManager g3s) cubeName
	
	-- basic parameters
	let lsize = 1.0
	let cp = 1.0 * lsize
	let cm = -1.0 * lsize

	liftIO $ ManualObject.begin mo "BaseWhiteNoLighting" OT_TRIANGLE_LIST "General"
	
	sequence $ map (\(x, y, z, c) -> liftIO $ ManualObject.position2 mo x y z >> ManualObject.colour mo c) [
		(cm, cp, cm, (Colour 0.0 1.0 0.0 1.0) ),
		(cp, cp, cm, (Colour 1.0 1.0 0.0 1.0) ),
		(cp, cm, cm, (Colour 1.0 0.0 0.0 1.0) ),
		(cm, cm, cm, (Colour 0.0 0.0 0.0 1.0) ),
		
		(cm, cp, cp, (Colour 0.0 1.0 1.0 1.0) ),
		(cp, cp, cp, (Colour 1.0 1.0 1.0 1.0) ),
		(cp, cm, cp, (Colour 1.0 0.0 1.0 1.0) ),
		(cm, cm, cp, (Colour 0.0 0.0 1.0 1.0) )   ]
	
	sequence $ map (\(x,y,z) -> liftIO $ ManualObject.triangle mo x y z) [
		(0, 1, 2),
		(2, 3, 0),
		(4, 6, 5),
		(6, 4, 7),
		
		(0, 4, 5),
		(5, 1, 0),
		(2, 6, 7),
		(7, 3, 2),
		
		(0, 7, 4),
		(7, 0, 3),
		(1, 5, 6),
		(6, 2, 1) ]
	
	liftIO $ ManualObject.end mo
	liftIO $ ManualObject.convertToMesh mo meshName "General"
	return (ManualMesh meshName)



createObject3DFromMesh :: Mesh -> MHGamer3D Object3D
createObject3DFromMesh mesh = do
	rs <- ask
	let g3s = graphics3DSystem rs
	-- first create entity from the mesh
	entity <- case mesh of 
		CubeMesh -> liftIO $ SceneManager.createEntity6 (g3sSceneManager g3s) PT_CUBE
		SphereMesh -> liftIO $ SceneManager.createEntity6 (g3sSceneManager g3s) PT_SPHERE
		PlaneMesh -> liftIO $ SceneManager.createEntity6 (g3sSceneManager g3s) PT_PLANE
		(NamedMesh name) -> liftIO $ SceneManager.createEntity3 (g3sSceneManager g3s) name
		(ManualMesh name) -> liftIO $ SceneManager.createEntity3 (g3sSceneManager g3s) name
	-- now create node and attach entity to it
	rootNode <- liftIO $ SceneManager.getRootSceneNode (g3sSceneManager g3s)
	let vzero = Vec3 0.0 0.0 0.0
	let qident = Q (Vec4 1.0 0.0 0.0 0.0)
	node <- liftIO $ SceneNode.createChildSceneNode rootNode vzero qident
	liftIO $ SceneNode.attachObject node entity
	-- return object
	return (SingleObject3D node entity)
		
createSphere :: MHGamer3D Object3D
createSphere = do
	ob <- createObject3DFromMesh SphereMesh
	return (ob)

createCube :: MHGamer3D Object3D
createCube = do
	ob <- createObject3DFromMesh CubeMesh
	return (ob)

createPlane :: MHGamer3D Object3D
createPlane = do
	ob <- createObject3DFromMesh PlaneMesh
	return (ob)

loadMesh :: String -> MHGamer3D Object3D
loadMesh name = do
	ob <- createObject3DFromMesh (NamedMesh name)
	return (ob)

createLine :: Vec3 -> Vec3 -> Colour -> MHGamer3D Object3D
createLine vStart vEnd colour = do
	mesh <- createColouredLineMesh vStart vEnd (NamedMaterial "BaseWhiteNoLighting") colour
	ob <- createObject3DFromMesh mesh
	return ob

createColouredLine vStart vEnd material colour = do
	mesh <- createColouredLineMesh vStart vEnd material colour
	ob <- createObject3DFromMesh mesh
	return ob

createColouredCube material colour = do
	mesh <- createColouredCubeMesh material colour
	ob <- createObject3DFromMesh mesh
	return ob

createRainbowCube = do
	mesh <- createRainbowCubeMesh 
	ob <- createObject3DFromMesh mesh
	return ob

instance Position3D Object3D where

	position3D obj = do
		pos <- liftIO $ Node.getPosition (getNode obj)
		return (pos)
		
	positionTo3D obj pos = do
		liftIO $ Node.setPosition  (getNode obj) pos
		return ()
	
instance Scale3D Object3D where

	scale3D obj = do
		pos <- liftIO $ Node.getScale  (getNode obj)
		return (pos)
		
	scaleTo3D obj pos = do
		liftIO $ Node.setScale  (getNode obj) pos
		return ()
	
instance Orientation3D Object3D where

	orientation3D obj = do
		q <- liftIO $ Node.getOrientation  (getNode obj)
		let uq = mkNormal q
		return uq
	
	orientationTo3D obj uq = do
		liftIO $ Node.setOrientation  (getNode obj) (fromNormal uq)
		return ()

setObjectMaterial :: Object3D -> Material -> MHGamer3D ()
setObjectMaterial object (NamedMaterial name) = do
	let entities = getEntities object
	sequence $ map (\entity -> liftIO $ Entity.setMaterialName entity name "General") entities
	return ()


-- |This function groups objects into a new object
-- it is not perfoming any geometric operations, it just groups the 
-- input objects. Can be used, to move and rotate a group.

combineObjects :: [Object3D] -- ^A list of objects, to be grouped
		-> MHGamer3D (Object3D) -- ^The return value is a new GraphicsObject
		
combineObjects listObjects = do
	rs <- ask
	let g3s = graphics3DSystem rs
	rootNode <- liftIO $ SceneManager.getRootSceneNode (g3sSceneManager g3s)
	let vzero = Vec3 0.0 0.0 0.0
	let qident = Q (Vec4 1.0 0.0 0.0 0.0)
	node <- liftIO $ SceneNode.createChildSceneNode rootNode vzero qident
	sequence_ (map ( \object -> do
						let objectnode = getNode object
						parent <- liftIO $ Node.getParent objectnode
						liftIO $ Node.removeChild2 parent objectnode
						liftIO $ Node.addChild node objectnode
						return ()
						)
								listObjects)
	return ( CombinedObject3D node listObjects)

-- yaw, roll, pitch functions
-- functions, to rotate on axis, relative to object
rotRelativeToObjectAxis :: Object3D -> Vec3 -> Float -> MHGamer3D ()
rotRelativeToObjectAxis object axis val = do
	qob <- orientation3D object
	let odir = actU qob axis
	let qrot = rotU odir val
	let nrot = qrot .*. qob
	orientationTo3D object nrot
	return ()
	
-- | rotate object on own axis (yaw) by angle
yaw3D :: Object3D -> Angle -> MHGamer3D ()
yaw3D object val = rotRelativeToObjectAxis object (Vec3 0.0 1.0 0.0) (fromAngle val)

-- | rotate object on own axis (roll) by angle
roll3D :: Object3D -> Angle -> MHGamer3D ()
roll3D object val = rotRelativeToObjectAxis object (Vec3 0.0 0.0 1.0) (fromAngle val)

-- | rotate object on own axis (pitch) by angle
pitch3D :: Object3D -> Angle -> MHGamer3D ()
pitch3D object val = rotRelativeToObjectAxis object (Vec3 1.0 0.0 0.0) (fromAngle val)

