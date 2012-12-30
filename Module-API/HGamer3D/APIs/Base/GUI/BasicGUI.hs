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

-- BasicGUI.hs

-- |Base API, GUI Module, BasicGUI Sub-Module.
-- Basic GUI functionality.


module HGamer3D.APIs.Base.GUI.BasicGUI

(
	GUIElement (..),
	
	loadGuiLayoutFromFile,
	addGuiElToDisplay,
	removeGuiElFromDisplay,
	
	enableGuiEl,
	disableGuiEl,
	activateGuiEl,
	deactivateGuiEl,
	showGuiEl,
	hideGuiEl,
	
	getChildGuiEl,
	findChildGuiElRecursive,
	getGuiElProperty,
	setGuiElProperty,
	
	loadGuiScheme,
	loadGuiFont,
	setGuiDefaultFont,
	setGuiDefaultMouseCursor,
	setGuiDefaultTooltip,
	
	listboxAddText,
	comboboxAddText,
	
	mapGuiElEventToFunction,
	
	
)

where

import GHC.Ptr

import HGamer3D.APIs.Base.Engine.Types
import HGamer3D.APIs.Base.Engine.Engine
import HGamer3D.Bindings.CEGUI.ClassPtr
import HGamer3D.Bindings.CEGUI.Utils

import HGamer3D.Bindings.CEGUI.ClassWindow as Window
import HGamer3D.Bindings.CEGUI.ClassWindowManager as WindowManager
import HGamer3D.Bindings.CEGUI.ClassWindowManagerHG3D as WindowManagerHG3D
import HGamer3D.Bindings.CEGUI.ClassWindow as Window
import HGamer3D.Bindings.CEGUI.ClassSystem as System
import HGamer3D.Bindings.CEGUI.ClassSystemHG3D as SystemHG3D
import HGamer3D.Bindings.CEGUI.ClassPropertySet as PropertySet
import HGamer3D.Bindings.CEGUI.ClassHG3DEventStaticFunctions as EvtSF 
import HGamer3D.Bindings.CEGUI.ClassHG3DListboxStaticFunctions as ListboxSF
import HGamer3D.Bindings.CEGUI.ClassHG3DWindowStaticFunctions as WindowSF

import HGamer3D.Data.HG3DClass
import Control.Monad.Trans
import Control.Monad.Reader
import HGamer3D.Data.Vector

data GUIElement = GUIElement HG3DClass

loadGuiLayoutFromFile :: String -> String -> MHGamer3D GUIElement
loadGuiLayoutFromFile layoutFile prefix = do
	rs <- ask
	let gui = guiSystem rs
	let hg3dWinMgr = guiWindowManagerHG3D gui
	window <- liftIO $ WindowManagerHG3D.loadWindowLayoutHG3D hg3dWinMgr layoutFile prefix
	return (GUIElement window)

addGuiElToDisplay :: GUIElement -> MHGamer3D ()
addGuiElToDisplay (GUIElement window) = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	guiSheet <- liftIO $ System.getGUISheet guiS
	liftIO $ Window.addChildWindow2 guiSheet window

removeGuiElFromDisplay :: GUIElement -> MHGamer3D ()
removeGuiElFromDisplay (GUIElement window) = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	guiSheet <- liftIO $ System.getGUISheet guiS
	liftIO $ Window.removeChildWindow2 guiSheet window

enableGuiEl :: GUIElement -> MHGamer3D ()
enableGuiEl (GUIElement window) = do
	liftIO $ Window.enable window

disableGuiEl :: GUIElement -> MHGamer3D ()
disableGuiEl (GUIElement window) = do
	liftIO $ Window.disable window

activateGuiEl :: GUIElement -> MHGamer3D ()
activateGuiEl (GUIElement window) = do
	liftIO $ Window.activate window

deactivateGuiEl :: GUIElement -> MHGamer3D ()
deactivateGuiEl (GUIElement window) = do
	liftIO $ Window.deactivate window

showGuiEl :: GUIElement -> MHGamer3D ()
showGuiEl (GUIElement window) = do
	liftIO $ Window.show window

hideGuiEl :: GUIElement -> MHGamer3D ()
hideGuiEl (GUIElement window) = do
	liftIO $ Window.hide window

getChildGuiEl :: GUIElement -> String -> MHGamer3D GUIElement
getChildGuiEl (GUIElement window) name = do
	window <- liftIO $ Window.getChild window name
	return (GUIElement window)

findChildGuiElRecursive :: GUIElement -> String -> MHGamer3D (Maybe GUIElement)
findChildGuiElRecursive (GUIElement window) name = do
	window <- liftIO $ Window.getChildRecursive window name
	if (ocPtr window) == nullPtr then do
		return Nothing
		else do
			return (Just (GUIElement window))
	
getGuiElProperty :: GUIElement -> String -> MHGamer3D String
getGuiElProperty (GUIElement window) name = do
	prop <- liftIO $ PropertySet.getProperty window name
	return prop

setGuiElProperty :: GUIElement -> String -> String -> MHGamer3D ()
setGuiElProperty (GUIElement window) name value = do
	liftIO $ PropertySet.setProperty window name value

loadGuiScheme :: String -> MHGamer3D ()
loadGuiScheme schemeName = do
	rs <- ask
	let gui = guiSystem rs
	liftIO $ SystemHG3D.schemeManagerCreate (guiSchemeManager gui) schemeName

loadGuiFont :: String -> MHGamer3D ()	
loadGuiFont fontName = do
	rs <- ask
	let gui = guiSystem rs
	liftIO $ SystemHG3D.fontManagerCreate (guiFontManager gui) fontName
	
setGuiDefaultFont :: String -> MHGamer3D ()	
setGuiDefaultFont fontName = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	liftIO $ System.setDefaultFont guiS fontName
	
setGuiDefaultMouseCursor :: String -> String -> MHGamer3D ()	
setGuiDefaultMouseCursor schemeName cursorName = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	liftIO $ System.setDefaultMouseCursor3 guiS schemeName cursorName

setGuiDefaultTooltip :: String -> MHGamer3D ()	
setGuiDefaultTooltip ttName = do
	rs <- ask
	let gui = guiSystem rs
	let guiS = guiGUI gui
	liftIO $ System.setDefaultTooltip2  guiS  ttName

mapGuiElEventToFunction :: GUIElement -> String -> EventFunction a -> EventMap a -> MHGamer3D (EventMap a)
mapGuiElEventToFunction (GUIElement window) eventName function eventMap = do
	functionTag <- getUniqueName "Event"
	let newmap = mapFunctionToTag function functionTag eventMap
	liftIO $ EvtSF.subscribeScriptedEvent window eventName functionTag
	return newmap

comboboxAddText :: GUIElement -> String -> MHGamer3D ()
comboboxAddText (GUIElement window) itemname = do
	realcombo <- liftIO $ WindowSF.castWindowToCombobox window
	liftIO $ ListboxSF.comboboxAddItem realcombo itemname
	
listboxAddText :: GUIElement -> String -> MHGamer3D ()
listboxAddText (GUIElement window) itemname = do
	reallistbox <- liftIO $ WindowSF.castWindowToListbox window
	liftIO $ ListboxSF.listboxAddItem reallistbox itemname
	
	
