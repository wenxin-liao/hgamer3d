"""
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2011, 2012 Peter Althainz

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

import wx
import wx.xrc
from wx.lib.pubsub import Publisher
import os, os.path, uuid
import GuiBase

class MyDialogCreateNamespace(GuiBase.DialogAddNamespace):
	
	def okHandler( self, event ):
		# check name not empty
		name = (self.txtCtrlNamespace.GetValue()).strip()
		if len(name) > 0:
			Publisher().sendMessage("config.namespace.add", name)
		self.Close()
	
	def cancelHandler( self, event ):
		self.Close()

class MyDialogCreateBasicType(GuiBase.DialogCreateBasicType):
	
	def okHandler( self, event ):
		# check name not empty
		name = (self.txtCtrlTypeName.GetValue()).strip()
		if len(name) > 0:
			Publisher().sendMessage("config.basictypes.add", "Type" + name)
		self.Close()
	
	def cancelHandler( self, event ):
		self.Close()

class MyDialogAddCTypeMapping(GuiBase.DialogAddCTypeMapping):
	
	def setFrame(self, frame):
		self.frame = frame

	# Virtual event handlers, overide them in your derived class
	def okHandler( self, event ):
		# check name not empty
		typename = (self.txtCtrlTypeName.GetValue()).strip()
		if len(typename) > 0:
			Publisher.sendMessage("config.basictypes.addmapping", typename)
		self.Close()
	
	def cancelHandler( self, event ):
		self.Close()


class DialogUtils():
	
	def __init__(self, parent):
		self.parent = parent
					
	def selectFile(self, initDir, fSave, message, selector):
		
		if fSave:
			style = wx.SAVE
		else:
			style = wx.OPEN & wx.FILE_MUST_EXIST
		dlg = wx.FileDialog(self.parent, message, initDir, "", selector, style)
		r = None
		if dlg.ShowModal() == wx.ID_OK:
			filename = dlg.GetFilename()
			dirname = dlg.GetDirectory()
			r = (filename, dirname)
		dlg.Destroy()
		return r

	def selectDirectory(self, initDir, fMustExist, message):
		
		style = wx.DD_DEFAULT_STYLE
		if fMustExist:
			style = wx.DD_DIR_MUST_EXIST
		dlg = wx.DirDialog(self.parent, message, initDir, style)
		r = None
		if dlg.ShowModal() == wx.ID_OK:
			r = dlg.GetPath()
		dlg.Destroy()
		return r

