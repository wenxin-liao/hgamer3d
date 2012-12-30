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
from PyHBind.Components import ConfigStore
from HeaderFiles import HeaderFilesView

import Enums


class StructsView(Enums.EnumsView):

	
#
# handler and functions, structs tab
#

	def reloadStructsGui(self, nlist):
		self.listBoxStructs.Clear()
		for name in nlist:
			self.listBoxStructs.Append(name)
		self.strucConfig = None

	def listBoxStructsHandler(self, event):
		name = self.listBoxStructs.GetStringSelection()
		ind = self.listBoxStructs.GetSelection()
		Publisher.sendMessage("config.struct.selected", (ind,name))
		
	def setStructConfigData(self, struct, attributes, methods, conf):
		# set info field
		self.structConfig = conf
		
		infostr = "name: " + struct.name + "\n"
		infostr = infostr + "id: " + struct.id + "\n"
		infostr = infostr + "brief: " + struct.brief + "\n"
		infostr = infostr + "detail: " + struct.detail + "\n"
		infostr = infostr + "protection: " + struct.protection + "\n"
		infostr = infostr + "\nattributes:\n"
		for attr in attributes:
			infostr = infostr + "  " + attr.type + " " + attr.name + "  (" + attr.brief + "|" + attr.detail +")\n"
		infostr = infostr + "\nmethods:\n"
		for (m, p) in methods:
			infostr = infostr + "  " + m.type + " " + m.name + "  (" + m.brief + "|" + m.detail +")\n"
		self.txtCtrlStructInfo.SetValue(infostr)
		
		# set struct configs
		self.txtCtrlStructHName.SetValue(conf.htype)
		self.txtCtrlStructValuePrefix.SetValue(conf.valuePrefix)
		self.txtCtrlStructStructName.SetValue(conf.structname)
		self.txtCtrlStructComment.SetValue(conf.comment)
		self.txtCtrlStructHeaderFile.SetValue(conf.hFile)
		self.txtCtrlStructChsFile.SetValue(conf.chsFile)
		self.txtCtrlStructCTypes.SetValue(conf.cTypes)
		self.checkBoxStructSelected.SetValue(conf.selected)
		
	def structCTypesSetHandler(self, event):
		if self.structConfig:
			self.structConfig.cTypes = self.txtCtrlStructCTypes.GetValue()
		
	def structHNameSetHandler( self, event ):
		if self.structConfig:
			self.structConfig.htype = self.txtCtrlStructHName.GetValue()
			self.txtCtrlStructName.SetValue("Struct" + self.structConfig.htype)
	
	def structValuePrefixSetHandler( self, event ):
		if self.structConfig:
			self.structConfig.valuePrefix = self.txtCtrlStructValuePrefix.GetValue()
	
	def structStructNameSetHandler( self, event ):
		if self.structConfig:
			self.structConfig.structname = self.txtCtrlStructStructName.GetValue()
	
	def structSelectedHandler( self, event ):
		if self.structConfig:
			self.structConfig.selected = self.checkBoxStructSelected.GetValue()
	
	def structCommentHandler( self, event ):
		if self.structConfig:
			self.structConfig.comment = self.txtCtrlStructComment.GetValue()
	
	def structHeaderFileHandler( self, event ):
		if self.structConfig:
			self.structConfig.hFile = self.txtCtrlStructHeaderFile.GetValue()
	
	def stuctChsFileHandler( self, event ):
		if self.structConfig:
			self.structConfig.chsFile = self.txtCtrlStructChsFile.GetValue()
		
		
