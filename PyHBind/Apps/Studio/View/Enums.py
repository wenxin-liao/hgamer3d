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

import BasicTypes


class EnumsView(BasicTypes.BasicTypesView):


#
# enums tab
#
		
	def reloadEnumsGui(self, nlist):
		self.listBoxEnums.Clear()
		for name in nlist:
			self.listBoxEnums.Append(name)
		self.enumConfig = None

	def listBoxEnumsHandler(self, event):
		name = self.listBoxEnums.GetStringSelection()
		ind = self.listBoxEnums.GetSelection()
		Publisher.sendMessage("config.enum.selected", (ind,name))
		
	def setEnumConfigData(self, en, val, conf):
		self.enumConfig = conf
		
		infostr = "name: " + en.name + "\n"
		infostr = infostr + "id: " + en.id + "\n"
		infostr = infostr + "brief: " + en.brief + "\n"
		infostr = infostr + "detail: " + en.detail + "\n"
		infostr = infostr + "protection: " + en.protection + "\n"
		infostr = infostr + "\nvalues:\n"
		for v in val:
			infostr = infostr + "  " + v.name + " " + v.value + "  (" + v.brief + "|" + v.detail +")\n"
		self.txtCtrlEnumInfo.SetValue(infostr)
		
		# set enum configs
		self.txtCtrlEnumName.SetValue("Enum" + conf.nameSuffix)
		self.txtCtrlEnumNameSuffix.SetValue(conf.nameSuffix)
		self.txtCtrlEnumValuePrefix.SetValue(conf.valuePrefix)
		self.txtCtrlEnumComment.SetValue(conf.comment)
		self.checkBoxEnumSelected.SetValue(conf.selected)
		
	def enumNameSuffixHandler( self, event ):
		if self.enumConfig:
			self.enumConfig.nameSuffix = self.txtCtrlEnumNameSuffix.GetValue()
			self.txtCtrlEnumName.SetValue("Enum" + self.enumConfig.nameSuffix)
	
	def enumValuePrefixHandler( self, event ):
		if self.enumConfig:
			self.enumConfig.valuePrefix = self.txtCtrlEnumValuePrefix.GetValue()
	
	def checkboxEnumSelectedHandler( self, event ):
		if self.enumConfig:
			self.enumConfig.selected = self.checkBoxEnumSelected.GetValue()
	
	def enumCommentHandler( self, event ):
		if self.enumConfig:
			self.enumConfig.comment = self.txtCtrlEnumComment.GetValue()
	


