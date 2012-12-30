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

import Structs


class ClassesView(Structs.StructsView):

#
# classes tab
#

	def reloadClassesGui(self, nlist):
		self.classConfig = None
		self.classMethod = None
		self.listBoxClasses.Clear()
		for name in nlist:
			self.listBoxClasses.Append(name)

	def listBoxClassesHandler(self, event):
		name = self.listBoxClasses.GetStringSelection()
		ind = self.listBoxClasses.GetSelection()
		Publisher.sendMessage("config.class.selected", (ind,name))
		
	def setClassConfigData(self, struct, attributes, methods, conf, parents, childs):
		# set info field
		self.classConfig = conf
		self.classMethods = methods
		
		infostr = "name: " + struct.name + "\n"
		infostr = infostr + "id: " + struct.id + "\n"
		infostr = infostr + "brief: " + struct.brief + "\n"
		infostr = infostr + "detail: " + struct.detail + "\n"
		infostr = infostr + "protection: " + struct.protection + "\n"
		
		infostr = infostr + "\nparent clases:\n"
		for par in parents:
			infostr = infostr + "  " + par + "\n"
			
			
		infostr = infostr + "\nchild clases:\n"
		for chi in childs:
			infostr = infostr + "  " + chi + "\n"
			
		infostr = infostr + "\nattributes:\n"
		for attr in attributes:
			infostr = infostr + "  " + attr.type + " " + attr.name + "  (" + attr.brief + "|" + attr.detail +")\n"

		self.txtCtrlClassInfo.SetValue(infostr)
		
		self.listBoxClassMethods.Clear()
		for (m, p) in methods:
			self.listBoxClassMethods.Append(m.name)
		self.classMethod = None
		
		# set struct configs
		self.txtCtrlClassHName.SetValue(conf.htype)
		self.txtCtrlClassPrefix.SetValue(conf.prefix)
		self.txtCtrlClassComment.SetValue(conf.comment)
		self.checkBoxClassSelected.SetValue(conf.selected)
		
	def classHNameSetHandler( self, event ):
		if self.classConfig:
			self.classConfig.htype = self.txtCtrlClassHName.GetValue()
			self.txtCtrlClassName.SetValue("Class" + self.classConfig.htype)
	
	def classPrefixSetHandler( self, event ):
		if self.classConfig:
			self.classConfig.prefix = self.txtCtrlClassPrefix.GetValue()
	
	def classSelectedHandler( self, event ):
		if self.classConfig:
			self.classConfig.selected = self.checkBoxClassSelected.GetValue()
	
	def classCommentHandler( self, event ):
		if self.classConfig:
			self.classConfig.comment = self.txtCtrlClassComment.GetValue()
	
	def listBoxClassMethodsHandler( self, event ):
		name = self.listBoxClassMethods.GetStringSelection()
		ind = self.listBoxClassMethods.GetSelection()
		i = 0
		self.classMethod = None
		for (m, p) in self.classMethods:
			if i == ind:
				self.classMethod = m
				self.classMethodParameters = p
			i = i + 1
				
		if self.classMethod:
			Publisher.sendMessage("config.classmethod.selected", (self.classMethod, self.classMethodParameters))


	def displayClassMethodInfo(self, infostr, name):
		self.txtCtrlClassMethodInfo.SetValue(infostr)
		self.txtCtrlClassMethodName.SetValue(name)
			
			

	def classMethodExcludedHandler(self, event):
		if self.classMethod and self.classConfig:
			flag = self.checkBoxClassMethodExcluded.GetValue()
			Publisher.sendMessage("config.classmethod.excluded", (self.classMethod.id, flag))
			
	def classMethodSetExcludedFlag(self, flag):
		self.checkBoxClassMethodExcluded.SetValue(flag)
	
	
	

