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

import Config


class BasicTypesView(Config.ConfigView):


	
	#
	# handler basic types in types tab
	#

	# selection changed
	def listBoxBasicTypesHandler(self, event):
		name = self.listBoxBasicTypes.GetStringSelection()
		item = None
		for i in self.model.getBasicTypes():
			if i.name == name:
				item = i
		if item:
			self.selectedBasicTypesItem = item
			self.setGuiBasicTypes()
			
	def generateBasicTypesHandler(self, event):
		Publisher().sendMessage("config.basictypes.generatedefault", None)
			
	def checkBoxBasicTypeHandler(self, event):
		if self.selectedBasicTypesItem:
			self.selectedBasicTypesItem.flagString = self.checkBoxBasicTypeString.GetValue()
		
	def txtCtrlBasicTypeCommentHandler(self, event):
		if self.selectedBasicTypesItem:
			self.selectedBasicTypesItem.comment = self.txtCtrlBasicTypeComment.GetValue()
			
	def basicTypeStringConstructorHandler(self, event):
		if self.selectedBasicTypesItem:
			if self.selectedBasicTypesItem.flagString:
				self.selectedBasicTypesItem.stringConstructor = self.txtCtrlStringConstructor.GetValue()
			
	def basicTypeAsCharPtrMethodHandler(self, event):
		if self.selectedBasicTypesItem:
			if self.selectedBasicTypesItem.flagString:
				self.selectedBasicTypesItem.asCharPointerMethod = self.txtCtrlAsCharPtrMethod.GetValue()
			
			

	def setGuiBasicTypes(self):
		
		item = self.selectedBasicTypesItem
		# set name, ctype, htype, marshaller
		if item:
			self.txtCtrlBasicTypeName.SetValue(item.name)
			self.txtCtrlBasicTypeCType.SetValue(item.ctype)
			self.txtCtrlBasicTypeHType.SetValue(item.htype)
			self.txtCtrlBasicTypeInMarshaller.SetValue(item.inMarshaller)
			self.txtCtrlBasicTypeOutMarshaller.SetValue(item.outMarshaller)
			self.txtCtrlBasicTypeComment.SetValue(item.comment)
			self.checkBoxBasicTypeString.SetValue(item.flagString)
			if item.flagString:
				self.txtCtrlStringConstructor.SetValue(item.stringConstructor)
				self.txtCtrlAsCharPtrMethod.SetValue(item.asCharPointerMethod)
			else:
				self.txtCtrlStringConstructor.SetValue("")
				self.txtCtrlAsCharPtrMethod.SetValue("")
				
				
				
		# set ctype mappings
			self.listBoxMappedCTypes.Clear()
			mappedCTypes = self.model.getMappedCTypes(item.mappedCTypes)
			for ctype in mappedCTypes:
				self.listBoxMappedCTypes.Append(ctype.type)

	def clearGuiBasicTypesSelection(self):
		# types
		self.txtCtrlBasicTypeName.SetValue("")
		self.txtCtrlBasicTypeCType.SetValue("")
		self.txtCtrlBasicTypeHType.SetValue("")
		self.txtCtrlBasicTypeInMarshaller.SetValue("")
		self.txtCtrlBasicTypeOutMarshaller.SetValue("")
		self.listBoxBasicTypes.Clear()
		self.listBoxMappedCTypes.Clear()
		for item in self.model.getBasicTypes():
			self.listBoxBasicTypes.Append(item.name)


	# delete item
	def listBoxBasicTypesKeyUpHandler(self, event):
		if event.GetKeyCode() == wx.WXK_DELETE:
			item = self.selectedBasicTypesItem
			if item:
				Publisher().sendMessage("config.basictypes.delitem", item)
				
	def delBasicTypesIndex(self, ind):
		self.listBoxBasicTypes.Delete(ind)
		self.clearGuiBasicTypesSelection()		

	def addBasicTypeHandler( self, event ):
		Publisher.sendMessage("config.basictypes.adddialog", None)

	def addBasicType(self, name):
		self.listBoxBasicTypes.Append(name)
		
	def getBasicTypesSelectedItem(self):
		return self.selectedBasicTypesItem
		
	def basicTypeNameSetHandler( self, event ):
		name = self.txtCtrlBasicTypeName.GetValue()
		Publisher.sendMessage("config.basictypes.set.name", name)
	
	def basicTypeCTypeSetHandler( self, event ):
		ctype = self.txtCtrlBasicTypeCType.GetValue()
		Publisher.sendMessage("config.basictypes.set.ctype", ctype)
	
	def basicTypeHTypeSetHandler( self, event ):
		htype = self.txtCtrlBasicTypeHType.GetValue()
		Publisher.sendMessage("config.basictypes.set.htype", htype)
		
	def basicTypeInMarshallerSetHandler( self, event ):
		inMarshaller = self.txtCtrlBasicTypeInMarshaller.GetValue()
		Publisher.sendMessage("config.basictypes.set.inmarshaller", inMarshaller)
	
	def basicTypeOutMarshallerSetHandler( self, event ):
		outMarshaller = self.txtCtrlBasicTypeOutMarshaller.GetValue()
		Publisher.sendMessage("config.basictypes.set.outmarshaller", outMarshaller)

	def addCTypeHandler( self, event ):
		Publisher.sendMessage("config.basictypes.adddialogmapping", None)
		
	def addBasicTypeCTypeMapping(self, typename, item):
		self.listBoxMappedCTypes.Append(typename)

	def listBoxMappedCTypesKeyUpHandler(self, event):
		if event.GetKeyCode() == wx.WXK_DELETE:
			Publisher.sendMessage("config.basictypes.delmapping", None)
			
	def getBasicTypeCTypeMappingSelectedItem(self):
		return self.listBoxMappedCTypes.GetStringSelection()

	def delBasicTypeCTypeMapping(self, ind):
		self.listBoxMappedCTypes.Delete(ind)
		
