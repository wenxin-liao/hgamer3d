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

from wx.lib.pubsub import Publisher
import PyHBind.Apps.Studio

class BasicTypesController:

	def __init__(self, model, view):
		self.model = model
		self.view = view
		
		Publisher.subscribe(self.psDelBasicTypesItem, "config.basictypes.delitem")
		Publisher.subscribe(self.psAddBasicTypesDialog, "config.basictypes.adddialog")
		Publisher.subscribe(self.psAddBasicTypes, "config.basictypes.add")
		Publisher.subscribe(self.psSetBasicTypeProps, "config.basictypes.set")
		Publisher.subscribe(self.psAddBasicCTypeMappingDialog, "config.basictypes.adddialogmapping")
		Publisher.subscribe(self.psAddBasicCTypeMapping, "config.basictypes.addmapping")
		Publisher.subscribe(self.psDelBasicCTypeMapping, "config.basictypes.delmapping")

		Publisher.subscribe(self.generateDefaultTypes, "config.basictypes.generatedefault")

	def generateDefaultTypes(self, message):
		# create all the default types
		for (name, ctype, htype, inm, outm, mct, fString, cm) in [
		
			("TypeInt", "int", "Int", "fromIntegral", "peekIntConv", ["int", "size_t", "long"], False, "normal integer (two word)"),
			("TypeUnsignedInt", "unsigned int", "Int", "fromIntegral", "peekIntConv", ["unsigned int", "uint"], False, "unsigned integer"),
			("TypeUnsignedWord", "unsigned short", "Int", "fromIntegral", "peekIntConv", ["unsigned short"], False, "unsigned short integer (one word)"),

			("TypeFloat", "float", "Float", "realToFrac", "peekFloatConv", ["float"], False, "normal float (two word)"),
			("TypeDouble", "double", "Double", "realToFrac", "peekFloatConv", ["double"], False, "normal double (four word)"),
			("TypeBool", "int", "Bool", "fromBool", "peekBoolUtil", ["bool"], False, "bool"),
			
			("TypeString", "char", "String", "withCString", "peekCString", ["char"], True, "normal char string"),
			("TypeWString", "wchar_t", "String", "withCUString", "peekCUString", ["wchar_t"], True, "wide char string"),
		]:
			bt = self.model.addBasicType(name)
			bt.ctype = ctype
			bt.htype = htype
			bt.inMarshaller = inm
			bt.outMarshaller = outm
			bt.flagString = fString
			bt.comment = cm
			for t in mct:
				self.model.addBasicTypeCTypeMapping(t, bt)
		self.view.clearGuiBasicTypesSelection()
		

	# basic type implementation
	
	def psDelBasicTypesItem(self, message):
		item = message.data
		toDel = -1
		ind = 0
		for i in self.model.getBasicTypes():
			if i.name == item.name:
				toDel = ind
				self.model.removeMappedCTypes(i.mappedCTypes)
			ind = ind + 1
		if toDel >= 0:
			del self.model.getBasicTypes()[toDel]
			self.view.delBasicTypesIndex(toDel)


	def psAddBasicTypesDialog(self, message):
		dialog = PyHBind.Apps.Studio.View.MyDialogCreateBasicType(self.view)
		dialog.ShowModal()
		dialog.Destroy()
		
	def psAddBasicTypes(self, message):
		name = message.data
		self.model.addBasicType(name)
		self.view.addBasicType(name)

	def psSetBasicTypeProps(self, message):
		item = self.view.getBasicTypesSelectedItem()
		if item:
			topic = message.topic[3]
			if topic == "name":
				item.name = message.data
			elif topic == "ctype":
				item.ctype = message.data
			elif topic == "htype":
				item.htype = message.data
			elif topic == "inmarshaller":
				item.inMarshaller = message.data
			elif topic == "outmarshaller":
				item.outMarshaller = message.data
				
	def psAddBasicCTypeMappingDialog(self, message):
		item = self.view.getBasicTypesSelectedItem()
		if item:
			dialog = PyHBind.Apps.Studio.View.MyDialogAddCTypeMapping(self.view)
			dialog.ShowModal()
			dialog.Destroy()
		
	def psAddBasicCTypeMapping(self, message):
		selecteditem = self.view.getBasicTypesSelectedItem()
		typename = message.data
		if selecteditem:
			mappedCTypes = self.model.getMappedCTypes(selecteditem.mappedCTypes)
			fadd = True
			for ctype in mappedCTypes:
				if typename == ctype.type:
					fadd = False
			if fadd: 
				self.model.addBasicTypeCTypeMapping(typename, selecteditem)
				self.view.addBasicTypeCTypeMapping(typename, selecteditem)

	def psDelBasicCTypeMapping(self, message):
		selecteditem = self.view.getBasicTypesSelectedItem()
		if selecteditem:
			typename = self.view.getBasicTypeCTypeMappingSelectedItem()
			mappedCTypes = self.model.getMappedCTypes(selecteditem.mappedCTypes)
			ind = 0
			toDel = -1
			for ctype in mappedCTypes:
				if ctype.type == typename:
					toDel = ind
				ind = ind + 1
			if toDel >= 0:
				self.view.delBasicTypeCTypeMapping(toDel)
				del self.model.getMappedCTypes(selecteditem.mappedCTypes)[toDel]
	
