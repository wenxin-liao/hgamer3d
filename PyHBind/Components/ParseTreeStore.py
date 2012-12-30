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

# ParseTreeStore.py

#
# Data Structures of parsed header files (generated for example by DoxyParser Application)
#

import os.path
from PyHBind.Base import ItemStore

class ParseTreeStore(ItemStore):
	
	def __init__(self):
		  
		self.types = {
		
			"Class" : [("name", "str", ""),
					   ("id", "str", ""),
					   ("protection", "str", "public"),
					   ("brief", "str", ""),
					   ("detail", "str", ""),
					   ("attributes", "str", ""),
					   ("methods", "str", ""),
					   ("flagtemplate", "bool", False),
					   ("templateparameters", "str", ""),
					   ("includefile", "str", "dummy-includefile"),
					   ("lineStart", "int", 0),
					   ("lineEnd", "int", -1),
					  ],
					  
			"ClassRef" : [
					   ("parent", "str", ""),
					   ("child", "str", ""),
					  ],
							
			"Struct" : [("name", "str", ""),
					   ("id", "str", ""),
					   ("protection", "str", "public"),
					   ("brief", "str", ""),
					   ("detail", "str", ""),
					   ("attributes", "str", ""),
					   ("methods", "str", ""),
					   ("flagtemplate", "bool", False),
					   ("templateparameters", "str", ""),
					   ("includefile", "str", "dummy-includefile"),
					   ("lineStart", "int", 0),
					   ("lineEnd", "int", -1),
					  ],
							
			"Variable"  : [ ("name", "str", ""),
			                ("id", "str", ""),
			                ("type", "str", ""),
			                ("protection", "str", ""),
						    ("brief", "str", ""),
						    ("detail", "str", ""),
	
						  ],
							
			"Method" : [   ("name", "str", ""),
						   ("id", "str", ""),
						   ("type", "str", ""),
			               ("protection", "str", ""),
						   ("static", "bool", False),
						   ("virtual", "bool", True),
						   ("parameters", "str", ""),
						   ("brief", "str", ""),
						   ("detail", "str", ""),
						   ("returncomment", "str", ""),
					     ],
					  
			"Function" : [   ("name", "str", ""),
						   ("id", "str", ""),
						   ("type", "str", ""),
			               ("protection", "str", ""),
						   ("static", "bool", False),
						   ("virtual", "bool", True),
						   ("parameters", "str", ""),
						   ("brief", "str", ""),
						   ("detail", "str", ""),
						   ("returncomment", "str", ""),
					     ],
					  
			"Parameter" : [("name", "str", ""),
					       ("type", "str", ""),
					       ("flagdefault", "bool", False),
					       ("default", "str", ""),
					       ("out", "bool", False),
					       ("description", "str", "")
					      ],	
					      
			"Templateparameter" : [	("name", "str", ""),
									("type", "str", ""),
								  ],
					      
			"Enum" : [	("name", "str", ""),
						("id", "str", ""),
						("protection", "str", ""),
						("brief", "str", ""),
						("detail", "str", ""),
						("values", "str", ""),
					    ("includefile", "str", "dummy-includefile"),
					    ("lineStart", "int", 0),
					    ("lineEnd", "int", -1),
					 ],
					 
			"EnumValue" : [	("name", "str", ""),
							("id", "str", ""),
							("protection", "str", ""),
							("brief", "str", ""),
							("detail", "str", ""),
					        ("flagvalue", "bool", False),
							("value", "str", ""),
					      ],

			"Typedef" : [	("name", "str", ""),
							("id", "str", ""),
							("protection", "str", ""),
							("brief", "str", ""),
							("detail", "str", ""),
					        ("type", "str", ""),
							("includefile", "str", "dummy-includefile"),
							("lineStart", "int", 0),
							("lineEnd", "int", -1),
					      ],
				}
				
		ItemStore.__init__(self, self.types)
		
	def initialize(self):
		self.setItem("/classes", [])
		self.setItem("/enums", [])
		self.setItem("/functions", [])
		self.setItem("/structs", [])
		self.setItem("/typedefs", [])
		self.setItem("/hierarchy", [])
		
	# class hierarchy
	
	def appendHierarchyItem(self, classref):
		self.getItem("/hierarchy").append(classref)

	# template parameters
	def addClassTemplateParameters(self, templateparameters, params):
		self.setItem("/classes-template-parameters/" + templateparameters, params)
	def addStructTemplateParameters(self, templateparameters, params):
		self.setItem("/structs-template-parameters/" + templateparameters, params)
		
	# class and struct
	def addClass(self, cl, attributes, methods):
		self.getItem("/classes").append(cl)
		self.setItem("/classes-attributes/" + cl.attributes, attributes)
		self.setItem("/classes-methods/" + cl.methods, methods)
	def addStruct(self, cl, attributes, methods):
		self.getItem("/structs").append(cl)
		self.setItem("/structs-attributes/" + cl.attributes, attributes)
		self.setItem("/structs-methods/" + cl.methods, methods)

	# enums
	def addEnum(self, enum, values):
		self.setItem("/enums-values/" + enum.values, values)
		self.getItem("/enums").append(enum)

	# typedefs
	def addTypedef(self, td):
		self.getItem("/typedefs").append(td)
		
	# functions
	def addFunction(self, func, paras):
		self.setItem("/functions-parameters/" + func.parameters, paras)
		self.getItem("/functions").append(func)

	# methods parameters
	def addClassMethodParameters(self, parid, paras):	
		self.setItem("/classes-method-parameters/" + parid, paras)
	def addStructMethodParameters(self, parid, paras):	
		self.setItem("/structs-method-parameters/" + parid, paras)
					




# after parsing the data, results are present in more then one file, to access it
# the data model of parse tree store contains many files, one for each doxygen dir
	
class MultiParseTreeStore:
	
	def __init__(self):
		pass
	
	def getUnknownItemById(self, refid):
		for itemList in [
			self._getTypeItemList("/enums"),
			self._getTypeItemList("/structs"),
			self._getTypeItemList("/classes") ]:
				for item in itemList:
					if item.id == refid:
						return item

	def loadData(self, fileArray):
	
		self.data = {}
		for (filename, uuid) in fileArray:
			store = ParseTreeStore()
			if os.path.exists(filename):
				store.read(filename)
				self.data[uuid] = store
			else:
				print "MultiParseTreeStore, path not found:", filename
				
	def _getTypeNameList(self, typename):
		newlist = []
		for store in self.data.values():
			for item in store.getItem(typename):
				newlist.append(item.name)
		return newlist 
		
	def _getTypeItemList(self, typename):
		newlist = []
		for store in self.data.values():
			for item in store.getItem(typename):
				newlist.append(item)
		return newlist 
		
	def _getTypeItemByIndex(self, typename, index):
		newlist = []
		for store in self.data.values():
			for item in store.getItem(typename):
				newlist.append(item)
		return newlist[index]

	def _getTypeItemById(self, typename, idtag):
		for store in self.data.values():
			for item in store.getItem(typename):
				if idtag == item.id:
					return item
		return None

	def _getEnumValuesByRefId(self, refid):
		key = "/enums-values/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		return []
		
	def _getStructAttributesByRefId(self, refid):
		key = "/structs-attributes/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		
	def _getStructMethodsByRefId(self, refid):
		key = "/structs-methods/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		
	def _getStructMethodParametersByRefId(self, refid):
		key = "/structs-method-parameters/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		
	def _getClassAttributesByRefId(self, refid):
		key = "/classes-attributes/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		
	def _getClassMethodsByRefId(self, refid):
		key = "/classes-methods/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		
	def _getClassMethodParametersByRefId(self, refid):
		key = "/classes-method-parameters/" + refid
		for store in self.data.values():
			if store.checkKey(key):
				return store.getItem(key)
		
	def getStructsList(self):
		return self._getTypeNameList("/structs")
		
	def getClassesList(self):
		return self._getTypeNameList("/classes")
		
	def getTypedefsList(self):
		return self._getTypeNameList("/typedefs")
		
	def getEnumsList(self):
		return self._getTypeNameList("/enums")
		
	def getFunctionsList(self):
		return self._getTypeNameList("/functions")
		
	def getEnumItemsList(self):
		return self._getTypeItemList("/enums")

	def getStructItemsList(self):
		return self._getTypeItemList("/structs")

	def getClassItemsList(self):
		return self._getTypeItemList("/classes")

	def getClassRefList(self):
		return self._getTypeItemList("/hierarchy")



	# Struct by id and index
	#
	
	def _getStructData(self, item):
		attributes = self._getStructAttributesByRefId(item.attributes)
		methods = self._getStructMethodsByRefId(item.methods)
		ms = []
		for m in methods:
			ms.append( (m, self._getStructMethodParametersByRefId(m.parameters)) )
		return (item, attributes, ms)
		
	def getStructByIndex(self, index):
		item = self._getTypeItemByIndex("/structs", index)
		return self._getStructData(item)
		
	def getStructById(self, name):
		item = self._getTypeItemById("/structs", name)
		return self._getStructData(item)
		
		
		
	# Class by id and index
	#
	
	def _getClassData(self, item):
		attributes = self._getClassAttributesByRefId(item.attributes)
		methods = self._getClassMethodsByRefId(item.methods)
		ms = []
		for m in methods:
			ms.append( (m, self._getClassMethodParametersByRefId(m.parameters)) )
		return (item, attributes, ms)
		
	def getClassByIndex(self, index):
		item = self._getTypeItemByIndex("/classes", index)
		return self._getClassData(item)
		
	def getClassById(self, name):
		item = self._getTypeItemById("/classes", name)
		return self._getClassData(item)


	# Enum by id and index
	#
	
	def _getEnumData(self, item):
		values = self._getEnumValuesByRefId(item.values)
		return (item, values)
		
	def getEnumById(self, name):
		item = self._getTypeItemById("/enums", name)
		return self._getEnumData(item)

	def getEnumByIndex(self, index):
		item = self._getTypeItemByIndex("/enums", index)
		return self._getEnumData(item)
		
		
		
	# Typedef by id and index
	#
	
	def getTypedefByIndex(self, index):
		return self._getTypeItemByIndex("/typedefs", index)
		
		
	def getTypedefById(self, name):
		return self._getTypeItemById("/typedefs", name)
		

	# Function by id and index
	#
	
	def getFunctionByIndex(self):
		return self._getTypeItemByIndex("/functions", index)
		
	def getFunctionById(self):
		return self._getTypeItemById("/functions", name)
		

		
