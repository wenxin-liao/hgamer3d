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

import PyHBind.Base
import os, os.path
from wx.lib.pubsub import Publisher


class ConfigStore:

	def __init__(self):
		
		self.types = {
		
		#
		# Basic Configuration, Module Configuration, Files, Directories
		# this data builds the basic setup of the module
		#
		
			"BaseConfig" : [("projectName", "str", "New PyHBind Project"),
							("parentProjectLabel", "str", "HGamer3D"),
							("baseDir", "str", "."),
							("buildDir", "str", ".")
							],
			"ModuleConfig" : [("moduleTitle", "str", "Module Title"),
							("subModule", "str", "Sub.Module"),
							("cabalName", "str", "HGamer3D-title-Binding"),
							("cabalSynopsis", "str", ""),
							("cabalDescription", "str", ""),
							("cabalDepends", "str", ""),
							("cabalUrl", "str", ""),
							("cabalCategory", "str", ""),
							("moduleVersion", "str", "0.1.0")
							],
			"Directory"  : [("path", "str", "."),
			                ("uuid", "str", "not set, invalid")
							],
			"File" : [("name", "str", "."),
					  ("path", "str", "")
					  ],
					  
			"Namespace" : [("name", "str", "."),
					  ],
					  
			"HeaderDir" : [("name", "str", "."),
					  ("path", "str", ""),
					  ("uuidOfTopLevel", "str", "not set, invalid") ],
					  
			"HeaderFile" : [("name", "str", "."),
					  ("path", "str", ""),
					  ("selected", "bool", False),
					  ("uuidOfTopLevel", "str", "not set, invalid")],

		#
		# Configuration of some baisc types
		#
					  
			"BasicType" : [("name", "str", ""),
						   ("ctype", "str", ""),
						   ("htype", "str", ""),
						   ("inMarshaller", "str", ""),
						   ("outMarshaller", "str", ""),
						   ("mappedCTypes", "str", ""),
						   ("flagString", "bool", False),
						   ("stringConstructor", "str", "std:string"),
						   ("asCharPointerMethod", "str", "c_str()"),
						   ("comment", "str", ""),
						   ],
						   
			"MappedCType" : [("type", "str", "")],
					  
		#
		# Workflow Status
		#			  
					  
			"Status" : [
					    ("xmlUpToDate", "bool", False), ],
					    
		#
		# Configuration of single items
		# After parsing of the C/C++ Source files by doxygen an additional
		# datastructure of the parsed header files exists. For each 
		# item of those parsed data, additional configuration can be 
		# done, this is shown here.
		# The original data is not changed, extended, since it can 
		# be changed by new parsing.
		# The reference between both worlds ith the "id" field in the 
		# Doxygen output xml.
		#
		
			"EnumConfiguration" : [
					("id", "str", ""),
					("nameSuffix", "str", ""),
					("valuePrefix", "str", ""),
					("selected", "bool", False),
					("comment", "str", ""),
				],
				
			"StructConfiguration" : [
					("id", "str", ""),
					("htype", "str", ""),
					("valuePrefix", "str", ""),
					("structname", "str", ""),
					("hFile", "str", ""),
					("chsFile", "str", ""),
					("cTypes", "str", ""),
					("selected", "bool", False),
					("comment", "str", ""),
				],
				
				
				
			"ClassConfiguration" : [
					("id", "str", ""),
					("htype", "str", ""),
					("prefix", "str", ""),
					("selected", "bool", False),
					("comment", "str", ""),
				],
				
			"ExcludedMethodConfiguration" : [
					("id", "str", ""),
				],
				
			"FunctionConfiguration" : [
					("id", "str", ""),
					("selected", "bool", False),
					("comment", "str", ""),
				],
				
			"TypedefConfiguration" : [
					("id", "str", ""),
					("selected", "bool", False),
					("comment", "str", ""),
				],
				
		
		}
				
		self.config = None


	def newConfig(self):
	
		self.config = PyHBind.Base.ItemStore(self.types)
		
		# config
		self.confBase = self.config.createItem("BaseConfig")
		self.confModule = self.config.createItem("ModuleConfig")
		self.config.setItem("/config/base", self.confBase)
		self.config.setItem("/config/module", self.confModule)
		# header dirs, lib dirs, source dirs
		self.confHeaderDirs = []
		self.config.setItem("/config/headerdirs", self.confHeaderDirs)
		self.confHeaderItems = {}
		self.confLibDirs = []
		self.config.setItem("/config/libdirs", self.confLibDirs)
		self.confLibDlls = []
		self.config.setItem("/config/libdlls", self.confLibDlls)
		self.confLibLibs = []
		self.config.setItem("/config/liblibs", self.confLibLibs)
		self.confSourceDirs = []
		self.config.setItem("/config/sourcedirs", self.confSourceDirs)
		# status
		self.config.setItem("/status", self.config.createItem("Status"))
		# headers included in source
		self.confIncludedHeaders = []
		self.config.setItem("/headers/includedHeaders", self.confIncludedHeaders)
		# namesspaces
		self.confNamespaces = []
		self.config.setItem("/config/namespaces", self.confNamespaces)
		
		# types
		self.typesBasic = []
		self.config.setItem("/types/basic", self.typesBasic)
		# excludedd Methods
		self.excludedMethods = []
		self.config.setItem("/classes/excludedMethods", self.excludedMethods)
		

	def loadConfig(self, path, filename):
		
		self.config = PyHBind.Base.ItemStore(self.types)
		self.config.read(path + "/" + filename)
		self.confBase = self.config.getItem("/config/base")
		if not self.config.checkKey("/config/module"):
			self.config.setItem("/config/module", self.config.createItem("ModuleConfig"))
		self.confModule = self.config.getItem("/config/module")
		self.confHeaderDirs = self.config.getItem("/config/headerdirs")
		self.confHeaderItems = {}
		for item in self.confHeaderDirs:
			self.confHeaderItems[item.uuid] = self.config.getItem("/headers/" + item.uuid)
		
	
		self.confLibDirs = self.config.getItem("/config/libdirs")
		if not self.config.checkKey("/config/libdlls"):
			self.config.setItem("/config/libdlls", [])
		self.confLibDlls = self.config.getItem("/config/libdlls")
		if not self.config.checkKey("/config/liblibs"):
			self.config.setItem("/config/liblibs", [])
		self.confLibLibs = self.config.getItem("/config/liblibs")
		if not self.config.checkKey("/status"):
			self.config.setItem("/status", self.config.createItem("Status"))
		if not self.config.checkKey("/config/namespaces"):
			self.config.setItem("/config/namespaces", [])
		self.confNamespaces = self.config.getItem("/config/namespaces")
		if not self.config.checkKey("/config/sourcedirs"):
			self.config.setItem("/config/sourcedirs", [])
		self.confSourceDirs = self.config.getItem("/config/sourcedirs")

		if not self.config.checkKey("/headers/includedHeaders"):
			self.config.setItem("/headers/includedHeaders", [])
		self.confIncludedHeaders = self.config.getItem("/headers/includedHeaders")
		
		# types
		if not self.config.checkKey("/types/basic"):
			self.typesBasic = []
			self.config.setItem("/types/basic", self.typesBasic)
		else:
			self.typesBasic = self.config.getItem("/types/basic")

		# excludedd Methods
		if not self.config.checkKey("/classes/excludedMethods"):
			self.excludedMethods = []
			self.config.setItem("/classes/excludedMethods", self.excludedMethods)
		else:
			self.excludedMethods = self.config.getItem("/classes/excludedMethods")

	
		# class, function, enum, ... configurations by id
		self.itemConfigurations = {}
		if not self.config.checkKey("/config/allitems"):
			self.config.setItem("/config/allitems", [])
		for item in self.config.getItem("/config/allitems"):
			self.itemConfigurations[item.id] = item
			

		
	def saveConfig(self, path, filename):
		if self.config != None:
			self.config.write(path + "/" + filename)
			
	def getConfig(self):
		return self.config
			
	def getBase(self):
		if self.config != None:
			return self.confBase
		else:
			return None

	def getModule(self):
		if self.config != None:
			return self.confModule
		else:
			return None

	def getLibDirs(self):
		if self.config != None:
			return self.confLibDirs
		else:
			return None
			
	def getSourceDirs(self):
		if self.config != None:
			return self.confSourceDirs
		else:
			return None
			
	def getSourceCppFiles(self):
		files = []
		pathfiles = []
		for item in self.confSourceDirs:
			for file in os.listdir(item.path):
				if file[-4:] == ".cpp":
					pathfiles.append(item.path + "/" + file)
					files.append(file)
		return (files, pathfiles)
		
	def getSourceHeaderFiles(self):
		files = []
		pathfiles = []
		for item in self.confSourceDirs:
			for file in os.listdir(item.path):
				if file[-2:] == ".h":
					pathfiles.append(item.path + "/" + file)
					files.append(file)
		return (files, pathfiles)
		
	def getLibDlls(self):
		if self.config != None:
			return self.confLibDlls
		else:
			return None

	def getLibLibs(self):
		if self.config != None:
			return self.confLibLibs
		else:
			return None

	def getHeaderDirs(self):
		if self.config != None:
			return self.confHeaderDirs
		else:
			return None

	def getNamespaces(self):
		if self.config != None:
			return self.confNamespaces
		else:
			return None

	def getHeaderItems(self):
		if self.config != None:
			return self.confHeaderItems
		else:
			return None
			
	def getIncludedHeaders(self):
		if self.config != None:
			return self.confIncludedHeaders
		else:
			return None
			
	def getStatus(self):
		if self.config != None:
			return self.config.getItem("/status")
		else:
			return None

	def getBasicTypes(self):
		if self.config != None:
			return self.typesBasic
		else:
			return None

	def getMappedCTypes(self, uuid):
		if self.config != None:
			return self.config.getItem("/types/basic/mappedctypes/" + uuid)
		else:
			return None

	def removeMappedCTypes(self, uuid):
		if self.config != None:
			self.config.removeItem("/types/basic/mappedctypes/" + uuid)
		else:
			return None

	# functions to walk header dirs and build tree of header files
	
	def _walkAddHeaders(self, args, dirname, files):
		(basePath, arr, uuidOfTopLevel) = args
		newdir = str(os.path.relpath(dirname, basePath).replace(os.sep, "/"))
		arr.append(self._addHeaderDirectory(newdir, uuidOfTopLevel))
		for f in files:
			if (f[-2:] == ".h") or (f[-4:] == ".hpp"):
				arr.append(self._addHeaderFile(newdir, str(f), uuidOfTopLevel))
	
	def _addHeaderFile(self, path, name, uuidOfTopLevel):
		item = self.config.createItem("HeaderFile")
		item.path = path.replace(os.sep, "/")
		item.uuidOfTopLevel = uuidOfTopLevel
		item.name = name
		return item
		
	def _addHeaderDirectory(self, path, uuidOfTopLevel):
		item = self.config.createItem("HeaderDir")
		item.path = path.replace(os.sep, "/")
		item.uuidOfTopLevel = uuidOfTopLevel
		return item
		
	def addHeaderFilesFromHeaderDir(self, basePath, startPathItem):
		arr = []
		self.confHeaderItems[startPathItem.uuid] = arr
		startPath = startPathItem.path
		walkPath = basePath + "/" + startPath
		os.path.walk(walkPath, self._walkAddHeaders, (walkPath, arr, startPathItem.uuid) )
		self.config.setItem("/headers/" + startPathItem.uuid, arr)
		
	def removeHeaderDir(self, index):
		rItem = self.confHeaderDirs[index]
		del self.confHeaderItems[rItem.uuid]
		self.config.removeItem(("/headers/" + rItem.uuid))
		del self.confHeaderDirs[index]

	def appendHeaderDir(self, path):
		pathItem = self.config.createItem("Directory")
		pathItem.path = path
		pathItem.uuid = self.config.getNewUuid()
		self.confHeaderDirs.append(pathItem)
		return pathItem
		
	# functions to add lib dir
	
	def appendLibDir(self, path):
		pathItem = self.config.createItem("Directory")
		pathItem.path = path
		pathItem.uuid = self.config.getNewUuid()
		self.confLibDirs.append(pathItem)
		return pathItem
		
	def appendLibraryDll(self, filename, path):
		fileItem = self.config.createItem("File")
		fileItem.path = path
		fileItem.name = filename
		self.confLibDlls.append(fileItem)
		return fileItem
		
	def appendLibraryLib(self, filename, path):
		fileItem = self.config.createItem("File")
		fileItem.path = path
		fileItem.name = filename
		self.confLibLibs.append(fileItem)
		return fileItem
		
	# functions to add source dirs
	
	def appendSourceDir(self, path):
		pathItem = self.config.createItem("Directory")
		pathItem.path = path
		pathItem.uuid = self.config.getNewUuid()
		self.confSourceDirs.append(pathItem)
		return pathItem
		
		
	# basic types
	
	def addBasicType(self, name):
		bt = self.config.createItem("BasicType")
		bt.name = name
		bt.mappedCTypes = self.config.getNewUuid()
		self.config.setItem("/types/basic/mappedctypes/" + bt.mappedCTypes, [])
		self.typesBasic.append(bt)
		return bt
		
	def addBasicTypeCTypeMapping(self, typename, item):
		ct = self.config.createItem("MappedCType")
		ct.type = typename
		mappedCTypes = self.config.getItem("/types/basic/mappedctypes/" + item.mappedCTypes)
		mappedCTypes.append(ct)


#
# here the section with single item configurations start
#


	#
	# get and store configuration items by id, all are saved in one 
	# array, the configurations array, for faster access a hash 
	# is provided during load and unload
	#
	
	
	
	# internal, to add one item

	def _addItemConfig(self, idtag, item):
		item.id = idtag
		self.config.getItem("/config/allitems").append(item)
		self.itemConfigurations[idtag] = item
		
	# get one item
	
	def _getItemConfig(self, idtag):
		if idtag in self.itemConfigurations.keys():
			return self.itemConfigurations[idtag]
		else:
			return None
	
	#
	# enum configuration
	#
	
	def getEnumConfiguration(self, idtag):
		item = self._getItemConfig(idtag)
		if not item:
			item = self.config.createItem("EnumConfiguration")
			self._addItemConfig(idtag, item)
		return item

	#
	# class configuration
	#
	
	def getClassConfiguration(self, idtag):
		item = self._getItemConfig(idtag)
		if not item:
			item = self.config.createItem("ClassConfiguration")
			self._addItemConfig(idtag, item)
		return item


	#
	# typedef configuration
	#
	
	def getTypedefConfiguration(self, idtag):
		item = self._getItemConfig(idtag)
		if not item:
			item = self.config.createItem("TypedefConfiguration")
			self._addItemConfig(idtag, item)
		return item

	#
	# struct configuration
	#
	
	def getStructConfiguration(self, idtag):
		item = self._getItemConfig(idtag)
		if not item:
			item = self.config.createItem("StructConfiguration")
			self._addItemConfig(idtag, item)
		return item
		
		
#
#	ExcludedMethodConfiguration handling
#

	def isMethodExcluded(self, idtag):
		for m in self.excludedMethods:
			if m.id == idtag:
				return True
		return False
		
	def setMethodExcluded(self, idtag, flag):
		# remove
		if not flag:
			ind = 0
			toRemove = -1
			for m in self.excludedMethods:
				if m.id == idtag:
					toRemove = ind
				ind = ind + 1
			if toRemove >= 0:
				del self.excludedMethods[toRemove]
				
		# append
		if flag:
			if not self.isMethodExcluded(idtag):
				m = self.config.createItem("ExcludedMethodConfiguration")
				m.id = idtag
				self.excludedMethods.append(m)
				
