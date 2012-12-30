# -*- coding: utf-8 -*-

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

#
# Items and ItemStore class, to generically store items and reload them 
# later, independent of version
#

import sys, yaml, uuid

class ItemField:
		
	validtypes = [
		"int",
		"bool",
		"str",
		"float"
	]
		
	def __init__(self, fieldname, fieldtype, defaultvalue):
		
		if fieldtype not in ItemField.validtypes:
			print "ItemField, not supported type", fieldtype
			raise Exception()
		
		self.fname = fieldname
		self.ftypeString = fieldtype
		if fieldtype == "str":
			self.ftype = unicode
		else:
			self.ftype = eval(fieldtype)
		self.fvalue = self.ftype(defaultvalue)
			
	def fieldtype(self):
		return self.ftype
		
	def fieldtypeStr(self):
		return self.ftypeString
		
	def fieldname(self):
		return self.fname
		
	def defaultvalue(self):
		return self.fvalue
		
	def getTuple(self):
		return (self.fname, self.ftypeString, self.fvalue)
		
	
class ItemClass:
	
	def __init__( self, name, fields):
		
		self.ic_name = name
		self.ic_typeinfo = {}
		self.ic_fields = []
		
		for fieldname, fieldtype, defvalue in fields:
			self.ic_fields.append(fieldname)		# sorting needed
			self.ic_typeinfo[fieldname] = ItemField(fieldname, fieldtype, defvalue)

	def className(self):
		return self.ic_name
		
	def classTypes(self):
		return self.ic_typeinfo
		
	def classFields(self):
		return self.ic_fields
		


class Item:
	
	def __init__(self, itemclass):
		self.internal_itemclass = itemclass
		self.internal_itemdata = {}
		

	def setFromArray(self, dataArr):
		i = 0
		for name in self.internal_itemclass.classFields():
			self.internal_itemdata[name] = (self.internal_itemclass.classTypes()[name].fieldtype())(dataArr[i])
			i = i + 1

	def getClassName(self):
		return self.internal_itemclass.className()
		
	def getClass(self):
		return self.internal_itemclass
		
	def getArray(self):
		arr = []
		for name in self.internal_itemclass.classFields():
			arr.append(self.__getattr__(name))
		return arr
		
	def __getattr__(self, name):
		
		if name in self.internal_itemclass.classFields():
			if name in self.internal_itemdata.keys():
				return self.internal_itemdata[name]
			else:
				return self.internal_itemclass.classTypes()[name].defaultvalue()
		else:
			print "ItemStore.Item, __getattr__, item does not support this attribute", name
			raise Exception()
	
	def __nonzero__(self):
		return True
		
		
	def __setattr__(self, name, value):
		
		if name in ["internal_itemclass", "internal_itemdata"]:
			self.__dict__[name] = value
		else:
			if name in self.internal_itemclass.classFields():
				self.internal_itemdata[name] = (self.internal_itemclass.classTypes()[name].fieldtype())(value)
			else:
				print "ItemStore.Item, __setattr__, item does not support this attribute", name
				raise Exception()
				
	def __repr__(self):
		return "{ItemStore-Item: " + self.internal_itemclass.className() + "}"
		
class ItemTypes:
	
		def __init__(self, typeDict):
			
			self.classes = {}
			for className in typeDict.keys():
				newClass = ItemClass(className, typeDict[className])
				self.classes[className] = newClass
				
		def createItem(self, className):
			return Item(self.classes[className])
			
		def getClass(self, className):
			return self.classes[className]
		

class ItemStore:
	
	# a config store contains a hash of items or item arrays
	# the hash value is in path format "/rangeA/rangeB" 
	# this allows for easy lookup and versioning
	
	def __init__(self, typesDict):
		
		self.types = ItemTypes(typesDict)
		self.config = {}
		
	def setItem(self, key, item):
		
		checkOk = True
		if type(item) == list:
			for i in item:
				if i.__class__.__name__ != "Item":
					checkOk = False
		elif item.__class__.__name__ != "Item":
			checkOk = False
		
		if checkOk:
			self.config[key] = item
		else:
			print "ItemStore only supports arrays and Items, not", type(item)
			raise Exception()

	def removeItem(self, key):
		
		if self.checkKey(key):
			del self.config[key]
		
	def checkKey(self, key):
		return key in self.config.keys()
		
	def getItem(self, key):
		
		return self.config[key]
		
	def createItem(self, classname):
		
		return self.types.createItem(classname)

	def getNewUuid(self):
		return str(uuid.uuid4())

	def write(self, filename):

		# basic structure, to write
		dictionary = {}
		# open file
		fout = open(filename, "w")
		
		# first gather all classes and build type library
		allClasses = {}
		dictionary["classes"] = {}
		for value in self.config.values():
			if type(value) == list:
				for item in value:
					allClasses[item.getClassName()] = item.getClass()
			else:
				allClasses[value.getClassName()] = value.getClass()
			
		for className in allClasses.keys():
			c = allClasses[className]
			fields = []
			for fieldname in c.classFields():
				field = c.classTypes()[fieldname]
				fields.append( [fieldname, field.fieldtypeStr(), field.defaultvalue()] )
				
			dictionary["classes"][className] = fields
			
		# create dictionary of values
		
		dictionary["items"] = {}
		for key in self.config.keys():
			
			value = self.config[key]
			if type(value) == list:
				itemsD = []
				for item in value:
					itemsD.append({ "class" : item.getClassName(), "values" : item.getArray() })
			else:
				itemsD = { "class" : value.getClassName(), "values" : value.getArray() }
			dictionary["items"][key] = itemsD
		
		fout.write(yaml.dump(dictionary))
		fout.close()
		
		
	def read(self, filename):
		
		# open file and read
		fin = open(filename, "r")
		dictionary = yaml.load(fin)
		fin.close()
		
		# read in file classes
		filetypes = ItemTypes(dictionary["classes"])
		
		# now read items and convert
		items = dictionary["items"]
		for key in items.keys():
			item = items[key]
			if type(item) == list:
				newItem = []
				for litem in item:
					itemclass = litem["class"]
					itemvalues = litem["values"]
					# construct new item with classname from self.types
					i = 0
					lnewItem = self.types.createItem(itemclass)
					for fieldname in filetypes.getClass(itemclass).classFields():
						value = itemvalues[i]
						if fieldname in self.types.getClass(itemclass).classFields():
							lnewItem.__setattr__(fieldname, value)
						i = i + 1
					newItem.append(lnewItem)
				
			else:
				itemclass = item["class"]
				itemvalues = item["values"]
				# construct new item with classname from self.types
				i = 0
				newItem = self.types.createItem(itemclass)
				for fieldname in filetypes.getClass(itemclass).classFields():
					value = itemvalues[i]
					if fieldname in self.types.getClass(itemclass).classFields():
						newItem.__setattr__(fieldname, value)
					i = i + 1
					
			self.config[key] = newItem
			

			
			
# and here is, how to use
# ConfigStore class is all you need

if __name__ == "__main__":
		
	# create one store, with class definitions for Place and Person 
	store1 = ConfigStore ({
		"Place" : [("street", "str", ""),
					("number", "int", 0) ],
		"Person" : [
					("prename", "str", ""),
					("lastname", "str", "<no lastname set>"),
					("isMale", "bool", False)
				]
			})
			
	# create two items in this store and save
	place1 = store1.createItem("Place")
	store1.setItem("firstPlace", place1)
	person1 = store1.createItem("Person")
	store1.setItem("firstPerson", person1)
	# set parameters
	place1.street = "Kensington Street"
	place1.number = 10
	person1.prename = "Klaus"
	# write to file
	store1.write("config-store-test.cfg")
	
	# create second store, with slightly different class definitions
	store2 = ConfigStore ({
		"Place" : [("number", "int", 0),
			       ("street", "str", ""),	# fields swapped
			       ("area", "int", -1) # new field
					 ],
		"Person" : [
					("prename", "str", ""),
					("lastname", "str", "<no lastname set>")
					# field skipped, was not politically correct
				]
			})
	# load store from file
	store2.read("config-store-test.cfg")
	# get items and show them
	# also look into file config-store-test.cfg, to see nice layout
	place2 = store2.getItem("firstPlace")
	print "street: " + place2.street + ", number: " + str(place2.number) + ", area: " + str(place2.area)
	person2 = store2.getItem("firstPerson")
	print "prename: " + person2.prename + ", lastname:", person2.lastname
	print "array format:"
	print store2.getItem("firstPlace").getArray()
	print store2.getItem("firstPerson").getArray()
