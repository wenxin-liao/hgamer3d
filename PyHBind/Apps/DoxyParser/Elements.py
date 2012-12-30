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

# Elements.py

#
# in DoxyParser module, functions, to add all types of elements from
# xml parse files
#

from xml.etree import ElementTree
from DoxTool import DoxTool
import os, os.path


# Elements
# --------

class Elements(DoxTool):

	def __init__(self, store):
		self.store = store
		
	def addEnum(self, sdef, compoundName):
		
		for md in sdef.findall("memberdef"):
			if md.attrib["kind"] == "enum":
	#				print "adding enum:", md.find("name").text
				
				values = []
				enum = self.store.createItem("Enum")
				enum.id = md.attrib["id"]
				enum.name = md.find("name").text
				if compoundName and len(compoundName) > 0:
					enum.name = compoundName + "::" + enum.name
				enum.protection = md.attrib["prot"]
				enum.brief = self.getAllText(md.find("briefdescription"))
				enum.detail = self.getAllText(md.find("detaileddescription"))
				enum.values = self.store.getNewUuid()
				
				for ev in md.findall("enumvalue"):
					value = self.store.createItem("EnumValue")
					value.name = ev.find("name").text
					value.protection = ev.attrib["prot"]
					value.brief = self.getAllText(ev.find("briefdescription"))
					value.detail = self.getAllText(ev.find("detaileddescription"))
					ini = ev.find("initializer")
					if ini != None:
						value.flagvalue = True
						value.value = self.getText(ini)
					else:
						value.flagvalue = False
					values.append(value)
				
				self.store.addEnum(enum, values)
				
		
	def addTypedef(self, sdef):
		for md in sdef.findall("memberdef"):
			if md.attrib["kind"] == "typedef":
#				print "adding typedef:", md.find("name").text
				
				td = self.store.createItem("Typedef")
				td.id = md.attrib["id"]
				td.name = md.find("name").text
				td.type = self.getText(md.find("type"))
				td.protection = md.attrib["prot"]
				td.brief = self.getAllText(md.find("briefdescription"))
				td.detail = self.getAllText(md.find("detaileddescription"))
				self.store.addTypedef(td)

	def addFunction(self, sdef):
		functions = []
		for md in sdef.findall("memberdef"):
			
			if md.attrib["kind"] == "function":
				
#				print "adding function:", md.find("name").text
				(func, paras) = self.readFunctionXML(md, "Function")
				self.store.addFunction(func, paras)
				
				
	def addType(self, sec, compoundName):
		# add enums
		self.addEnum(sec, compoundName)

		
	def addAttributes(self, sec, attributes):
		for md in sec.findall("memberdef"):
			# add memberdef
			if md.attrib["kind"] == "variable":
				var = self.store.createItem("Variable")
				var.id = md.attrib["id"]
				var.name = md.find("name").text
				var.type = self.getText(md.find("type"))
				var.protection = md.attrib["prot"]
				var.brief = self.getAllText(md.find("briefdescription"))
				var.detail = self.getAllText(md.find("detaileddescription"))
				attributes.append(var)
		
	def addMethods(self, sec, methods, storeKey):
		for md in sec.findall("memberdef"):
			
			if md.attrib["kind"] == "function":
				
				(func, paras) = self.readFunctionXML(md, "Method")
				if storeKey == "/classes":
					self.store.addClassMethodParameters(func.parameters, paras)
				elif storeKey == "/structs":
					self.store.addStructMethodParameters(func.parameters, paras)
						
				methods.append(func)
			
			else:
				print md.attrib["kind"]
						


