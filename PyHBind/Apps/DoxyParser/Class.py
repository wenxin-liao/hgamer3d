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

# Class.py

from xml.etree import ElementTree
from DoxTool import DoxTool
from Elements import Elements
import os, os.path


class ClassOrStruct(DoxTool):
	

	def readXML(self, refid, filename, store):
		
		self.attributes = []
		self.methods = []
		
		self.refid = refid
		self.store = store
		self.el = Elements(store)
		self.path = os.path.dirname(filename).replace(os.sep, "/")
		baum = ElementTree.parse(filename)
		ind = baum.getroot()
		
		# find class refid
		for cpdef in ind.getchildren():
			if cpdef.attrib["id"] == refid:
				# add class parameters
				cl = self.store.createItem(self.className)
				cl.name = cpdef.find("compoundname").text
				cl.id = refid
				cl.protection = cpdef.attrib["prot"]
				cl.brief = self.getAllText(cpdef.find("briefdescription"))
				cl.detail = self.getAllText(cpdef.find("detaileddescription"))
				cl.methods = self.store.getNewUuid()
				cl.attributes = self.store.getNewUuid()
				
				# check for subclasses
				for bases in cpdef.findall("basecompoundref"):
					classref = self.store.createItem("ClassRef")
					if "refid" in bases.attrib.keys():
						classref.parent = bases.attrib["refid"]
						classref.child = cl.id
						self.store.appendHierarchyItem(classref)
					else:
						print "no refid found in basecompoundref", cl.name
				
				# check for template
				tl = cpdef.find("templateparamlist")
				if tl != None:
					params = []
					cl.flagtemplate = True
					cl.templateparameters = self.store.getNewUuid()
					for p in tl.findall("param"):
						newp = self.store.createItem("Templateparameter")
						n = p.find("declname")
						if n != None:
							newp.name = n.text
						newp.type = self.getText(p.find("type"))
						params.append(newp)
					if self.storeKey == "/classes":
						self.store.addClassTemplateParameters(cl.templateparameters, params)
					elif self.storeKey == "/structs":
						self.store.addStructTemplateParameters(cl.templateparameters, params)
					else:
						print "Wrong key:", self.storeKey
				else:
					cl.flagtemplate = False
				
				# add methods and attributes
				for sec in cpdef.findall("sectiondef"):
					# read section
					kindsec = sec.attrib["kind"]
					if kindsec in ["public-type", "protected-type", "private-type"]:
						# add types
						self.el.addType(sec, cl.name)
					elif kindsec in ["public-attrib", "protected-attrib", "private-attrib"]:
						# add attributes
						self.el.addAttributes(sec, self.attributes)
					elif kindsec in ["public-func", "protected-func", "private-func"]:
						# add functions
						self.el.addMethods(sec, self.methods, self.storeKey)
					elif kindsec in ["public-static-func", "protected-static-func", "private-static-func"]:
						# add static functions
						self.el.addMethods(sec, self.methods, self.storeKey)
					else:
						pass
#						print "unknown kind in DoxyParser.Class:",kindsec
						
				# store all in storage
				if self.storeKey == "/classes":
					self.store.addClass(cl, self.attributes, self.methods)
				elif self.storeKey == "/structs":
					self.store.addStruct(cl, self.attributes, self.methods)
				else:
					print "Wrong key:", self.storeKey
				
						

		
class Class(ClassOrStruct):
	
	def __init__(self):
		self.className = "Class"
		self.storeKey = "/classes"
		
class Struct(ClassOrStruct):
	
	def __init__(self):
		self.className = "Struct"
		self.storeKey = "/structs"
		
