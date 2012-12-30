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

# Index.py


from xml.etree import ElementTree
import os, os.path
from Class import Class, Struct
from Group import Group


class Index:
	
	def readXML(self, filename, store):
		self.store = store
		store.initialize()
		
		self.path = os.path.dirname(filename).replace(os.sep, "/")
		baum = ElementTree.parse(filename)
		ind = baum.getroot()
		
		for compound in ind.getchildren():
			kind = compound.attrib["kind"]
			if kind == "class":
				self.addClass(compound)
				
			elif kind == "struct":
				self.addStruct(compound)
				
			elif kind == "namespace":
				self.addNamespace(compound)
				
			elif kind == "file":
				self.addFile(compound)
				
			elif kind == "group":
				self.addGroup(compound)
				
			elif kind == "dir":
				self.addDir(compound)
				
			else:
				print "found compound, not identified:", kind
				
				
	def addClass(self, cp):
		refid = cp.attrib["refid"]
		filename = self.path + "/" + refid + ".xml"
#		print "\nadding class:", cp.find("name").text
		Class().readXML(refid, filename, self.store)
		
	def addStruct(self, cp):
		refid = cp.attrib["refid"]
		filename = self.path + "/" + refid + ".xml"
#		print "\nadding struct:", cp.find("name").text
		Struct().readXML(refid, filename, self.store)
		
	def addNamespace(self, cp):
		refid = cp.attrib["refid"]
		filename = self.path + "/" + refid + ".xml"
#		print "\nadding namespace:", cp.find("name").text
		Group().readXML(refid, filename, self.store)
		
	def addFile(self, cp):
		pass
		
	def addGroup(self, cp):
		pass
#		refid = cp.attrib["refid"]
#		filename = self.path + "/" + refid + ".xml"
#		print "\nadding group:", cp.find("name").text
#		Group().readXML(refid, filename, self.store)
		
	def addDir(self, cp):
		pass
			
		
	
		
	
