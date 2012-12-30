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

# Group.py

from xml.etree import ElementTree
from DoxTool import DoxTool
from Elements import Elements
import os, os.path

class Group(DoxTool):
	
	# reads groups and namespaces, equivalently
	
	def readXML(self, refid, filename, store):
		
		self.refid = refid
		self.store = store
		self.path = os.path.dirname(filename).replace(os.sep, "/")
		baum = ElementTree.parse(filename)
		ind = baum.getroot()
		self.el = Elements(self.store)

		# find class refid
		for cpdef in ind.getchildren():
			cpname = cpdef.find("compoundname").text
			if cpdef.attrib["id"] == refid:
				# now we are in the right compound, search for sectiondefs
				for sdef in cpdef.findall("sectiondef"):
					skind = sdef.attrib["kind"]
					if skind == "enum":
						self.el.addEnum(sdef, cpname)
						
					elif skind == "typedef":
						self.el.addTypedef(sdef)
						
					elif skind == "func":
						self.el.addFunction(sdef)

			
						
