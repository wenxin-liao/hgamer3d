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

# DoxTool.py

from xml.etree import ElementTree

class DoxTool:
	
	def getAllText(self, elem):
		return self.getTextFromDescription(elem)
		
		it = elem.itertext()
		text = ""
		for i in it:
			# first check if text is empty
			s = i.strip()
			if len(s) > 0:
				if len(text) > 0:
					text = text + "\n"
				text = text + s
		return text
		
	def getTextFromDescription(self, elem):
		para = elem.find("para")
		if para != None:
			text = self.getTextFromPara(para)
		else:
			text = ""
		return text
		
	def getTextFromPara(self, para):
		if para.text != None:
			text = para.text
		else:
			text = ""
		for child in para.getchildren():
			# simple formatting tags
			if child.tag in ["title", "ref", "bold", "preformatted", "emphasis", "verbatim"]:
				text = text + self.getText(child)
			# special tags
			elif child.tag in ["table", "ulink"]:
				text = text + self.getText(child)
			# breaks and separators
			elif child.tag in ["linebreak", "simplesectsep"]:
				text = text + "\n"
			# sub paras
			elif child.tag == "para":
				text = text + self.getTextFromPara(child)
			elif child.tag == "simplesect":
				if child.attrib["kind"] == "return":
					# found return comment
					pass
				else:
					subpara = child.find("para")
					text = text + self.getTextFromPara(child)
			# parameterlist, will be handled below
			elif child.tag == "parameterlist":
				pass # no action for parameterlist
			# other lists
			elif child.tag in ["orderedlist", "itemizedlist"]:
				text = text + self.getText(child)
			else:
				print "found new type in para:", child.tag
		return text
		
	def getReturnText(self, elem):
		text = ""
		para = elem.find("para")
		if para != None:
			for child in para.getchildren():
				# simple formatting tags
				if child.tag == "simplesect" and child.attrib["kind"] == "return":
					text = self.getText(child)
		return text
		
		
	def getText(self, elem):
		text = ""
		if elem.text != None:
			text = text + elem.text
		for child in elem.getchildren():
			if child.text != None:
				text = text + child.text
			if child.tail != None:
				text = text + child.tail
		return text
		
		
		
	def readFunctionXML(self, md, className):
		
		# add function information
		func = self.store.createItem(className)
		func.id = md.attrib["id"]
		func.name = md.find("name").text
		func.type = self.getText(md.find("type"))
		func.protection = md.attrib["prot"]
		func.brief = self.getAllText(md.find("briefdescription"))
		func.detail = self.getAllText(md.find("detaileddescription"))
		func.static = md.attrib["static"] == "yes"
		func.virtual = md.attrib["virt"] != "non-virtual"
		func.parameters = self.store.getNewUuid()
		func.returncomment = self.getReturnText(md.find("detaileddescription"))
		
		if md.attrib["virt"] not in ["pure-virtual", "virtual","non-virtual"]:
			print "found virt attribute, not considered:", md.attrib["virt"]
			
		# add parameter information
		paras = []
		for para in md.findall("param"):
			p = self.store.createItem("Parameter")
			p.type = self.getText(para.find("type"))
			if p.type != "void":
				decl = para.find("declname")
				if decl != None:
					p.name = para.find("declname").text
				else:
					print "parameter without declname in", p.type
				default = para.find("defval")
				if default != None:
					p.flagdefault = True
					p.default = self.getText(default)
				else:
					p.flagdefault = False
				paras.append(p)
				
		# add comments to parameters
		dd = md.find("detaileddescription")
		if dd != None:
			par = dd.find("para")
			if par != None:
				parcom = par.find("parameterlist")
				if parcom != None:
#							print parcom
					for item in parcom.findall("parameteritem"):
						fitem = item.find("parameternamelist").find("parametername")
						if fitem != None:
							pname = fitem.text
						else:
							pname = ""
#								print pname
						fitem = item.find("parameterdescription").find("para")
						if fitem != None:
							desc = self.getText(fitem)
						else:
							desc = ""
						
#								print desc
						for p in paras:
							if (len(pname) > 0) and (len(desc) > 0) and (p.name == pname):
								p.description = desc
								
		return (func, paras)
					

				
