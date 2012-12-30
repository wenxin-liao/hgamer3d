"""
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2011 Peter Althainz

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

# Writer.Enum


# creates h, chs, cpp files for enum structures

import os, re
import PyHBind.Base

def enumHeaderText(modulename, enumitem, valuesitem, enumconfig):
	enumName = "Enum" + enumconfig.nameSuffix
	text = ""
	text = text + "enum " + enumName + "\n{\n"
	l = len(valuesitem)
	i = 1
	for val in valuesitem:
		text = text + "  " + enumconfig.valuePrefix + val.name
		if val.flagvalue:
			text = text + " " + val.value
#			text = text + " = " + val.value
		if i < l:
			text = text + ","
		text = text + " // " + val.brief
		if len(val.detail) > 0:
			tet = text + ", " + val.detail
		text = text + "\n"
		i = i + 1
	text = text + "};\n"
	return text

def handleEnumFiles(modulename, config, enumitem, valuesitem, enumconfig, fWriteEnum = True, fWriteComments = False):
	
	enumName = "Enum" + enumconfig.nameSuffix
	writer = PyHBind.Base.FileWriter(modulename)
	
	if fWriteEnum:
		
		# header file
		#

		deftext = "_DEFINED_HG3D_" + enumName 
		otherincludes = "#ifndef " + deftext + "\n#define " + deftext + "\n"

		writer.clearText()
		writer.addText(writer.commentText(writer.templateText("sourcefile"), "//"))
		writer.addText(writer.templateText("header"))
		writer.replaceText(
			[
				("license", writer.commentText(writer.templateText("license"), "//")),
				("thisfile", enumName + ".h"),
				("otherincludes", otherincludes),
				("comment", writer.commentText(enumconfig.comment, "//")),
			] )
		writer.addText(enumHeaderText(modulename, enumitem, valuesitem, enumconfig))
		writer.addText ("#endif \n")
		writer.write("include/" + enumName + ".h")
		
		# chs file
		#

		writer.clearText()
		writer.addText(writer.templateText("chs"))
		writer.replaceText(
			[
				("sourcefile", writer.commentText(writer.templateText("sourcefile"), "--")),
				("license", writer.commentText(writer.templateText("license"), "--")),
				("thisfile", enumName + ".chs"),
				("module", modulename),
				("modulename", "HGamer3D." + config.getModule().subModule + "." + enumName),
				("includefile", enumName + ".h"),
				("otherincludes", ""),
				("comment", writer.commentText(enumconfig.comment, "--")),
			] )
		text = ""
		text = text + "{#enum " + enumName + " {} deriving (Eq)#}"
		writer.addText(text)
		fname = "c2hs-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + enumName
		writer.write(fname + ".chs")
		
		
	if fWriteComments:
		
		# modify hs files
		#
		

		# name of hs file
		fnamein = "c2hs-build/dist/build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + enumName
		fnameout = "doc-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + enumName
		
		textout = ""

	
		# build comments into hs file
		#
		
		fin = writer.openread(fnamein + ".hs")
		
		# first find data Enumxyz =
		
#		line = fin.readline()
#		while line:
#			if line.find("data Enum" + valuesitem[0].name + " ="):
				
		
		start = True
		reStart = "(^data " + enumName + " =)(.*$)"
		line = fin.readline()
		
		for val in valuesitem:

			# readlines until value found

			comment = val.brief
			if len(val.detail) > 0:
				comment = comment + " " + val.detail
			mfound = False
			
			while line and not mfound:
					
				if start:
					# search patter "data EnumXYZ = "
					mg = re.match(reStart, line)
					if mg:
						start = False
						textout = textout + mg.group(1) + "\n"
						textout = textout + "   -- | " + comment.replace("\n", "\n--") + "\n"
						textout = textout + "   " + mg.group(2) + "\n"
						mfound = True
					else:
						textout = textout + line
				
					
				else:
					if line.find(val.name) >= 0:
						
						mfound = True
						textout = textout + "   -- | " + comment.replace("\n", "\n--") + "\n"
						textout = textout + "   " + line
					
					else:
						textout = textout + line
						
				line = fin.readline()
				
		while line:
			textout = textout + line
			line = fin.readline()
				
		fin.close()
		writer.clearText()
		writer.addText(textout)
		writer.write(fnameout + ".hs")
		

	

def writeEnumFiles(modulename, config, enumitem, valuesitem, enumconfig):
	handleEnumFiles(modulename, config, enumitem, valuesitem, enumconfig)

def addCommentsToEnumFiles(modulename, config, enumitem, valuesitem, enumconfig):
	handleEnumFiles(modulename, config, enumitem, valuesitem, enumconfig, False, True)
	
