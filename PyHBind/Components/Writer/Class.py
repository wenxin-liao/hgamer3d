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

# Writer.Class


# creates h, chs, cpp files for class structures

import os, pdb
import PyHBind.Base
import PyHBind.Components


# anonymous class
class Ano:
	pass
	
# helper comments
def getCmt(item):
	text = ""
	bl = len(item.brief) > 0
	dl = len(item.detail) > 0
	if bl:
		text = text + item.brief
	if (not bl) and dl:
		text = text + item.detail
	if bl and dl:
		text = text + " - Details: " + item.detail
	text = text.replace ("\n", "\n--")
	return text
	
	
class MethodNameStore:
	
	def __init__(self):
		
		# structure: name -> array of id -> name, newname
		self.methodNameStore = {}
		
	def addMethodName(self, name, mid):
		if not name in self.methodNameStore.keys():
			# name not already existing -> newname = oldname
			self.methodNameStore[name] = [(mid, name, name)]
		else:
			arr = self.methodNameStore[name]
			# check for same id (just to be sure
			for (mid2, name, newname) in arr:
				if mid == mid2:
					return		# already added
			# append with new name		
			arr.append( (mid, name, name + str(len(arr)+1)) )
			
	def getMethodAlias(self, name, mid):
		if name in self.methodNameStore.keys():
			arr = self.methodNameStore[name]
			for (mid2, name, newname) in arr:
				if mid == mid2:
					return newname
		return None


def handleClassFiles(modulename, config, cl, attributes, methods, classconfig, fWriteClass = True, fComments = False):

	marshaller = PyHBind.Components.Marshaller.Marshaller(modulename, config)
	types = PyHBind.Base.Types()
	
	className = "Class" + classconfig.htype
	writer = PyHBind.Base.FileWriter(modulename)


	# data structure gathering
	#
	
	aClass = Ano()
	aClass.className = className
	aClass.cName = cl.name
	aClass.modulename = modulename
	aClass.item = cl
	aClass.config = classconfig
	aClass.methods = []
	aClass.nameTypeAndFlags = ("thisclass" , types.getTypeAndFlags(aClass.cName + " *", aClass.cName, False))
	aClass.cppdependencies = []
	aClass.hdependencies = []

	# here handling of double method names, new structure, method names, based on method id
	# double methods are named, before detection if parameter is enabled or method is enabled, this allows
	# later adding of methods, without clashing existing interfaces!
	
	methodNameStore = MethodNameStore()
	for (method, paras) in methods:
		methodNameStore.addMethodName(method.name, method.id)
		

	
	# add class method declarations, find dependencies, exclude methods
	for (method, paras) in methods:
		allOk = True
		cppdeps = []
		hdeps = []
		paratypes = []
		
		# check if public
		if method.protection != "public":
			allOk = False
		
		# check types
		if method.type != "void" and len(method.type) > 0:
			
			returnTInst = types.getTypeAndFlags(method.type, aClass.cName, True)
			if not returnTInst:
				allOk = False
			else:
				if returnTInst.mType.item:		# simple types, no item, no deps!
				
					headerName = ""
					# construct the correct include file name
					if returnTInst.mType.config.getClassName() == "EnumConfiguration":
						headerName = "Enum" + returnTInst.mType.config.nameSuffix + ".h"
						# enums only in hdeps, not needed and wanted in cpp file
						if len(headerName) > 0 and headerName not in hdeps:
							hdeps.append(headerName)
					elif returnTInst.mType.config.getClassName() == "StructConfiguration":
						headerName = "Struct" + returnTInst.mType.config.htype + ".h"
						# struct in hdeps and cppdeps needed
						if len(headerName) > 0 and headerName not in hdeps:
							hdeps.append(headerName)
						if len(headerName) > 0 and headerName not in cppdeps:
							cppdeps.append(headerName)
					elif returnTInst.mType.config.getClassName() == "ClassConfiguration":
						headerName = "Class" + returnTInst.mType.config.htype + ".h"
						# class only in hdeps
						if len(headerName) > 0 and headerName not in hdeps:
							hdeps.append(headerName)
					
				

		else:
			returnTInst = None
			
		for para in paras:
			p = types.getTypeAndFlags(para.type, aClass.cName, False) 
			if p:
				paratypes.append( (para.name, p) )
				if p.mType.item:		# simple types, no item, no deps!

					headerName = ""
					# construct the correct include file name
					if p.mType.config.getClassName() == "EnumConfiguration":
						headerName = "Enum" + p.mType.config.nameSuffix + ".h"
						# enums only in hdeps, not needed and wanted in cpp file
						if len(headerName) > 0 and headerName not in hdeps:
							hdeps.append(headerName)
					elif p.mType.config.getClassName() == "StructConfiguration":
						headerName = "Struct" + p.mType.config.htype + ".h"
						# struct in hdeps and cppdeps needed
						if len(headerName) > 0 and headerName not in hdeps:
							hdeps.append(headerName)
						if len(headerName) > 0 and headerName not in cppdeps:
							cppdeps.append(headerName)
					elif p.mType.config.getClassName() == "ClassConfiguration":
						headerName = "Class" + p.mType.config.htype + ".h"
						# class only in hdeps
						if len(headerName) > 0 and headerName not in hdeps:
							hdeps.append(headerName)
					
						
			else:
				allOk = False
		
		if allOk and (not config.isMethodExcluded(method.id)):
			
			# now prepare method for being actually handled
			aMethod = Ano()
			aMethod.ptypes = paratypes
			aMethod.returnTInst = returnTInst
			aMethod.item = method
			aMethod.paras = paras
			
			# check for constructor, destructor, normal method
			mn = method.name
			cn = aClass.cName.split("::")[-1]
			if mn == cn:
				aMethod.isConOrDestructor = "constructor"
			elif mn == ("~" + cn):
				aMethod.isConOrDestructor = "destructor"
			else:
				aMethod.isConOrDestructor = "none"

			# add class as first parameter, in case not static and not constructor
			if method.static or (aMethod.isConOrDestructor == "constructor"):
				pass
			else:
				aMethod.ptypes = [aClass.nameTypeAndFlags] + aMethod.ptypes
				
			# add pointer to class as output parameter, in case constructor
			if aMethod.isConOrDestructor == "constructor":
				aMethod.returnTInst = types.getTypeAndFlags(aClass.item.name + " **", aClass.cName, True)
			
			aClass.methods.append(aMethod)
			
			# add deps
			for dep in hdeps:
				if dep not in aClass.hdependencies:
					aClass.hdependencies.append(dep)
			for dep in cppdeps:
				if dep not in aClass.cppdependencies:
					aClass.cppdependencies.append(dep)
	
	
	
	if fWriteClass:
	
			
		# header file
		#

		# dependendencies
		deftext = "_DEFINED_HG3D_" + aClass.className 
		depstext = "#ifndef " + deftext + "\n#define " + deftext + "\n"
		depstext = depstext + "\n#include \"ClassPtr.h\"\n"
		
		for d in aClass.hdependencies:
			if d != (aClass.className + ".h"):							 # do not include own header
				depstext = depstext + "#include \"" + d + "\"\n"
				
		writer.clearText()
		
		writer.addText(writer.commentText(writer.templateText("sourcefile"), "//"))
		writer.addText(writer.templateText("header"))
		writer.replaceText(
			[
				("license", writer.commentText(writer.templateText("license"), "//")),
				("thisfile", className + ".h"),
				("otherincludes", depstext),
				("comment", writer.commentText(classconfig.comment, "//")),
			] )
			
		text = "\n"
		
		# add class method declarations
		for method in aClass.methods:
			text = "// " + method.item.brief + "\n"
			text = text + marshaller.createCFunctionHeader(methodNameStore.getMethodAlias(method.item.name, method.item.id), method.returnTInst, method.ptypes, method.isConOrDestructor, method.item.static, aClass.config.prefix)
			text = text + ";\n\n"
			writer.addText(text)
				
		writer.addText ("#endif \n")
		writer.write("include/" + className + ".h")
		
		# cpp file
		#
		
		depstext = """#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <exception>
	#include "<module>DllDefines.h"
	#include "ClassPtr.h"
	"""
		# dependendencies
		for d in aClass.cppdependencies:
			if d != (aClass.className + ".h"):							 # do not include own header
				depstext = depstext + "#include \"" + d + "\"\n"
			
		# included headers
		for item in config.getIncludedHeaders():
			incfile = (item.path + "/" + item.name)
			depstext = depstext + "#include \"" + incfile + "\"\n"
			
		# source header files
		for sh in config.getSourceHeaderFiles()[0]:
			depstext = depstext + "#include \"" + sh + "\"\n"
			
		# included namespaces
		depstext = depstext + "\n"
		for item in config.getNamespaces():
			depstext = depstext + "using namespace " + item.name + ";\n"
			
		writer.clearText()
		writer.addText(writer.commentText(writer.templateText("sourcefile"), "//"))
		writer.addText(writer.templateText("cpp"))
		writer.replaceText(
			[
				("license", writer.commentText(writer.templateText("license"), "//")),
				("thisfile", className + ".cpp"),
				("modulename", "HGamer3D." + config.getModule().subModule + "." + className),
				("includefile", "../../../../include/" + className + ".h"),
				("otherincludes", depstext),
				("comment", writer.commentText(classconfig.comment, "//")),
				("module", modulename),
			] )

		# add cpp text
		
		# add class method definitions
		for method in aClass.methods:
			text = "// " + method.item.brief + "\n"
			text = text + marshaller.createCppFunction(methodNameStore.getMethodAlias(method.item.name, method.item.id), method.item.name, method.returnTInst, method.ptypes, method.isConOrDestructor, method.item.static, aClass.cName, aClass.config.prefix)
			text = text + "\n"
			writer.addText(text)


		# write cpp file
		fname = "dll-build/" + className
		writer.write(fname + ".cpp")
		
		
		# chs file
		#
		
		# dependencies
		depstext = "{# import HGamer3D.Bindings.<module>.Utils #}\n"
		depstext = depstext + "{# import HGamer3D.Bindings.<module>.ClassPtr #}\n"
		depstext = depstext + "{# import HGamer3D.Bindings.<module>.StructHG3DClass #}\n"
		for d in aClass.hdependencies:
			if d[0:5] != "Class":
				depstext = depstext + "{# import " + "HGamer3D." + config.getModule().subModule + "." + d[:-2] + " #}\n"
			
		writer.clearText()
		writer.addText(writer.templateText("chs"))
		writer.replaceText(
			[
				("sourcefile", writer.commentText(writer.templateText("sourcefile"), "--")),
				("license", writer.commentText(writer.templateText("license"), "--")),
				("thisfile", className + ".chs"),
				("modulename", "HGamer3D." + config.getModule().subModule + "." + className),
				("includefile", className + ".h"),
				("otherincludes", depstext),
				("comment", writer.commentText(classconfig.comment, "--")),
				("module", modulename),
			] )

		# add chs text
		
		for method in aClass.methods:
			text = marshaller.createChsFunction(methodNameStore.getMethodAlias(method.item.name, method.item.id), method.returnTInst, method.ptypes, method.isConOrDestructor, method.item.static, aClass.config.prefix)
			text = text + "\n"
			writer.addText(text)

		# write chs file
		fname = "c2hs-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + className
		writer.write(fname + ".chs")
	

	if fComments:
		
		# modify hs files
		#
		

		# name of hs file
		fnamein = "c2hs-build/dist/build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + className
		fnameout = "doc-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + className
		
		textout = ""

	
		# build comments into hs file
		#
		
		fin = writer.openread(fnamein + ".hs")
		
		for method in aClass.methods:
			
		
			# readlines until tpye signature found, to do this, search
			# a line, lookin like: "{- function getButtonCount -}"

			line = fin.readline()
			mfound = False
			while line and not mfound:
				
				testl = "{- function " + methodNameStore.getMethodAlias(method.item.name, method.item.id) + " -}"
				if line.find(testl) == 0:
					
					mfound = True
					# found function, next one will be the type signature
					tline = fin.readline()
					
					# write Haddock comment for function
					textout = textout + "-- | " + getCmt(method.item) + "\n"
					
					# write Haddock comments for parameters
					tarr = tline.split("->")

					# sort paras to in and out
					pout = []
					pin = []

					cName = aClass.cName.split("::")[-1]
					print cName, method.item.name
					
					# create one additional para if not con, destructor or static
					if (not method.item.static) and (method.item.name != cName):
						newp = Ano()

						if (method.item.name != ("~" + cName)):
							newp.name = "classpointer"
							newp.description = "pointer of Class instance from which this methods is called."
						else:
							newp.name = "classpointer"
							newp.description = "pointer of Class instance which is going to be deleted."
							
						pin.append(newp)
					
					for p in method.paras:
						if p.out:
							pout.append(p)
						else:
							pin.append(p)
							
					# return value comment, if present
					if len(method.item.returncomment) > 0:
						newp = Ano()
						newp.name = "return value"
						newp.description = method.item.returncomment.replace("\n", "\n--")
						pout.append(newp)
							
							
					ind = 0
					outdone = False
					for tpiece in tarr:

						if ind > 0:
							textout = textout + "  -> "
							
						textout = textout + tpiece
						
						if ind < len(pin):			# in parameter, each para one comment
							par = pin[ind]
							textout = textout + " -- ^ " + par.name
							if len(par.description) > 0:
								textout = textout + " - " + par.description.replace("\n", "\n--")
						elif ind >= len(pin) and (not outdone):	# out parameter, all at once
							textout = textout + " -- ^ " 
							for par in pout:
								textout = textout + par.name
								if len(par.description) > 0:
									textout = textout + " - " + par.description.replace("\n", "\n--")
								textout = textout + "    "
							outdone = True
						else:
							pass
							
							
						textout = textout + "\n"
						ind = ind + 1
						
				else:
					textout = textout + line
					line = fin.readline()
					
		line = fin.readline()
		while line:
			textout = textout + line
			line = fin.readline()
				
		fin.close()
		writer.clearText()
		writer.addText(textout)
		writer.write(fnameout + ".hs")


	
	
def addCommentsToClassFiles(modulename, config, cl, attributes, methods, classconfig):
	handleClassFiles(modulename, config, cl, attributes, methods, classconfig, False, True)
	
def writeClassFiles(modulename, config, cl, attributes, methods, classconfig):
	handleClassFiles(modulename, config, cl, attributes, methods, classconfig)

