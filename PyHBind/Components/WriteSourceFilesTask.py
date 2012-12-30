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

# WriteSourceFilesTask.py

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys

class WriteSourceFilesTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		# get module name, config, parsetree
		modname = ctx["modulename"]
		config = ctx["config"]
		parsetree = ctx["parsetree"]
		
		# write enums
		for enum in parsetree.getEnumItemsList():
			enumconfig = config.getEnumConfiguration(enum.id)
			if enumconfig.selected:
				(e, valuesitem) = parsetree.getEnumById(enum.id)
				PyHBind.Components.Writer.writeEnumFiles(modname, config, enum, valuesitem, enumconfig)

		# write structs
		for struct in parsetree.getStructItemsList():
			structconfig = config.getStructConfiguration(struct.id)
			if structconfig.selected:
				(s, attributes, methods) = parsetree.getStructById(struct.id)
				PyHBind.Components.Writer.writeStructFiles(modname, config, struct, attributes, methods, structconfig)
				
		# write hs util file
		PyHBind.Components.Writer.writeHsUtilFile(modname, config, parsetree)
		
		# write class-ptr files
		classlist = []
		for cl in parsetree.getClassItemsList():
			classconfig = config.getClassConfiguration(cl.id)
			if classconfig.selected:
				(c, attributes, methods) = parsetree.getClassById(cl.id)
				classlist.append((classconfig, c, attributes, methods))
		PyHBind.Components.Writer.writeClassPtrFiles(modname, config, parsetree, classlist)
		
		# write classes
		for cl in parsetree.getClassItemsList():
			classconfig = config.getClassConfiguration(cl.id)
			if classconfig.selected:
				(c, attributes, methods) = parsetree.getClassById(cl.id)
				PyHBind.Components.Writer.writeClassFiles(modname, config, cl, attributes, methods, classconfig)

		# copy source files from sources dir into build dir
		for sdir in config.getSourceDirs():
			
			files = []
			for f in os.listdir(sdir.path):
				if f[-4:] == ".cpp":
					sourceFile = sdir.path + "/" + f
					destFile = config.getBase().buildDir + "/dll-build/" + f
					cmd = "copy " + sourceFile + " " + destFile
					os.system(cmd.replace("/", "\\"))
		
		# write makefile
		PyHBind.Components.Writer.writeDllMakefile(modname, config, parsetree)
		
		# write Cabalfile and other files or cabal make
		PyHBind.Components.Writer.writeCabalfile(modname, config, parsetree)

		
				
		
