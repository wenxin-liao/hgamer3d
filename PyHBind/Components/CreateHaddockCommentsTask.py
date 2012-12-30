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

# CreateHaddockCommentsTask.py

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys

class CreateHaddockCommentsTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		# get module name, config, parsetree
		modname = ctx["modulename"]
		config = ctx["config"]
		parsetree = ctx["parsetree"]
		writer = PyHBind.Base.FileWriter(modname)

		# copy all hs files (if enum or struct files are needed)
		files = "HGamer3D/" + config.getModule().subModule.replace(".", "/")
		fromFiles = writer.getpath("c2hs-build") + "/dist/build/" + files + "/*.hs"
		toPath = writer.getpath("doc-build") + "/" + files
		if not os.path.exists(toPath):
			os.makedirs(toPath)
		cpcmd = "cp " + fromFiles + " " + toPath
		os.system(cpcmd)
		
		# Enum: handle enums here, tbd
		# write enums
		for enum in parsetree.getEnumItemsList():
			enumconfig = config.getEnumConfiguration(enum.id)
			if enumconfig.selected:
				(e, valuesitem) = parsetree.getEnumById(enum.id)
				PyHBind.Components.Writer.addCommentsToEnumFiles(modname, config, enum, valuesitem, enumconfig)
				
		
		# Class: create new hs files, include comments from parsing c headers in doc-build
		for cl in parsetree.getClassItemsList():
			classconfig = config.getClassConfiguration(cl.id)
			if classconfig.selected:
				(c, attributes, methods) = parsetree.getClassById(cl.id)
				PyHBind.Components.Writer.addCommentsToClassFiles(modname, config, cl, attributes, methods, classconfig)

		

		# create haddock documentation 
		files = ""
		pathfiles = "HGamer3D/" + config.getModule().subModule.replace(".", "/")
		for f in os.listdir((writer.getpath("doc-build") + "/" + pathfiles).replace("/", "\\")):
			if f[-3:] == ".hs":
				files = files + pathfiles + "/" + f + " "
		
		cmd1 = "c2hs -l"
		cmd2 = "haddock -o docs -h --hide C2HS " + files
		for cmd in [cmd1, cmd2]:
			cmdex = ("cd " + writer.getpath("doc-build") + self.cmdsep() + cmd)
			os.system(cmdex)
			
		# show docs
		cmd = "cd " + writer.getpath("doc-build") + self.cmdsep() + "start docs\\frames.html"
		os.system(cmd)
		

		
				
		
