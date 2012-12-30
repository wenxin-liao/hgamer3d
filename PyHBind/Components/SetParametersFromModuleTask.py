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

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys

class SetParametersFromModuleTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		# get module name
		modname = ctx["modulename"]
		
		# check valid environment
		# we need a file: "Module-<modulename>/Config/module-config.cfg"
		
		if os.path.exists("Module-" + modname + "/Config/module-config.cfg"):
			# load config
			config = PyHBind.Components.ConfigStore()
			config.loadConfig("Module-" + modname + "/Config", "module-config.cfg")
			ctx["config"] = config
			
			# load parsed xml trees
			parsetree = PyHBind.Components.MultiParseTreeStore()
			fileArray = []
			ds = config.getHeaderDirs() + config.getSourceDirs()
			for header in ds:
				path = "./" + config.getBase().buildDir + "/ParsedHeaders/" + header.uuid + "/parsed-xml-info.cfg"
				uuid = header.uuid
				fileArray.append( (path, uuid) )
			parsetree.loadData(fileArray)
			ctx["parsetree"] = parsetree
		else:
			print "could not open: ", "Module-" + modname + "/Config/module-config.cfg"
			sys.exit(-1)
			
