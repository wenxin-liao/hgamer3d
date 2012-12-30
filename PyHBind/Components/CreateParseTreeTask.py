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

from PyHBind.Base import Task, FileWriter
from PyHBind.Apps.DoxyParser import Index
import PyHBind.Components
import os, os.path, sys

class CreateParseTreeTask(Task):
	
	def doIt(self, ctx):
		
		# get module name, config, parsetree
		modname = ctx["modulename"]
		if "config" not in ctx.keys():
			if os.path.exists("Module-" + modname + "/Config/module-config.cfg"):
				# load config
				store = PyHBind.Components.ConfigStore()
				store.loadConfig("Module-" + modname + "/Config", "module-config.cfg")
				config = store.getConfig()
			else:
				print "no configuration for " + modname + " found, aborting!"
				sys.exit(-1)
		else:
			config = ctx["config"]
		
		writer = PyHBind.Base.FileWriter(modname)
		outPath = writer.getpath("ParsedHeaders")
		
		# for all headerdirs and all sourcedirs
		ds = config.getItem("/config/headerdirs") + config.getItem("/config/sourcedirs")
		for d in ds:
		
			# create new store
			store = PyHBind.Components.ParseTreeStore()
			# open index xml file and read it
			filename = outPath.replace(os.sep, "/") + "/" + d.uuid + "/xml/index.xml"
			filename = filename.replace("/", os.sep)
#			print filename
			Index().readXML(filename, store)
			# write store
			store.write(outPath + "/" + d.uuid + "/parsed-xml-info.cfg")
