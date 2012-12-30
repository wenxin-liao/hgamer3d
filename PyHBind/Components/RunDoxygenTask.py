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
import os, os.path, sys

class RunDoxygenTask(PyHBind.Base.Task):
	
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
		abspath = os.path.abspath(writer.getpath(".."))
		self.writer = writer
		
		# for all headerdirs 
		for d in config.getItem("/config/headerdirs"):
		
			# gather all selected files
			allSelected = ""
			
			# for all files which are selected in config, run doxy
			for i in config.getItem("/headers/" + d.uuid):
				if i.getClassName() == "HeaderFile":
					if i.selected == True:
						allSelected = allSelected + "\"" + abspath.replace(os.sep, "/") + "/" + d.path + "/" + i.path + "/" + i.name + "\" "
						
			# run doxy on all selected files at once, for one header-dir
			self.rundoxy(outPath + "/" + d.uuid, allSelected)

		# for all sourcedirs 
		for d in config.getItem("/config/sourcedirs"):
		
			# gather all selected files
			allSelected = ""
			for f in os.listdir(abspath.replace(os.sep, "/") + "/" + d.path):
				if f[-2:] == ".h":
					allSelected =  allSelected + "\"" + abspath.replace(os.sep, "/") + "/" + d.path + "/" + f + "\" "
						
			# run doxy on all selected files at once, for one header-dir
			self.rundoxy(outPath + "/" + d.uuid, allSelected)
		
		
	def rundoxy(self, outDir, allFileNames):
		
		# create special output dir for header parsing
		self.writer.createDir(outDir)
		
		# cd into that dir and execute doxygen command
		doxfile = outDir + "/" + "Doxyfile"
		fout = open(doxfile, "w")
		fout.write("EXTRACT_ALL = YES\n")
		fout.write("INPUT = " + allFileNames + "\n")
		fout.write("GENERATE_HTML = NO\n")
		fout.write("GENERATE_LATEX = NO\n")
		fout.write("GENERATE_RTF = NO\n")
		fout.write("GENERATE_MAN = NO\n")
		fout.write("GENERATE_XML = YES\n")
		fout.write("QUIET = YES\n")
		fout.write("WARNINGS = NO\n")
		fout.write("WARN_IF_DOC_ERROR = NO\n")
		fout.close()
		
		# run doxygen
		pathRun = os.path.abspath(outDir)
		cmd1 = "cd " + pathRun
		cmd2 = "doxygen"
		os.system(cmd1 + "&" + cmd2)

