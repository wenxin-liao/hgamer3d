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

# Writer.Cabalfile


# creates files, to build Haskell module, create Cabalfile and others

import os
import PyHBind.Base
import PyHBind.Components


def writeCabalfile(modname, config, parsetree):

	task = PyHBind.Base.Task()

	writer = PyHBind.Base.FileWriter(modname)
	moduleConfig = config.getModule()
	baseConfig = config.getBase()
	parentLabel = baseConfig.parentProjectLabel
	moduleName = moduleConfig.moduleTitle
	version = moduleConfig.moduleVersion
	dllversion = version.replace(".", "")


	# LICENSE file
	#
	writer.clearText()
	writer.addText(writer.templateText("license"))
	writer.write("c2hs-build/LICENSE")
	
	# Setup.hs file
	#
	writer.clearText()
	writer.addText(writer.commentText(writer.templateText("sourcefile"), "--"))
	writer.addText(writer.commentText(writer.templateText("license"), "--"))
	writer.addText(writer.templateText("setuphs"))
	writer.write("c2hs-build/Setup.hs")
	
	# C2HS file
	#
#	cmdc2hs = "cd " + writer.getpath("c2hs-build") + task.cmdsep() + "c2hs -l"
#	os.system(cmdc2hs)
	
	
	# Cabal file
	#
	
	# exposed modules
	mName = parentLabel + "." + moduleConfig.subModule
	path = "c2hs-build/" + mName.replace(".", "/")
	emods = []
	cmods = []
	smods = []
	exposedModules = ""
	otherModules = ""
	umods = []
	for fname in os.listdir(writer.getpath(path)):
		if fname[-4:] == ".chs":
			fmname = mName + "." + fname[:-4]
			name = fname[:-4]
			if  name == "Utils":
				umods.append(fmname)
			elif name == "ClassPtr" or name == "StructHG3DClass":
				if len(otherModules) > 0:
					otherModules = otherModules + ", "
				otherModules = otherModules + fmname
			else:
				if name[0:5] == "Class":
					cmods.append(fmname)
				elif name[0:6] == "Struct":
					smods.append(fmname)
				elif name[0:4] == "Enum":
					emods.append(fmname)
				else:
					print "Cabalfile, did not consider module:",name
				
	for fmname in umods:
		if len(exposedModules) > 0:
			exposedModules = exposedModules + ", "
		exposedModules = exposedModules + fmname
	if len(exposedModules) > 0:
		exposedModules = exposedModules + ", "
	exposedModules = exposedModules + otherModules
	for fmname in emods + smods + cmods:
		if len(exposedModules) > 0:
			exposedModules = exposedModules + ", "
		exposedModules = exposedModules + fmname
	
	
	# extra libraries
	extraLibraries = parentLabel + moduleName + dllversion
#	for lib in config.getLibDlls():
#		extraLibraries = extraLibraries + ", " + lib.name
	
	# extra source files
	extraSourceFiles = "Setup.hs"
	for fname in os.listdir(writer.getpath("include")):
		extraSourceFiles = extraSourceFiles + ", include/" + fname
	
	writer.clearText()
	writer.addText(writer.templateText("cabal"))
	writer.replaceText(
		[
			("version", version),
			("moduletitle", moduleConfig.cabalName),
			("synopsis", moduleConfig.cabalSynopsis),
			("description", writer.commentText(moduleConfig.cabalDescription, "  ")),
			("homepage", moduleConfig.cabalUrl),
			("category", moduleConfig.cabalCategory),
			("extraSourceFiles", extraSourceFiles),
			("buildDepends", moduleConfig.cabalDepends),
			("exposedModules", exposedModules),
			("extraLibraries", extraLibraries),
		] )
	fname = moduleConfig.cabalName + ".cabal"
	writer.write("c2hs-build/" + fname)
	
