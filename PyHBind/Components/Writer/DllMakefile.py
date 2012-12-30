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

# Writer.DllMakefile


# creates files, to build dll, makefile (CMake) and others

import os
import PyHBind.Base
import PyHBind.Components


def writeDllMakefile(modname, config, parsetree):

	writer = PyHBind.Base.FileWriter(modname)
	moduleConfig = config.getModule()
	baseConfig = config.getBase()
	parentLabel = baseConfig.parentProjectLabel
	moduleName = moduleConfig.moduleTitle
	version = moduleConfig.moduleVersion
	dllversion = version.replace(".", "")

	# buildcmd
	#
	writer.clearText()
	writer.addText(writer.templateText("dllbuildcmd"))
	writer.replaceText(
		[
			("license", writer.commentText(writer.templateText("license"), "REM")),
		] )
	writer.write("dll-build/build_cmd.template")
		
	# dlldefines
	#
	writer.clearText()
	writer.addText(writer.templateText("dlldefines"))
	writer.replaceText(
		[
			("license", writer.commentText(writer.templateText("license"), "//")),
			("module", modname),
			("libname", parentLabel + modname + dllversion),
		] )
	writer.write("include/" + modname + "DllDefines.h")
	
	# CMakeLists file
	#
	
	# what are the c-files, simple look in dll-build directory
	cfiles = ""
	searchpath = writer.getpath("dll-build")
	for f in os.listdir(searchpath):
		if f[-4:] == ".cpp":
			cfiles = cfiles + f + " "
		
	# what are libraries and include dirs
	includeDirs = "../include  "
	for hdir in (config.getHeaderDirs() + config.getSourceDirs()):
		includeDirs = includeDirs + "../../" + hdir.path + " "
	
	libDirs = ""
	for ldir in config.getLibDirs():
		libDirs = libDirs + "../../" + ldir.path + " "
	
	# add libs needed
	addDlls = ""
	for adll in config.getLibLibs():
		addDlls = addDlls + adll.name + " "
	
	# version
	versarr = version.split(".")
	larr = len(versarr)
	if larr > 0:
		versionMajor = versarr[0]
		if larr > 1:
			versionMinor = versarr[1]
		else:
			versionMinor = "0"
	else:
		versionMajor = "0"
		versionMinor = "1"
	
	writer.clearText()
	writer.addText(writer.templateText("dllCMakeLists"))
	writer.replaceText(
		[
			("license", writer.commentText(writer.templateText("license"), "#")),
			("module", modname),
			("project-name", parentLabel + modname),
			("version-major", versionMajor),
			("version-minor", versionMinor),
			("include-dirs", includeDirs),
			("lib-dirs", libDirs),
			("library-name", parentLabel + modname + dllversion),
			("c-files", cfiles),
			("add-dlls", addDlls),
		] )
	writer.write("dll-build/CMakeLists.txt")

