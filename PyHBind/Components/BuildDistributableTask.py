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

# BuildDistributableTask.py

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys, shutil


class BuildDistributableTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		modname = "Module-Dist"
		config = self.openConfig("Module-Dist/Distributable-Config/BuildConfig.cfg")
		
		title = config.get("ModuleInfo", "title")
		version = config.get("ModuleInfo", "version")
		version2 = version.replace(".", "")
		thismodule = config.get("ModuleInfo", "thismodule")
		
		writer = PyHBind.Base.FileWriter(modname + "-" + thismodule)
	
		origin = "Module-Dist/Distributable-Config"
		target = "Module-Dist-Distributable-Build/files"
		installer = "Module-Dist-Installer-Build"
		thirdparty = "Dep-Licenses"
		hgamer3d = target + os.sep + "hgamer3d"
		
		
		# copy files from Installer directory
		self.createDir(target)
		self.copyFolder(installer + os.sep + "files" + os.sep + "bin", target + os.sep + "bin")
		self.copyFolder(installer + os.sep + "files" + os.sep + "examples" + os.sep + "media", target + os.sep + "media")
		self.copyFileToFolder(installer + os.sep + "files" + os.sep + "examples" + os.sep + "plugins.cfg", target)
		self.copyFileToFolder(installer + os.sep + "files" + os.sep + "examples" + os.sep + "resources.cfg", target)
		self.copyFileToFolder(origin + os.sep + "executeProgram.bat", target)
		self.createDir(hgamer3d)
		self.copyFileToFolder("LICENSE.txt", hgamer3d)
		self.copyFileToFolder("DEPENDENCIES.txt", hgamer3d)
		self.copyFolder(thirdparty, hgamer3d + os.sep + "Third-Party-Licenses")
		
		# now, build the archive
		target = "Module-Dist-Distributable-Build"
		shutil.make_archive(target + os.sep + "HGamer3D-Distributable-" + version, "zip", target + os.sep + "files")



