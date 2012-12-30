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

# BuildInstaller.py

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys


installerFileStructure =			{
				"archive" : "Archive",
				"origin" : "Module-Dist/Installer-Config",
				"target" : "Module-Dist-Installer-Build",
				
			
				"third-party" : "Dep-Licenses",
				"examples" : "Examples",
				"assets" : "Dep-Assets",
				
				"deps-SFML" : "Dep-SFML",
				"deps-CEGUI" : "Dep-CEGUI",
				"deps-Ogre" : "Dep-OgreSDK",
				
				"build-Ogre" : "Module-Ogre-Build",
				"build-CEGUI" : "Module-CEGUI-DllBuild",
				"build-Installer" : "Module-Dist-Installer-Build",
			}
			
def getInstallerPath(name):
	return installerFileStructure[name]


class BuildInstallerTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		modname = "Module-Dist"
		config = self.openConfig("Module-Dist/Installer-Config/BuildConfig.cfg")
		
		title = config.get("ModuleInfo", "title")
		version = config.get("ModuleInfo", "version")
		version2 = version.replace(".", "")
		thismodule = config.get("ModuleInfo", "thismodule")
		
		writer = PyHBind.Base.FileWriter(modname + "-" + thismodule)
	
		# open information on to be copied files
		fin = open ("Module-Dist/Installer-Config/Files.txt", "r")
		copyInst = fin.read()
		fin.close()

		# substitute different path-names in in-file
		copyInst = copyInst.replace("<archive>", getInstallerPath("archive"))
		
		copyInst = copyInst.replace("<target>", getInstallerPath("target"))
		copyInst = copyInst.replace("<assets>", getInstallerPath("assets"))
		copyInst = copyInst.replace("<examples>", getInstallerPath("examples"))
		copyInst = copyInst.replace("<installer>", getInstallerPath("origin"))
		copyInst = copyInst.replace("<third-party>", getInstallerPath("third-party"))
		
		copyInst = copyInst.replace("<deps-sfml>", getInstallerPath("deps-SFML"))
		copyInst = copyInst.replace("<deps-ogre>", getInstallerPath("deps-Ogre"))
		copyInst = copyInst.replace("<deps-cegui>", getInstallerPath("deps-CEGUI"))
		
		copyInst = copyInst.replace("<version>", version2)
		
		# for all lines, first skip comments
		
		fComment = True
		for line in copyInst.split("\n"):
			
			if line[0:2] == "##":
				fComment = False
				
			if not fComment:
				
				if line[0:7] == "copydir":
					arr = line[8:].split(" -> ")
					src = arr[0]
					if len(arr) < 2:
						print "error in line: ", line
						target = "empty"
					else:
						target = arr[1]
					self.copyFolder(src, target)
					
				elif line[0:4] == "copy":
					arr = line[5:].split(" -> ")
					src = arr[0]
					if len(arr) < 2:
						print "error in line: ", line
						target = "empty"
					else:
						target = arr[1]
					self.copyFileToFolder(src, target)
					
				elif line[0:6] == "create":
					target = line[7:]
					self.createDir(target)
					
