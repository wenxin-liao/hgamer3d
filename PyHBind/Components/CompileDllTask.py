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

# CompileDllTask.py

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys

class CompileDllTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		# get module name, config, parsetree
		modname = ctx["modulename"]
		config = ctx["config"]
		parsetree = ctx["parsetree"]
		
		# build make file from cmake file
		writer = PyHBind.Base.FileWriter(modname)
		cmdcmake = "cd " + writer.getpath("dll-build") + self.cmdsep() + "cmake . -G \"Visual Studio 10\""
		print cmdcmake
		os.system(cmdcmake)
		# compile
		cmdbuild = "cd " + writer.getpath("dll-build") + self.cmdsep() + "cmd \"/C build.bat\""
		print cmdbuild
		os.system(cmdbuild)

