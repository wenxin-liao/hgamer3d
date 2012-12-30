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

# BuildExamplesTask.py

import PyHBind.Base
import PyHBind.Components

import os, os.path, sys, shutil


class BuildExamplesTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		
		origin = "Examples"
		examples = "Examples-Build"
		
		# this task build examples, each more important example
		# get's an own output directory with corresponding media

		
		# Getting-Started-Example
		# -------------
		target = examples + "/Getting-Started"
		# main directory content
		self.createDir(target)
		self.copyFileToFolder(origin + "/Getting-Started.hs", target)
		self.copyFileToFolder(origin + "/Util-Get-Path.hs", target)
		self.copyFileToFolder(origin + "/compile.bat", target)
		self.copyFileToFolder(origin + "/Getting-Started-Readme.txt", target)
		# media directory content
		self.copyFolder("Dep-Assets/examples-media", target + "/media")
		# create start bat
		os.system("cd " + target + "&&echo compile Getting-Started >startme.bat")
		
		# Jumping-Balls-Example
		# -------------
		target = examples + "/Jumping-Balls"
		# main directory content
		self.createDir(target)
		self.copyFileToFolder(origin + "/Jumping-Balls.hs", target)
		self.copyFileToFolder(origin + "/Util-Get-Path.hs", target)
		self.copyFileToFolder(origin + "/compile.bat", target)
		self.copyFileToFolder(origin + "/Jumping-Balls-Readme.txt", target)
		# media directory content
		self.copyFolder("Dep-Assets/ywing-media", target + "/media")
		# create start bat
		os.system("cd " + target + "&&echo compile Jumping-Balls >startme.bat")
		
		# Colliding-Balls-Example
		# -------------
		target = examples + "/Colliding-Balls"
		# main directory content
		self.createDir(target)
		self.copyFileToFolder(origin + "/Colliding-Balls.hs", target)
		self.copyFileToFolder(origin + "/Util-Get-Path.hs", target)
		self.copyFileToFolder(origin + "/compile.bat", target)
		self.copyFileToFolder(origin + "/Colliding-Balls-Readme.txt", target)
		# media directory content
		self.copyFolder("Dep-Assets/ywing-media", target + "/media")
		# create start bat
		os.system("cd " + target + "&&echo compile Colliding-Balls >startme.bat")
		
		# YWing-Example
		# -------------
		target = examples + "/YWing-Flight"
		# main directory content
		self.createDir(target)
		self.copyFileToFolder(origin + "/YWing-Flight.hs", target)
		self.copyFileToFolder(origin + "/Util-Get-Path.hs", target)
		self.copyFileToFolder(origin + "/compile.bat", target)
		self.copyFileToFolder(origin + "/YWing-Flight-Readme.txt", target)
		# media directory content
		self.copyFolder("Dep-Assets/ywing-media", target + "/media")
		self.copyFolder("Dep-Assets/incompetech-sounds", target + "/media/sound")
		# layout directory content
		layout = target + "/media/gui/layout"
		self.createDir(layout)
		self.copyFileToFolder(origin + "/statictext.layout", layout)
		self.copyFileToFolder(origin + "/configurator.layout", layout)
		# create start bat
		os.system("cd " + target + "&&echo compile YWing-Flight >startme.bat")
		
		# GUI-Widgets-Example
		# -------------
		target = examples + "/GUI-Widgets"
		# main directory content
		self.createDir(target)
		self.copyFileToFolder(origin + "/GUI-Widgets.hs", target)
		self.copyFileToFolder(origin + "/Util-Get-Path.hs", target)
		self.copyFileToFolder(origin + "/compile.bat", target)
		self.copyFileToFolder(origin + "/GUI-Widgets-Readme.txt", target)
		# media directory content
		self.copyFolder("Dep-Assets/examples-media", target + "/media")
		# layout directory content
		layout = target + "/media/gui/layout"
		self.createDir(layout)
		self.copyFileToFolder(origin + "/statictext.layout", layout)
		self.copyFileToFolder(origin + "/gui-widgets.layout", layout)
		# create start bat
		os.system("cd " + target + "&&echo compile GUI-Widgets >startme.bat")
		
		
