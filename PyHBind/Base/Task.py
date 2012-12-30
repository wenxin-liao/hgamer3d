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

# Task.py

#
# Does something in the chain of build commands (Action2)  
#


import os, os.path, shutil, sys, ConfigParser

__author__ = "Peter Althainz"

class Task:
	"""
	This class implements chainable tasks, which all work on a central 
	data structure, called configuration. 
	"""
	
	def doItInternal(self, ctx):
		print "executing: " + self.__module__ + "." + self.__class__.__name__
		self.doIt(ctx)

	# doIt(self) to be overwritten by subclass
	def doIt(self, ctx):
		pass

	# some utility functions, to be used in build actions
	#
	
	def isWindows(self):
		return os.name == "nt"
		
	def isLinux(self):
		return os.name != "nt"
		
	def cmdsep(self):
		if self.isWindows():
			return "&"
		else:
			return ";"
			
	# config utilities, to be used in build actions
	#
	
	def openConfig(self, path):
		config = ConfigParser.ConfigParser()
		cf = open(path)
		config.readfp(cf)
		cf.close()
		return config

	# generic file and folder utilities
	#
	
	def copyFile(self, src, target):
		shutil.copyfile(src.replace("/", os.sep), target.replace("/", os.sep))
		
	def copyFileToFolder(self, src, targetFolder):
		shutil.copy(src.replace("/", os.sep), targetFolder.replace("/", os.sep))
		
	def secureRemoveFolder(self, folder):
		if self.isWindows():
			cmd = "RMDIR " + folder + "/s /q"
		else:
			cmd = "rm -rf " + folder
		# check, if folder basename contains a "Build"
		if os.path.abspath(folder).find("Build") > 0:
			os.system(cmd)
		else:
			print "secureRemoveFolder was not in a Build, see Action.py"
			print "for folder:", folder
		
		
	def copyFolder(self, srcFolder, targetFolder):
		target = targetFolder.replace("/", os.sep)
		if os.path.exists(target):
			self.secureRemoveFolder(target)
		shutil.copytree(srcFolder.replace("/", os.sep), target)

	def createDir(self, targetDir):
		target = targetDir.replace("/", os.sep)
		if not os.path.exists(target):
			os.makedirs(target)

	def addFolderToZip(self, myZipFile, folder):
		for f in os.listdir(folder):
			if os.path.isfile(folder + os.sep + f):
				myZipFile.write(folder + os.sep + f)
			elif os.path.isdir(folder + os.sep + f):
				self.addFolderToZip(myZipFile,folder + os.sep + f)
				


				

class TaskList(Task):
	"""
	a chain of Actions, which itself is an action
	"""
	
	def __init__(self, actionlist):
		self.actions = actionlist
		
	def doIt(self, ctx):
		for action in self.actions:
			action.doItInternal(ctx)

