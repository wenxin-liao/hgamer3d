"""
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2012 Peter Althainz

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

# FileWriter.py

import os, shutil

class FileWriter:
	
	def __init__(self, modulename):

		self.text = ""
		self.modulename = modulename
		
		# define directories
		self.templatepath = "./Module-" + modulename + "/Templates"
		self.buildpath = "./Module-" + modulename + "-Build"
		
	def addText(self, text):
		self.text = self.text + text
		
	def replaceText(self, replacements):
		for (key, value) in replacements:
			self.text = self.text.replace("<" + key + ">", value)
		
	def commentText(self, comment, marker):
		text = ""
		for line in comment.split("\n"):
			text = text + marker + " " + line + "\n"
		return text
			
	def templateText(self, template):
		path = self.templatepath + "/" + template + ".tmpl"
		infile = open(path, "r")
		text = infile.read()
		infile.close()
		return text

	def write(self, filepath):
		path = self.buildpath + "/" + filepath
		# check if content already in file, then exit
		if os.path.exists(path):
			inF = open(path,"r")
			inText = inF.read()
			inF.close()
			if inText == self.text:
				return
		# write new file
		basepath = os.path.dirname(path)
		if not os.path.exists(basepath):
			os.makedirs(basepath)
		outF = open(path, "w")
		outF.write(self.text)
		outF.close()

	def openread(self, filepath):
		path = self.buildpath + "/" + filepath
		handle = open(path, "r")
		return handle
		
	def openwrite(self, filepath):
		path = self.buildpath + "/" + filepath
		handle = open(path, "w")
		return handle
		
	def movefile(self, src, dest):
		pathsrc = self.buildpath + "/" + src
		pathdest = self.buildpath + "/" + dest
		shutil.move(pathsrc, pathdest)
		
	def delfile(self, filepath):
		path = self.buildpath + "/" + filepath
		os.remove(path)

	def getpath(self, filepath):
		path = self.buildpath + "/" + filepath
		return path
		
	def asName(self, name):
		"""returns a new Haskellish name for a C-function"""
		newName = ""
		toHigher = False
		for char in name:
			if char in "_-":
				toHigher = True
			else:
				if toHigher:
					newName = newName + char.upper()
				else:
					newName = newName + char
				toHigher = False
		return newName

	def clearText(self):
		self.text = ""




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



















