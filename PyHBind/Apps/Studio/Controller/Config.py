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

from wx.lib.pubsub import Publisher
import os, os.path

import PyHBind.Apps.Studio.View


class ConfigController:
	
	def __init__(self, model, view):
		self.model = model
		self.view = view
		
		# pathBase, will be set to the directory of the current configuration
		# the base path given by the user will be relative to this directory
		# all other pathes in the configuration will be relative to the base path given 
		# by the user.
		
		self.confPath = "."					# path to config file
		self.configfile = None
		self.start = None
		self.selectedBasicTypesItem = None
		
		# register all event handler
		pub = Publisher()
		pub.subscribe(self.psConfigBase, "config.base")
		pub.subscribe(self.psConfigLibdir, "config.libdir")
		pub.subscribe(self.psConfigNamespace, "config.namespace")
		pub.subscribe(self.psConfigLibraryDll, "config.libdll")
		pub.subscribe(self.psConfigLibraryLib, "config.liblib")
		pub.subscribe(self.psConfigHeaderdir, "config.headerdir")
		pub.subscribe(self.rescanHeaderDirs, "header.rescandirs")
		
#
# message handler
#
		
	def psConfigBase(self, message):
		topic = message.topic[2]
		if topic == "selectbasedirectory":
			self.selectBaseDirectory()
		elif topic == "selectbuilddirectory":
			self.selectBuildDirectory()
		elif topic == "setprojectname":
			self.model.getBase().projectName = message.data
		elif topic == "setparentprojectlabel":
			self.model.getBase().parentProjectLabel = message.data
		elif topic == "setmoduletitle":
			self.model.getModule().moduleTitle = message.data
		elif topic == "setsubmodule":
			self.model.getModule().subModule = message.data
		elif topic == "setcabalname":
			self.model.getModule().cabalName = message.data
		elif topic == "setmoduleversion":
			self.model.getModule().moduleVersion = message.data
			
		elif topic == "setcabalurl":
			self.model.getModule().cabalUrl = message.data
		elif topic == "setcabalsynopsis":
			self.model.getModule().cabalSynopsis = message.data
		elif topic == "setcabaldescription":
			self.model.getModule().cabalDescription = message.data
		elif topic == "setcabaldepends":
			self.model.getModule().cabalDepends = message.data
		elif topic == "setcabalcategory":
			self.model.getModule().cabalCategory = message.data


	#
	#	todo implement deletion of header dirs libdirs library dlls
	#
	
	def psConfigHeaderdir(self, message):
		topic = message.topic[2]
		if topic == "add":
			self.addHeaderDir()
		if topic == "delete":
			self.removeHeaderDir(message.data)
		
	def psConfigLibdir(self, message):
		topic = message.topic[2]
		if topic == "add":
			self.addLibDir()
		if topic == "delete":
			self.removeLibDir(message.data)
		
	def psConfigLibraryDll(self, message):
		topic = message.topic[2]
		if topic == "add":
			self.addLibraryDll()
		if topic == "delete":
			self.removeLibraryDll(message.data)
		
	def psConfigLibraryLib(self, message):
		topic = message.topic[2]
		if topic == "add":
			self.addLibraryLib()
		if topic == "delete":
			self.removeLibraryLib(message.data)
		
	def psConfigNamespace(self, message):
		topic = message.topic[2]
		if topic == "add":
			self.addNamespace(message.data)
		if topic == "adddialog":
			self.addNamespaceDialog()
		if topic == "delete":
			self.removeNamespace(message.data)
		

#
# base functionalitites of the model
#

	# functionality for path and config loading
		
	def newConfig(self):
		self.model.newConfig()
		self.configfile = None
		self.confPath = "."
		Publisher.sendMessage("config.path.changed", ".")
		Publisher.sendMessage("status.change.reload", ".")
		
	def loadConfig(self, path):
		self.configfile = os.path.basename(path)
		self.confPath = os.path.dirname(path)
		self.model.loadConfig(self.confPath, self.configfile)
		Publisher.sendMessage("config.path.changed", self.confPath)
		Publisher.sendMessage("status.change.reload", ".")

# here follows relative path mechanism

	def getPathRel(self, path):
		absp = os.path.abspath(self.confPath.replace("/", os.sep) + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		rpath = os.path.relpath(path.replace("/", os.sep), absp)
		rpath = rpath.replace(os.sep, "/")
		return rpath
		
	def getPathAbs(self, path):
		absp = os.path.abspath(self.confPath.replace("/", os.sep) + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		absp2 = os.path.abspath(absp + os.sep + path.replace("/", os.sep))
		return absp2

# need to change also the sources path

	def correctAllPaths(self, bpathOld, bpathNew):
		# lib dirs
		for item in self.model.getLibDirs():
			opath = item.path.replace("/", os.sep)
			aopath = os.path.abspath(bpathOld + os.sep + opath)
			rnpath = os.path.relpath(aopath, bpathNew)
			npath = rnpath.replace(os.sep, "/")
			item.path = npath
		# library dll files
		for item in self.model.getLibDlls():
			opath = item.path.replace("/", os.sep)
			aopath = os.path.abspath(bpathOld + os.sep + opath)
			rnpath = os.path.relpath(aopath, bpathNew)
			npath = rnpath.replace(os.sep, "/")
			item.path = npath
		# header dirs
		for item in self.model.getHeaderDirs():
			opath = item.path.replace("/", os.sep)
			aopath = os.path.abspath(bpathOld + os.sep + opath)
			rnpath = os.path.relpath(aopath, bpathNew)
			npath = rnpath.replace(os.sep, "/")
			item.path = npath
		# sources dirs
		for item in self.model.getSourceDirs():
			opath = item.path.replace("/", os.sep)
			aopath = os.path.abspath(bpathOld + os.sep + opath)
			rnpath = os.path.relpath(aopath, bpathNew)
			npath = rnpath.replace(os.sep, "/")
			item.path = npath
		# build dir
		opath = self.model.getBase().buildDir.replace("/", os.sep)
		aopath = os.path.abspath(bpathOld + os.sep + opath)
		rnpath = os.path.relpath(aopath, bpathNew)
		npath = rnpath.replace(os.sep, "/")
		self.model.getBase().buildDir = npath
		# redo all guivalues
		self.view.setGuiValues()
	
	def newConfPath(self, path):
		# here code, to fix all changes
		osBaseDir = (self.confPath + "/" + self.model.getBase().baseDir).replace("/", os.sep)
		apath = os.path.abspath(path)
		# correct base bath, all other remain the same
		opath = os.path.abspath(osBaseDir)
		rnpath = os.path.relpath(opath, apath)
		npath = rnpath.replace(os.sep, "/")
		self.model.getBase().baseDir = npath
		self.view.setGuiBaseValues()
		# set new path
		self.confPath = apath.replace(os.sep, "/")
		Publisher.sendMessage("config.path.changed", self.confPath)
		

	def newBaseDir(self, path):
		osConfPath = self.confPath.replace("/", os.sep)
		rpath = os.path.relpath(path.replace("/", os.sep), osConfPath)
		# here code, to fix all changes
		bpathOld = os.path.abspath(osConfPath + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		bpathNew = os.path.abspath(osConfPath + "/" + rpath)
		# set new path
		npath = rpath.replace(os.sep, "/")
		self.model.getBase().baseDir = npath
		# correct path
		self.correctAllPaths(bpathOld, bpathNew)




#
# handler implementation section, external high-level interface commands
#

	# start of program
	
	def startAll(self, filename):
		if filename != None:
			fpath = os.path.abspath(filename)
			if os.path.exists(fpath):
				self.loadConfig(fpath)
		self.view.setGuiValues()
		
	# handler of the file menu, project new, open, save, save as, 
	
	def newProject( self ):
		self.newConfig()
		self.view.setGuiValues()
		
	def openProject( self ):
		out = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectFile(self.confPath, False, "open project", "*.cfg")
		if out != None:
			(filename, path) = out
			self.loadConfig(path + os.sep + filename)
			self.view.setGuiValues()
	
	def saveProject( self ):
		if self.configfile == None:
			self.saveProjectAs()
		else:
			self.model.saveConfig(self.confPath, self.configfile)
			
	def saveProjectAs( self ):
		out = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectFile(self.confPath, True, "save project", "*.cfg")
		if out != None:
			(filename, path) = out
			self.newConfPath(path)
			self.configfile = filename
			self.model.saveConfig(self.confPath, self.configfile)
	
	# basedir and builddir
	
	def selectBaseDirectory( self ):
		opath = self.getPathAbs(".")
		npath = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectDirectory(opath, False, "select base directory")
		if npath != None:
			self.newBaseDir(npath)
	
	def selectBuildDirectory( self ):
		opath = self.getPathAbs(self.model.getBase().buildDir)
		npath = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectDirectory(opath, False, "select build directory")
		if npath != None:
			rnpath = self.getPathRel(npath)
			self.model.getBase().buildDir = rnpath
			self.view.setGuiBaseValues()
			Publisher.sendMessage("status.change.builddir", None)

	
    # header dir s
    
	def listHeaderDirDeleteHandler( self, event ):
		event.Skip()
	
	def addHeaderDir( self ):
		opath = self.getPathAbs(".")
		out = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectDirectory(opath, False, "select header directory")
		if out != None:
			path = self.getPathRel(out)
			item = self.model.appendHeaderDir(path)
			self.model.addHeaderFilesFromHeaderDir(self.getPathAbs("."), item)
			self.view.appendHeaderDir(path)
			self.view.setGuiHeaderDirsValues()
			
	def rescanHeaderDirs(self, message):
		for item in self.model.getHeaderDirs():
			self.model.addHeaderFilesFromHeaderDir(self.getPathAbs("."), item)
		self.view.setGuiHeaderDirsValues()

	def removeHeaderDir(self, index):
		rItem = self.model.getHeaderDirs()[index]
		self.view.headerFilesView.removeHeaderDirFromView(rItem)
		self.model.removeHeaderDir(index)
		
	
    # lib dirs

	def listLibDirDeleteHandler( self, event ):
		event.Skip()
	
	def addLibDir( self ):
		opath = self.getPathAbs(".")
		out = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectDirectory(opath, False, "select library directory")
		if out != None:
			path = self.getPathRel(out)
			item = self.model.appendLibDir(path)
			self.view.appendLibDir(path)
		
	def removeLibDir( self, index ):
		del self.model.getLibDirs()[index]
		self.view.removeLibDir(index)
		
	def addLibraryDll( self ):
		opath = self.getPathAbs(".")
		out = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectFile(opath, False, "select dll file", "*.dll")
		if out != None:
			(filename, dirname) = out
			path = self.getPathRel(dirname)
			item = self.model.appendLibraryDll(filename, path)
			self.view.appendLibraryDll(path + "/" + filename)
		
	def removeLibraryDll( self, index ):
		del self.model.getLibDlls()[index]
		self.view.removeLibraryDll(index)
		
	def addLibraryLib( self ):
		opath = self.getPathAbs(".")
		out = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectFile(opath, False, "select dll file", "*.lib")
		if out != None:
			(filename, dirname) = out
			path = self.getPathRel(dirname)
			item = self.model.appendLibraryLib(filename, path)
			self.view.appendLibraryLib(path + "/" + filename)
		
	def addNamespaceDialog( self ):
		dialog = PyHBind.Apps.Studio.View.MyDialogCreateNamespace(self.view)
		dialog.ShowModal()
		dialog.Destroy()
		
	def addNamespace(self, name):
		item = self.model.getConfig().createItem("Namespace")
		item.name = name
		self.model.getNamespaces().append(item)
		self.view.appendNamespace(name)
		
	def removeNamespace( self, index ):
		del self.model.getNamespaces()[index]
		self.view.removeNamespace(index)
		
	
	
