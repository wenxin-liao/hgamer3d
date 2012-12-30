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

import wx
import wx.xrc
from wx.lib.pubsub import Publisher
import os, os.path, uuid
from PyHBind.Components import ConfigStore
from HeaderFiles import HeaderFilesView

import GuiBase


class ConfigView(GuiBase.Frame1Base):


	
	def __init__(self, model):
		self.model = model
		GuiBase.Frame1Base.__init__(self, None)
		self.headerFilesView = HeaderFilesView(self.listCtrlHeaderDirs, self.treeCtrlHeaderFiles, self.model)
		
		self.confPath = "."
		Publisher().subscribe(self.psConfigPathChanged, "config.path.changed")
		
		self.selectedBasicTypesItem = None
		self.selectedHeaderFile = None

#
# config view
#
		
	def psConfigPathChanged(self, message):
		self.confPath = message.data

	def getPathAbs(self, path):
		absp = os.path.abspath(self.confPath.replace("/", os.sep) + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		absp2 = os.path.abspath(absp + os.sep + path.replace("/", os.sep))
		return absp2
		
	def setGuiBaseValues(self):
		# Base Config
		base = self.model.getBase()
		self.textCtrlProjectName.SetValue(base.projectName)
		self.txtCtrlParentProjectLabel.SetValue(base.parentProjectLabel)
		self.textCtrlBaseDir.SetValue(base.baseDir)
		self.textCtrlBuildDir.SetValue(base.buildDir)


	def setGuiModuleValues(self):
		# Module Config
		module = self.model.getModule()
		self.txtCtrlModuleTitle.SetValue(module.moduleTitle)
		self.txtCtrlSubModule.SetValue(module.subModule)
		self.txtCtrlCabalName.SetValue(module.cabalName)
		self.txtCtrlModuleVersion.SetValue(module.moduleVersion)
		
		self.txtCtrlCabalUrl.SetValue(module.cabalUrl)
		self.txtCtrlCabalSynopsis.SetValue(module.cabalSynopsis)
		self.txtCtrlCabalDescription.SetValue(module.cabalDescription)
		self.txtCtrlCabalDepends.SetValue(module.cabalDepends)
		self.txtCtrlCabalCategory.SetValue(module.cabalCategory)
		

	def setGuiLibDirsValues(self):
		# library dirs
		libDirs = self.model.getLibDirs()
		self.listCtrlLibDirs.DeleteAllItems()
		for path in libDirs:
			self.listCtrlLibDirs.Append([path.path])
		
	def setGuiLibDllsValues(self):
		# dll files
		libDlls = self.model.getLibDlls()
		self.listBoxLibraryDlls.Clear()
		for fileitem in libDlls:
			self.listBoxLibraryDlls.Append(fileitem.path + "/" + fileitem.name)
		
	def setGuiLibLibsValues(self):
		# dll files
		libLibs = self.model.getLibLibs()
		self.listBoxLibraryLibs.Clear()
		for fileitem in libLibs:
			self.listBoxLibraryLibs.Append(fileitem.path + "/" + fileitem.name)
		
	def setGuiHeaderDirsValues(self):
		self.headerFilesView.setGuiValues()
		self.listBoxHeadersToInclude.Clear()
		for item in self.model.getIncludedHeaders():
			self.listBoxHeadersToInclude.Append(item.path + "/" + item.name)
		
	# this is called when new config needs a reset of all elements
	def setGuiValues(self):
		self.setGuiBaseValues()
		self.setGuiModuleValues()
		self.setGuiLibDirsValues()
		self.setGuiLibDllsValues()
		self.setGuiLibLibsValues()
		self.setGuiHeaderDirsValues()
		self.setGuiNamespacesValues()
		self.clearGuiBasicTypesSelection()
		Publisher().sendMessage("status.change.xmldatachanged", None)

	#
	# handlers of the configuration tab
	#

	# basedir and builddir
	
	def selectBaseDirectoryHandler( self, event ):
		Publisher().sendMessage("config.base.selectbasedirectory", None)
	
	def selectBuildDirectoryHandler( self, event ):
		Publisher().sendMessage("config.base.selectbuilddirectory", None)
	
    # list of header and lib dirs
    
	def listHeaderDirDeleteHandler( self, event ):
		if event.GetKeyCode() == wx.WXK_DELETE:
			listitem = self.listCtrlHeaderDirs.GetNextItem(-1, wx.LIST_NEXT_ALL, wx.LIST_STATE_SELECTED)
			if listitem != -1:
				self.listCtrlHeaderDirs.DeleteItem(listitem)
				Publisher().sendMessage("config.headerdir.delete", listitem)
	
	def listLibDirDeleteHandler( self, event ):
		if event.GetKeyCode() == wx.WXK_DELETE:
			listitem = self.listCtrlLibDirs.GetNextItem(-1, wx.LIST_NEXT_ALL, wx.LIST_STATE_SELECTED)
			if listitem != -1:
				Publisher().sendMessage("config.libdir.delete", listitem)
			
	def listBoxLibraryDllsKeyUp(self, event):
		if event.GetKeyCode() == wx.WXK_DELETE:
			index = self.listBoxLibraryDlls.GetSelections()[0]
			Publisher().sendMessage("config.libdll.delete", index)
	
	def listBoxLibraryLibsKeyUp(self, event):
		if event.GetKeyCode() == wx.WXK_DELETE:
			index = self.listBoxLibraryLibs.GetSelections()[0]
			Publisher().sendMessage("config.liblib.delete", index)
	
	def addHeaderDirHandler( self, event ):
		Publisher().sendMessage("config.headerdir.add", None)
	
	def addLibDirHandler( self, event ):
		Publisher().sendMessage("config.libdir.add", None)

	def addLibraryDllHandler( self, event ):
		Publisher().sendMessage("config.libdll.add", None)

	def addLibraryLibHandler( self, event ):
		Publisher().sendMessage("config.liblib.add", None)

	def appendHeaderDir(self, path):
		self.listCtrlHeaderDirs.Append([path])
		
	def appendLibraryDll(self, name):
		self.listBoxLibraryDlls.Append(name)
		
	def removeLibraryDll(self, index):
		self.listBoxLibraryDlls.Delete(index)
		
	def appendLibraryLib(self, name):
		self.listBoxLibraryLibs.Append(name)
		
	def removeLibraryLib(self, index):
		self.listBoxLibraryLibs.Delete(index)
		
	def appendLibDir(self, path):
		self.listCtrlLibDirs.Append([path])
		
	def removeLibDir(self, index):
		self.listCtrlLibDirs.Delete(index)
		
	# 
	# handler of the file menu, project new, open, save, save as, exit
	#
	
	def projectNewHandler( self, event ):
		Publisher().sendMessage("menu.file.newproject", None)
		
	def openProjectHandler( self, event ):
		Publisher().sendMessage("menu.file.openproject", None)
	
	def saveProjectHandler( self, event ):
		Publisher().sendMessage("menu.file.saveproject", None)
			
	def saveAsProjectHandler( self, event ):
		Publisher().sendMessage("menu.file.saveprojectas", None)
	
	def exitProgramHandler( self, event ):
		Publisher().sendMessage("menu.file.exit", None)
		self.Close(True)
		
	#
	# handler of the workflow menu, info, parse header files
	#

	def workflowInfoHandler( self, event ):
		event.Skip()
	
	def workflowParseHandler( self, event ):
		Publisher().sendMessage("menu.workflow.parseheaderfiles", None)
	
	#
	# tab change handler
	#
	
	def nbPageChangedHandler(self, event):
		tab = event.EventObject.GetChildren()[event.Selection]
		tabName = tab.GetName()
		Publisher().sendMessage("notebook changed", tabName)
		event.Skip()
			
		
	#
	# handler of the header files tag
	#
	
	def treeCtrlHeaderFilesImageActivatedHandler(self, event):
		item = event.GetItem()
		self.headerFilesView.changeSelection(item)
	
	def treeCtrlHeaderFilesLeftUpHandler(self, event):
		item = self.treeCtrlHeaderFiles.GetSelection()
		if item != None:
			myItem = self.treeCtrlHeaderFiles.GetPyData(item)
			if myItem:
				self.selectedHeaderFile = myItem
				if myItem.getClassName() == "HeaderFile":
					for pItem in self.model.getHeaderDirs():
						if pItem.uuid == myItem.uuidOfTopLevel:
							topLevelPath =  pItem.path
					openPath = self.getPathAbs(topLevelPath + "/" + myItem.path + "/" + myItem.name)
					fin = open(openPath, "r")
					text = fin.read()
					fin.close()
					self.richTextHeaderFile.Clear()
					self.richTextHeaderFile.AppendText(text)
		

	def buttonAddHeaderToIncludesHandler(self, event):
		item = self.selectedHeaderFile
		if (item) and (item.getClassName() == "HeaderFile"):
			self.model.getIncludedHeaders().append(item)
			self.listBoxHeadersToInclude.Append(item.path + "/" + item.name)

	#
	# handler of the namespaces tag
	#
	
	def addNamespaceHandler(self, event):
		Publisher().sendMessage("config.namespace.adddialog", None)

	def listBoxNamespacesKeyUp( self, event ):
		if event.GetKeyCode() == wx.WXK_DELETE:
			index = self.listBoxNamespaces.GetSelections()[0]
			Publisher().sendMessage("config.namespace.delete", index)
		
	def appendNamespace(self, namespace):
		self.listBoxNamespaces.Append(namespace)
		
	def removeNamespace(self, index):
		self.listBoxNamespaces.Delete(index)
		
	def setGuiNamespacesValues(self):
		namespaces = self.model.getNamespaces()
		self.listBoxNamespaces.Clear()
		for namespace in namespaces:
			self.listBoxNamespaces.Append(namespace.name)
		
	
#	def listBoxHeadersToIncludeHandler(self, event):
#		self.indexSelectedHeaders = 
		
	# delete
	def listBoxHeadersToIncludeKeyUp(self, event):
		if event.GetKeyCode() == wx.WXK_DELETE:
			index = self.listBoxHeadersToInclude.GetSelections()[0]
			arr = self.model.getIncludedHeaders()
			del arr[index]
			self.listBoxHeadersToInclude.Clear()
			for item in arr:
				self.listBoxHeadersToInclude.Append(item.path + "/" + item.name)
			
		
	#
	# handler of the module info entries
	#
	
	def projectNameSetHandler( self, event ):
		name = self.textCtrlProjectName.GetValue()
		Publisher().sendMessage("config.base.setprojectname", name)
	
	def projectParentLabelSetHandler( self, event ):
		name = self.txtCtrlParentProjectLabel.GetValue()
		Publisher().sendMessage("config.base.setparentprojectlabel", name)
	
	def moduleTitleSetHandler( self, event ):
		title = self.txtCtrlModuleTitle.GetValue()
		Publisher().sendMessage("config.base.setmoduletitle", title)
	
	def subModuleSetHandler( self, event ):
		submodule = self.txtCtrlSubModule.GetValue()
		Publisher().sendMessage("config.base.setsubmodule", submodule)
	
	def cabalNameSetHandler( self, event ):
		cabalname = self.txtCtrlCabalName.GetValue()
		Publisher().sendMessage("config.base.setcabalname", cabalname)
	
	def moduleVersionSetHandler( self, event ):
		moduleversion = self.txtCtrlModuleVersion.GetValue()
		Publisher().sendMessage("config.base.setmoduleversion", moduleversion)


		
	def cabalUrlSetHandler( self, event ):
		cabalurl = self.txtCtrlCabalUrl.GetValue()
		Publisher().sendMessage("config.base.setcabalurl", cabalurl)
	
	def cabalSynopsisSetHandler( self, event ):
		cabalsyn = self.txtCtrlCabalSynopsis.GetValue()
		Publisher().sendMessage("config.base.setcabalsynopsis", cabalsyn)
	
	def cabalDescriptionSetHandler( self, event ):
		cabaldes = self.txtCtrlCabalDescription.GetValue()
		Publisher().sendMessage("config.base.setcabaldescription", cabaldes)
	
	def cabalDependsSetHandler( self, event ):
		cabaldeps = self.txtCtrlCabalDepends.GetValue()
		Publisher().sendMessage("config.base.setcabaldepends", cabaldeps)
	
	def cabalCategorySetHandler( self, event ):
		cabalcat = self.txtCtrlCabalCategory.GetValue()
		Publisher().sendMessage("config.base.setcabalcategory", cabalcat)
	
		
	
