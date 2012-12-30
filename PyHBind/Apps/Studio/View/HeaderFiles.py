# -*- coding: latin-1 -*-

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

import os, os.path, wx
from PyHBind.Components import ConfigStore
from wx.lib.pubsub import Publisher


class HeaderFilesView:
	
	def __init__(self, listCtrl, treeCtrl, model):
		self.model = model
		self.listCtrl = listCtrl
		self.treeCtrl = treeCtrl
		self.il = wx.ImageList(16,16)
		self.fldridx = self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_FOLDER, wx.ART_OTHER, (16,16)))
		self.notselidx = self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_DELETE,   wx.ART_OTHER, (16,16)))
		self.fileidx = self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_NORMAL_FILE, wx.ART_OTHER, (16,16)))
		self.fldropidx = self.il.Add(wx.ArtProvider.GetBitmap(wx.ART_REMOVABLE, wx.ART_OTHER, (16,16)))
		self.treeCtrl.AssignImageList(self.il)
		self.guiRoot = self.treeCtrl.AddRoot("Header Directories")
		
		self.confPath = "."
		Publisher().subscribe(self.psConfigPathChanged, "config.path.changed")
		
		
	def psConfigPathChanged(self, message):
		self.confPath = message.data
		
	def getPathAbs(self, path):
		absp = os.path.abspath(self.confPath.replace("/", os.sep) + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		absp2 = os.path.abspath(absp + os.sep + path.replace("/", os.sep))
		return absp2
		
	def setGuiValues(self):
		headerDirs = self.model.getHeaderDirs()
		items = self.model.getHeaderItems()
		self.listCtrl.DeleteAllItems()
		for path in headerDirs:
			self.listCtrl.Append([path.path])
			
#		self.treeCtrl.DeleteChildren(self.guiRoot)
		for item in headerDirs:
			if self.findIndexInTreeCtrl(self.guiRoot, item.path) == None:
				hdroot = self.treeCtrl.AppendItem(self.guiRoot, item.path)
				if hdroot.IsOk():
					self.treeCtrl.SetItemImage(hdroot, self.fldropidx,wx.TreeItemIcon_Normal)
					for fitem in items[item.uuid]:
						self.addFileItemToTreeCtrl(hdroot, fitem)

	def removeHeaderDirFromView(self, rItem):
		headerDirs = self.model.getHeaderDirs()
		item = self.findIndexInTreeCtrl(self.guiRoot, rItem.path)
		if item != None:
			self.treeCtrl.Delete(item)
		
	def addFileItemToTreeCtrl(self, root, fitem):
		path = fitem.path
		if fitem.getClassName() == "HeaderFile":
			path = path + "/" + fitem.name
		
		narr = path.split("/")
		runId = root
		for name in narr:
			newRunId = self.findIndexInTreeCtrl(runId, name)
			if newRunId == None:
				newRunId = self.treeCtrl.AppendItem(runId, name)
				if name == narr[-1]:
					self.treeCtrl.SetPyData(newRunId, fitem)
					if fitem.getClassName() == "HeaderFile":
						if fitem.selected:
							self.treeCtrl.SetItemImage(newRunId, self.fileidx,wx.TreeItemIcon_Normal)
						else:
							self.treeCtrl.SetItemImage(newRunId, self.notselidx,wx.TreeItemIcon_Normal)
					else:
						self.treeCtrl.SetItemImage(newRunId, self.fldridx,wx.TreeItemIcon_Normal)
						
				else:
					print "does this happen" # does not happen, just, to be sure
					self.treeCtrl.SetItemImage(newRunId, self.fldridx,wx.TreeItemIcon_Normal)
					
			runId = newRunId
			
	def findIndexInTreeCtrl(self, runId, name):
		subItem = self.treeCtrl.GetFirstChild(runId)[0]
		while subItem.IsOk():
			if self.treeCtrl.GetItemText(subItem) == name:
				return subItem
			subItem = self.treeCtrl.GetNextSibling(subItem)
		return None
		
		
					
					
	def changeSelection(self, itemId):
		myItem = self.treeCtrl.GetPyData(itemId)
		if myItem.getClassName() == "HeaderFile":
		
			selected = not myItem.selected
			myItem.selected = selected
			Publisher.sendMessage("status.change.headerfile", None)
			
			if selected:
				self.treeCtrl.SetItemImage(itemId, self.fileidx,wx.TreeItemIcon_Normal)
			else:
				self.treeCtrl.SetItemImage(itemId, self.notselidx,wx.TreeItemIcon_Normal)
				
			

