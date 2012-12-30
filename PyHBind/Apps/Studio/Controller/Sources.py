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
import PyHBind.Apps.Studio.View



class SourcesController:
	

	def __init__(self, model, view, xmlData, configController):
		self.model = model
		self.view = view
		self.xmlData = xmlData
		self.configController = configController
		
		Publisher.subscribe(self.psXmlDataChanged, "status.change.xmldatachanged")
		Publisher.subscribe(self.addSourceDialog, "sources.adddialog")

	def addSourceDialog(self, message):
		opath = self.configController.getPathAbs(self.model.getBase().buildDir)
		npath = PyHBind.Apps.Studio.View.DialogUtils(self.view).selectDirectory(opath, False, "select source directory")
		if npath != None:
			rnpath = self.configController.getPathRel(npath)
			self.model.appendSourceDir(rnpath)
			self.view.addSourceDir(rnpath)
		
	def psXmlDataChanged(self, message):
		self.reloadGui()
	
	def reloadGui(self):
		nlist = self.model.getSourceDirs()
		self.view.reloadSourcesGui(nlist)
