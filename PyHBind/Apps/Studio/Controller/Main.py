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

import PyHBind.Apps.Studio.Model, PyHBind.Apps.Studio.View, PyHBind.Apps.Studio.Controller

from wx.lib.pubsub import Publisher
import os, os.path



class MainController:
	
	def __init__(self, model, view):
		self.model = model
		self.view = view
		
		self.xmlData = PyHBind.Apps.Studio.Model.ParsedXml.ParsedXmlModel(self.model)
		
		self.controllerConfig = PyHBind.Apps.Studio.Controller.ConfigController(self.model, self.view)
		self.controllerBasicTypes = PyHBind.Apps.Studio.Controller.BasicTypesController(self.model, self.view)
		self.controllerWorkflow = PyHBind.Apps.Studio.Controller.WorkflowController(self.model, self.view)

		self.controllerStructs = PyHBind.Apps.Studio.Controller.StructsController(self.model, self.view, self.xmlData)
		self.controllerEnums = PyHBind.Apps.Studio.Controller.EnumsController(self.model, self.view, self.xmlData)
		self.controllerClasses = PyHBind.Apps.Studio.Controller.ClassesController(self.model, self.view, self.xmlData)
		self.controllerFunctions = PyHBind.Apps.Studio.Controller.FunctionsController(self.model, self.view, self.xmlData)
		self.controllerTypedefs = PyHBind.Apps.Studio.Controller.TypedefsController(self.model, self.view, self.xmlData)
		self.controllerSources = PyHBind.Apps.Studio.Controller.SourcesController(self.model, self.view, self.xmlData, self.controllerConfig)
		

		Publisher().subscribe(self.psMenuFile, "menu.file")
		Publisher().subscribe(self.psParseHeaderFiles, "header.parsefiles")

	def psMenuFile(self, message):
		topic = message.topic[2]
		
		if topic == "openproject":
			self.controllerConfig.openProject()
			self.xmlData.reloadData()
				
		elif topic == "newproject":
			self.controllerConfig.newProject()
			self.xmlData.reloadData()
			
		elif topic == "saveproject":
			self.controllerConfig.saveProject()
			
		elif topic == "saveprojectas":
			self.controllerConfig.saveProjectAs()
			
		elif topic == "exit":
			self.view.Close(True)
			
		
	def psParseHeaderFiles(self, message):
			self.controllerWorkflow.parseHeaderFiles()
			
	def startAll(self, filename):
		
#		self.headers = HeaderSelection(self.config)
#		self.headers.initGui(self.treeCtrlHeaderFiles)
#		self.setGuiValues()
		fopened = False
		if filename != None:
			fpath = os.path.abspath(filename)
			if os.path.exists(fpath):
				self.controllerConfig.loadConfig(fpath)
				fopened = True
		if not fopened:
			self.controllerConfig.newConfig()

		self.xmlData.reloadData()
		self.view.setGuiValues()
