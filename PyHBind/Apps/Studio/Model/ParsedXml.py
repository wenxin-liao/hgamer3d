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
import PyHBind.Base, PyHBind.Apps.DoxyParser, PyHBind.Components
import os, os.path


class ParsedXmlModel(PyHBind.Components.MultiParseTreeStore):


	def __init__(self, model):
		self.model = model
		self.data = None
		self.config = model.getConfig()
		self.confPath = "."
		Publisher.subscribe(self.psXmlChanged, "status.change.xmlchanged")
		Publisher().subscribe(self.psConfigPathChanged, "config.path.changed")
		
	def psConfigPathChanged(self, message):
		self.confPath = message.data

	def getPathAbs(self, path):
		absp = os.path.abspath(self.confPath.replace("/", os.sep) + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		absp2 = os.path.abspath(absp + os.sep + path.replace("/", os.sep))
		return absp2
		
	def psXmlChanged(self, message):
		self.reloadData()


	# to be changed
	def reloadData(self):
		fileArray = []
		ds = self.model.getHeaderDirs() + self.model.getSourceDirs()
		for header in ds:
			path = self.getPathAbs(self.model.getBase().buildDir + "/ParsedHeaders/" + header.uuid + "/parsed-xml-info.cfg")
			uuid = header.uuid
			fileArray.append( (path, uuid) )
		self.loadData(fileArray)
		Publisher.sendMessage("status.change.xmldatachanged", None)


		
