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

import Sources


class MainFrameView(Sources.SourcesView):

	
	# the purpose of this class, is to gather all methods from the chain
	# of view classes, this solely has one purpose, to split the source
	# files into multiple ones!

	def rescanDirsHandler(self, event):
		Publisher.sendMessage("header.rescandirs", None)
		
	def parseHeadersHandler(self, event):
		Publisher.sendMessage("header.parsefiles", None)
		
