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

import sys
import wx

import Model, View, Controller
import PyHBind.Components

fname = None

class StudioApp(wx.App):

	def OnInit(self):

		# the model
		self.model = PyHBind.Components.ConfigStore()
		# the view
		self.view = View.MainFrameView(self.model)
		# the controller
		self.controller = Controller.MainController(self.model, self.view)
		# start
		self.view.Show()
		self.SetTopWindow(self.view)
		self.controller.startAll(fname)
		return True


def setFileName(name):
	global fname
	fname = name
		
