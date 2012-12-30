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
import PyHBind.Components

class WorkflowController:
	
	# some of this is obsolete, but parseheaders below is in use!
	
	def __init__(self, model, view):
		self.model = model
		self.view = view

		self.confPath = "."
		Publisher().subscribe(self.psConfigPathChanged, "config.path.changed")
		Publisher().subscribe(self.psStatusChange, "status.change")
		
	def psConfigPathChanged(self, message):
		self.confPath = message.data

	def psStatusChange(self, message):
		which = message.topic[2]
		if which == "reload":
			# new values set by new config
			pass
			
		elif which == "builddir":
			# meaning, there is no parsed files, since new directory
			self.model.getStatus().xmlUpToDate = False
			pass

		elif which == "headerfile":
			# headerfile selection changed
			self.model.getStatus().xmlUpToDate = False
			pass
			
		elif which == "xmlchanged":
			# headers parsed to xml just done
			self.model.getStatus().xmlUpToDate = True
			pass
			

	def getPathAbs(self, path):
		absp = os.path.abspath(self.confPath.replace("/", os.sep) + os.sep + self.model.getBase().baseDir.replace("/", os.sep))
		absp2 = os.path.abspath(absp + os.sep + path.replace("/", os.sep))
		return absp2
		
	def parseHeaderFiles(self):
		# parse header files and parse xml
		print "start parsing header files..."
		ctx = {}
		ctx["modulename"] = self.model.getModule().moduleTitle
		ctx["config"] = self.model.getConfig()
		PyHBind.Components.RunDoxygenTask().doIt(ctx)
		PyHBind.Components.CreateParseTreeTask().doIt(ctx)
		Publisher.sendMessage("status.change.xmlchanged", None)
		print "parsing header files finalized!"
