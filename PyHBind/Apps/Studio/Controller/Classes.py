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


class ClassesController:
	

	def __init__(self, model, view, xmlData):
		self.model = model
		self.view = view
		self.xmlData = xmlData
		
		Publisher.subscribe(self.psXmlDataChanged, "status.change.xmldatachanged")
		Publisher.subscribe(self.itemSelected, "config.class.selected")
		Publisher.subscribe(self.methodSelected, "config.classmethod.selected")
		Publisher.subscribe(self.methodExcluded, "config.classmethod.excluded")

	def itemSelected(self, message):
		(ind, name) = message.data
		(item, attributes, methods) = self.xmlData.getClassByIndex(ind)
		# get configuration
		
		conf = self.model.getClassConfiguration(item.id)
		parents = []
		childs = []
		for ref in self.xmlData.getClassRefList():
			# parent found
			if ref.child == item.id:
				parid = ref.parent
				# get item name
				item = self.xmlData.getUnknownItemById(parid)
				parname = item.name
				parents.append(parname)
			# child found
			if ref.parent == item.id:
				childid = ref.child
				# get child name
				item = self.xmlData.getUnknownItemById(childid)
				childname = item.name
				childs.append(childname)
			
		self.view.setClassConfigData(item, attributes, methods, conf, parents, childs)

	def methodSelected(self, message):
		(m, p) = message.data
		
		# basic method info
		infostr = "Type: " + m.type + "\n"
		infostr = infostr + "Brief: " + m.brief + "\n"
		infostr = infostr + "Detail: " + m.detail +"\n"
		infostr = infostr + "Protection: " + m.protection + "\n"
		infostr = infostr + "Virtual: " + str(m.virtual) + "\n"
		infostr = infostr + "Static: " + str(m.static) + "\n"
		infostr = infostr + "\n"
		
		# add parameter info
		infostr = infostr + "parameters:\n"
		for par in p:
			infostr = infostr + "  " + par.name + " (" + par.type + ") | " + par.description + "\n"
		
		self.view.displayClassMethodInfo(infostr, m.name)
		excludedFlag = self.model.isMethodExcluded(m.id)
		self.view.classMethodSetExcludedFlag(excludedFlag)
		
	def methodExcluded(self, message):
		(methodId, excludedFlag) = message.data
		self.model.setMethodExcluded(methodId, excludedFlag)

	def psXmlDataChanged(self, message):
		self.reloadGui()
	
	def reloadGui(self):
		nlist = self.xmlData.getClassesList()
		self.view.reloadClassesGui(nlist)
