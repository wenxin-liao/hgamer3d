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

# RegisterTypesTask.py

import PyHBind.Base
import PyHBind.Components


import os, os.path, sys

def getNamesFromQualifiedName(name):
	return [name]
	
	arrstr = name.split("::")
	arrstr.reverse()
	names = []
	name = ""
	for chunk in arrstr:
		if len(name) > 0:
			name = "::" + name
		name = chunk + name
		names.append(name)
	return names
		
class RegisterTypesTask(PyHBind.Base.Task):
	
	def doIt(self, ctx):
		types = PyHBind.Base.Types()
		
		# get module name, config, parsetree
		modname = ctx["modulename"]
		config = ctx["config"]
		parsetree = ctx["parsetree"]
		
		# register basic types
		for basictype in config.getBasicTypes():
			btNames = []
			for mt in config.getMappedCTypes(basictype.mappedCTypes):
				btNames.append(mt.type)
			if basictype.flagString:
				types.registerTypeWithNames(PyHBind.Components.Marshaller.StringMarshalledType(None, basictype), btNames)
			else:
				types.registerTypeWithNames(PyHBind.Components.Marshaller.SimpleMarshalledType(None, basictype), btNames)
		
		# register enum types
		for enum in parsetree.getEnumItemsList():
			enumconfig = config.getEnumConfiguration(enum.id)
			if enumconfig.selected:
#				print enum.name
				types.registerTypeWithNames(PyHBind.Components.Marshaller.EnumMarshalledType(enum, enumconfig), getNamesFromQualifiedName(enum.name))

		# register struct types
		for struct in parsetree.getStructItemsList():
			structconfig = config.getStructConfiguration(struct.id)
			if structconfig.selected:
				# name from struct itself
				names = getNamesFromQualifiedName(struct.name)
				# names from ctypes in config
				ctypenames = structconfig.cTypes.split(",")
				for name in ctypenames:
					if len(name) > 0:
						names.append(name)
				# register all names
				types.registerTypeWithNames(PyHBind.Components.Marshaller.StructMarshalledType(struct, structconfig), names)
				
		# register class types
		for cl in parsetree.getClassItemsList():
			classconfig = config.getClassConfiguration(cl.id)
			if classconfig.selected:
#				print cl.name
				types.registerTypeWithNames(PyHBind.Components.Marshaller.ClassMarshalledType(cl, classconfig), getNamesFromQualifiedName(cl.name))
				
				
