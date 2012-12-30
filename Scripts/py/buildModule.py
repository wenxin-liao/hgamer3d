"""
This source file is part of HGamer3D
(A project to enable 3D game development in Haskell)
For the latest info, see http://www.althainz.de/HGamer3D.html

(c) 2011 Peter Althainz

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

# buildModule.py

#
# creates Source files
#

import sys, os
sys.path.append(".")
import PyHBind.Components


# module types
cabalModules = ["API", "Data"]
toolModules = ["Installer", "Dependencies", "Examples"]
bindingModules = ["SFML", "Ogre", "CEGUI", "Bullet", "Enet"]


def doBindingModule(module):
	ctx = {}
	ctx["modulename"] = module

	# createSource
	PyHBind.Components.SetParametersFromModuleTask().doIt(ctx)
	PyHBind.Components.RegisterTypesTask().doIt(ctx)
	PyHBind.Components.WriteSourceFilesTask().doIt(ctx)

	# buildFromSource
	PyHBind.Components.SetParametersFromModuleTask().doIt(ctx)
	PyHBind.Components.CompileDllTask().doIt(ctx)
	PyHBind.Components.CompileCabalTask().doIt(ctx)


def doCabalModule(module):
	ctx = {}
	ctx["modulename"] = module

	# directly build the module, per runhaskell
	PyHBind.Components.BuildCabalModuleTask().doIt(ctx)


def doToolModule(module):
	
	ctx = {}
	# go back to oldstyle do script
	if module == "Installer":
		PyHBind.Components.BuildInstallerTask().doIt(ctx)
	elif module == "Distributable":
		PyHBind.Components.BuildDistributableTask().doIt(ctx)
	elif module == "Dependencies":
		PyHBind.Components.BuildDependenciesTask().doIt(ctx)
	elif module == "Examples":
		PyHBind.Components.BuildExamplesTask().doIt(ctx)


if len(sys.argv) > 1:
	module = sys.argv[1]
	
	if module in bindingModules:
		doBindingModule(module)
		sys.exit(0)
		
	if module in cabalModules:
		doCabalModule(module)
		sys.exit(0)

	if module in toolModules:
		doToolModule(module)
		sys.exit(0)
		
print "usage: Scripts/build <module>"
print "  modules:"
for m in cabalModules + bindingModules + toolModules:
	print "   " + m
		
	
