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

#
# This utility allow easy registering of new versions of 
# HGamer3D packages during development
#

#
# configure here the packages and dependencies
#

# enter short name, long name without version
#

packageList = {
	"Data" : "HGamer3D-Data",
	"Ogre" : "HGamer3D-Ogre-Binding",
	"SFML" : "HGamer3D-SFML-Binding",
	"CEGUI" : "HGamer3D-CEGUI-Binding",
	"Enet" : "HGamer3D-Enet-Binding",
	"Bullet" : "HGamer3D-Bullet-Binding",
	"API" : "HGamer3D"
	}

# enter dependencies, short name
#
	
dependencies = {
	"Data" : [],
	"Ogre" : ["Data"],
	"SFML" : ["Data"],
	"CEGUI" : ["Data"],
	"Enet" : ["Data"],
	"Bullet" : ["Data"],
	"API" : ["Data", "Ogre", "CEGUI", "SFML", "Enet", "Bullet"]
	}

# enter, if package is pure cabal package or binding package
#

pureCabal = ["Data", "API"]
binding = ["Ogre", "SFML", "CEGUI", "Enet", "Bullet"]


# no changes needed below this line

import sys, os
from subprocess import Popen, PIPE


#
# worker functions, start with _
#

def _registerPath(name, path):
	cmd1 = "dir " + path + "/HGamer3D-*.tar.gz"
	fileName = Popen(cmd1, stdout=PIPE).communicate()[0]
	cmd2 = "cabal install --global " + fileName
	os.system(cmd2)
	
def _register(name):
	print "register", name
	if name in pureCabal:
		_registerPath(name,"Module-" + name + "-Build")
	elif name in binding:
		_registerPath(name, "Module-" + name + "-Build/c2hs-build/dist")

def _unregister(name):
	print "unregister", name
	cmd = "ghc-pkg unregister " + packageList[name]
	os.system(cmd)

def _dependsOn(name):
	deps = []
	for d in dependencies[name]:
		if d not in deps:
			deps.append(d)
	if len(deps) > 0:
		for x in deps:
			for a in _dependsOn(x):
				if a not in deps:
					deps.append(a)
	return deps
	
	
def _cmp(namea, nameb):
	# build list of dependendts of 
	if namea in _dependsOn(nameb):
		return -1
	elif nameb in _dependsOn(namea):
		return 1
	else:
		return 0
		
		
def _installed():
	installed = []
	txt = Popen("ghc-pkg list", stdout=PIPE).communicate()[0]
	lines = txt.split("\n")
	for package in packageList.keys():
		val = packageList[package]
		found = False
		for line in lines:
			if line.find(val + "-0") > -1:
				found = True
		if found:
			installed.append(package)
	return sorted(installed, cmp=_cmp)

def _sortedpackages():
	return sorted(packageList.keys(), cmp=_cmp)
	
def _upperlower(pack):
	installed = _installed()
	sortedlist = _sortedpackages()
	# check if package is known
	if pack not in sortedlist:
		print "package", pack, "not known"
		sys.exit(-1)
	# check if we need to remove deps
	index = sortedlist.index(pack)
	us = sortedlist[index+1:]
	upper = []
	for u in us:
		if u in installed:
			if pack in _dependsOn(u):
				upper.append(u)
	# check if we need to install deps
	ls = sortedlist[:index]
	lower = []
	for l in ls:
		if l not in installed:
			if l in _dependsOn(pack):
				lower.append(l)
	return (upper, lower, installed)
	

#
# commands of the script
#
			
	 
# no argument, simply show instaled packages
#
if len(sys.argv) == 1:
	print _installed()

# one argument, re-register this package, do all neccessary steps in between
#	
if len(sys.argv) == 2:
	pack = sys.argv[1]
	(upper, lower, installed) = _upperlower(pack)
	if len(upper) > 0:
		for p in reversed(upper):
			_unregister(p)
	# unregister this package
	if pack in installed:
		_unregister(pack)
	# install lowers
	for l in lower:
		_register(l)
	# register this package
	_register(pack)
	# register upper packages
	for p in upper:
		_register(p)
	
# two arguments, command "deps", show dependencies
#	
if len(sys.argv) == 3 and sys.argv[1] == "deps":
	print sorted(_dependsOn(sys.argv[2]), cmp=_cmp)

# two arguments, command "down", unregister all packages including this one
#
if len(sys.argv) == 3 and sys.argv[1] == "down":
	pack = sys.argv[2]
	(upper, lower, installed) = _upperlower(pack)
	if len(upper) > 0:
		for p in reversed(upper):
			_unregister(p)
	# unregister this package
	if pack in installed:
		_unregister(pack)

# two arguments, command "up", register all packages until this one
#
if len(sys.argv) == 3 and sys.argv[1] == "up":
	pack = sys.argv[2]
	(upper, lower, installed) = _upperlower(pack)
	# install lowers
	for l in lower:
		_register(l)
	if pack not in installed:
		_register(pack)
