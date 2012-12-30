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

# Writer.ClassPtr


# creates h, chs, cpp files for class ptr source file

import os
import PyHBind.Base
import PyHBind.Components


# anonymous class
class Ano:
	pass


def writeClassPtrFiles(modulename, config, parsetree, classlist):

	marshaller = PyHBind.Components.Marshaller.Marshaller(modulename, config)
	types = PyHBind.Base.Types()
	writer = PyHBind.Base.FileWriter(modulename)

	# header file writing
	#
	
	depstext = ""
	
	
	commentText = """Here are the methods defined, which do the class pointer
marshalling and the casting of subclasses to higher classes"""
	
	utilText = """#ifndef CLASSPTR_INCLUDE_H
#define CLASSPTR_INCLUDE_H

typedef struct hg3dclass_struct {
	void *ptr;
	void *fptr;
} hg3dclass_struct;

void *getHG3DClassPtr(hg3dclass_struct inSt, const char* className);

"""

	endText = """#endif
"""

	writer.clearText()

	writer.addText(writer.commentText(writer.templateText("sourcefile"), "//"))
	writer.addText(writer.templateText("header"))
	writer.addText(utilText)
	
	writer.replaceText(
		[
			("classid", modulename),
			("license", writer.commentText(writer.templateText("license"), "//")),
			("thisfile", "ClassPtr.h"),
			("otherincludes", depstext),
			("comment", writer.commentText(commentText, "//")),
		] )
		
	text = "\n"
	
	for (cc, cl, a, m) in classlist:
			classid = cc.htype
			clang = cl.name.split("::")[-1]
			text = text + "typedef void Class" + clang + "; \n"
			text = text + "hg3dclass_struct getHG3DClass_" + clang + "(void *ptrIn);\n\n"

	writer.addText(text)
	writer.addText(endText)
	writer.write("include/ClassPtr.h")
	
	# cpp file writing
	#

	depstext = """#include <iostream>
#include <typeinfo>
#include <stdio.h>
#include <exception>
#include <<modulename>DllDefines.h>
"""
	for item in config.getIncludedHeaders():
		incfile = (item.path + "/" + item.name)
		depstext = depstext + "#include \"" + incfile + "\"\n"
		
	# source header files
	for sh in config.getSourceHeaderFiles()[0]:
		depstext = depstext + "#include \"" + sh + "\"\n"
			
	utilText = """typedef struct hg3dclass_struct {
	void *ptr;
	void *fptr;
} hg3dclass_struct;

void *getHG3DClassPtr(hg3dclass_struct inSt, const char* className)
{
	void *(*ptrcaster)(const char*, void*);
	ptrcaster = (void *(*)(const char*, void*))(inSt.fptr);
	return ((*ptrcaster)(className, inSt.ptr));
}

"""

	writer.clearText()
	writer.addText(writer.commentText(writer.templateText("sourcefile"), "//"))
	writer.addText(writer.templateText("cpp"))
	writer.addText(utilText)
	
	writer.replaceText(
		[
			("modulesmall", modulename.lower()),
			("license", writer.commentText(writer.templateText("license"), "//")),
			("thisfile", "ClassPtr.cpp"),
			("otherincludes", depstext),
			("modulename", modulename),
			("comment", writer.commentText(commentText, "//")),
		] )
		
	text = "\n"
	
	crlist = parsetree.getClassRefList()
	
	for (cc, cl, a, m) in classlist:
		classid = cc.htype
		clangs = cl.name.split("::")[-1] 
		clang = cl.name
		
		# comment
		text = "//\n// " + clang + "\n//\n\n"
		
		# ptr caster
		text = text + "// Ptr Caster\n"
		text = text + "void *internalHG3D_" + clangs + "_PtrCaster(const char* className, void* ptrIn) {\n"
		text = text + "\t" + "if (strcmp(className, \"" + clang + "\") == 0) {\n"
		text = text + "\t\t" + "return ptrIn;\n"
		text = text + "\t};\n"
		
		# first, build local tree of parent relationships
		allParents = []
		lenAP = 0
	
		for critem in crlist:
			if critem.child == cl.id:
				allParents.append(critem.parent)
					
		while lenAP < len(allParents):
			lenAP = len(allParents)
			newParents = []
			for p in allParents:
				for critem in crlist:
					if (critem.child == p) and (critem.parent not in allParents) and (critem.parent not in newParents):
						newParents.append(critem.parent)
			allParents = allParents + newParents
			
		for parent in allParents:
			cItem = config.getClassConfiguration(parent)
			(pItem, a, m) = parsetree.getClassById(parent)
			text = text + "\t" + "if (strcmp(className, \"" + pItem.name + "\") == 0) {\n"
			text = text + "\t\t" + "return (void *)(" + pItem.name + " *)(" + clang + " *)ptrIn;\n"
			text = text + "\t};\n"
			
		text = text + "\tprintf(\"PtrCaster not successful, Class: " + clang + " is not a subclass of %s!\\n\",className);\n"
		text = text + "\treturn (void *)0;\n"
		text = text + "};\n\n"

		writer.addText(text)
	
		# function, to return ptr
		text = "// getHG3DClass\n"
		text = text + "hg3dclass_struct getHG3DClass_" + clangs + "(void *ptrIn)\n"
		text = text + "{\n"
		text = text + "\t" + "hg3dclass_struct st;\n"
		text = text + "\tst.ptr = ptrIn;\n"
		text = text + "\tst.fptr = (void *)(&internalHG3D_" + clangs + "_PtrCaster);\n"
		text = text + "\treturn st;\n"
		text = text + "};\n\n"
		
		writer.addText(text)
	
	writer.write("dll-build/ClassPtr.cpp")
	
	# chs file writing
	#
	
	writer.clearText()
	writer.addText(writer.templateText("chs"))
	
	# replacements
	inclfiletext = "ClassPtr.h"
	depstext = "{# import HGamer3D.Bindings.<module>.Utils #}\n"
#	depstext = ""
	haskellmodule = "HGamer3D.Bindings." + modulename + ".ClassPtr"
	commentText = "Class Ptr Utilities"
	
	writer.replaceText(
		[
			("sourcefile", writer.commentText(writer.templateText("sourcefile"), "--")),
			("license", writer.commentText(writer.templateText("license"), "--")),
			("thisfile", "ClassPtr.chs"),
			("includefile", inclfiletext),
			("otherincludes", depstext),
			("module", modulename),
			("modulename", haskellmodule),
			("comment", writer.commentText(commentText, "--")),
		] )
		

	for (cc, cl, a, m) in classlist:
		classid = cc.htype
		clang = cl.name.split("::")[-1]
		text = "{- class Class" + classid + " -}\n" + "{#pointer *Class" + clang + " as Class" + classid + "#}\n"
		writer.addText(text)
		
	fname = "c2hs-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/ClassPtr"
	writer.write(fname + ".chs")
	
	return
	
	# compile chs file
	cmd = "c2hs " + writer.getpath(fname + ".chs")
	os.system(cmd)
	
	# build comments into hs file
	#
	
	fin = writer.openread(fname + ".hs")
	fout = writer.openwrite(fname + ".newhs")
	found = False
	i = 0
	line = fin.readline()
	while line:
		outline = line
		fout.write(outline)
		line = fin.readline()
	fout.close()	
	fin.close()
	
	# delete all but hs file
	#
	
	for todel in [
		fname + ".hs",
		fname + ".chs",
		fname + ".chs.h",
		fname + ".chi",
	]:
		writer.delfile(todel)
	writer.movefile(fname + ".newhs", fname + ".hs")
