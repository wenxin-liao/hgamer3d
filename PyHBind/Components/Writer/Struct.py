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

# Writer.Struct


# creates h, chs, cpp files for struct structures

import os
import PyHBind.Base


def writeStructFiles(modulename, config, struct, attributes, methods, structconfig):
	
	structName = "Struct" + structconfig.htype
	writer = PyHBind.Base.FileWriter(modulename)
	
	# header file
	#

	writer.clearText()
	writer.addText(writer.commentText(writer.templateText("sourcefile"), "//"))
	writer.addText(writer.templateText("header"))
	writer.replaceText(
		[
			("license", writer.commentText(writer.templateText("license"), "//")),
			("thisfile", structName + ".h"),
			("otherincludes", ""),
			("comment", writer.commentText(structconfig.comment, "//")),
		] )
	writer.addText(structconfig.hFile)
	writer.write("include/" + structName + ".h")
	
	# chs file
	#

	writer.clearText()
	writer.addText(writer.templateText("chs"))
	writer.replaceText(
		[
			("sourcefile", writer.commentText(writer.templateText("sourcefile"), "--")),
			("license", writer.commentText(writer.templateText("license"), "--")),
			("thisfile", structName + ".chs"),
			("module", modulename),
			("modulename", "HGamer3D." + config.getModule().subModule + "." + structName),
			("includefile", structName + ".h"),
			("otherincludes", ""),
			("comment", writer.commentText(structconfig.comment, "--")),
		] )
		
	writer.addText("\n" + structconfig.chsFile)

	text = "\n"
#	text = text + "{#pointer *" + structconfig.structname + " as " + structconfig.htype + "Ptr -> " + structconfig.htype + " #}\n"
#	text = text + "\nwith" + structconfig.htype + " :: " + structconfig.htype + " -> (" + structconfig.htype + "Ptr -> IO b) -> IO b\n"
#	text = text + "with" + structconfig.htype + " = with\n"
	writer.addText(text)

	fname = "c2hs-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/" + structName
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
