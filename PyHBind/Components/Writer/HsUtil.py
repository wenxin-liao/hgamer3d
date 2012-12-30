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

# Writer.HsUtil


# creates hs file for utilities source file

import os
import PyHBind.Base
import PyHBind.Components


# anonymous class
class Ano:
	pass


def writeHsUtilFile(modulename, config, parsetree):

	marshaller = PyHBind.Components.Marshaller.Marshaller(modulename, config)
	types = PyHBind.Base.Types()
	writer = PyHBind.Base.FileWriter(modulename)

	
	# chs file writing
	#
	
	writer.clearText()
	writer.addText(writer.templateText("chs"))
	
	# replacements
	inclfiletext = "StructHG3DClass.h"
	depstext = ""
	haskellmodule = "HGamer3D.Bindings." + modulename + ".Utils"
	commentText = "Marshalling Utilities"
	
	text = """
import Control.Monad (liftM)

-- Strings with explicit length
--
withCStringLenIntConv :: Num n => String -> ((CString, n) -> IO a) -> IO a
withCStringLenIntConv s f    = withCStringLen s $ \(p, n) -> f (p, fromIntegral n)

peekCStringLenIntConv :: Integral n => (CString, n) -> IO String
peekCStringLenIntConv (s, n) = peekCStringLen (s, fromIntegral n)

-- Marshalling of numerals
--

withIntConv   :: (Storable b, Integral a, Integral b) 
              => a -> (Ptr b -> IO c) -> IO c
withIntConv    = with . fromIntegral

withFloatConv :: (Storable b, RealFloat a, RealFloat b) 
              => a -> (Ptr b -> IO c) -> IO c
withFloatConv  = with . realToFrac

peekIntConv   :: (Storable a, Integral a, Integral b) 
              => Ptr a -> IO b
peekIntConv    = liftM fromIntegral . peek

peekFloatConv :: (Storable a, RealFloat a, RealFloat b) 
              => Ptr a -> IO b
peekFloatConv  = liftM realToFrac . peek


-- String Conversion functions
--

withCUString b f = withCWString b (f . castPtr)
peekCUString = peekCWString . castPtr
alloc64k = allocaBytes (1024 * 64)

-- c2hs replacements of utility functions, to get rid of annoying c2hs
-- deprecated messages

-- Passing Enums

cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = cIntConv . fromEnum

cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . cIntConv


-- Passing Booleans by reference
--

withBoolUtil :: (Integral a, Storable a) => Bool -> (Ptr a -> IO b) -> IO b
withBoolUtil  = with . fromBool

peekBoolUtil :: (Integral a, Storable a) => Ptr a -> IO Bool
peekBoolUtil  = liftM toBool . peek


-- Passing enums by reference
--

withEnumUtil :: (Enum a, Integral b, Storable b) => a -> (Ptr b -> IO c) -> IO c
withEnumUtil  = with . cFromEnum

peekEnumUtil :: (Enum a, Integral b, Storable b) => Ptr b -> IO a
peekEnumUtil  = liftM cToEnum . peek

-- Conversion routines
-- -------------------

-- |Integral conversion
--
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

-- |Floating conversion
--
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac

-- |Convert a C enumeration to Haskell.
--
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . fromIntegral

-- |Convert a Haskell enumeration to C.
--
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

"""
	writer.addText(text)
	
	writer.replaceText(
		[
			("sourcefile", writer.commentText(writer.templateText("sourcefile"), "--")),
			("license", writer.commentText(writer.templateText("license"), "--")),
			("thisfile", "Utils.hs"),
			("includefile", inclfiletext),
			("otherincludes", depstext),
			("module", modulename),
			("modulename", haskellmodule),
			("comment", writer.commentText(commentText, "--")),
		] )
		

	fname = "c2hs-build/HGamer3D/" + config.getModule().subModule.replace(".", "/") + "/Utils"
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
