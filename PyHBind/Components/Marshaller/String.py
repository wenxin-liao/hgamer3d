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

# Base/Marshaller/String.py

import os, re, sys
import PyHBind.Base


class StringMarshalledType(PyHBind.Base.MarshalledType):

	
	""" 
	This type is a mixed type, because, we have two different occurences.
	It can be a char* (or wchar_t*) or it can be a String type of some
	sort.
	
	The function isCharType distinguishes between both
	
	Outside of C/C++ it is in all cases a [Char], therefore it makes sense
	to combine both cases in one marshaller.
	
	char, wchar_t type:
	-------------------
	return value (out)
		const pointer - pointer to const string returned -> copy
		pointer - treated as const pointer -> copy
	normal parameter (in)
		const pointer - in pointer -> use
	normal parameter (out)
		pointer - use as out pointer (buffer with strlen)
		
	String type:
	------------
	return value  (out)
		plain value
		reference 
	normal parameter (in)
		plain Value
		const plain Value
		const reference
	normal parameter (out)
		reference
	
  
	"""
	
	def isCharType(self, basetype):
		if basetype == "char" or basetype == "wchar_t":
			return True
		else:
			return False
			
	
	def areFlagsValid(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		
		if self.isCharType(basetype):
			# must be a pointer and no doublepointer
			if fPointer and (not fDoublePointer):
				return True
			else:
				return False
		else:
			if fPointer or fDoublePointer:
				return False
#			if fReturn and fConst:
#				return False
			return True
			
	def getOutFlag(self, flags, fReturn, basetype):
		
		(fConst, fReference, fPointer, fDoublePointer) = flags
		if fReturn:
			return True
			
		if self.isCharType(basetype):
			if fConst:
				return False
			else:
				return True

		else:
			if (not fConst) and fReference:
				return True
			else:
				return False


	def ctype(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		C type declaration, used in C-function declaration
		"""
		return "char *"
		
	def cpptype(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		CPP type declaration, used in variable declaration for cpp variable
		"""
		if self.isCharType(basetype):
			text = basetype
			if fConst:
				text = "const " + text
			if fPointer:
				text = text + " *"

		else:
			text = self.config.stringConstructor
#			if fConst:
#				text = "const " + text
			
		return text
			
	def cppcall(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		modifier before the variable name, for the invocation of the function,
		depends on the difference between the cpp function parameter type and the variable
		type. Can be "&" or "".
		"""
		return ""

	def fromCtoCpp(self, cname, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		""" 
		returns transformation from c to cpp
		can be a simple cast or some more complex invocations
		"""
		if basetype == "char":
			text = "(" + basetype + "*) " + cname
		elif basetype == "wchar_t":
			text = "(" + basetype + "*) " + cname
		else:
			text = self.config.stringConstructor + "((const char*) " + cname + ")"
		return text
		
		
	def fromCpptoC_Line(self, cname, cppname, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		returns transformation from cpp to c
		can be a simple cast or some more complex invocations
		"""
		
		if basetype == "char":
			text = "if (strlen( (" + basetype + "*) " + cppname + ") < (1024 * 64 - 1))  { \n"
			text = text + "strcpy(" + cname + ", (" + basetype + "*) " + cppname + "); } else {\n"
			text = text + "strcpy(" + cname + ", \"error: outstring larger then 64k\");};\n"
			
		elif basetype == "wchar_t":
			text = "if (wcslen( (" + basetype + "*) " + cppname + ") < (512 * 64 - 2))  { \n"
			text = text + "wcscpy(" + cname + ", (" + basetype + "*) " + cppname + "); } else {\n"
			text = text + "wcscpy(" + cname + ", L\"error: outstring larger then 64k\");};\n"
			
		else:
			text = "  if (strlen( (char *) " + cppname + ".c_str()) < (1024 * 64 - 1))  { \n"
			text = text + "    strcpy(" + cname + ", (char *) " + cppname + "." + self.config.asCharPointerMethod + "); \n"
			text = text + "  } else {\n"
			text = text + "    strcpy(" + cname + ", \"error: outstring larger then 64k\");\n"
			text = text + "  };\n"
			
		return text

	def chsFuncDefinition(self, flags, fReturn, basetype):
		if self.getOutFlag(flags, fReturn, basetype):
			return "alloc64k- `" + self.config.htype + "' " + self.config.outMarshaller + "*"	
		else:
			return self.config.inMarshaller + "* `" + self.config.htype + "' "
		
