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

# Base/Marshaller/Struct.py

import os, re, sys
import PyHBind.Base


class StructMarshalledType(PyHBind.Base.MarshalledType):

	""" 
	This type is a value type, that means those structs are handled as values.
	Therefore outside of C/C++ those types are handled as values, which
	are denoting value semantics, not pointer semantics.
	
	(In case of SmartPtr, this is still value semantics, with pointer as value)
	
	return value (out):
		
		no flags - simple copy constructor	
		const pointer, pointer - dereference it and copy value in return
		
	normal parameter (in):
	
		no flags - in value, copy it
		const pointer - in value, copy it
		const reference - in value, copy it
	
	normal parameter (out):
		pointer - out value - copy to
		reference - out value - copy to

	- double pointers are useless/have no meaning
	  
	"""
	
	
	def areFlagsValid(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags

		if fDoublePointer:
			return False
			
		return True;	
		
	
	def getOutFlag(self, flags, fReturn,  basetype):

		(fConst, fReference, fPointer, fDoublePointer) = flags

		if fReturn:
			return True
			
		if fConst or (not (fPointer or fReference)):
			return False
			
		return True
			


	def ctype(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		C type declaration, used in C-function declaration
		"""
		text = "struct " + self.config.structname + " *"
		return text
		
	def cpptype(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		CPP type declaration, used in variable declaration for cpp variable
		"""
		text = basetype
		if fPointer:
			text = text + "*"
			if fConst:						# only if pointer, is const ok
				text = "const " + text
		return text
			
	def cppcall(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		modifier before the variable name, for the invocation of the function,
		depends on the difference between the cpp function parameter type and the variable
		type. Can be "&" or "".
		"""
		return "" 

	def fromCtoCpp(self, cname, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		""" 
		returns transformation from c to cpp
		can be a simple cast or some more complex invocations
		"""
		if fPointer:
			text = "((" + basetype + "*) " + cname + ")"
		else:
			text = "*((" + basetype + "*) " + cname + ")"
		return text
			
	def fromCpptoC(self, cppname, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		returns transformation from cpp to c
		can be a simple cast or some more complex invocations
		"""
		text = "*((struct " + self.config.structname + "*) &" + cppname + ")"
		return text
		
	def chsFuncDefinition(self, flags, fReturn,  basetype):
		if self.getOutFlag(flags, fReturn,  basetype):
			return "alloca- `" + self.config.htype + "' peek" + self.config.htype + "*"	
		else:
			return "with" + self.config.htype + "* `" + self.config.htype + "' "
		
