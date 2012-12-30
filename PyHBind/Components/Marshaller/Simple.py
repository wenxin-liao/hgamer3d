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

# Base/Marshaller/Simple.py

import os, re, sys
import PyHBind.Base


class SimpleMarshalledType(PyHBind.Base.MarshalledType):


	""" 
	A Simple C-Type (bool, int, double, float, ...), which can come
	in as a plain, pointer or reference value.
	
	(what about arrays?)
	
	The Haskell value is the plain type and pointer and refernce gives
	a reason to treat it as out parameter.
	
	
	return value (out)
		plain
		pointer -> this denotes an array which is given back, not handled currently
	normal parameter (in)
		plain
		const reference
		const pointer
	normal parameter (out)
		pointer 
		reference
		
	"""
	
	def areFlagsValid(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		
		if fDoublePointer:
			return False
			
		if fReturn:
			if fPointer or fDoublePointer or fReference or fConst:
				return False
				
		return True
				
	def getOutFlag(self, flags, fReturn, basetype):
		
		(fConst, fReference, fPointer, fDoublePointer) = flags
		
		if fReturn:
			return True
			
		if fPointer or fReference:
			if fConst:
				return False
			else:
				return True
		else:
			return False

	def ctype(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		C type declaration, used in C-function declaration
		"""
		text = self.config.ctype
		
		if self.getOutFlag(flags, fReturn, basetype):
			text = text + " *"
			
		if fConst:
			text = "const " + text
		return text
		
	def cpptype(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		CPP type declaration, used in variable declaration for cpp variable
		"""
		text = basetype
		if fConst:
			text = "const " + text
		if fPointer and not self.getOutFlag(flags, fReturn, basetype):
			text = text + " *"
		if fReference and not self.getOutFlag(flags, fReturn, basetype):
			text = text + " &"
		return text
			
	def cppcall(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		modifier before the variable name, for the invocation of the function,
		depends on the difference between the cpp function parameter type and the variable
		type. Can be "&" or "".
		"""
		if fPointer:
			return "&"
		else:
			return "" 

	def fromCtoCpp(self, cname, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		""" 
		returns transformation from c to cpp
		can be a simple cast or some more complex invocations
		"""
		text = "(" + basetype + ")"
		if fPointer:
			text = text + "(*" + cname + ")"
		else:
			text = text + cname
		return text
		
	def fromCpptoC(self, cppname, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		returns transformation from cpp to c
		can be a simple cast or some more complex invocations
		"""
		if fConst:
			text = "(const " + self.config.ctype + ")" + cppname
		else:
			text = "(" + self.config.ctype + ")" + cppname
		return text		
		
	def chsFuncDefinition(self, flags, fReturn, basetype):
		if self.getOutFlag(flags, fReturn, basetype):
			return "alloca- `" + self.config.htype + "' " + self.config.outMarshaller + "*"	
		else:
			return self.config.inMarshaller + " `" + self.config.htype + "' "
		
