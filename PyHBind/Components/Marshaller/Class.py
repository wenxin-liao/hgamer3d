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

# Base/Marshaller/Class.py

import os, re, sys
import PyHBind.Base


class ClassMarshalledType(PyHBind.Base.MarshalledType):

	
	""" 
	This type is the types of pointer wrappers to full classes. All handling 
	is done with pointer semantics. In Haskell the classes are represented by
	a structure, which holds two pointers. 
	
	As C/C++ types pointers, doublepointers are valid.
	
	return value (out):
		pointer - out value
		reference - out value
		
	normal parameter (in):
		pointer - in value
		const pointer - in value
		const ref - in value
		ref - in value
	
	normal parameter (out):
		double pointer - out value
		
	"""
	
	
	def areFlagsValid(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		
		if fReturn:
			if fPointer or fReference:
				return True
			else:
				return False
				
		if fDoublePointer and fConst:
			return False
		else:
			return True
			
		

	def getOutFlag(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		
		if fReturn:
			return True
		else:
			if fDoublePointer:	
				return True
			else:
				return False	
			

	def ctype(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		C type declaration, used in C-function declaration
		"""
		text = "struct hg3dclass_struct *"
		return text
		
		
	def fcallmod(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		if fReturn and fReference:
			return "&"
		else:
			return ""
		
		
		
	def cpptype(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		CPP type declaration, used in variable declaration for cpp variable
		"""
		text = basetype + " *"
		if fConst:
			text = "const " + text
		return text
			
	def cppcall(self, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		modifier before the variable name, for the invocation of the function,
		depends on the difference between the cpp function parameter type and the variable
		type. Can be "&" or "".
		"""
		if fDoublePointer:
			return "&"
		elif fReference:
			return "*"
		else:
			return ""
			
	def fromCtoCpp(self, cname, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		""" 
		returns transformation from c to cpp
		can be a simple cast or some more complex invocations
		"""
		text = "static_cast<" + basetype + "*> "
		text = text + "(getHG3DClassPtr(*" + cname + ", \"" + basetype + "\"))"
#		if fReference:
#			text = "*(" + text + ")"
		return text
		
	def fromCpptoC(self, cppname, flags, fReturn, basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		returns transformation from cpp to c
		can be a simple cast or some more complex invocations
		"""
		baseonly = basetype.split("::")[-1]
		text = "getHG3DClass_" + baseonly + "((void *) " + cppname + ");\n"
		return text		
		
	def chsFuncDefinition(self, flags, fReturn, basetype):
		hname = "HG3DClass"
		if self.getOutFlag(flags, fReturn, basetype):
			return "alloca-" + " `" + hname + "' " + "peek*"	
		else:
			return "with" + hname + "*" + " `" + hname + "' "
		
