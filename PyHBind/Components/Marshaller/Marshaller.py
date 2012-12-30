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

# Base/Marshaller/Basic.py

# haskell function name from c function name traversal
#

class Marshaller:
	"""
	provides the basic functionality and default implementations for a 
	marshaller. A good deal of the functions are overwritten by the specific
	marshallers, which are subclassed from this one. The information about the
	type modification (pointer, const, ref, ...) is kept in the base class
	and used by the subclasses, to distinguish between different cases.
	
	The subclasses needs to implement all functions for the allowed combinations 
	of cases. The allowed combinations of modifications is checked, during creation. 
	"""
	
	def __init__(self, modulename, config):
		self.modulename = modulename
		self.config = config
		
	
	#
	# C/CPP part of marshaller
	#
	
	"""
	To understand the naming of the methods, have a look at the following
	C++ Code, which transforms the C interface function into a Cpp call

	"in" refers to a variable being input to the function
	"out" refers to a result variable, being output of the function

	extern "C" void func_c(..., ..., ctype in_name_c, ..., ctype out_name_c, ..., ctype result_c)
	{
		cpp_type in_name_cpp = fromCtoCpp<in_name_c>;
		cpp_type out_name_cpp;
		cpp_type result_name_cpp;
		
		result_name_cpp = func(..., ..., (cppcall)in_name_cpp, ..., ... (cppcall)out_name_cpp, ...);
		
		*out_name_c = fromCppToC<out_name_cpp>;  ( called as fromCppToCLine<(cname, cppname) )
		*result_c = fromCppToC<result_name_cpp>; 
	}
	"""
	
	def createCFunctionHeader(self, funcName, returnType, parameterTypes, conOrDeStructor, isStatic, classPrefix):
		text = ""
		
		# function header line
		text = text + "void "
		if len(classPrefix) > 0:
			text = text + classPrefix + "_"
		if conOrDeStructor == "none":
			text = text + funcName + "("
		elif conOrDeStructor == "constructor":
			text = text + "construct("
		elif conOrDeStructor == "destructor":
			text = text + "destruct("
			
		partext = ""
		for (name, tItem) in parameterTypes:
			partext = partext + tItem.mType.ctype(tItem.flags, False, tItem.basetype) + " " + name + "_c, "
		if returnType:
			partext = partext + returnType.mType.ctype(returnType.flags, True, returnType.basetype) + " result_c, " 
		if len(partext) > 0:
			partext = partext[:-2]
		text = text + partext + ")"
		return text
	
	
	def createCppFunction(self, funcName, callFuncName, returnType, parameterTypes, conOrDestructor, isStatic, className, classPrefix):
		# function header
		text = "extern \"C\" " + self.modulename + "_LIB_EXPORT " + self.createCFunctionHeader(funcName, returnType, parameterTypes, conOrDestructor, isStatic, classPrefix) + "\n{\n"
		
		# body lines before cpp function call
		for (name, tItem) in parameterTypes:
			text = text + "  " + tItem.mType.cpptype(tItem.flags, False, tItem.basetype) + " " + name + "_cpp"
			if not tItem.mType.getOutFlag(tItem.flags, False, tItem.basetype):
				text = text + " = " + tItem.mType.fromCtoCpp(name + "_c", tItem.flags, False, tItem.basetype)
			text = text + ";\n"
		if returnType:
			text = text + "  " + returnType.mType.cpptype(returnType.flags, True, returnType.basetype).strip() + " result_cpp;\n"
			
		# function call
		text = text + "  "
		if returnType:
			text = text + "result_cpp = "
			text = text + returnType.mType.fcallmod(returnType.flags, True, returnType.basetype)
			
		text = text + "("
		
		if conOrDestructor == "constructor":
			text = text + "new "
			if len(className) > 0:
				text = text + className
			else:
				text = text + callFuncName
			text = text + "("
		elif conOrDestructor == "destructor":
			text = text + "delete "
		else:
			if not isStatic:
				(name, tItem) = parameterTypes[0]
				text = text + name + "_cpp->"
				text = text + callFuncName + "("
			else:
				if len(className) > 0:
					text = text + className + "::"
				text = text + callFuncName + "("
			
		# parameters
		partext = ""
		if (not isStatic) and (conOrDestructor != "destructor") and (conOrDestructor != "constructor"):
			pTypes = parameterTypes[1:]
		else:
			pTypes = parameterTypes
			
		for (name, tItem) in pTypes:
			partext = partext + tItem.mType.cppcall(tItem.flags, False, tItem.basetype) + name + "_cpp, "
		if len(partext) > 0:
			partext = partext[:-2]
		text = text + partext	
		
		# close parameters
		if conOrDestructor != "destructor":
			text = text + ")"
		text = text + ");\n"
		
		# return parameters
		for (name, tItem) in parameterTypes:
			if tItem.mType.getOutFlag(tItem.flags, False, tItem.basetype):
				text = text + tItem.mType.fromCpptoC_Line(name + "_c", name + "_cpp", tItem.flags, True, tItem.basetype)
		# return value
		if returnType:
			text = text + returnType.mType.fromCpptoC_Line("result_c", "result_cpp", returnType.flags, True, returnType.basetype)
		# close function
		text = text + "};\n"
		return text
		
	
	#
	# CHS part of marshaller
	#
	
	def asName(self, name):
		"""returns a new Haskellish name for a C-function"""
		newName = ""
		toHigher = False
		for char in name:
			if char in "_-":
				toHigher = True
			else:
				if toHigher:
					newName = newName + char.upper()
				else:
					newName = newName + char
				toHigher = False
		return newName

	
	def createChsFunction(self, funcName, returnType, parameterTypes, conOrDestructor, isStatic, classPrefix):
		
		text = ""
	
		# function declaration
		text = text + "{- function " + funcName + " -}\n"
		# function definition
		text = text + "{#fun "
		if len(classPrefix) > 0:
			text = text + classPrefix + "_"
		if conOrDestructor == "constructor":
			text = text + "construct as " + self.asName("new") + " \n{ "
		elif conOrDestructor == "destructor":
			text = text + "destruct as " + self.asName("delete") + " \n{ "
		else:
			text = text + funcName  + " as " + self.asName(funcName) + " \n{ "

		# parameters, remaining parameters
		ind = 0
		for (name, tItem) in parameterTypes:
			if ind > 0:
				text = text + ",\n "
			text = text + tItem.mType.chsFuncDefinition(tItem.flags, False, tItem.basetype)
			ind = ind + 1

		# return type
		if returnType:
			if len(parameterTypes) > 0:
				text = text + ",\n "
			text = text + returnType.mType.chsFuncDefinition(returnType.flags, True, returnType.basetype)
			
		# close function definition
		text = text + "} -> `()'  #}\n"
		
		return text
