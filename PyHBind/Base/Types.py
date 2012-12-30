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

# Base/Marshaller/Types.py

"""
This is the basics of all marshalling logic. Each variable or parameter in a method
call does have a cpp type. Those types are first reduced to their base type, stripping
of pointer, const, reference declarations. The base type is then looked up in the main 
Type table.

The type table contains Types, which have Marshalling functionality and 
configuration and parsetree information of the specific type.
"""

import re

class TypeInstance:
	
	# this one will gather all information on a specific type, we need during
	# the marshalling process
	
	# the flags, the basetype (Cpp origin type, without pointer, ref
	# and the marshalled type
	
	def __init__(self, basetype, mType, flags):
		self.basetype = basetype
		self.flags = flags
		self.mType = mType
	
	
	
class MarshalledType:
	
	# this class contain the type specific information for marshalling
	# for each type this is the parsed item (self.item), the configuration
	# (self.config) and the marshalling routines itself
	

	def __init__(self, item, config):
	
		self.item = item
		self.config = config
		
	def item(self):
		return self.item
	
	def config(self):
		return self.config
		
# to be overwritten in subclasses
#
		

	def getOutFlag(self, flags, fReturn,  basetype):
		print "Marshaller does not support needed function"
		sys.exit(-1)
		
	def areFlagsValid(self, flags, fReturn,  basetype):
		return False
		

#
# here are coming the marshalling routines, needs to be implemented by sub-classes
#

	def fcallmod(self, flags, fReturn,  basetype):
		return ""
		
	def ctype(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		C type declaration, used in C-function declaration
		"""
		print "Marshaller does not support needed function"
		sys.exit(-1)
	
	def cpptype(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		CPP type declaration, used in variable declaration for cpp variable
		"""
		text = baseType
		if fConst:
			text = "const " + text
		if fPointer:
			text = text + " *"
		if fDoublePointer:
			text = text + " *"
		if fReference:
			text = text + " &"
		return text
			
	def cppcall(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		modifier before the variable name, for the invocation of the function,
		depends on the difference between the cpp function parameter type and the variable
		type. Can be "&" or "".
		"""
		if fPointer or fDoublePointer:
			return "&"
		else:
			return "" 

	def fromCtoCpp(self, cname, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		""" 
		returns transformation from c to cpp
		can be a simple cast or some more complex invocations
		"""
		print "Marshaller does not support needed function"
		sys.exit(-1)
		
	def fromCpptoC_Line(self, cname, cppname, flags, fReturn,  basetype):
		text = "  *" + cname + " = " + self.fromCpptoC(cppname, flags, fReturn, basetype) + ";\n"
		return text
		
	def fromCpptoC(self, cppname, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""
		returns transformation from cpp to c
		can be a simple cast or some more complex invocations
		"""
		print "Marshaller does not support needed function"
		sys.exit(-1)
		
	def chsFuncDefinition(self, flags, fReturn,  basetype):
		(fConst, fReference, fPointer, fDoublePointer) = flags
		"""provides the marshalling string within the C2HS func definition"""
		print "Marshaller does not support needed function"
		sys.exit(-1)
		


class Types:
	# this class holds all the information on types and gives back
	# when asked for
	
	registeredTypes = {}
	
	
	def __init__(self):
			
		self.reSpace = re.compile("\s+")
		self.reRemoveStart = re.compile("^\s*")
		self.reRemoveEnd = re.compile("\s*$")
		self.rePointer = re.compile("^\s*(.*?)\s*\*$")
		self.reReference = re.compile("^\s*(.*?)\s*&$")
		self.reConst = re.compile("^\s*const\s+(.*?)\s*$")

		self.baseAndFlagsCache = {}

	def registerTypeWithNames(self, mType, names):
		for tname in names:
			if tname in Types.registeredTypes.keys():
				print "type already existing, cannot register: ", tname
			else:
				Types.registeredTypes[tname] = mType
		

	def _normalizeTypestring(self, typestring):
		"""take original typestring and normalize it, return normalized version"""
		ts = typestring
		# remove space at begin and end, also sub remaining space with only one
		ts = self.reRemoveStart.sub("", ts)
		ts = self.reRemoveEnd.sub("", ts)
		ts = self.reSpace.sub(" ", ts)
		return ts
		
	def getTypeAndFlags(self, typestring, classContext, fReturn):
		
		tString = self._normalizeTypestring(typestring)
		base = tString
		
		# Flags
		#
		
		fPointer = False
		fDoublePointer = False
		fReference = False
		fConst = False
		
		# check pointer
		mo = self.rePointer.match(tString)
		if mo:
			base = mo.group(1)
			fPointer = True
			# check for double pointer
			mod = self.rePointer.match(base)
			if mod:
				base = mod.group(1)
				fDoublePointer = True
		# check reference
		mo = self.reReference.match(tString)
		if mo:
			base = mo.group(1)
			fReference = True
		# check const
		mo = self.reConst.match(base)
		if mo:
			base = mo.group(1)
			fConst = True
			
		flags = (fConst, fReference, fPointer, fDoublePointer)
		
		# Type
		#
		
		# search type in complete class context:
		mType = None
		if (classContext + "::" + base) in Types.registeredTypes.keys():
			mType = Types.registeredTypes[classContext + "::" + base]
		else:
			nspaces = classContext.split("::")
			for n in range(1,len(nspaces)):
				ns = "::".join(nspaces[:-n])
				if (ns + "::" + base) in Types.registeredTypes.keys():
					mType = Types.registeredTypes[ns + "::" + base]
					break
					
		if not mType and (base in Types.registeredTypes.keys()):
			mType = Types.registeredTypes[base]
			
		if not mType:
			print "type", base, "not registered!"
			return None
			
		
		# base should be full qualified basename -> lookup in mType
		# this is not correct in case of type mappings!
		newBase = base
		if mType.item:
			# do only if base is part of full-qualified name
			if base == mType.item.name.split("::")[-1]:
				newBase = mType.item.name
		
		# check valid flags
		if not mType.areFlagsValid(flags, fReturn,  newBase):
			print "type registered", base, "but flags not valid", fReturn, flags
			return None
			
		rval = TypeInstance(newBase, mType, flags)
		
		return rval
		
