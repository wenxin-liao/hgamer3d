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

# Components Writer Module, __init__.py

from Enum import writeEnumFiles, addCommentsToEnumFiles
from Struct import writeStructFiles
from Class import writeClassFiles, addCommentsToClassFiles
from ClassPtr import writeClassPtrFiles
from HsUtil import writeHsUtilFile
from DllMakefile import writeDllMakefile
from Cabalfile import writeCabalfile
