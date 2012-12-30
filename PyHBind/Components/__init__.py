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

# Components Module, __init__.py

import Writer
import Marshaller
from ConfigStore import ConfigStore
from ParseTreeStore import ParseTreeStore, MultiParseTreeStore
from CreateParseTreeTask import CreateParseTreeTask
from RunDoxygenTask import RunDoxygenTask 
from SetParametersFromModuleTask import SetParametersFromModuleTask
from WriteSourceFilesTask import WriteSourceFilesTask
from RegisterTypesTask import RegisterTypesTask
from CompileDllTask import CompileDllTask
from CompileCabalTask import CompileCabalTask
from BuildCabalModuleTask import BuildCabalModuleTask
from CreateHaddockCommentsTask import CreateHaddockCommentsTask
from BuildInstallerTask import BuildInstallerTask
from BuildDistributableTask import BuildDistributableTask
from BuildDependenciesTask import BuildDependenciesTask
from BuildExamplesTask import BuildExamplesTask
