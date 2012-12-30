# -*- coding: utf-8 -*- 

###########################################################################
## Python code generated with wxFormBuilder (version Feb  9 2012)
## http://www.wxformbuilder.org/
##
## PLEASE DO "NOT" EDIT THIS FILE!
###########################################################################

import wx
import wx.xrc
import wx.richtext

ID_NEW = 1000
ID_SAVE_AS = 1001
ID_EXIT = 1002

###########################################################################
## Class Frame1Base
###########################################################################

class Frame1Base ( wx.Frame ):
	
	def __init__( self, parent ):
		wx.Frame.__init__ ( self, parent, id = wx.ID_ANY, title = u"PyHBind", pos = wx.DefaultPosition, size = wx.Size( 1000,700 ), style = wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL )
		
		self.SetSizeHintsSz( wx.DefaultSize, wx.DefaultSize )
		
		boxsizer1 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.notebook1 = wx.Notebook( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, 0 )
		self.nbPanelConfig = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL, u"config" )
		fgsizer1 = wx.FlexGridSizer( 2, 2, 0, 0 )
		fgsizer1.SetFlexibleDirection( wx.BOTH )
		fgsizer1.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer48 = wx.FlexGridSizer( 2, 3, 0, 0 )
		fgSizer48.SetFlexibleDirection( wx.BOTH )
		fgSizer48.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer49 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer49.SetFlexibleDirection( wx.BOTH )
		fgSizer49.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgsizerDirs = wx.FlexGridSizer( 4, 3, 0, 0 )
		fgsizerDirs.SetFlexibleDirection( wx.BOTH )
		fgsizerDirs.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText1 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Basic Project Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1.Wrap( -1 )
		self.m_staticText1.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgsizerDirs.Add( self.m_staticText1, 0, wx.ALL, 5 )
		
		self.m_panel5 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgsizerDirs.Add( self.m_panel5, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel6 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgsizerDirs.Add( self.m_panel6, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText8 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Project Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText8.Wrap( -1 )
		fgsizerDirs.Add( self.m_staticText8, 0, wx.ALL, 5 )
		
		self.textCtrlProjectName = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgsizerDirs.Add( self.textCtrlProjectName, 0, wx.ALL, 5 )
		
		self.m_panel42 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgsizerDirs.Add( self.m_panel42, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText68 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Parent Project Label", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText68.Wrap( -1 )
		fgsizerDirs.Add( self.m_staticText68, 0, wx.ALL, 5 )
		
		self.txtCtrlParentProjectLabel = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgsizerDirs.Add( self.txtCtrlParentProjectLabel, 0, wx.ALL, 5 )
		
		self.m_panel7 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgsizerDirs.Add( self.m_panel7, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText4 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Base Directory", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText4.Wrap( -1 )
		fgsizerDirs.Add( self.m_staticText4, 0, wx.ALL, 5 )
		
		self.textCtrlBaseDir = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 300,-1 ), 0 )
		self.textCtrlBaseDir.Enable( False )
		
		fgsizerDirs.Add( self.textCtrlBaseDir, 0, wx.ALL, 5 )
		
		self.buttonSelectBaseDirectory = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"..", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgsizerDirs.Add( self.buttonSelectBaseDirectory, 0, wx.ALL, 5 )
		
		self.m_staticText5 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Build Directory", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText5.Wrap( -1 )
		fgsizerDirs.Add( self.m_staticText5, 0, wx.ALL, 5 )
		
		self.textCtrlBuildDir = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 300,-1 ), 0 )
		self.textCtrlBuildDir.Enable( False )
		
		fgsizerDirs.Add( self.textCtrlBuildDir, 0, wx.ALL, 5 )
		
		self.buttonSelectBuildDirectory = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"..", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgsizerDirs.Add( self.buttonSelectBuildDirectory, 0, wx.ALL, 5 )
		
		
		fgSizer49.Add( fgsizerDirs, 1, wx.EXPAND, 5 )
		
		self.m_panel49 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer49.Add( self.m_panel49, 1, wx.EXPAND |wx.ALL, 5 )
		
		fgsizerHeaderLib = wx.FlexGridSizer( 6, 2, 0, 0 )
		fgsizerHeaderLib.SetFlexibleDirection( wx.BOTH )
		fgsizerHeaderLib.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText6 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Header Directories", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText6.Wrap( -1 )
		self.m_staticText6.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgsizerHeaderLib.Add( self.m_staticText6, 0, wx.ALL, 5 )
		
		self.m_staticText7 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Library Directories", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText7.Wrap( -1 )
		self.m_staticText7.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgsizerHeaderLib.Add( self.m_staticText7, 0, wx.ALL, 5 )
		
		self.listCtrlHeaderDirs = wx.ListCtrl( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.Size( 250,120 ), wx.LC_LIST|wx.LC_SINGLE_SEL )
		fgsizerHeaderLib.Add( self.listCtrlHeaderDirs, 0, wx.ALL, 5 )
		
		self.listCtrlLibDirs = wx.ListCtrl( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.Size( 250,120 ), wx.LC_LIST|wx.LC_SINGLE_SEL )
		fgsizerHeaderLib.Add( self.listCtrlLibDirs, 0, wx.ALL, 5 )
		
		bSizer3 = wx.BoxSizer( wx.VERTICAL )
		
		self.buttonAddHeaderDir = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer3.Add( self.buttonAddHeaderDir, 0, wx.ALL, 5 )
		
		
		fgsizerHeaderLib.Add( bSizer3, 1, wx.EXPAND, 5 )
		
		bSizer5 = wx.BoxSizer( wx.VERTICAL )
		
		self.buttonAddLibDir = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer5.Add( self.buttonAddLibDir, 0, wx.ALL, 5 )
		
		
		fgsizerHeaderLib.Add( bSizer5, 1, wx.EXPAND, 5 )
		
		self.m_staticText221 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Libraray Dll's", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText221.Wrap( -1 )
		self.m_staticText221.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgsizerHeaderLib.Add( self.m_staticText221, 0, wx.ALL, 5 )
		
		self.m_staticText2211 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Libraray Lib's", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText2211.Wrap( -1 )
		self.m_staticText2211.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgsizerHeaderLib.Add( self.m_staticText2211, 0, wx.ALL, 5 )
		
		listBoxLibraryDllsChoices = []
		self.listBoxLibraryDlls = wx.ListBox( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.Size( 250,120 ), listBoxLibraryDllsChoices, 0 )
		fgsizerHeaderLib.Add( self.listBoxLibraryDlls, 0, wx.ALL, 5 )
		
		listBoxLibraryLibsChoices = []
		self.listBoxLibraryLibs = wx.ListBox( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.Size( 250,120 ), listBoxLibraryLibsChoices, 0 )
		fgsizerHeaderLib.Add( self.listBoxLibraryLibs, 0, wx.ALL, 5 )
		
		self.buttonAddLibraryDll = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgsizerHeaderLib.Add( self.buttonAddLibraryDll, 0, wx.ALL, 5 )
		
		self.buttonAddLibraryDll1 = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgsizerHeaderLib.Add( self.buttonAddLibraryDll1, 0, wx.ALL, 5 )
		
		
		fgSizer49.Add( fgsizerHeaderLib, 1, wx.EXPAND, 5 )
		
		
		fgSizer48.Add( fgSizer49, 1, wx.EXPAND, 5 )
		
		self.m_panel48 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer48.Add( self.m_panel48, 1, wx.EXPAND |wx.ALL, 5 )
		
		fgSizer50 = wx.FlexGridSizer( 0, 2, 0, 0 )
		fgSizer50.SetFlexibleDirection( wx.BOTH )
		fgSizer50.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText9 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Module Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText9.Wrap( -1 )
		self.m_staticText9.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer50.Add( self.m_staticText9, 0, wx.ALL, 5 )
		
		self.m_panel8 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer50.Add( self.m_panel8, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText10 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Title", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText10.Wrap( -1 )
		fgSizer50.Add( self.m_staticText10, 0, wx.ALL, 5 )
		
		self.txtCtrlModuleTitle = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlModuleTitle, 0, wx.ALL, 5 )
		
		self.m_staticText11 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Sub-Module", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText11.Wrap( -1 )
		fgSizer50.Add( self.m_staticText11, 0, wx.ALL, 5 )
		
		self.txtCtrlSubModule = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlSubModule, 0, wx.ALL, 5 )
		
		self.m_staticText12 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Cabal Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText12.Wrap( -1 )
		fgSizer50.Add( self.m_staticText12, 0, wx.ALL, 5 )
		
		self.txtCtrlCabalName = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlCabalName, 0, wx.ALL, 5 )
		
		self.m_staticText13 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Version", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText13.Wrap( -1 )
		fgSizer50.Add( self.m_staticText13, 0, wx.ALL, 5 )
		
		self.txtCtrlModuleVersion = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlModuleVersion, 0, wx.ALL, 5 )
		
		self.m_staticText74 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"URL", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText74.Wrap( -1 )
		fgSizer50.Add( self.m_staticText74, 0, wx.ALL, 5 )
		
		self.txtCtrlCabalUrl = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlCabalUrl, 0, wx.ALL, 5 )
		
		self.m_staticText75 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Synopsis", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText75.Wrap( -1 )
		fgSizer50.Add( self.m_staticText75, 0, wx.ALL, 5 )
		
		self.txtCtrlCabalSynopsis = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlCabalSynopsis, 0, wx.ALL, 5 )
		
		self.m_staticText76 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Description", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText76.Wrap( -1 )
		fgSizer50.Add( self.m_staticText76, 0, wx.ALL, 5 )
		
		self.txtCtrlCabalDescription = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,150 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer50.Add( self.txtCtrlCabalDescription, 0, wx.ALL, 5 )
		
		self.m_staticText77 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Depends", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText77.Wrap( -1 )
		fgSizer50.Add( self.m_staticText77, 0, wx.ALL, 5 )
		
		self.txtCtrlCabalDepends = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlCabalDepends, 0, wx.ALL, 5 )
		
		self.m_staticText78 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Category", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText78.Wrap( -1 )
		fgSizer50.Add( self.m_staticText78, 0, wx.ALL, 5 )
		
		self.txtCtrlCabalCategory = wx.TextCtrl( self.nbPanelConfig, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer50.Add( self.txtCtrlCabalCategory, 0, wx.ALL, 5 )
		
		self.m_staticText811 = wx.StaticText( self.nbPanelConfig, wx.ID_ANY, u"Namespaces", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText811.Wrap( -1 )
		fgSizer50.Add( self.m_staticText811, 0, wx.ALL, 5 )
		
		listBoxNamespacesChoices = []
		self.listBoxNamespaces = wx.ListBox( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.Size( 250,100 ), listBoxNamespacesChoices, 0 )
		fgSizer50.Add( self.listBoxNamespaces, 0, wx.ALL, 5 )
		
		self.m_panel46 = wx.Panel( self.nbPanelConfig, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer50.Add( self.m_panel46, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.buttonAddNamespace = wx.Button( self.nbPanelConfig, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer50.Add( self.buttonAddNamespace, 0, wx.ALL, 5 )
		
		
		fgSizer48.Add( fgSizer50, 1, wx.EXPAND, 5 )
		
		
		fgsizer1.Add( fgSizer48, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelConfig.SetSizer( fgsizer1 )
		self.nbPanelConfig.Layout()
		fgsizer1.Fit( self.nbPanelConfig )
		self.notebook1.AddPage( self.nbPanelConfig, u"Configuration", True )
		self.nbPanelHeader = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL, u"header" )
		fgSizer4 = wx.FlexGridSizer( 3, 3, 0, 0 )
		fgSizer4.SetFlexibleDirection( wx.BOTH )
		fgSizer4.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText71 = wx.StaticText( self.nbPanelHeader, wx.ID_ANY, u"Header File Selection", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText71.Wrap( -1 )
		self.m_staticText71.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer4.Add( self.m_staticText71, 0, wx.ALL, 5 )
		
		self.m_staticText81 = wx.StaticText( self.nbPanelHeader, wx.ID_ANY, u"Header File Preview", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText81.Wrap( -1 )
		self.m_staticText81.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer4.Add( self.m_staticText81, 0, wx.ALL, 5 )
		
		self.m_staticText73 = wx.StaticText( self.nbPanelHeader, wx.ID_ANY, u"Header to Include", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText73.Wrap( -1 )
		self.m_staticText73.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer4.Add( self.m_staticText73, 0, wx.ALL, 5 )
		
		self.treeCtrlHeaderFiles = wx.TreeCtrl( self.nbPanelHeader, wx.ID_ANY, wx.DefaultPosition, wx.Size( 300,500 ), wx.TR_DEFAULT_STYLE )
		fgSizer4.Add( self.treeCtrlHeaderFiles, 0, wx.ALL, 5 )
		
		self.richTextHeaderFile = wx.richtext.RichTextCtrl( self.nbPanelHeader, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 400,400 ), 0|wx.VSCROLL|wx.HSCROLL|wx.NO_BORDER|wx.WANTS_CHARS )
		fgSizer4.Add( self.richTextHeaderFile, 1, wx.ALL|wx.EXPAND, 5 )
		
		fgSizer47 = wx.FlexGridSizer( 0, 1, 0, 0 )
		fgSizer47.SetFlexibleDirection( wx.BOTH )
		fgSizer47.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_button13 = wx.Button( self.nbPanelHeader, wx.ID_ANY, u"Add Selected Header", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer47.Add( self.m_button13, 0, wx.ALL, 5 )
		
		listBoxHeadersToIncludeChoices = []
		self.listBoxHeadersToInclude = wx.ListBox( self.nbPanelHeader, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,460 ), listBoxHeadersToIncludeChoices, 0 )
		fgSizer47.Add( self.listBoxHeadersToInclude, 0, wx.ALL, 5 )
		
		
		fgSizer4.Add( fgSizer47, 1, wx.EXPAND, 5 )
		
		bSizer8 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.m_button16 = wx.Button( self.nbPanelHeader, wx.ID_ANY, u"rescan Dirs", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer8.Add( self.m_button16, 0, wx.ALL, 5 )
		
		self.m_button17 = wx.Button( self.nbPanelHeader, wx.ID_ANY, u"parse Headers", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer8.Add( self.m_button17, 0, wx.ALL, 5 )
		
		
		fgSizer4.Add( bSizer8, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelHeader.SetSizer( fgSizer4 )
		self.nbPanelHeader.Layout()
		fgSizer4.Fit( self.nbPanelHeader )
		self.notebook1.AddPage( self.nbPanelHeader, u"Header Files", False )
		self.nbPanelSource = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer12141 = wx.FlexGridSizer( 1, 2, 0, 0 )
		fgSizer12141.SetFlexibleDirection( wx.BOTH )
		fgSizer12141.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer1641 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer1641.SetFlexibleDirection( wx.BOTH )
		fgSizer1641.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText2441 = wx.StaticText( self.nbPanelSource, wx.ID_ANY, u"Source Dirs", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText2441.Wrap( -1 )
		self.m_staticText2441.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer1641.Add( self.m_staticText2441, 0, wx.ALL, 5 )
		
		listBoxSourceDirsChoices = []
		self.listBoxSourceDirs = wx.ListBox( self.nbPanelSource, wx.ID_ANY, wx.DefaultPosition, wx.Size( 300,200 ), listBoxSourceDirsChoices, 0 )
		fgSizer1641.Add( self.listBoxSourceDirs, 0, wx.ALL, 5 )
		
		self.buttonAddSourceDir = wx.Button( self.nbPanelSource, wx.ID_ANY, u"Add Source Dir", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer1641.Add( self.buttonAddSourceDir, 0, wx.ALL, 5 )
		
		
		fgSizer12141.Add( fgSizer1641, 1, wx.EXPAND, 5 )
		
		fgSizer1741 = wx.FlexGridSizer( 2, 3, 0, 0 )
		fgSizer1741.SetFlexibleDirection( wx.BOTH )
		fgSizer1741.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		
		fgSizer12141.Add( fgSizer1741, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelSource.SetSizer( fgSizer12141 )
		self.nbPanelSource.Layout()
		fgSizer12141.Fit( self.nbPanelSource )
		self.notebook1.AddPage( self.nbPanelSource, u"Sources", False )
		self.nbPanelTypes = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL, u"three" )
		fgSizer9 = wx.FlexGridSizer( 3, 3, 0, 0 )
		fgSizer9.SetFlexibleDirection( wx.BOTH )
		fgSizer9.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer10 = wx.FlexGridSizer( 3, 1, 0, 0 )
		fgSizer10.SetFlexibleDirection( wx.BOTH )
		fgSizer10.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText14 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Basic Types", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText14.Wrap( -1 )
		self.m_staticText14.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer10.Add( self.m_staticText14, 0, wx.ALL, 5 )
		
		listBoxBasicTypesChoices = []
		self.listBoxBasicTypes = wx.ListBox( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,200 ), listBoxBasicTypesChoices, 0 )
		fgSizer10.Add( self.listBoxBasicTypes, 0, wx.ALL, 5 )
		
		bSizer51 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.buttonAddBasicType = wx.Button( self.nbPanelTypes, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer51.Add( self.buttonAddBasicType, 0, wx.ALL, 5 )
		
		self.buttonGenerateDefaultBasicTypes = wx.Button( self.nbPanelTypes, wx.ID_ANY, u"Generate Defaults", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer51.Add( self.buttonGenerateDefaultBasicTypes, 0, wx.ALL, 5 )
		
		
		fgSizer10.Add( bSizer51, 1, wx.EXPAND, 5 )
		
		
		fgSizer9.Add( fgSizer10, 1, wx.EXPAND, 5 )
		
		fgSizer12 = wx.FlexGridSizer( 6, 2, 0, 0 )
		fgSizer12.SetFlexibleDirection( wx.BOTH )
		fgSizer12.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText15 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Type Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText15.Wrap( -1 )
		fgSizer12.Add( self.m_staticText15, 0, wx.ALL, 5 )
		
		self.m_panel11 = wx.Panel( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer12.Add( self.m_panel11, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText16 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText16.Wrap( -1 )
		fgSizer12.Add( self.m_staticText16, 0, wx.ALL, 5 )
		
		self.txtCtrlBasicTypeName = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		self.txtCtrlBasicTypeName.Enable( False )
		
		fgSizer12.Add( self.txtCtrlBasicTypeName, 0, wx.ALL, 5 )
		
		self.m_staticText18 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Haskell Type", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText18.Wrap( -1 )
		fgSizer12.Add( self.m_staticText18, 0, wx.ALL, 5 )
		
		self.txtCtrlBasicTypeHType = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer12.Add( self.txtCtrlBasicTypeHType, 0, wx.ALL, 5 )
		
		self.m_staticText40 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Comment", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText40.Wrap( -1 )
		fgSizer12.Add( self.m_staticText40, 0, wx.ALL, 5 )
		
		self.txtCtrlBasicTypeComment = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,100 ), 0 )
		fgSizer12.Add( self.txtCtrlBasicTypeComment, 0, wx.ALL, 5 )
		
		self.m_staticline3 = wx.StaticLine( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
		fgSizer12.Add( self.m_staticline3, 0, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticline4 = wx.StaticLine( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
		fgSizer12.Add( self.m_staticline4, 0, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText54 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Implementation Details", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText54.Wrap( -1 )
		fgSizer12.Add( self.m_staticText54, 0, wx.ALL, 5 )
		
		self.m_panel36 = wx.Panel( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer12.Add( self.m_panel36, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText19 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"C Interface-Type", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText19.Wrap( -1 )
		fgSizer12.Add( self.m_staticText19, 0, wx.ALL, 5 )
		
		self.txtCtrlBasicTypeCType = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer12.Add( self.txtCtrlBasicTypeCType, 0, wx.ALL, 5 )
		
		self.m_staticText20 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"In-Marshaller", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText20.Wrap( -1 )
		fgSizer12.Add( self.m_staticText20, 0, wx.ALL, 5 )
		
		self.txtCtrlBasicTypeInMarshaller = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer12.Add( self.txtCtrlBasicTypeInMarshaller, 0, wx.ALL, 5 )
		
		self.m_staticText21 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Out-Marshaller", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText21.Wrap( -1 )
		fgSizer12.Add( self.m_staticText21, 0, wx.ALL, 5 )
		
		self.txtCtrlBasicTypeOutMarshaller = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer12.Add( self.txtCtrlBasicTypeOutMarshaller, 0, wx.ALL, 5 )
		
		self.m_staticline5 = wx.StaticLine( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
		fgSizer12.Add( self.m_staticline5, 0, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticline6 = wx.StaticLine( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
		fgSizer12.Add( self.m_staticline6, 0, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText69 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Strings", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText69.Wrap( -1 )
		fgSizer12.Add( self.m_staticText69, 0, wx.ALL, 5 )
		
		self.m_panel441 = wx.Panel( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer12.Add( self.m_panel441, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText39 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"String", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText39.Wrap( -1 )
		fgSizer12.Add( self.m_staticText39, 0, wx.ALL, 5 )
		
		self.checkBoxBasicTypeString = wx.CheckBox( self.nbPanelTypes, wx.ID_ANY, u"this is a string type", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer12.Add( self.checkBoxBasicTypeString, 0, wx.ALL, 5 )
		
		self.m_staticText70 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"Constructor", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText70.Wrap( -1 )
		fgSizer12.Add( self.m_staticText70, 0, wx.ALL, 5 )
		
		self.txtCtrlStringConstructor = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer12.Add( self.txtCtrlStringConstructor, 0, wx.ALL, 5 )
		
		self.m_staticText711 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"As Char* Method", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText711.Wrap( -1 )
		fgSizer12.Add( self.m_staticText711, 0, wx.ALL, 5 )
		
		self.txtCtrlAsCharPtrMethod = wx.TextCtrl( self.nbPanelTypes, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, 0 )
		self.txtCtrlAsCharPtrMethod.SetMinSize( wx.Size( 250,-1 ) )
		
		fgSizer12.Add( self.txtCtrlAsCharPtrMethod, 0, wx.ALL, 5 )
		
		
		fgSizer9.Add( fgSizer12, 1, wx.EXPAND, 5 )
		
		fgSizer13 = wx.FlexGridSizer( 3, 1, 0, 0 )
		fgSizer13.SetFlexibleDirection( wx.BOTH )
		fgSizer13.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText22 = wx.StaticText( self.nbPanelTypes, wx.ID_ANY, u"C-Types", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText22.Wrap( -1 )
		fgSizer13.Add( self.m_staticText22, 0, wx.ALL, 5 )
		
		listBoxMappedCTypesChoices = []
		self.listBoxMappedCTypes = wx.ListBox( self.nbPanelTypes, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,300 ), listBoxMappedCTypesChoices, 0 )
		fgSizer13.Add( self.listBoxMappedCTypes, 0, wx.ALL, 5 )
		
		bSizer6 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.buttonAddCType = wx.Button( self.nbPanelTypes, wx.ID_ANY, u"Add", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer6.Add( self.buttonAddCType, 0, wx.ALL, 5 )
		
		
		fgSizer13.Add( bSizer6, 1, wx.EXPAND, 5 )
		
		
		fgSizer9.Add( fgSizer13, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelTypes.SetSizer( fgSizer9 )
		self.nbPanelTypes.Layout()
		fgSizer9.Fit( self.nbPanelTypes )
		self.notebook1.AddPage( self.nbPanelTypes, u"Types", False )
		self.nbPanelEnums = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL, u"four" )
		fgSizer1211 = wx.FlexGridSizer( 1, 3, 0, 0 )
		fgSizer1211.SetFlexibleDirection( wx.BOTH )
		fgSizer1211.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer161 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer161.SetFlexibleDirection( wx.BOTH )
		fgSizer161.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText241 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Enums", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText241.Wrap( -1 )
		self.m_staticText241.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer161.Add( self.m_staticText241, 0, wx.ALL, 5 )
		
		listBoxEnumsChoices = []
		self.listBoxEnums = wx.ListBox( self.nbPanelEnums, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,500 ), listBoxEnumsChoices, 0 )
		fgSizer161.Add( self.listBoxEnums, 0, wx.ALL, 5 )
		
		
		fgSizer1211.Add( fgSizer161, 1, wx.EXPAND, 5 )
		
		fgSizer1221 = wx.FlexGridSizer( 6, 2, 0, 0 )
		fgSizer1221.SetFlexibleDirection( wx.BOTH )
		fgSizer1221.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText1511 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Configuration", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1511.Wrap( -1 )
		fgSizer1221.Add( self.m_staticText1511, 0, wx.ALL, 5 )
		
		self.m_panel1111 = wx.Panel( self.nbPanelEnums, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer1221.Add( self.m_panel1111, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText1611 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1611.Wrap( -1 )
		fgSizer1221.Add( self.m_staticText1611, 0, wx.ALL, 5 )
		
		self.txtCtrlEnumName = wx.TextCtrl( self.nbPanelEnums, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		self.txtCtrlEnumName.Enable( False )
		
		fgSizer1221.Add( self.txtCtrlEnumName, 0, wx.ALL, 5 )
		
		self.m_staticText1811 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Name Suffix", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1811.Wrap( -1 )
		fgSizer1221.Add( self.m_staticText1811, 0, wx.ALL, 5 )
		
		self.txtCtrlEnumNameSuffix = wx.TextCtrl( self.nbPanelEnums, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer1221.Add( self.txtCtrlEnumNameSuffix, 0, wx.ALL, 5 )
		
		self.m_staticText1911 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Value Prefix", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1911.Wrap( -1 )
		fgSizer1221.Add( self.m_staticText1911, 0, wx.ALL, 5 )
		
		self.txtCtrlEnumValuePrefix = wx.TextCtrl( self.nbPanelEnums, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer1221.Add( self.txtCtrlEnumValuePrefix, 0, wx.ALL, 5 )
		
		self.m_staticText3911 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Selected", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText3911.Wrap( -1 )
		fgSizer1221.Add( self.m_staticText3911, 0, wx.ALL, 5 )
		
		self.checkBoxEnumSelected = wx.CheckBox( self.nbPanelEnums, wx.ID_ANY, u"this Enum is selected", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer1221.Add( self.checkBoxEnumSelected, 0, wx.ALL, 5 )
		
		self.m_staticText4011 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Comment", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText4011.Wrap( -1 )
		fgSizer1221.Add( self.m_staticText4011, 0, wx.ALL, 5 )
		
		self.txtCtrlEnumComment = wx.TextCtrl( self.nbPanelEnums, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,60 ), 0 )
		fgSizer1221.Add( self.txtCtrlEnumComment, 0, wx.ALL, 5 )
		
		
		fgSizer1211.Add( fgSizer1221, 1, wx.EXPAND, 5 )
		
		fgSizer171 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer171.SetFlexibleDirection( wx.BOTH )
		fgSizer171.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer181 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer181.SetFlexibleDirection( wx.BOTH )
		fgSizer181.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText251 = wx.StaticText( self.nbPanelEnums, wx.ID_ANY, u"Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText251.Wrap( -1 )
		fgSizer181.Add( self.m_staticText251, 0, wx.ALL, 5 )
		
		self.txtCtrlEnumInfo = wx.TextCtrl( self.nbPanelEnums, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 400,500 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer181.Add( self.txtCtrlEnumInfo, 0, wx.ALL, 5 )
		
		
		fgSizer171.Add( fgSizer181, 1, wx.EXPAND, 5 )
		
		
		fgSizer1211.Add( fgSizer171, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelEnums.SetSizer( fgSizer1211 )
		self.nbPanelEnums.Layout()
		fgSizer1211.Fit( self.nbPanelEnums )
		self.notebook1.AddPage( self.nbPanelEnums, u"Enums", False )
		self.nbPanelStructs = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer121 = wx.FlexGridSizer( 1, 2, 0, 0 )
		fgSizer121.SetFlexibleDirection( wx.BOTH )
		fgSizer121.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer16 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer16.SetFlexibleDirection( wx.BOTH )
		fgSizer16.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText24 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Structs", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText24.Wrap( -1 )
		self.m_staticText24.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer16.Add( self.m_staticText24, 0, wx.ALL, 5 )
		
		listBoxStructsChoices = []
		self.listBoxStructs = wx.ListBox( self.nbPanelStructs, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,530 ), listBoxStructsChoices, 0 )
		fgSizer16.Add( self.listBoxStructs, 0, wx.ALL, 5 )
		
		
		fgSizer121.Add( fgSizer16, 1, wx.EXPAND, 5 )
		
		fgSizer17 = wx.FlexGridSizer( 2, 2, 0, 0 )
		fgSizer17.SetFlexibleDirection( wx.BOTH )
		fgSizer17.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer122 = wx.FlexGridSizer( 6, 2, 0, 0 )
		fgSizer122.SetFlexibleDirection( wx.BOTH )
		fgSizer122.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText151 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Configuration", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText151.Wrap( -1 )
		fgSizer122.Add( self.m_staticText151, 0, wx.ALL, 5 )
		
		self.m_panel111 = wx.Panel( self.nbPanelStructs, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer122.Add( self.m_panel111, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText161 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText161.Wrap( -1 )
		fgSizer122.Add( self.m_staticText161, 0, wx.ALL, 5 )
		
		self.txtCtrlStructName = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		self.txtCtrlStructName.Enable( False )
		
		fgSizer122.Add( self.txtCtrlStructName, 0, wx.ALL, 5 )
		
		self.m_staticText181 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Haskell Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText181.Wrap( -1 )
		fgSizer122.Add( self.m_staticText181, 0, wx.ALL, 5 )
		
		self.txtCtrlStructHName = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer122.Add( self.txtCtrlStructHName, 0, wx.ALL, 5 )
		
		self.m_staticText191 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Value Prefix", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText191.Wrap( -1 )
		fgSizer122.Add( self.m_staticText191, 0, wx.ALL, 5 )
		
		self.txtCtrlStructValuePrefix = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer122.Add( self.txtCtrlStructValuePrefix, 0, wx.ALL, 5 )
		
		self.m_staticText201 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"C-Struct Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText201.Wrap( -1 )
		fgSizer122.Add( self.m_staticText201, 0, wx.ALL, 5 )
		
		self.txtCtrlStructStructName = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer122.Add( self.txtCtrlStructStructName, 0, wx.ALL, 5 )
		
		self.m_staticText391 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Selected", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText391.Wrap( -1 )
		fgSizer122.Add( self.m_staticText391, 0, wx.ALL, 5 )
		
		self.checkBoxStructSelected = wx.CheckBox( self.nbPanelStructs, wx.ID_ANY, u"this Struct is selected", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer122.Add( self.checkBoxStructSelected, 0, wx.ALL, 5 )
		
		self.m_staticText401 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Comment", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText401.Wrap( -1 )
		fgSizer122.Add( self.m_staticText401, 0, wx.ALL, 5 )
		
		self.txtCtrlStructComment = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,60 ), 0 )
		fgSizer122.Add( self.txtCtrlStructComment, 0, wx.ALL, 5 )
		
		self.m_staticText80 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"C-Types", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText80.Wrap( -1 )
		fgSizer122.Add( self.m_staticText80, 0, wx.ALL, 5 )
		
		self.txtCtrlStructCTypes = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer122.Add( self.txtCtrlStructCTypes, 0, wx.ALL, 5 )
		
		
		fgSizer17.Add( fgSizer122, 1, wx.EXPAND, 5 )
		
		fgSizer185 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer185.SetFlexibleDirection( wx.BOTH )
		fgSizer185.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText255 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Header File Content", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText255.Wrap( -1 )
		fgSizer185.Add( self.m_staticText255, 0, wx.ALL, 5 )
		
		self.txtCtrlStructHeaderFile = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 300,250 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer185.Add( self.txtCtrlStructHeaderFile, 0, wx.ALL, 5 )
		
		
		fgSizer17.Add( fgSizer185, 1, wx.EXPAND, 5 )
		
		fgSizer18 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer18.SetFlexibleDirection( wx.BOTH )
		fgSizer18.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText25 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText25.Wrap( -1 )
		fgSizer18.Add( self.m_staticText25, 0, wx.ALL, 5 )
		
		self.txtCtrlStructInfo = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 320,250 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer18.Add( self.txtCtrlStructInfo, 0, wx.ALL, 5 )
		
		
		fgSizer17.Add( fgSizer18, 1, wx.EXPAND, 5 )
		
		fgSizer186 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer186.SetFlexibleDirection( wx.BOTH )
		fgSizer186.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText256 = wx.StaticText( self.nbPanelStructs, wx.ID_ANY, u"CHS File Content", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText256.Wrap( -1 )
		fgSizer186.Add( self.m_staticText256, 0, wx.ALL, 5 )
		
		self.txtCtrlStructChsFile = wx.TextCtrl( self.nbPanelStructs, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 300,250 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer186.Add( self.txtCtrlStructChsFile, 0, wx.ALL, 5 )
		
		
		fgSizer17.Add( fgSizer186, 1, wx.EXPAND, 5 )
		
		
		fgSizer121.Add( fgSizer17, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelStructs.SetSizer( fgSizer121 )
		self.nbPanelStructs.Layout()
		fgSizer121.Fit( self.nbPanelStructs )
		self.notebook1.AddPage( self.nbPanelStructs, u"Structs", False )
		self.nbPanelClasses = wx.Panel( self.notebook1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer1212 = wx.FlexGridSizer( 1, 2, 0, 0 )
		fgSizer1212.SetFlexibleDirection( wx.BOTH )
		fgSizer1212.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer162 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer162.SetFlexibleDirection( wx.BOTH )
		fgSizer162.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText242 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Classes", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText242.Wrap( -1 )
		self.m_staticText242.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer162.Add( self.m_staticText242, 0, wx.ALL, 5 )
		
		listBoxClassesChoices = []
		self.listBoxClasses = wx.ListBox( self.nbPanelClasses, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,500 ), listBoxClassesChoices, 0 )
		fgSizer162.Add( self.listBoxClasses, 0, wx.ALL, 5 )
		
		
		fgSizer1212.Add( fgSizer162, 1, wx.EXPAND, 5 )
		
		fgSizer172 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer172.SetFlexibleDirection( wx.BOTH )
		fgSizer172.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer182 = wx.FlexGridSizer( 1, 2, 0, 0 )
		fgSizer182.SetFlexibleDirection( wx.BOTH )
		fgSizer182.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer1222 = wx.FlexGridSizer( 6, 2, 0, 0 )
		fgSizer1222.SetFlexibleDirection( wx.BOTH )
		fgSizer1222.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText1512 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Class Configuration", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1512.Wrap( -1 )
		fgSizer1222.Add( self.m_staticText1512, 0, wx.ALL, 5 )
		
		self.m_panel1112 = wx.Panel( self.nbPanelClasses, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer1222.Add( self.m_panel1112, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText1612 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1612.Wrap( -1 )
		fgSizer1222.Add( self.m_staticText1612, 0, wx.ALL, 5 )
		
		self.txtCtrlClassName = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		self.txtCtrlClassName.Enable( False )
		
		fgSizer1222.Add( self.txtCtrlClassName, 0, wx.ALL, 5 )
		
		self.m_staticText1812 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Haskell Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1812.Wrap( -1 )
		fgSizer1222.Add( self.m_staticText1812, 0, wx.ALL, 5 )
		
		self.txtCtrlClassHName = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer1222.Add( self.txtCtrlClassHName, 0, wx.ALL, 5 )
		
		self.m_staticText1912 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"General Prefix", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText1912.Wrap( -1 )
		fgSizer1222.Add( self.m_staticText1912, 0, wx.ALL, 5 )
		
		self.txtCtrlClassPrefix = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,-1 ), 0 )
		fgSizer1222.Add( self.txtCtrlClassPrefix, 0, wx.ALL, 5 )
		
		self.m_staticText3912 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Selected", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText3912.Wrap( -1 )
		fgSizer1222.Add( self.m_staticText3912, 0, wx.ALL, 5 )
		
		self.checkBoxClassSelected = wx.CheckBox( self.nbPanelClasses, wx.ID_ANY, u"this Class is selected", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer1222.Add( self.checkBoxClassSelected, 0, wx.ALL, 5 )
		
		self.m_staticText4012 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Comment", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText4012.Wrap( -1 )
		fgSizer1222.Add( self.m_staticText4012, 0, wx.ALL, 5 )
		
		self.txtCtrlClassComment = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 250,90 ), 0 )
		fgSizer1222.Add( self.txtCtrlClassComment, 0, wx.ALL, 5 )
		
		
		fgSizer182.Add( fgSizer1222, 1, wx.EXPAND, 5 )
		
		fgSizer187 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer187.SetFlexibleDirection( wx.BOTH )
		fgSizer187.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText252 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Class Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText252.Wrap( -1 )
		fgSizer187.Add( self.m_staticText252, 0, wx.ALL, 5 )
		
		self.txtCtrlClassInfo = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 350,210 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer187.Add( self.txtCtrlClassInfo, 0, wx.ALL, 5 )
		
		
		fgSizer182.Add( fgSizer187, 1, wx.EXPAND, 5 )
		
		
		fgSizer172.Add( fgSizer182, 1, wx.EXPAND, 5 )
		
		self.m_staticline31 = wx.StaticLine( self.nbPanelClasses, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.LI_HORIZONTAL )
		fgSizer172.Add( self.m_staticline31, 0, wx.EXPAND |wx.ALL, 5 )
		
		fgSizer36 = wx.FlexGridSizer( 0, 3, 0, 0 )
		fgSizer36.SetFlexibleDirection( wx.BOTH )
		fgSizer36.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		fgSizer1621 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer1621.SetFlexibleDirection( wx.BOTH )
		fgSizer1621.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText2421 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Methods", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText2421.Wrap( -1 )
		self.m_staticText2421.SetFont( wx.Font( 8, 74, 90, 92, False, "MS Shell Dlg 2" ) )
		
		fgSizer1621.Add( self.m_staticText2421, 0, wx.ALL, 5 )
		
		listBoxClassMethodsChoices = []
		self.listBoxClassMethods = wx.ListBox( self.nbPanelClasses, wx.ID_ANY, wx.DefaultPosition, wx.Size( 200,240 ), listBoxClassMethodsChoices, 0 )
		fgSizer1621.Add( self.listBoxClassMethods, 0, wx.ALL, 5 )
		
		
		fgSizer36.Add( fgSizer1621, 1, wx.EXPAND, 5 )
		
		fgSizer12221 = wx.FlexGridSizer( 6, 2, 0, 0 )
		fgSizer12221.SetFlexibleDirection( wx.BOTH )
		fgSizer12221.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText15121 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Method Conf.", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText15121.Wrap( -1 )
		fgSizer12221.Add( self.m_staticText15121, 0, wx.ALL, 5 )
		
		self.m_panel11121 = wx.Panel( self.nbPanelClasses, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer12221.Add( self.m_panel11121, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText16121 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText16121.Wrap( -1 )
		fgSizer12221.Add( self.m_staticText16121, 0, wx.ALL, 5 )
		
		self.txtCtrlClassMethodName = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		self.txtCtrlClassMethodName.Enable( False )
		
		fgSizer12221.Add( self.txtCtrlClassMethodName, 0, wx.ALL, 5 )
		
		self.m_staticText39121 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Excluded", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText39121.Wrap( -1 )
		fgSizer12221.Add( self.m_staticText39121, 0, wx.ALL, 5 )
		
		self.checkBoxClassMethodExcluded = wx.CheckBox( self.nbPanelClasses, wx.ID_ANY, u"this Method is excluded", wx.DefaultPosition, wx.DefaultSize, 0 )
		fgSizer12221.Add( self.checkBoxClassMethodExcluded, 0, wx.ALL, 5 )
		
		
		fgSizer36.Add( fgSizer12221, 1, wx.EXPAND, 5 )
		
		fgSizer1871 = wx.FlexGridSizer( 2, 1, 0, 0 )
		fgSizer1871.SetFlexibleDirection( wx.BOTH )
		fgSizer1871.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_staticText2521 = wx.StaticText( self.nbPanelClasses, wx.ID_ANY, u"Method Information", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText2521.Wrap( -1 )
		fgSizer1871.Add( self.m_staticText2521, 0, wx.ALL, 5 )
		
		self.txtCtrlClassMethodInfo = wx.TextCtrl( self.nbPanelClasses, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 220,250 ), wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		fgSizer1871.Add( self.txtCtrlClassMethodInfo, 0, wx.ALL, 5 )
		
		
		fgSizer36.Add( fgSizer1871, 1, wx.EXPAND, 5 )
		
		
		fgSizer172.Add( fgSizer36, 1, wx.EXPAND, 5 )
		
		
		fgSizer1212.Add( fgSizer172, 1, wx.EXPAND, 5 )
		
		
		self.nbPanelClasses.SetSizer( fgSizer1212 )
		self.nbPanelClasses.Layout()
		fgSizer1212.Fit( self.nbPanelClasses )
		self.notebook1.AddPage( self.nbPanelClasses, u"Classes", False )
		
		boxsizer1.Add( self.notebook1, 1, wx.EXPAND |wx.ALL, 5 )
		
		
		self.SetSizer( boxsizer1 )
		self.Layout()
		self.statusBar = self.CreateStatusBar( 1, wx.ST_SIZEGRIP, wx.ID_ANY )
		self.menubar1 = wx.MenuBar( 0 )
		self.menuProject = wx.Menu()
		self.menuItemNew = wx.MenuItem( self.menuProject, ID_NEW, u"New", wx.EmptyString, wx.ITEM_NORMAL )
		self.menuProject.AppendItem( self.menuItemNew )
		
		self.menuItemProjectOpen = wx.MenuItem( self.menuProject, wx.ID_ANY, u"Open", wx.EmptyString, wx.ITEM_NORMAL )
		self.menuProject.AppendItem( self.menuItemProjectOpen )
		
		self.menuItemProjectSave = wx.MenuItem( self.menuProject, wx.ID_ANY, u"Save", wx.EmptyString, wx.ITEM_NORMAL )
		self.menuProject.AppendItem( self.menuItemProjectSave )
		
		self.menuItemSaveAs = wx.MenuItem( self.menuProject, ID_SAVE_AS, u"Save As", wx.EmptyString, wx.ITEM_NORMAL )
		self.menuProject.AppendItem( self.menuItemSaveAs )
		
		self.menuItemExit = wx.MenuItem( self.menuProject, ID_EXIT, u"Exit", wx.EmptyString, wx.ITEM_NORMAL )
		self.menuProject.AppendItem( self.menuItemExit )
		
		self.menubar1.Append( self.menuProject, u"Project" ) 
		
		self.SetMenuBar( self.menubar1 )
		
		
		self.Centre( wx.BOTH )
		
		# Connect Events
		self.notebook1.Bind( wx.EVT_NOTEBOOK_PAGE_CHANGED, self.nbPageChangedHandler )
		self.textCtrlProjectName.Bind( wx.EVT_TEXT, self.projectNameSetHandler )
		self.txtCtrlParentProjectLabel.Bind( wx.EVT_TEXT, self.projectParentLabelSetHandler )
		self.textCtrlBaseDir.Bind( wx.EVT_TEXT, self.baseDirSetHandler )
		self.buttonSelectBaseDirectory.Bind( wx.EVT_BUTTON, self.selectBaseDirectoryHandler )
		self.textCtrlBuildDir.Bind( wx.EVT_TEXT, self.buildDirSetHandler )
		self.buttonSelectBuildDirectory.Bind( wx.EVT_BUTTON, self.selectBuildDirectoryHandler )
		self.listCtrlHeaderDirs.Bind( wx.EVT_LIST_KEY_DOWN, self.listHeaderDirDeleteHandler )
		self.listCtrlLibDirs.Bind( wx.EVT_LIST_KEY_DOWN, self.listLibDirDeleteHandler )
		self.buttonAddHeaderDir.Bind( wx.EVT_BUTTON, self.addHeaderDirHandler )
		self.buttonAddLibDir.Bind( wx.EVT_BUTTON, self.addLibDirHandler )
		self.listBoxLibraryDlls.Bind( wx.EVT_KEY_UP, self.listBoxLibraryDllsKeyUp )
		self.listBoxLibraryLibs.Bind( wx.EVT_KEY_UP, self.listBoxLibraryLibsKeyUp )
		self.buttonAddLibraryDll.Bind( wx.EVT_BUTTON, self.addLibraryDllHandler )
		self.buttonAddLibraryDll1.Bind( wx.EVT_BUTTON, self.addLibraryLibHandler )
		self.txtCtrlModuleTitle.Bind( wx.EVT_TEXT, self.moduleTitleSetHandler )
		self.txtCtrlSubModule.Bind( wx.EVT_TEXT, self.subModuleSetHandler )
		self.txtCtrlCabalName.Bind( wx.EVT_TEXT, self.cabalNameSetHandler )
		self.txtCtrlModuleVersion.Bind( wx.EVT_TEXT, self.moduleVersionSetHandler )
		self.txtCtrlCabalUrl.Bind( wx.EVT_TEXT, self.cabalUrlSetHandler )
		self.txtCtrlCabalSynopsis.Bind( wx.EVT_TEXT, self.cabalSynopsisSetHandler )
		self.txtCtrlCabalDescription.Bind( wx.EVT_TEXT, self.cabalDescriptionSetHandler )
		self.txtCtrlCabalDepends.Bind( wx.EVT_TEXT, self.cabalDependsSetHandler )
		self.txtCtrlCabalCategory.Bind( wx.EVT_TEXT, self.cabalCategorySetHandler )
		self.listBoxNamespaces.Bind( wx.EVT_KEY_UP, self.listBoxNamespacesKeyUp )
		self.buttonAddNamespace.Bind( wx.EVT_BUTTON, self.addNamespaceHandler )
		self.treeCtrlHeaderFiles.Bind( wx.EVT_TREE_ITEM_ACTIVATED, self.treeCtrlHeaderFilesImageActivatedHandler )
		self.treeCtrlHeaderFiles.Bind( wx.EVT_TREE_SEL_CHANGED, self.treeCtrlHeaderFilesLeftUpHandler )
		self.treeCtrlHeaderFiles.Bind( wx.EVT_TREE_STATE_IMAGE_CLICK, self.treeCtrlHeaderFilesImageClickHandler )
		self.m_button13.Bind( wx.EVT_BUTTON, self.buttonAddHeaderToIncludesHandler )
		self.listBoxHeadersToInclude.Bind( wx.EVT_KEY_UP, self.listBoxHeadersToIncludeKeyUp )
		self.m_button16.Bind( wx.EVT_BUTTON, self.rescanDirsHandler )
		self.m_button17.Bind( wx.EVT_BUTTON, self.parseHeadersHandler )
		self.listBoxSourceDirs.Bind( wx.EVT_KEY_UP, self.eventSourceDirsKeyUp )
		self.listBoxSourceDirs.Bind( wx.EVT_LISTBOX, self.listBoxSourceDirsHandler )
		self.buttonAddSourceDir.Bind( wx.EVT_BUTTON, self.eventAddSourceDir )
		self.listBoxBasicTypes.Bind( wx.EVT_KEY_UP, self.listBoxBasicTypesKeyUpHandler )
		self.listBoxBasicTypes.Bind( wx.EVT_LISTBOX, self.listBoxBasicTypesHandler )
		self.buttonAddBasicType.Bind( wx.EVT_BUTTON, self.addBasicTypeHandler )
		self.buttonGenerateDefaultBasicTypes.Bind( wx.EVT_BUTTON, self.generateBasicTypesHandler )
		self.txtCtrlBasicTypeName.Bind( wx.EVT_TEXT, self.basicTypeNameSetHandler )
		self.txtCtrlBasicTypeHType.Bind( wx.EVT_TEXT, self.basicTypeHTypeSetHandler )
		self.txtCtrlBasicTypeComment.Bind( wx.EVT_TEXT, self.txtCtrlBasicTypeCommentHandler )
		self.txtCtrlBasicTypeCType.Bind( wx.EVT_TEXT, self.basicTypeCTypeSetHandler )
		self.txtCtrlBasicTypeInMarshaller.Bind( wx.EVT_TEXT, self.basicTypeInMarshallerSetHandler )
		self.txtCtrlBasicTypeOutMarshaller.Bind( wx.EVT_TEXT, self.basicTypeOutMarshallerSetHandler )
		self.checkBoxBasicTypeString.Bind( wx.EVT_CHECKBOX, self.checkBoxBasicTypeHandler )
		self.txtCtrlStringConstructor.Bind( wx.EVT_TEXT, self.basicTypeStringConstructorHandler )
		self.txtCtrlAsCharPtrMethod.Bind( wx.EVT_TEXT, self.basicTypeAsCharPtrMethodHandler )
		self.listBoxMappedCTypes.Bind( wx.EVT_KEY_UP, self.listBoxMappedCTypesKeyUpHandler )
		self.buttonAddCType.Bind( wx.EVT_BUTTON, self.addCTypeHandler )
		self.listBoxEnums.Bind( wx.EVT_LISTBOX, self.listBoxEnumsHandler )
		self.txtCtrlEnumName.Bind( wx.EVT_TEXT, self.basicTypeNameSetHandler )
		self.txtCtrlEnumNameSuffix.Bind( wx.EVT_TEXT, self.enumNameSuffixHandler )
		self.txtCtrlEnumValuePrefix.Bind( wx.EVT_TEXT, self.enumValuePrefixHandler )
		self.checkBoxEnumSelected.Bind( wx.EVT_CHECKBOX, self.checkboxEnumSelectedHandler )
		self.txtCtrlEnumComment.Bind( wx.EVT_TEXT, self.enumCommentHandler )
		self.listBoxStructs.Bind( wx.EVT_LISTBOX, self.listBoxStructsHandler )
		self.txtCtrlStructName.Bind( wx.EVT_TEXT, self.basicTypeNameSetHandler )
		self.txtCtrlStructHName.Bind( wx.EVT_TEXT, self.structHNameSetHandler )
		self.txtCtrlStructValuePrefix.Bind( wx.EVT_TEXT, self.structValuePrefixSetHandler )
		self.txtCtrlStructStructName.Bind( wx.EVT_TEXT, self.structStructNameSetHandler )
		self.checkBoxStructSelected.Bind( wx.EVT_CHECKBOX, self.structSelectedHandler )
		self.txtCtrlStructComment.Bind( wx.EVT_TEXT, self.structCommentHandler )
		self.txtCtrlStructCTypes.Bind( wx.EVT_TEXT, self.structCTypesSetHandler )
		self.txtCtrlStructHeaderFile.Bind( wx.EVT_TEXT, self.structHeaderFileHandler )
		self.txtCtrlStructChsFile.Bind( wx.EVT_TEXT, self.stuctChsFileHandler )
		self.listBoxClasses.Bind( wx.EVT_LISTBOX, self.listBoxClassesHandler )
		self.txtCtrlClassHName.Bind( wx.EVT_TEXT, self.classHNameSetHandler )
		self.txtCtrlClassPrefix.Bind( wx.EVT_TEXT, self.classPrefixSetHandler )
		self.checkBoxClassSelected.Bind( wx.EVT_CHECKBOX, self.classSelectedHandler )
		self.txtCtrlClassComment.Bind( wx.EVT_TEXT, self.classCommentHandler )
		self.listBoxClassMethods.Bind( wx.EVT_LISTBOX, self.listBoxClassMethodsHandler )
		self.checkBoxClassMethodExcluded.Bind( wx.EVT_CHECKBOX, self.classMethodExcludedHandler )
		self.Bind( wx.EVT_MENU, self.projectNewHandler, id = self.menuItemNew.GetId() )
		self.Bind( wx.EVT_MENU, self.openProjectHandler, id = self.menuItemProjectOpen.GetId() )
		self.Bind( wx.EVT_MENU, self.saveProjectHandler, id = self.menuItemProjectSave.GetId() )
		self.Bind( wx.EVT_MENU, self.saveAsProjectHandler, id = self.menuItemSaveAs.GetId() )
		self.Bind( wx.EVT_MENU, self.exitProgramHandler, id = self.menuItemExit.GetId() )
	
	def __del__( self ):
		pass
	
	
	# Virtual event handlers, overide them in your derived class
	def nbPageChangedHandler( self, event ):
		event.Skip()
	
	def projectNameSetHandler( self, event ):
		event.Skip()
	
	def projectParentLabelSetHandler( self, event ):
		event.Skip()
	
	def baseDirSetHandler( self, event ):
		event.Skip()
	
	def selectBaseDirectoryHandler( self, event ):
		event.Skip()
	
	def buildDirSetHandler( self, event ):
		event.Skip()
	
	def selectBuildDirectoryHandler( self, event ):
		event.Skip()
	
	def listHeaderDirDeleteHandler( self, event ):
		event.Skip()
	
	def listLibDirDeleteHandler( self, event ):
		event.Skip()
	
	def addHeaderDirHandler( self, event ):
		event.Skip()
	
	def addLibDirHandler( self, event ):
		event.Skip()
	
	def listBoxLibraryDllsKeyUp( self, event ):
		event.Skip()
	
	def listBoxLibraryLibsKeyUp( self, event ):
		event.Skip()
	
	def addLibraryDllHandler( self, event ):
		event.Skip()
	
	def addLibraryLibHandler( self, event ):
		event.Skip()
	
	def moduleTitleSetHandler( self, event ):
		event.Skip()
	
	def subModuleSetHandler( self, event ):
		event.Skip()
	
	def cabalNameSetHandler( self, event ):
		event.Skip()
	
	def moduleVersionSetHandler( self, event ):
		event.Skip()
	
	def cabalUrlSetHandler( self, event ):
		event.Skip()
	
	def cabalSynopsisSetHandler( self, event ):
		event.Skip()
	
	def cabalDescriptionSetHandler( self, event ):
		event.Skip()
	
	def cabalDependsSetHandler( self, event ):
		event.Skip()
	
	def cabalCategorySetHandler( self, event ):
		event.Skip()
	
	def listBoxNamespacesKeyUp( self, event ):
		event.Skip()
	
	def addNamespaceHandler( self, event ):
		event.Skip()
	
	def treeCtrlHeaderFilesImageActivatedHandler( self, event ):
		event.Skip()
	
	def treeCtrlHeaderFilesLeftUpHandler( self, event ):
		event.Skip()
	
	def treeCtrlHeaderFilesImageClickHandler( self, event ):
		event.Skip()
	
	def buttonAddHeaderToIncludesHandler( self, event ):
		event.Skip()
	
	def listBoxHeadersToIncludeKeyUp( self, event ):
		event.Skip()
	
	def rescanDirsHandler( self, event ):
		event.Skip()
	
	def parseHeadersHandler( self, event ):
		event.Skip()
	
	def eventSourceDirsKeyUp( self, event ):
		event.Skip()
	
	def listBoxSourceDirsHandler( self, event ):
		event.Skip()
	
	def eventAddSourceDir( self, event ):
		event.Skip()
	
	def listBoxBasicTypesKeyUpHandler( self, event ):
		event.Skip()
	
	def listBoxBasicTypesHandler( self, event ):
		event.Skip()
	
	def addBasicTypeHandler( self, event ):
		event.Skip()
	
	def generateBasicTypesHandler( self, event ):
		event.Skip()
	
	def basicTypeNameSetHandler( self, event ):
		event.Skip()
	
	def basicTypeHTypeSetHandler( self, event ):
		event.Skip()
	
	def txtCtrlBasicTypeCommentHandler( self, event ):
		event.Skip()
	
	def basicTypeCTypeSetHandler( self, event ):
		event.Skip()
	
	def basicTypeInMarshallerSetHandler( self, event ):
		event.Skip()
	
	def basicTypeOutMarshallerSetHandler( self, event ):
		event.Skip()
	
	def checkBoxBasicTypeHandler( self, event ):
		event.Skip()
	
	def basicTypeStringConstructorHandler( self, event ):
		event.Skip()
	
	def basicTypeAsCharPtrMethodHandler( self, event ):
		event.Skip()
	
	def listBoxMappedCTypesKeyUpHandler( self, event ):
		event.Skip()
	
	def addCTypeHandler( self, event ):
		event.Skip()
	
	def listBoxEnumsHandler( self, event ):
		event.Skip()
	
	
	def enumNameSuffixHandler( self, event ):
		event.Skip()
	
	def enumValuePrefixHandler( self, event ):
		event.Skip()
	
	def checkboxEnumSelectedHandler( self, event ):
		event.Skip()
	
	def enumCommentHandler( self, event ):
		event.Skip()
	
	def listBoxStructsHandler( self, event ):
		event.Skip()
	
	
	def structHNameSetHandler( self, event ):
		event.Skip()
	
	def structValuePrefixSetHandler( self, event ):
		event.Skip()
	
	def structStructNameSetHandler( self, event ):
		event.Skip()
	
	def structSelectedHandler( self, event ):
		event.Skip()
	
	def structCommentHandler( self, event ):
		event.Skip()
	
	def structCTypesSetHandler( self, event ):
		event.Skip()
	
	def structHeaderFileHandler( self, event ):
		event.Skip()
	
	def stuctChsFileHandler( self, event ):
		event.Skip()
	
	def listBoxClassesHandler( self, event ):
		event.Skip()
	
	def classHNameSetHandler( self, event ):
		event.Skip()
	
	def classPrefixSetHandler( self, event ):
		event.Skip()
	
	def classSelectedHandler( self, event ):
		event.Skip()
	
	def classCommentHandler( self, event ):
		event.Skip()
	
	def listBoxClassMethodsHandler( self, event ):
		event.Skip()
	
	def classMethodExcludedHandler( self, event ):
		event.Skip()
	
	def projectNewHandler( self, event ):
		event.Skip()
	
	def openProjectHandler( self, event ):
		event.Skip()
	
	def saveProjectHandler( self, event ):
		event.Skip()
	
	def saveAsProjectHandler( self, event ):
		event.Skip()
	
	def exitProgramHandler( self, event ):
		event.Skip()
	

###########################################################################
## Class DialogCreateBasicType
###########################################################################

class DialogCreateBasicType ( wx.Dialog ):
	
	def __init__( self, parent ):
		wx.Dialog.__init__ ( self, parent, id = wx.ID_ANY, title = u"Create Basic Type", pos = wx.DefaultPosition, size = wx.Size( 416,168 ), style = wx.DEFAULT_DIALOG_STYLE )
		
		self.SetSizeHintsSz( wx.DefaultSize, wx.DefaultSize )
		
		fgSizer10 = wx.FlexGridSizer( 3, 4, 0, 0 )
		fgSizer10.SetFlexibleDirection( wx.BOTH )
		fgSizer10.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_panel12 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel12, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel13 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel13, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel14 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel14, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel15 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel15, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel16 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel16, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText22 = wx.StaticText( self, wx.ID_ANY, u"Basic Type Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText22.Wrap( -1 )
		fgSizer10.Add( self.m_staticText22, 0, wx.ALL, 5 )
		
		self.txtCtrlTypeName = wx.TextCtrl( self, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer10.Add( self.txtCtrlTypeName, 0, wx.ALL, 5 )
		
		self.m_panel17 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel17, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel18 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel18, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel19 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel19, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel20 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		bSizer7 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.m_button9 = wx.Button( self.m_panel20, wx.ID_ANY, u"Ok", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer7.Add( self.m_button9, 0, wx.ALL, 5 )
		
		self.m_button10 = wx.Button( self.m_panel20, wx.ID_ANY, u"Cancel", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer7.Add( self.m_button10, 0, wx.ALL, 5 )
		
		
		self.m_panel20.SetSizer( bSizer7 )
		self.m_panel20.Layout()
		bSizer7.Fit( self.m_panel20 )
		fgSizer10.Add( self.m_panel20, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel21 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel21, 1, wx.EXPAND |wx.ALL, 5 )
		
		
		self.SetSizer( fgSizer10 )
		self.Layout()
		
		self.Centre( wx.BOTH )
		
		# Connect Events
		self.m_button9.Bind( wx.EVT_BUTTON, self.okHandler )
		self.m_button10.Bind( wx.EVT_BUTTON, self.cancelHandler )
	
	def __del__( self ):
		pass
	
	
	# Virtual event handlers, overide them in your derived class
	def okHandler( self, event ):
		event.Skip()
	
	def cancelHandler( self, event ):
		event.Skip()
	

###########################################################################
## Class DialogAddCTypeMapping
###########################################################################

class DialogAddCTypeMapping ( wx.Dialog ):
	
	def __init__( self, parent ):
		wx.Dialog.__init__ ( self, parent, id = wx.ID_ANY, title = u"Add C-Type Mapping", pos = wx.DefaultPosition, size = wx.Size( 416,168 ), style = wx.DEFAULT_DIALOG_STYLE )
		
		self.SetSizeHintsSz( wx.DefaultSize, wx.DefaultSize )
		
		fgSizer10 = wx.FlexGridSizer( 3, 4, 0, 0 )
		fgSizer10.SetFlexibleDirection( wx.BOTH )
		fgSizer10.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_panel12 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel12, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel13 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel13, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel14 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel14, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel15 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel15, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel16 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel16, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText22 = wx.StaticText( self, wx.ID_ANY, u"C-Type Name", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText22.Wrap( -1 )
		fgSizer10.Add( self.m_staticText22, 0, wx.ALL, 5 )
		
		self.txtCtrlTypeName = wx.TextCtrl( self, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer10.Add( self.txtCtrlTypeName, 0, wx.ALL, 5 )
		
		self.m_panel17 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel17, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel18 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel18, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel19 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel19, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel20 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		bSizer7 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.m_button9 = wx.Button( self.m_panel20, wx.ID_ANY, u"Ok", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer7.Add( self.m_button9, 0, wx.ALL, 5 )
		
		self.m_button10 = wx.Button( self.m_panel20, wx.ID_ANY, u"Cancel", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer7.Add( self.m_button10, 0, wx.ALL, 5 )
		
		
		self.m_panel20.SetSizer( bSizer7 )
		self.m_panel20.Layout()
		bSizer7.Fit( self.m_panel20 )
		fgSizer10.Add( self.m_panel20, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel21 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel21, 1, wx.EXPAND |wx.ALL, 5 )
		
		
		self.SetSizer( fgSizer10 )
		self.Layout()
		
		self.Centre( wx.BOTH )
		
		# Connect Events
		self.m_button9.Bind( wx.EVT_BUTTON, self.okHandler )
		self.m_button10.Bind( wx.EVT_BUTTON, self.cancelHandler )
	
	def __del__( self ):
		pass
	
	
	# Virtual event handlers, overide them in your derived class
	def okHandler( self, event ):
		event.Skip()
	
	def cancelHandler( self, event ):
		event.Skip()
	

###########################################################################
## Class DialogAddNamespace
###########################################################################

class DialogAddNamespace ( wx.Dialog ):
	
	def __init__( self, parent ):
		wx.Dialog.__init__ ( self, parent, id = wx.ID_ANY, title = u"Add Namespace", pos = wx.DefaultPosition, size = wx.Size( 416,168 ), style = wx.DEFAULT_DIALOG_STYLE )
		
		self.SetSizeHintsSz( wx.DefaultSize, wx.DefaultSize )
		
		fgSizer10 = wx.FlexGridSizer( 3, 4, 0, 0 )
		fgSizer10.SetFlexibleDirection( wx.BOTH )
		fgSizer10.SetNonFlexibleGrowMode( wx.FLEX_GROWMODE_SPECIFIED )
		
		self.m_panel12 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel12, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel13 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel13, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel14 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel14, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel15 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel15, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel16 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel16, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_staticText22 = wx.StaticText( self, wx.ID_ANY, u"Namespace", wx.DefaultPosition, wx.DefaultSize, 0 )
		self.m_staticText22.Wrap( -1 )
		fgSizer10.Add( self.m_staticText22, 0, wx.ALL, 5 )
		
		self.txtCtrlNamespace = wx.TextCtrl( self, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.Size( 200,-1 ), 0 )
		fgSizer10.Add( self.txtCtrlNamespace, 0, wx.ALL, 5 )
		
		self.m_panel17 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel17, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel18 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel18, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel19 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel19, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel20 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		bSizer7 = wx.BoxSizer( wx.HORIZONTAL )
		
		self.m_button9 = wx.Button( self.m_panel20, wx.ID_ANY, u"Ok", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer7.Add( self.m_button9, 0, wx.ALL, 5 )
		
		self.m_button10 = wx.Button( self.m_panel20, wx.ID_ANY, u"Cancel", wx.DefaultPosition, wx.DefaultSize, 0 )
		bSizer7.Add( self.m_button10, 0, wx.ALL, 5 )
		
		
		self.m_panel20.SetSizer( bSizer7 )
		self.m_panel20.Layout()
		bSizer7.Fit( self.m_panel20 )
		fgSizer10.Add( self.m_panel20, 1, wx.EXPAND |wx.ALL, 5 )
		
		self.m_panel21 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.TAB_TRAVERSAL )
		fgSizer10.Add( self.m_panel21, 1, wx.EXPAND |wx.ALL, 5 )
		
		
		self.SetSizer( fgSizer10 )
		self.Layout()
		
		self.Centre( wx.BOTH )
		
		# Connect Events
		self.m_button9.Bind( wx.EVT_BUTTON, self.okHandler )
		self.m_button10.Bind( wx.EVT_BUTTON, self.cancelHandler )
	
	def __del__( self ):
		pass
	
	
	# Virtual event handlers, overide them in your derived class
	def okHandler( self, event ):
		event.Skip()
	
	def cancelHandler( self, event ):
		event.Skip()
	

