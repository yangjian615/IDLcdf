; docformat = 'rst'
;
; NAME:
;       MrCDF_Browser__DEFINE
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
;   All rights reserved.                                                                 ;
;                                                                                        ;
;   Redistribution and use in source and binary forms, with or without modification,     ;
;   are permitted provided that the following conditions are met:                        ;
;                                                                                        ;
;       * Redistributions of source code must retain the above copyright notice,         ;
;         this list of conditions and the following disclaimer.                          ;
;       * Redistributions in binary form must reproduce the above copyright notice,      ;
;         this list of conditions and the following disclaimer in the documentation      ;
;         and/or other materials provided with the distribution.                         ;
;       * Neither the name of the <ORGANIZATION> nor the names of its contributors may   ;
;         be used to endorse or promote products derived from this software without      ;
;         specific prior written permission.                                             ;
;                                                                                        ;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY  ;
;   EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES ;
;   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT  ;
;   SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,       ;
;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED ;
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR   ;
;   BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN     ;
;   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN   ;
;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  ;
;   DAMAGE.                                                                              ;
;*****************************************************************************************
;
; PURPOSE:
;+
;       A class for selecting CDF files and variables from within those files.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Categories:
;       CDF Utilities, File I/O, Data Reader
;
; :History:
;   Modification History::
;       02/09/2015  -   Written by Matthew Argall
;       2015/02/02  -   Added the GetSelect method. Renamed ::Quit to ::Cancel. Added
;                           the BLOCK keyword. All UVALUES now contain the same
;                           information. Added the CANCEL and SELECT_* object properties
;                           and the GetProperty method. ::Cancel does not destroy the
;                           object, in case we are in ::Init. - MRA
;       2015/08/15  -   Renamed GetSelect to GetSelection. Added the BOUDNS text box
;                           to the GUI and incoproated it into Import. File and variable
;                           nodes are initially expanded. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to create a GUI from which a CDF Variable can be
;   selected.
;
; :Params:
;       GROUP_LEADER:       in, optional, type=int
;                           The widget ID of an existing widget that serves as group
;                               leader for the newly-created widget.
;
; :Keywords:
;       BLOCK:          in, optional, type=boolean, default=0
;                       If set, the gui will block the command line.
;-
pro MrCDF_Browser::Create_GUI, group_leader, $
BLOCK=block
	compile_opt idl2
	on_error, 2
	
	;
	; General layout:
	;   ___________________________
	;   | TREE   |  TEXT          |
	;   |        |  LABEL         |
	;   |        |  BUTTON TEXT   |
	;   |        |  LABEL  TEXT   |
	;   |   OK  CANCEL            |
	;   |_________________________|
	;

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------

	;Make a top-level base with or without a groupleader. cdf_read_gui2 is called by other
	;blocking widgets, so if a group_leader is given, then make cdf_read_gui2 modal.
	if n_elements(group_leader) eq 0 then begin
		no_block = ~keyword_set(block)
		self.tlb = widget_base(TITLE='Read CDF', /COLUMN, XOFFSET=200, YOFFSET=100, $
		                       UNAME='tlb', /BASE_ALIGN_CENTER)
	endif else begin
		no_block = 0
		self.tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Read CDF', /COLUMN, $
		                       XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
		                       /MODAL)
	endelse

;---------------------------------------------------------------------
;Create Tree Root ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Put a label above the tree saying how many data variables are shown and how
	;many other variables are shown/hidden.
	treeBase  = widget_base(self.tlb, ROW=1)

	;Adjust the width of the widget to fit the width of the longest variable name
	vnames = self.oCDF -> GetVarNames(COUNT=nVars)
	varlen = max(strlen(vnames) + 10)
	
	;Determine the height and width
	ysize = (40 > nVars)  * !d.y_ch_size
	xsize = (30 > 10+varlen) * !d.x_ch_size

	;Create the root of the widget tree.
	self.oCDF -> GetProperty, FILENAME=filename
	self.treeID = widget_tree(treeBase, /FOLDER, /EXPANDED, $
	                          VALUE=filename, UNAME='TREEROOT', $
	                          XSIZE=xsize, YSIZE=ysize)

;---------------------------------------------------------------------
;Create Info Box /////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Create a column base
	infoBase = widget_base(treeBase, COLUMN=1)
	
	;Put an info box next to the tree
	infoID = widget_text(infoBase, /WRAP, UNAME='InfoBox', FRAME=2, /SCROLL, $
	                     XSIZE=(60 > 2*varlen), YSIZE=nVars)
	
	;Create a label
	labelID = widget_label(infoBase, UNAME='VARLABEL', VALUE='Variable Name for Import:')
	
	;Create import button and variable name box
	importID = widget_base(infobase, ROW=1)
	buttonID = widget_button(importID, VALUE='Import', UNAME='IMPORT', $
	                         UVALUE={object: self, method: 'Import_Data'}) 
	textID   = widget_text(importID, UNAME='VARNAME', VALUE='', /EDITABLE)
	
	;Create "Record Bound" and "Depend Bound" boxes
	boundID = widget_base(infobase, ROW=1)
	labelID = widget_label(boundID, UNAME='BOUNDLABEL', VALUE='Bounds', FRAME=3)
	textID  = widget_text(boundID, UNAME='BOUNDS', VALUE='', /EDITABLE, $
	                      /ALL_EVENTS, UVALUE={object: self, method: 'Bounds_Events'})

;---------------------------------------------------------------------
;Create OK and Cancel Buttons ////////////////////////////////////////
;---------------------------------------------------------------------
	okBase = widget_base(self.tlb, ROW=1)
	button = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
	                       UVALUE={object: self, method:'Ok'})
	button = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
	                       UVALUE={object: self, method:'Cancel'})

;---------------------------------------------------------------------
;Validate Pointers and Build the Tree ////////////////////////////////
;---------------------------------------------------------------------
	;Simulate an event to build the tree.
	self -> Create_Tree

;---------------------------------------------------------------------
;Create the State Variable, Realize, and Start Event Handling ////////
;---------------------------------------------------------------------

	;Realize the top-level base
	widget_control, self.tlb, /REALIZE
	
	;Call XMANAGER
	xmanager, 'MrCDF_Browser', self.tlb, cleanup='MrCDF_Browser_Cleanup', $
	          event_handler='MrCDF_Browser_Events', NO_BLOCK=no_block

end


;+
;   The purpose of this method is to manage how the widget tree is populated and to
;   change the widget label text to describe how many variables are shown/hidden.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCDF_Browser::Create_Tree, event
	compile_opt idl2
	on_error, 2

;---------------------------------------------------------------------
;Destroy the Previous Tree Structure /////////////////////////////////
;---------------------------------------------------------------------    
	;Find which widgets have the root as a parent
	nChildren = widget_info(self.treeID, /N_CHILDREN)
	if nChildren gt 0 then begin
		all_children = widget_info(self.treeID, /ALL_CHILDREN)
		for i = 0, nChildren - 1 do widget_control, all_children[i], /DESTROY
	endif

;---------------------------------------------------------------------
;Create the New Tree Structure ///////////////////////////////////////
;---------------------------------------------------------------------
	;Get the Z-variables only.
	varnames   = self.oCDF -> GetVarNames(COUNT=nVars)
	gAttrNames = self.oCDF -> GetAttrNames(COUNT=nGAttrs)

	;Create a node for the file
	self.oCDF -> GetProperty, FILENAME=filename
	rootID = widget_tree(self.treeID, /FOLDER, /EXPANDED, $
	                     UNAME  = 'FILE', $
	                     UVALUE = { object:  self, $
	                                method:  'Tree_Events', $
	                                name:    filename, $
	                                scope:   '', $
	                                type:    'FILE', $
	                                varname: '' $
	                              }, $
	                     VALUE  = 'File')
	
	;Create a node for global attributes and variables
	gAttrID = widget_tree(rootID, /FOLDER, $
	                      UNAME  = 'GATTRNODE', $
	                      UVALUE = { object:  self, $
	                                 method:  'Tree_Events', $
	                                 name:    '', $
	                                 scope:   '', $
	                                 type:    'NONE', $
	                                 varname: '' $
	                               }, $
	                      VALUE  = 'Global Attributes')
	varID   = widget_tree(rootID, /FOLDER, /EXPANDED, $
	                      UNAME  = 'VARNODE', $
	                      UVALUE = { object:  self, $
	                                 method:  'Tree_Events', $
	                                 name:    '', $
	                                 scope:   '', $
	                                 type:    'NONE', $
	                                 varname: '' $
	                               }, $
	                      VALUE  = 'Variables')

	;Create a node for each global attribute and variable
	for i = 0, nGAttrs - 1 do self -> Create_Tree_Attr, gAttrID, gAttrNames[i]
	for i = 0, nVars   - 1 do self -> Create_Tree_Var,  varID,   varnames[i]
end


;+
;   Create a leaf node associated with a CDF attribute. Global attributes will be nestled
;   under the root tree node while variable attributes will be nestled under their
;   respective variable node.
;
;       ROOT
;           Global Attribute
;           Global Attribute
;           Global Attribute
;
;       ROOT
;           Variable
;               Variable Attribute
;               Variable Attribute
;               Variable Attribute
;
; :Params:
;       PARENTID:       in, required, type=long
;                       Widget ID of the parent tree node.
;       ATTRNAME:       in, required, type=string
;                       Name of the attribute.
;
; :Keywords:
;       VARNAME:        in, optional, type=string
;                       If `ATTRNAME` is a variable attribute, then this is
;                           the name of the variable it is associated with.
;-
pro MrCDF_Browser::Create_Tree_Attr, parentID, attrName, $
VARNAME=varname
	compile_opt idl2
	on_error, 2

	;Create the UValue
	uvalue = {object:  self, $
	          method:  'Tree_Events', $
	          name:    attrName, $
	          scope:   'GLOBAL', $
	          type:    'ATTRIBUTE', $
	          varname: ''}
	
	;Variable attribute?
	if n_elements(varname) gt 0 then begin
		uvalue.scope   = 'VARIABLE'
		uvalue.varname = varname
	endif

	;Create a leave node
	branchID = widget_tree(parentID, $
	                       UNAME  = attrName, $
	                       UVALUE = uvalue, $
	                       VALUE  = attrName)
end


;+
;   Create a branch node for a CDF variable
;
;       ROOT
;           Variable
;           Variable
;           Variable
;
; :Params:
;       PARENTID:       in, required, type=long
;                       Widget ID of the parent tree node.
;       VARNAME:        in, required, type=string
;                       The variable name to be attached to the Tree root.
;-
pro MrCDF_Browser::Create_Tree_Var, parentID, varName
	compile_opt idl2
	on_error, 2


	;Create a user value
	uvalue = { object: self, $
	           method:  'Tree_Events', $
	           name:    varName, $
	           scope:   '', $
	           type:    'VARIABLE', $
	           varname: '' $
	         }

	;Create a node under under the tree root
	branchID = widget_tree(parentID, /FOLDER, $
	                       UNAME  = VarName, $
	                       UVALUE = uvalue, $
	                       VALUE  = varName)

	;Get all of the attributes
	tf_has = self.oCDF -> HasVar(varName, OBJECT=oVar)
	attrNames = oVar -> GetAttrNames(COUNT=nAttrs)

	;Set the attribute node
	for i = 0, nAttrs - 1 do self -> Create_Tree_Attr, branchID, attrNames[i], VARNAME=varname
end


;+
;   The purpose of this program is to handle events generated by the "IMPORT" button.
;   Import data to the $MAIN$ level.
;
; :Params:
;       EVENT:                  in, required, type=structure
;                               A button_event structure generated by a button press
;-
pro MrCDF_Browser::Import_Data, event
	compile_opt idl2, hidden
	on_error, 2

	;Determine what was selected by examining the parent
	nodeID = widget_info(self.treeID, /TREE_SELECT)
	widget_control, nodeID, GET_UVALUE=uvalue
	
	case uvalue.type of
		;The "Global Attributes" or "Variables" folders
		'FILE':      data = self.oCDF -> ToStruct()
		'VARIABLE':  data = self.oCDF -> Read(uvalue.name, BOUNDS=self.bounds)
		'ATTRIBUTE': begin
			if uvalue.scope eq 'GLOBAL' $
				then data = self.oCDF -> GetGlobalAttrValue(uvalue.name) $
				else data = self.oCDF -> GetVarAttrValue(uvalue.varname, uvalue.name)
		endcase
		'NONE': return ;Do nothing
		else: message, 'Invalid type: "' + uvalue.type + '".'
	endcase

	;Retrieve the variable name into which the data will be saved.
	nameID = widget_info(event.top, FIND_BY_UNAME='VARNAME')
	widget_control, nameID, GET_VALUE=name
	
	;Ensure a valid IDL variable name
	name = idl_validname(name, /CONVERT_ALL)

	; Create a main-level variable.
	(Scope_VarFetch(name, LEVEL=1, /ENTER)) = data
	Print, 'A variable named "' + name + '" has been created at the main IDL level.'
end


;+
;   The purpose of this program is to handle events generated by the OK button.
;
; :Params:
;       EVENT:                  in, required, type=structure
;                               A button_event structure generated by a button press
;-
pro MrCDF_Browser::Ok, event
	compile_opt idl2, hidden

	;Destroy the widget.
	widget_control, event.top, /destroy
end



;+
;   Event handler for the "Cancel" button.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCDF_Browser::Cancel, event
	compile_opt idl2
	
	;Cancelled
	self.cancel = 1
	
	;Destroy the widget
	widget_control, event.top, /DESTROY
end


;+
;   The purpose of this method is to handle events triggered by the widget tree.
;
; :Params:
;       EVENT:                  in, required, type=structure
;                               A button_event structure generated the windows manager
;-
pro MrCDF_Browser::Tree_Events, event
	compile_opt idl2

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;RETURN -- if this was not a select event
	;       -- if a folder was selected. Updating the labels causes the widget to be
	;           repositioned for some reason.
	if event.type ne 0 then return

;---------------------------------------------------------------------
;Has the Selection Changed? //////////////////////////////////////////
;---------------------------------------------------------------------

	;Find the parent ID
	uname = widget_info(event.id, /UNAME)
	if uname eq self.selection then return
	
	;Set the new selection
	widget_control, event.id, GET_UVALUE=uvalue
	self.select_uname   = uname
	self.select_name    = uvalue.name
	self.select_type    = uvalue.type
	self.select_scope   = uvalue.scope
	self.select_varname = uvalue.varname

;---------------------------------------------------------------------
;Update Text Widget //////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	case uvalue.type of
		'FILE':      self -> GetInfo_File
		'ATTRIBUTE': self -> GetInfo_Attr, uvalue.name, VARNAME=uvalue.varname
		'VARIABLE':  self -> GetInfo_Var,  uvalue.name
		'NONE':      return ;Do nothing.
		else: message, 'Unknown tree event type "' + uvalue.type + '".'
	endcase

;---------------------------------------------------------------------
;Update Text Widget //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Update the variable name field
	nameID = widget_info(event.top, FIND_BY_UNAME='VARNAME')
	widget_control, nameID, SET_VALUE=uname
end


;+
;   Event handler for the Bounds text box.
;
; :Params:
;       EVENT:      in, required, type=structure
;                   An event structure returned by the windows manager.
;-
pro MrCDF_Browser::Bounds_Events, event
	compile_opt idl2
	on_error, 2
	
	;Insert single (0) or multiple (1) characters, delete characters (2), or highlight (3)
	case event.type of
		0: self.bounds = strmid(self.bounds, 0, event.offset-1) + string(event.ch) + strmid(self.bounds, event.offset-1)
		1: self.bounds = strmid(self.bounds, 0, event.offset-1) + event.str        + strmid(self.bounds, event.offset-1)
		2: self.bounds = strmid(self.bounds, 0, event.offset)                      + strmid(self.bounds, event.offset + event.length)
		3: ;Do nothing for highlighting
		else: message, 'Invalid event type.'
	endcase
end


;+
;   Obtain information about an attribute so that it can be displayed in the test widget.
;
; :Params:
;       ATTRNAME:       in, required, type=structure
;                       Name of the attribute for which information is wanted.
;-
pro MrCDF_Browser::GetInfo_Attr, attrName, $
VARNAME=varname
	compile_opt idl2

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Default
	if n_elements(varname) eq 0 then varname = ''

	;Newline character
	if (!d.name eq 'WIN') $
		then newline = string([13B, 10B]) $
		else newline = string(10B)

;---------------------------------------------------------------------
; Attribute Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
	tf_has = self.oCDF -> HasAttr(attrName, OBJECT=oAttr)
	oAttr -> GetProperty, SCOPE=scope
	if varname eq '' $
		then data = self.oCDF -> GetGlobalAttrValue(attrName, CDF_TYPE=cdf_type) $
		else data = self.oCDF -> GetVarAttrValue(varname, attrName, CDF_TYPE=cdf_type)
	
	;Number of data values
	nData = n_elements(data) 
	iMax  = (nData - 1) < 20

	info = ['Attribute:   "' + attrName + '"', $
	        'Scope:       ' + scope, $
	        'CDF Type:    ' + cdf_type, $
	        '# Elements:  ' + strtrim(nData, 2), $
	        'Data:']
;---------------------------------------------------------------------
; Format the Data ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	case 1 of
		MrIsA(data, /INTEGER): fmt = '(i0)'
		MrIsA(data, /REAL):    fmt = '(f0)'
		MrIsA(data, 'STRING'): fmt = '(a0)'
		else: message, 'Unexpected data type "' + size(data, /TNAME), '".'
	endcase
	
	;Join data into a string
	if iMax gt 0 $
		then dataStr = '[' + strjoin(string(data[0:iMax], FORMAT=fmt), ', ') + ']' $
		else dataStr = string(data[0:iMax], FORMAT=fmt)
;---------------------------------------------------------------------
; Update Text Box ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Separate elements with a new line
	info = strjoin([info, dataStr], newline)
	
	;Update the info box.
	infoID = widget_info(self.tlb, FIND_BY_UNAME='InfoBox')
	widget_control, infoID, SET_VALUE=info
end


;+
;   Get information about the CDF file so that it can be displayed in the text widget.
;-
pro MrCDF_Browser::GetInfo_File
	compile_opt idl2

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Newline character
	if (!d.name eq 'WIN') $
		then newline = string([13B, 10B]) $
		else newline = string(10B)

;---------------------------------------------------------------------
; Attribute Properties ///////////////////////////////////////////////
;---------------------------------------------------------------------
	self.oCDF -> GetProperty, FILENAME = filename, $
	                          COMPRESSION = compression, $
	                          ENCODING    = encoding, $
	                          DECODING    = decoding, $
	                          GZIP_LEVEL  = gzip_level, $
	                          MAJORITY    = majority, $
	                          NGATTRS     = nGAttrs, $
	                          NVATTRS     = nVAttrs, $
	                          NRVARS      = nRVars, $
	                          NZVARS      = nZVars

	info = [ 'DIRECTORY:', $
	         file_dirname(filename), $
	         '', $
	         'FILENAME:', $
	         file_basename(filename), $
	         '', $
	         'COMPRESSION:', $
	         '    ' + compression + ( n_elements(gzip_level) eq 0 ? '' : ' at level ' + strtrim(gzip_level, 2) ), $
	         '', $
	         'ENCODING/DECODING', $
	         '    ' + encoding + ' / ' + decoding, $
	         '', $
	         'MAJORITY', $
	         '    ' + majority, $
	         '', $
	         '# Global Attributes:      ' + strtrim(nGAttrs, 2), $
	         '# Variable Attributes:    ' + strtrim(nVAttrs, 2), $
	         '# R-Variables:            ' + strtrim(nRVars, 2), $
	         '# Z-Variables:            ' + strtrim(nZVars, 2) $
	       ]

;---------------------------------------------------------------------
; Update Text Box ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Update the info box.
	infoID = widget_info(self.tlb, FIND_BY_UNAME='InfoBox')
	widget_control, infoID, SET_VALUE=strjoin(info, newline)
end


;+
;   Obtain information about a variable so that it can be displayed in the test widget.
;
; :Params:
;       VARNAME:        in, required, type=structure
;                       Name of the variable for which information is wanted.
;-
pro MrCDF_Browser::GetInfo_Var, varname
	compile_opt idl2

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Newline character
	if (!d.name eq 'WIN') $
		then newline = string([13B, 10B]) $
		else newline = string(10B)

;---------------------------------------------------------------------
; Variable Properties ////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get the variable and its info.
	tf_has = self.oCDF -> HasVar(varname, OBJECT=oVar)
	oVar  -> GetProperty, NAME=name, CDF_TYPE=cdf_type, $
	                      DIMENSIONS=dimensions, MAXREC=maxrec, $
	                      COMPRESSION=compression, GZIP_LEVEL=gzip_level

	;Size of the variable
	if n_elements(dimensions) eq 0 then dimensions = 0
	if n_elements(dimensions) eq 1 && dimensions eq 0 then dimensions = 1
	var_size = '[' + strjoin(strtrim(dimensions, 2), ', ') + ', ' + strtrim(maxrec, 2) + ']'
	
	;Data preview
	iMax = (product(dimensions) * maxrec) < 20
	data = oVar -> GetValue(REC_COUNT=iMax)

	;Combine the info into an array
	info = ['Variable:     '  + name, $
	        'CDF Type:     '  + cdf_type, $
	        'Size:         '  + var_size, $
	        'Compression:  '  + compression + (n_elements(gzip_level) eq 0 ? '' : ' at level ' + strtrim(gzip_level, 2) ), $
	        'Data:']

;---------------------------------------------------------------------
; Format the Data ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Check if the variable has the "FORMAT" attribute. If it does, use it.
	if self.oCDF -> VarHasAttr(varname, 'FORMAT') then begin
		fmt = '(' + self.oCDF -> GetVarAttrValue(varname, 'FORMAT') + ')'
	;Otherwise, pick a generic format
	endif else begin
		case 1 of
			MrIsA(data, /INTEGER): fmt = '(i0)'
			MrIsA(data, /REAL):    fmt = '(f0)'
			MrIsA(data, 'STRING'): fmt = '(a0)'
			else: message, 'Unexpected data type "' + size(data, /TNAME), '".'
		endcase
	endelse

	;Join data into a string
	if iMax gt 0 $
		then dataStr = '[' + strjoin(string(data[0:iMax-1], FORMAT=fmt), ', ') + ']' $
		else dataStr = string(data[0], FORMAT=fmt)

;---------------------------------------------------------------------
; Update Text Box ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	
	;Separate elements with a new line
	info = strjoin([info, dataStr], newline)
	
	;Update the info box.
	infoID = widget_info(self.tlb, FIND_BY_UNAME='InfoBox')
	widget_control, infoID, SET_VALUE=info
end


;+
;   Retrieve object properties.
;
; :Keywords:
;       CANCEL:         out, optional, type=boolean
;                       Indicates that the GUI was cancelled.
;-
function MrCDF_Browser::GetProperty, $
CANCEL=cancel
	compile_opt idl2
	on_error, 2

	;Get object properties
	if arg_present(cancel) then cancel = self.cancel
end


;+
;   Retrieve information about the selected item.
;
; :Keywords:
;       SCOPE:          out, optional, type=string
;                       A named variable to hold the attribute scope. Options are either
;                           "VARIABLE" or "GLOBAL". If `TYPE` is not 'Attribute', an empty
;                           string is returned.
;       TYPE:           out, optional, type=string
;                       A named variable to hold the type of item selected. Options are::
;                           "FILE"
;                           "VARIABLE"
;                           "ATTRIBUTE"
;       VARNAME:        out, optional, type=string
;                       A named variable to hold the variable name associated with a
;                           variable attribute. If the selected item is not a variable
;                           attribute (see `TYPE` and `SCOPE`), the empty string is returned.
;
; :Returns:
;       NAME:           Name of the selected item.
;-
function MrCDF_Browser::GetSelection, $
BOUNDS=bounds, $
TYPE=type, $
SCOPE=scope, $
VARNAME=varname
	compile_opt idl2
	on_error, 2

	;Is anything selected?
	if self.select_name eq '' then message, 'Nothing has been selected.'
	
	;Retrieve the information
	name    = self.select_name
	type    = self.select_type
	scope   = self.select_scope
	varname = self.select_varname
	bounds  = self.bounds
	
	;Return the name of the selected item.
	return, name
end


;+
;   Foward events to the proper event-handling method.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCDF_Browser_Events, event
	compile_opt idl2

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Get the user value and call the appropriate event handling method.
	widget_control, event.id, GET_UVALUE=event_handler

	;Handle Scrool Wheel events differently
	call_method, event_handler.method, event_handler.object, event
end


;+
;   The purpose of this program is to clean up after the widget is destroyed. MrCDF_Browser
;   object itself will not be destroyed.
;-
pro MrCDF_Browser_Cleanup, tlb
	;GUI pointers will be destroyed when the MrCDF_Browser object is destroyed
end


;+
;   The purpose of this method is to destroy the object.
;
; :Params:
;       EVENT:          in, required, type=structure
;                       A event structure generated the windows manager.
;-
pro MrCDF_Browser::Destroy, event
	obj_destroy, self
end


;+
;   The purpose of this method is to clean up after the object is destroyed. If a MrCDF_Browser
;   GUI is open, it will also be destroyed.
;-
pro MrCDF_Browser::Cleanup
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return
	endif

	;Destroy the widget, if it exists
	isValid = widget_info(self.tlb, /VALID_ID)
	if isValid then widget_control, self.tlb, /DESTROY
end


;+
;   The purpose of this method is to initialize the CDF_Read object reference.
;
; :Params:
;       thisCDF:        in, optional, type=string/object
;                       Either the file name of the CDF data file from which data is to
;                           be opened and plotted, or a valid CDF_Read object reference.
;                           If not provided, a file selection dialog box will appear.
;
; :Keywords:
;       BLOCK:          in, optional, type=boolean, default=0
;                       If set, the gui will block the command line.
;       GROUP_LEADER:   in, optional, type=integer
;                       The group leader of the file selection gui. Use only when
;                           `FILENAME` undefined. Automatically sets `BLOCK`=1
;       _REF_EXTRA:     in, optional, type=any
;                       All keyword accepted by the CDF_READ::SET_CONFIG method are also
;                           accepted for keyword inheritance.
;
; :Returns:
;       object reference
;-
function MrCDF_Browser::Init, thisCDF, $
GROUP_LEADER = group_leader, $
BLOCK = block, $
_REF_EXTRA = extra
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		MrPrintF, 'LogErr'
		return, 0
	endif

	cdf_type   = size(thisCDF, /TNAME)

;---------------------------------------------------------------------
;OBJECT REFERENCE ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if cdf_type eq 'OBJREF' then begin
		;Object must be a valid MrCDF_File object.
		if obj_valid(thisCDF) eq 0 $
			then message, 'thisCDF is not a valid object reference.' $
			else if obj_class(thisCDF) ne 'MRCDF_FILE' then message, 'thisCDF must be a MrCDF_File object.'
		
		oCDF = thisCDF
;---------------------------------------------------------------------
;FILENAME ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if cdf_type eq 'STRING' then begin
		oCDF = MrCDF_File(thisCDF)
		if obj_valid(oCDF) eq 0 then return, 0
;---------------------------------------------------------------------
;SELECT FILE /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		oCDF = MrCDF_File(_EXTRA=extra)
		if obj_valid(oCDF) eq 0 then return, 0
	endelse
	
	self.oCDF = oCDF
	
	;Realize the GUI
	self -> Create_GUI, group_leader, BLOCK=block
	if self.cancel then return, 0
	
	return, 1
end


;+
;   Define the MrCDF_Browser class
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;
; :Fields:
;       CANCEL:             Indicates that the cancel button was pressed.
;       OCDF:               An object reference to a CDF_Read object
;       SELECTION:          Name of the Attribute or Variable that is currently selected.
;       TLB:                Top level base of the file- and variable-selection widgets
;       TREEID:             Widget ID of the wTree root
;-
pro MrCDF_Browser__define, class
	compile_opt idl2
	
	class = { MrCDF_Browser, $
	          tlb:            0L, $                ;Top Level Base
	          oCDF:           obj_new(), $         ;An object reference to a CDF_Read object
	          bounds:         '', $
	          cancel:         0B, $
	          selection:      '', $
	          select_name:    '', $
	          select_uname:   '', $
	          select_type:    '', $
	          select_scope:   '', $
	          select_varname: '', $
	          treeID:         0L $
	        }
end