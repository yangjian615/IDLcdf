; docformat = 'rst'
;
; NAME:
;       MrCDF_Browser__DEFINE
;
;*****************************************************************************************
;   Copyright (c) 2013, Matthew Argall                                                   ;
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
;		University of New Hampshire
;		Morse Hall, Room 113
;       8 College Rd.
;		Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Categories:
;
;       CDF Utilities, File I/O, Data Reader
;
; :History:
;   Modification History::
;       08/08/2012  -   Written by Matthew Argall
;       09/15/2013  -   Depend 3 variables were not being placed in the widget tree. Fixed.
;                           Because this is a blocking widget, attempts to destroy the
;                           object always occur within the INIT method, causing an error.
;                           To subvert this, a CANCEL property was added and is checked
;                           within INIT after the widget has been realized and the
;                           XManager has returned. Banners and lists are updating properly.
;                           - MRA
;-
;*****************************************************************************************
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
;       ATTRNAME:       in, required, type=string
;                       Name of the attribute.
;       OPARENT:        in, required, type=object
;                       Parent object of the attribute, either the CDF file object or
;                           one of its child variable objects.
;       PARENTID:       in, required, type=long
;                       Widget ID of the tree node associated with `OPARENT`.
;-
pro MrCDF_Browser::Create_AttrNode, attrName, oParent, parentID
	compile_opt idl2
	on_error, 2

	;Get the variable object
	oAttr = oParent[attrName]

	;Create a node under under the tree root
	;   - Note that the parent becomes the child
	;   - Instead of searching through the variable's attributes
	;   - The attributes object's GetVarAttrValue method takes the name of a variable.
	branchID = widget_tree(parentID, $
	                       UNAME  = attrName, $
	                       UVALUE = { object: self, $
	                                  method: 'Tree_Events', $
	                                  parent: oParent, $
	                                  name:   attrName $
	                                }, $
	                       VALUE  = attrName)
end


;+
;   The purpose of this method is to create a GUI from which a CDF Variable can be
;   selected.
;
; :Params:
;       GROUP_LEADER:       in, optional, type=int
;                           The widget ID of an existing widget that serves as group
;                               leader for the newly-created widget.
;-
pro MrCDF_Browser::Create_GUI, group_leader
	compile_opt idl2
	on_error, 2

;---------------------------------------------------------------------
;Make the Top Level Base /////////////////////////////////////////////
;---------------------------------------------------------------------

	;Make a top-level base with or without a groupleader. cdf_read_gui2 is called by other
	;blocking widgets, so if a group_leader is given, then make cdf_read_gui2 modal.
	if n_elements(group_leader) ne 0 then begin
		no_block = 0
		self.tlb = widget_base(GROUP_LEADER=group_leader, TITLE='Read CDF', /COLUMN, $
		                       XOFFSET=100, YOFFSET=100, UNAME='tlb', /BASE_ALIGN_CENTER, $
		                       /MODAL)
	endif else begin
		no_block = 0
		self.tlb = widget_base(TITLE='Read CDF', /COLUMN, XOFFSET=200, YOFFSET=100, $
		                       UNAME='tlb', /BASE_ALIGN_CENTER)
	endelse

;---------------------------------------------------------------------
;Create Tree Root ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Put a label above the tree saying how many data variables are shown and how
	;many other variables are shown/hidden.
	treeBase  = widget_base(self.tlb, ROW=1)

	;Adjust the width of the widget to fit the width of the longest variable name
	vnames = self.oCDF -> GetVarNames(COUNT=nVars)
	varwidth = max(strlen(vnames) + 10) * !d.x_ch_size
	
	;Determine the height
	ysize = (40 < nVars) * !d.y_ch_size

	;Create the root of the widget tree.
	self.oCDF -> GetProperty, FILENAME=filename
	self.treeID = widget_tree(treeBase, /FOLDER, /EXPANDED, $
	                          VALUE=filename, UNAME='TreeRoot', $
	                          XSIZE=varwidth, YSIZE=nVars*!d.y_ch_size)
	
	;Put an info box next to the tree
	infoID = widget_text(treeBase, /WRAP, UNAME='InfoBox', FRAME=2, $
	                     XSIZE=max(strlen(vnames)), YSIZE=nVars)

;---------------------------------------------------------------------
;Create OK and Cancel and "Show All" Buttons /////////////////////////
;---------------------------------------------------------------------
    okBase = widget_base(self.tlb, ROW=1)
    button = widget_button(okBase, /ALIGN_CENTER, UNAME='ok', VALUE='OK', $
                           UVALUE={object: self, method:'Ok'})
    button = widget_button(okBase, /ALIGN_CENTER, UNAME='cancel', VALUE='Cancel', $
                           UVALUE={object: self, method:'Quit'})

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
	vnames = self.oCDF -> GetVarNames(COUNT=nVars)

	;Create a node for the file
	self.oCDF -> GetProperty, FILENAME=filename
	rootID = widget_tree(self.treeID, /FOLDER, $
	                     UNAME  = 'Filename', $
	                     UVALUE = {object: self, method: 'Tree_Events'}, $
	                     VALUE  = filename)

	;Create a node for each variable
	for i = 0, nVars - 1 do self -> Create_VarNode, vnames[i], self.oCDF, rootID
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
;       VNAME:          in, required, type=string
;                       The variable name to be attached to the Tree root.
;-
pro MrCDF_Browser::Create_VarNode, varName, oParent, parentID
	compile_opt idl2
	on_error, 2

	;Get the variable object
	oVar = oParent[varName]

	;Create a node under under the tree root
	branchID = widget_tree(parentID, /FOLDER, $
	                       UNAME  = VarName, $
	                       UVALUE = { object: self, $
	                                  method: 'Tree_Events', $
	                                  parent: oParent, $
	                                  name:   varName $
	                                }, $
	                       VALUE  = varName)

	;Get all of the attributes
	attrNames = oVar -> GetAttrNames(COUNT=nAttrs)

	;Set the attribute node
	for i = 0, nAttrs - 1 do self -> Create_AttrNode, attrNames[i], oVar, branchID
end


;+
;   The purpose of this method is to handle events triggered by the List widgets.
;
; :Params:
;       EVENT:                  in, required, type=structure
;                               A button_event structure generated windows manager.
;-
pro MrCDF_Browser::List_Events, event
    ;do nothing
end


;+
;   The purpose of this program is to handle events generated by the OK button.
;   Specifically, it determines which data labels from the list pane, if any, were 
;   selected.
;
; :Params:
;       EVENT:                  in, required, type=structure
;                               A button_event structure generated by a button press
;-
pro MrCDF_Browser::Ok, event
	compile_opt idl2, hidden

	;Destroy the GUI
	self -> Quit, event
end


;+
;   The purpose of this method is to retrieve object properties.
;
; :Keywords:
;-
pro MrCDF_Browser::GetProperty, $
LABEL_1 = label_1, $
LABEL_2 = label_2, $
LABEL_3 = label_3, $
DEPEND = depend, $
DIMENSION = dimension, $
VARIABLE = variable
    compile_opt idl2

    if arg_present(label_1) and n_elements(*self.label_1) ne 0 then label_1 = *self.label_1
    if arg_present(label_2) and n_elements(*self.label_2) ne 0 then label_2 = *self.label_2
    if arg_present(label_3) and n_elements(*self.label_3) ne 0 then label_3 = *self.label_3
    if arg_present(depend) then depend = self.thisDepend
    if arg_present(dimension) then dimension = self.thisDim
    if arg_present(variable) then variable = self.thisVar
end



;+
;   The purpose of this program is to perform some cleanup and destroy the widget. It
;   is called by, e.g., Ok.
;
; :Params:
;       EVENT:              in, required, type=structure
;                           An event structure returned by the windows manager.
;-
pro MrCDF_Browser::Quit, event
    compile_opt idl2
    
    ;Destroy the widget.
    widget_control, event.top, /destroy
end


;+
;   The purpose of this method is to handle events triggered by the widget tree.
;
;   Current tasks::
;
;       1. Check if the selection has changed.
;       2. Update the information in the text box
;       3. Update the widget labels (called "banners" here to distinguish them from CDF
;           data labels) describing what is contained in the list panes
;       4. Update the data labels in the list panes
;
;   Note that:
;
;       CDF files are often disorganized and the data labels do not match the corresponding
;       DEPEND_# variable (e.g. DEPEND_1 does not correspond to LABL_PTR_1, nor does it
;       correspond to the first dimension of VARIABLE). Much of what follows is intended
;       to organize them. This will pay off later, when the "ok" button is pressed.
;
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
		void = cgErrorMsg()
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
	self.selection = uname
	if uname eq 'Filename' then return

;---------------------------------------------------------------------
;Update Text Widget //////////////////////////////////////////////////
;---------------------------------------------------------------------
	widget_control, event.id, GET_UVALUE=uvalue
	
	self -> Update_InfoBox, uvalue.parent, uvalue.name
end


;+
;   The purpose of this method is to update the banners that describe the list panes
;   when a new leaf is selected.
;
; :Params:
;       EVENT:          in, required, type=structure
;                       A button_event structure generated by the windows manager.
;       VNAME:          in, required, type=string
;                       The variable name whose data labels are to be displayed in the
;                           list panes of the GUI.
;-
pro MrCDF_Browser::Update_Banners, event, vname
    compile_opt idl2
    
    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;Fill the Labels /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Depend_N
    self.oCDF -> get_ConfigVals, vname, $
                                 DEPEND_1=depend_1, $
                                 DEPEND_2=depend_2, $
                                 DEPEND_3=depend_3
    if n_elements(depend_1) eq 0 then depend_1 = ''
    if n_elements(depend_2) eq 0 then depend_2 = ''
    if n_elements(depend_3) eq 0 then depend_3 = ''
    depend_n = [depend_1, depend_2, depend_3]

    ;Label IDs
    label_ids = intarr(3)
    label_ids[0] = widget_info(event.top, FIND_BY_UNAME='BANNER_0')
    label_ids[1] = widget_info(event.top, FIND_BY_UNAME='BANNER_1')
    label_ids[2] = widget_info(event.top, FIND_BY_UNAME='BANNER_2')

    ;Step through DEPEND_[1-3]
    for iDep = 0, 2 do begin

        ;If DEPEND_# exists, then update the label.
        if depend_n[iDep] ne '' $
            then widget_control, label_ids[iDep], SET_VALUE=depend_n[iDep] $
            else widget_control, label_ids[iDep], SET_VALUE=' '
    endfor
end


;+
;   The purpose of this method is to update the text box to diplay information relating
;   to the selected variable (or the variable associated with the selected DEPEND_#
;   variable).
;
; :Params:
;       EVENT:          in, required, type=structure
;                       A button_event structure generated the windows manager.
;       VNAME:          in, required, type=string
;                       The variable name whose data labels are to be displayed in the
;                           list panes of the GUI.
;-
pro MrCDF_Browser::Update_InfoBox, oParent, childName
	compile_opt idl2

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif

	;Newline character
	if (!d.name eq 'WIN') $
		then newline = string([13B, 10B]) $
		else newline = string(10B)

	;The parent is either a CDF file or variable object
	tf_var = 0
	case MrObj_Class(oParent) of
		'MRCDF_FILE':     tf_var = 1
		'MRCDF_VARIABLE': tf_var = 0
		else: message, 'Unknown object class "' + MrObj_Class(oParent) + '".'
	endcase

;---------------------------------------------------------------------
; Variable ///////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if tf_var then begin
		;Get the variable and its info.
		tf_has = oParent -> HasVar(childName, OBJECT=oVar)
		oVar  -> GetProperty, NAME=name, CDF_TYPE=cdf_type, $
		                      DIMENSIONS=dimensions, MAXREC=maxrec

		;Size of the variable
		if n_elements(dimensions) eq 1 && dimensions eq 0 then dimensions = 1
		var_size = '[' + strjoin(strtrim(dimensions, 2), ', ') + ', ' + strtrim(maxrec, 2) + ']'
		
		;Data preview
		iMax = (product(dimensions) * maxrec) < 20
		data = oVar -> GetValue()

		;Combine the info into an array
		info = ["Dataset: '" + name + "'", $
		        cdf_type, $
		        var_size, $
		        'Data:']
;---------------------------------------------------------------------
; Attribute //////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		tf_has = oParent -> HasAttr(childName, OBJECT=oAttr)
		data   = oParent -> GetAttrValue(childName, CDF_TYPE=cdf_type)
		oAttr -> GetProperty, NAME=name
		
		;Number of data values
		nData = n_elements(data) 
		iMax  = (nData - 1) < 20
		
		info = ['Attribute: "' + name + '"', $
		        string(FORMAT='(%"%i elements")', nData), $
		        cdf_type, $
		        'Data:']
	endelse
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
;   The purpose of this method is to update the list panes when a new leaf is selected.
;
; :Params:
;       EVENT:          in, required, type=structure
;                       A button_event structure generated the windows manager
;       VNAME:          in, required, type=string
;                       The variable name whose data labels are to be displayed in the
;                           list panes of the GUI.
;-
pro MrCDF_Browser::Update_Lists, event, vname
    compile_opt idl2
    
    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif

;---------------------------------------------------------------------
;Fill the Lists //////////////////////////////////////////////////////
;---------------------------------------------------------------------

    ;Make the label pointers easier to get to
    self.oCDF -> Get_ConfigVals, vname, $
                                 LABL_PTR_1=labl_ptr_1, $
                                 LABL_PTR_2=labl_ptr_2, $
                                 LABL_PTR_3=labl_ptr_3
                                 
    if n_elements(labl_ptr_1) eq 0 then labl_ptr_1 = ''
    if n_elements(labl_ptr_2) eq 0 then labl_ptr_2 = ''
    if n_elements(labl_ptr_3) eq 0 then labl_ptr_3 = ''
    label_pointers = [labl_ptr_1, labl_ptr_2, labl_ptr_3]
    
    ;Depend_N
    self.oCDF -> get_ConfigVals, vname, $
                                 DEPEND_1=depend_1, $
                                 DEPEND_2=depend_2, $
                                 DEPEND_3=depend_3
    if n_elements(depend_1) eq 0 then depend_1 = ''
    if n_elements(depend_2) eq 0 then depend_2 = ''
    if n_elements(depend_3) eq 0 then depend_3 = ''
    depend_n = [depend_1, depend_2, depend_3]
    
    ;IDs of the widget lists
    list_ids = indgen(3)
    list_ids[0] = widget_info(event.top, FIND_BY_UNAME='LIST_0')
    list_ids[1] = widget_info(event.top, FIND_BY_UNAME='LIST_1')
    list_ids[2] = widget_info(event.top, FIND_BY_UNAME='LIST_2')
    
    ;Step through all of the labels
    for iLabel = 0, 2 do begin

        ;If LABEL_# exits, fill the list with data labels.
        if self.oCDF -> varHasAtt(vname, label_pointers[iLabel], ATT_VALUE=label_name) then begin
            temp_labels = self.oCDF -> Read(label_name, /STRING)
            widget_control, list_ids[iLabel], SET_VALUE=temp_labels
        
        ;If DEPEND_N exists, use the data values as labels.
        endif else if depend_n[iLabel] ne '' then begin
            label_values = self.oCDF -> Read(depend_n[iLabel])
            temp_labels = string(label_values, FORMAT='(F0.3)')
            widget_control, list_ids[iLabel], SET_VALUE=temp_labels
        
        ;Otherwise, clear the list
        endif else begin
            widget_control, list_ids[iLabel], SET_VALUE=''
        endelse
    endfor
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
		void = cgErrorMsg()
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
		void = cgErrorMsg()
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
;       GROUP_LEADER:   in, optional, type=integer
;                       The group leader of the file selection gui. Use only when
;                           `FILENAME` undefined.
;       _REF_EXTRA:     in, optional, type=any
;                       All keyword accepted by the CDF_READ::SET_CONFIG method are also
;                           accepted for keyword inheritance.
;
; :Returns:
;       object reference
;-
function MrCDF_Browser::Init, thisCDF, $
GROUP_LEADER = group_leader, $
_REF_EXTRA = extra
	compile_opt idl2

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
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
			else if isa(thisCDF, 'MrCDF_File') eq 0 then message, 'thisCDF must be a MrCDF_File object.'
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
	self -> Create_GUI, group_leader
	
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
;       TLB:                Top level base of the file- and variable-selection widgets
;       OCDF:               An object reference to a CDF_Read object
;       ROOTID:             Widget ID of the wTree root
;-
pro MrCDF_Browser__define, class
	compile_opt idl2
	
	class = { MrCDF_Browser, $
	          tlb:       0L, $                 ;Top Level Base
	          oCDF:      obj_new(), $          ;An object reference to a CDF_Read object
	          selection: '', $
	          treeID:    0L $
	        }
end