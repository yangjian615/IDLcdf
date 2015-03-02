; docformat = 'rst'
;
; NAME:
;       MrCDF_Attribute__Define
;
;*****************************************************************************************
;   Copyright (c) 2014, Matthew Argall                                                   ;
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
;       This is class for parsing attribute information from CDF files. It is used as
;       a utility routine for the CDF_File object.
;
;       Information about CDF files can be found at the following site::
;           - `<User's Guide http://ppi.pds.nasa.gov/doc/cdf/CDF34-Users-Guide.pdf>`
;           - `<ISTP Guide http://spdf.gsfc.nasa.gov/istp_guide/istp_guide.html>`
;           - `<Offial IDL Patch http://cdaweb.gsfc.nasa.gov/pub/software/cdf/dist/cdf34_1/idl/>`
;           - `<CDF Home Page http://cdf.gsfc.nasa.gov/>`
;           - `<About Leap Seconds http://cdf.gsfc.nasa.gov/html/leapseconds.html>`
;
; :Categories:
;       CDF Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg.pro
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/03/07  -   Written by Matthew Argall
;       2014/03/22  -   The GetValue works. Removed the VALUE and DATATYPE properties.
;                           Added the _OverloadPrint method. No longer get the value
;                           and type within the ParseAttribute method. - MRA
;       2014/03/31  -   Removed all variable-related properties and the parse method,
;                           since variable attributes are associated with more than one
;                           variable. Added the ENTRYMASK to the Get*Value methods. - MRA
;       2014/05/08  -   Added the Quiet property to suppress warnings from the CDF DLM. - MRA
;       2014/05/09  -   Error in array creation when no GEntries exist. Fixed. Added the
;                           _OverloadHelp method. - MRA
;       2014/05/17  -   Added the ToStruct method. Keyword to return the number of values
;                           read. - MRA
;       2015/02/06  -   Restructured the _OverloadPrint/Help methods to provide more
;                           concise information. Can now retrieve the variable attribute
;                           entry mask. Cleared up confusions between numGEntries and
;                           maxGEntries. - MRA
;-
;*****************************************************************************************
;+
;   Provide information when the PRINT procedure is called.
;-
function MrCDF_Attribute::_OverloadPrint
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Get the entry information
    entryMask = self -> GetEntryMask(CDF_TYPE=cdf_type, MAXGENTRY=maxGEntry, $
                                     NUMGENTRIES=numGEntries, ERROR=the_error, $
                                     MAXRENTRY=maxREntry, MAXZENTRY=maxZEntry, $
                                     NUMRENTRIES=numREntries, NUMZENTRIES=numZEntries)
    if the_error ne 0 then message, /REISSUE_LAST
    
    nameStr      = string('Name:',          self.name,   FORMAT='(a-23, a0)')
    numberStr    = string('Number:',        self.number, FORMAT='(a-20, i0)')
    scopeStr     = string('Scope:',         self.scope,  FORMAT='(a-20, a0)')
    maxGEntryStr = string('Max G-Entry:',   maxGEntry,   FORMAT='(a-20, i0)')
    numGEntryStr = string('Num G-Entries:', numGEntries, FORMAT='(a-20, i0)')
    maxREntryStr = string('Max R-Entry:',   maxREntry,   FORMAT='(a-20, i0)')
    numREntryStr = string('Num R-Entries:', numREntries, FORMAT='(a-20, i0)')
    maxZEntryStr = string('Max Z-Entry:',   maxZEntry,   FORMAT='(a-20, i0)')
    numZEntryStr = string('Num Z-Entries:', numZEntries, FORMAT='(a-20, i0)')
    typeStr      = string('CDF Type:', "['" + strjoin(cdf_type, "', '") + "']", FORMAT='(a-20, a0)')
    maskStr      = string('Entry Mask:', '[' + strjoin(string(entryMask, FORMAT='(i1)'), ', ') + ']', FORMAT='(a-20, a0)')
    
    ;Append all of the strings together. Make a column so each is
    ;printed on its own line.
    output = [ [nameStr], $
               [numberStr], $
               [scopeStr], $
               [maxGEntryStr], $
               [numGEntryStr], $
               [maxREntryStr], $
               [numREntryStr], $
               [maxZEntryStr], $
               [numZEntryStr], $
               [typeStr], $
               [maskStr]]
    
    ;Offset everything form the name
    output[0,1:*] = '   ' + output[0,1:*]
    
    return, output
end


;+
;   Provide information when the PRINT procedure is called.
;-
function MrCDF_Attribute::_OverloadHelp, varname, $
VARIABLE=variable
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif

    ;Get the attribute value    
    if self.global eq 1 $
        then value = self -> GetGlobalAttrValue() $
        else if n_elements(variable) gt 0 then value = self -> GetVarAttrValue(variable)

    ;Value -> String
    case n_elements(value) of
        0: valueStr = ''
        1: valueStr = strtrim(value, 2)
        else: valueStr = '[' + strjoin(strtrim(value, 2), ', ') + ']'
    endcase
    
    ;Help string.
    outStr = string(self.number, self.name, self.scope, valueStr, $
                    FORMAT='(i3, 2x, a-20, 2x, a14, 2x, a0)')
    
    return, outStr
end


;+
;   Create a mask for the global entry numbers (gEntryNums) associated with a global
;   attribute. Global attribute values are array-like and are indexed by gEntryNums.
;   However, values do not need to exist at all gEntryNum. This method creates a mask
;   of 1s and 0s indicated if a value exists at the corresponding gEntryNum.
;
; :Keywords:
;       ERROR:              out, optional, type=integer
;                           Named variable to recieve the error code. 0 indicates no
;                               error. If present, the error message will be suppressed.
;       MAXGENTRY:          in, optional, type=long
;                           Highest global entry index associated with the attribute.
;       NUMGENTRIES:        in, optional, type=long
;                           Number of global entries associated with the attribute. Can
;                               be less than `MAXGENTRY`+1.
;
; :Returns:
;       ENTRYMASK:          Array of 1s and 0s indicating which which global entry numbers
;                               contain values. -1 is returned if no global entries exits.
;-
function MrCDF_Attribute::GetEntryMask, $
CDF_TYPE=cdf_type, $
ERROR=the_error, $
MAXGENTRY=maxGEntry, $
MAXRENTRY=maxREntry, $
MAXZENTRY=maxZEntry, $
NUMGENTRIES=numGEntries, $
NUMRENTRIES=numREntries, $
NUMZENTRIES=numZEntries
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, -1
    endif
    
    ;Get the CDF file ID
    fileID    = self.parent -> GetFileID()
    doCDFType = arg_present(cdf_type)
    
    ;Total number of global attributes
    self.parent -> GetProperty, NGATTRS=nGAttrs, NVATTRS=nVAttrs, NRVARS=nRVars, NZVARS=nZVars, NVARS=nVars

    ;Get entry information
    cdf_control, fileID, ATTRIBUTE=self.name, GET_ATTR_INFO=attr_info
    numGEntries = attr_info.numGEntries
    numREntries = attr_info.numREntries
    numZEntries = attr_info.numZEntries
    maxGEntry   = attr_info.maxGEntry
    maxREntry   = attr_info.maxREntry
    maxZEntry   = attr_info.maxZEntry
    cdf_type    = ''

;-----------------------------------------------------
; Global Attribute \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ; For global attributes, the G-Entry number acts as an array index. Each global
    ; attribute can have any number of values. They are numbered from 0 to maxGEntry,
    ; but not all entries must have a value, i.e. numGEntries can be less than maxGEntry+1.
    ;
    if self.global then begin
        ;Total number of entries, filled or not
        nEntries = maxGEntry + 1
    
        ;No entries?
        if numGEntries eq 0 then return, -1
        if doCDFtype then cdf_type = strarr(nEntries)
        
        ;Build entry mask and count hits
        entryMask  = bytarr(nEntries)
        gAttrCount = 0L
        
        ;Step through each entry
        for thisGEntry = 0, maxGEntry do begin
            tf_exists = cdf_attexists(fileID, self.name, thisGEntry)
            if tf_exists eq 0 then continue
            
            ;Get the CDF type?
            if doCDFType then begin
                cdf_attget, fileID, self.name, thisGEntry, value, CDF_TYPE=type
                cdf_type[gAttrCount]  = type
            endif

            ;Unmask the value
            entryMask[thisGEntry] = 1B
            gAttrCount++
        endfor
        
        ;Check that all were found
        if gAttrCount ne numGEntries then message, 'Not all global entries were found.', /INFORMATIONAL
;-----------------------------------------------------
; Variable Attribute \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ; For variable attributes, the entry number is the variable ID (or index). The maximum
    ; number of values that a variable attribute can have is equal to the total number of
    ; variables in the CDF file. However, the attribute need not apply to all variables,
    ; so numREntries + numZEntries can be less than nVars. And, like global attributes,
    ; adjacent indices (or variable IDs) need not have values, so we must test each time.
    ;
    endif else begin
        numVEntries = numREntries + numZEntries
    
        ;No variables associated with this attribute
        if numVEntries eq 0 then return, -1
        if doCDFtype then cdf_type = strarr(numVEntries)
    
        ;Build the mask
        entryMask  = bytarr(nVars)
        vAttrCount = 0L
        
        ;Step through r-Variables
        for iRVar = 0, maxREntry do begin
            tf_exists = cdf_attexists(fileID, self.name, iRVar)
            if tf_exists eq 0 then continue
            
            ;Get the CDF type?
            if doCDFType then begin
                cdf_attget, fileID, self.name, iRVar, value, CDF_TYPE=type
                cdf_type[vAttrCount] = type
            endif

            ;Unmask the value
            entryMask[iRVar] = 1B
            vAttrCount++
        endfor
        
        ;Step through z-Variables
        for iZVar = nRVars, nRVars+maxZEntry do begin
            tf_exists = cdf_attexists(fileID, self.name, iZVar, /ZVARIABLE)
            if tf_exists eq 0 then continue
            
            ;Get the CDF type?
            if doCDFType then begin
                cdf_attget, fileID, self.name, iZVar, value, CDF_TYPE=type, /ZVARIABLE
                cdf_type[vAttrCount] = type
            endif

            ;Unmask the value
            entryMask[iZVar] = 1B
            vAttrCount++
        endfor
        
        ;Check that all were found
        if vAttrCount ne numVEntries then message, 'Not all variable attribute entries were found.', /INFORMATIONAL
    endelse
    
    return, entryMask
end


;+
;   Return the CDF attribute name.
;
; :Returns:
;       ATTRNAME:           CDF attribute name.
;-
function MrCDF_Attribute::GetName
    return, self.name
end


;+
;   Return the CDF attribute number.
;
; :Returns:
;       ATTRNUM:            CDF attribute number.
;-
function MrCDF_Attribute::GetNumber
    return, self.number
end


;+
;   Return the value of the attribute.
;
;   NOTES:
;       - Attribute values must be scalars (i.e. at most one value per entry number).
;       - Attribute values can be of different types.
;       - Entry numbers can be skipped.
;
; :Params:
;       ENTRYNUM:           in, out, optional, type=long/longarr
;                           If an index or index array is given, then they correspond to
;                               the global entry numbers whose values are to be retrieved.
;                               If named variable is provided, the global entry numbers
;                               all entries containing values will be returned. Values may
;                               be stored at discontiguous entry numbers.
;
; :Keywords:
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of `ATTRVALUE`. Not all attribute values need
;                               to be of the same datatype.
;       COUNT:              out, optional, type=integer
;                           Number of entry values returned.
;       ERROR:              out, optional, type=integer
;                           Named variable to recieve the error code. 0 indicates no
;                               error. If present, the error message will be suppressed.
;       ENTRYMASK:          out, optional, type=bytarr
;                           An array of 1's and 0's indicating whether or not a value
;                               has been written to the global entry number corresponding
;                               to a given array index. Attribute values do not have to
;                               be contiguous. If `ENTRYNUM` was provided, ENTRYMASK will
;                               correspond only to those entry numbers given.
;
; :Returns:
;       ATTRVALUE:          out, required, type=object/array
;                           Value(s) of the attribute. If a value does not exist at a
;                               particular entry number, it will be skipped
;                               (see `ENTRYMASK`). Global attribute values do not have to
;                               be the same type. If they are not, a "LinkedList" object
;                               will be returned instead of an array.
;-
function MrCDF_Attribute::GetGlobalAttrValue, entryNum, $
CDF_TYPE=cdf_type, $
COUNT=nEntries, $
ERROR=the_error, $
ENTRYMASK=entryMask, $
ZVARIABLE=zvariable
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if obj_valid(attrValueList) then obj_destroy, attrValueList
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, -1
    endif
    
    ;Get the CDF file ID
    fileID = self.parent -> GetFileID()
    nEntryNum = n_elements(entryNum)
    cdf_type  = ''

;-----------------------------------------------------
; Specific Entries \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if nEntryNum gt 0 then begin
        cdf_type   = strarr(nEntryNum)
        gEntryMask = bytarr(nEntryNum)
    
        ;Step through each entry
        for i = 0, nEntryNum-1 do begin
            theEntry = entryNum[i]
        
            ;Make sure the entry number exists
            if cdf_attexists(fileID, self.name, theEntry) eq 0 then $
                message, 'Entry number (' + strtrim(theEntry, 2) + ') does not exist ' + $
                         'for global attribute "' + self.name + '".'
            
            ;Get the value
            cdf_attget, fileID, self.name, theEntry, attrValue, CDF_TYPE=type
            
            ;Store the value and type
            cdf_type  = type
            gEntryMask = 1B
        endfor

;-----------------------------------------------------
; All Entries \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else begin
        ;Get entry information
        cdf_control, fileID, ATTRIBUTE=self.name, GET_ATTR_INFO=attr_info
        numGEntries = attr_info.numGEntries
        maxGEntry   = attr_info.maxGEntry
        
        ;Return if there are no entries
        if numGEntries eq 0 then return, -1
        
        ;
        ; Global attribute entries are like array indices. Unlike an array, however, not
        ; all entries must have a value. So, while the total number of entries, numGEntries,
        ; may be 2, the values might be stored at entries 100 and 101. The variable maxGEntry
        ; is the 0-based index of the last index. We must allocate at least maxGEntry+1
        ; elements to our arrays to capture all entry locations.
        ;
        nEntries   = maxGEntry + 1
        cdf_type   = strarr(numGEntries)
        entryNum   = lindgen(numGEntries)
        gEntryMask = bytarr(nEntries)
        theType    = ''
    
        gAttrCount = 0L
        for iGEntry = 0L, maxGEntry do begin
            ;Check if there is a value at this entry
            if cdf_attexists(fileID, self.name, iGEntry) eq 0 then continue
        
            ;Get the value
            cdf_attget, fileID, self.name, iGEntry, value, CDF_TYPE=type
            
            ;Is this the first value?
            if n_elements(attrValue) eq 0 then begin
                attrValue = make_array(nEntries, TYPE=size(value, /TYPE))
                theType   = type
            endif
            
            ;Cannot combine values of different types
            if theType ne '' && type ne theType then begin
                message, 'Multiple CDF types encountered.', /CONTINUE
                message, '   Get global attribute values individually with gEntryNum.', /CONTINUE
                message, '   Check keywords for useful information.', /CONTINUE
            endif else begin
                attrValue[gAttrCount] = value
            endelse

            ;Store the value and type
            gEntryMask[iGEntry]  = 1B
            cdf_type[gAttrCount] = type
            entryNum[gAttrCount] = iGEntry
            gAttrCount++
        endfor
        
        ;Trim results
        if gAttrCount ne nEntries then $
            message, 'Not all Global Attributes found.', /INFORMATIONAL
    endelse
    
    ;Return a scalar?
    if n_elements(attrValue) eq 1 then begin
        attrValue   = attrValue[0]
        cdf_type    = cdf_type[0]
        entryNum    = entryNum[0]
        gEntryMask = gEntryMask[0]
    endif
    
    return, attrValue
end


;+
;   Return the value of the attribute.
;
; :Params:
;       VARNAME:            in, out, optional, type=long/longarr
;                           Name of the variable for which the attribute value is to be
;                               returned.
;
; :Keywords:
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of `ATTRVALUE`. Posibilities are: 'CDF_BYTE',
;                               'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                               'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                               'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                               'CDF_UINT2', 'CDF_UINT4' or 'UNKNOWN'.
;
; :Returns:
;       ATTRVALUE:          Value(s) of the attribute.
;-
function MrCDF_Attribute::GetVarAttrValue, varName, $
CDF_TYPE=cdf_type, $
COUNT=nElements
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(thisQuiet) gt 0 then !quiet = thisQuiet
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Get the CDF file ID
    fileID = self.parent -> GetFileID()

    ;Quiet
    thisQuiet = !quiet
    !quiet = self.quiet

    ;Get the value -- STATUS will be 0 if SELF is a global attribute or if the
    ;                 variable does not contain the attribute.
    cdf_attget_entry, fileID, self.name, varName, attrEntryType, attrValue, status, $
                      CDF_TYPE=cdf_type, ZVARIABLE=zvariable
    
    ;Unquiet
    !quiet = thisQuiet

    ;Message if the value could not be retrieved.
    if status eq 0 then $
        message, 'Variable ' + varName + ' does not have attribute "' + self.name + '".'
    
    nElements = n_elements(attrValue)
    return, attrValue
end


;+
;
; :Keywords:
;       NAME:           in, optional, type=string
;                       CDF attribute name.
;       NUMBER:         in, optional, type=integer
;                       CDF attribute number.
;       CDF_TYPE:       out, optional, type=string
;                       CDF data type. Posibilities are: 'CDF_BYTE',
;                           'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                           'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                           'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                           'CDF_UINT2', 'CDF_UINT4'.
;       GLOBAL:         out, optional, type=boolean
;                       1 if the attribute is global in scope. 0 for variable scope.
;       SCOPE:          out, optional, type=string
;                       Scope of the attribute. Possibilities are "GLOBAL_SCOPE",
;                           "GLOBAL_SCOPE_ASSUMED", "VARIABLE_SCOPE", and
;                           "VARAIBLE_SCOPE_ASSUMED".
;       VALUE:          out, optional, type=any
;                       Value of the attribute.
;-
pro MrCDF_Attribute::GetProperty, $
NAME=name, $
NUMBER=number, $
TYPE=type, $
CDF_TYPE=cdf_type, $
GLOBAL=global, $
SCOPE=scope, $
VALUE=value
    compile_opt strictarr
    on_error, 2
    
    ;Get Properties
    if arg_present(cdf_type) then cdf_type =  self.cdf_type
    if arg_present(name)     then name     =  self.name
    if arg_present(number)   then number   =  self.number
    if arg_present(type)     then type     =  self.type
    if arg_present(global)   then global   =  self.global
    if arg_present(scope)    then scope    =  self.scope
    if arg_present(value)    then value    = *self.value
end


;+
;   The purpose of this method is to parse a global attribute from a CDF file.
;-
pro MrCDF_Attribute::ParseGlobalAttribute
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;inquire about the attribute to get its name and scope
    parentID = self.parent -> GetFileID()
    attrNum  = cdf_attnum(parentID, self.name)
    cdf_attinq,  parentID, self.name, attrName, attrScope, maxREntry, maxZEntry
    
    ;Make a data structure of the attribute information
    self.number = attrNum
    self.scope  = attrScope
end


;+
;   The purpose of this method is to parse an attribute from a CDF file.
;-
pro MrCDF_Attribute::ParseVariableAttribute
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(thisQuiet) gt 0 then !quiet = thisQuiet
        void = cgErrorMsg()
        return
    endif
    
    ;inquire about the attribute to get its name and scope
    parentID = self.parent -> GetFileID()
    attnum   = cdf_attnum(parentID, self.name)
    cdf_attinq,  parentID, self.name, attname, scope, maxREntry, maxZEntry
    cdf_control, parentID, ATTRIBUTE=attnum, GET_ATTR_INFO=att_info

    ;Quiet
    thisQuiet = !quiet
    !quiet = self.quiet

    ;Variable Attribute
    varinq = cdf_varinq(parentID, self.varname)
    cdf_attget_entry, parentID, attname, self.varname, attType, attValue, status, $
                      CDF_TYPE=cdf_type, ZVARIABLE=varInq.is_zvar
    if status eq 0 then message, 'Attribute "' + attname + '" does not exist for variable "' + self.varname + '".'

    ;Unquiet
    !quiet = thisQuiet

    ;Convert bytes back to strings.
    if cdf_type eq 'CDF_CHAR' || cdf_type eq 'CDF_UCHAR' then attValue = string(attValue)
    
    ;Make a data structure of the attribute information
    self.number   = attnum
    self.scope    = scope
end


;+
;   Set properties of the object.
;
; :Keywords:
;       QUIET:          in, optional, type=boolean
;                       If set, warning messages from the CDF DLM will not be output.
;-
pro MrCDF_Attribute::SetProperty, $
QUIET=quiet
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    if n_elements(quiet) gt 0 then self.quiet = keyword_set(quiet)
end


;+
;   Parse attribute information into a structure.
;
; :Params:
;       VARNAME:        in, optional, type=string
;                       If the attribute is variable in scope, then the associated
;                           variable name must be provided.
;-
function MrCDF_Attribute::ToStruct, varName
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Make sure a variable name was given for variable attributes.
    if self.global eq 0 then begin
        if MrIsA(varName, /SCALAR, 'STRING') eq 0 then $
            message, 'To create a structure from a variable attribute, VARNAME must be provided.'
    endif
    
    ;Get the parent's ID
    parentID = self.parent -> GetFileID()
    cdf_control, parentID, ATTRIBUTE=self.name, GET_ATTR_INFO=attr_info
    
    ;Read the attribute data
    if self.global $
        then data = self -> GetGlobalAttrValue(CDF_TYPE=cdf_type, COUNT=nElements) $
        else data = self -> GetVarAttrValue(varName, CDF_TYPE=cdf_type, COUNT=nElements)
    
    ;Was there any data?
    if nElements eq 0 then data = '<NoData>'
    
    ;Create the structure
    attr_struct = {_NAME:     self.name, $
                   _TYPE:          'ATTRIBUTE', $
                   _SCOPE:    self.scope, $
                   _DATA:          data, $
                   _CDF_TYPE:      cdf_type, $
                   _NELEMENTS:     nElements}
    
    return, attr_struct
end


;+
;   Clean up after the object is destroyed
;-
pro MrCDF_Attribute::cleanup
    ;Nothing to clean up
end


;+
;   The initialization method.
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           CDF attribute name found in `PARENT`.
;       PARENT:             in, required, type=object
;                           CDF_File object
;
; :Keywords:
;       QUIET:              in, optional, type=boolean, default=1
;                           If set, warnings from the CDF DLM  will be suppressed.
;                               This is the default.
;-
function MrCDF_Attribute::init, attrName, parent, $
QUIET=quiet
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Check Inputs
    if MrIsA(attrName, /SCALAR, 'STRING') eq 0 then $
        message, 'AttrName must be a scalar string.'
    
    if MrIsA(parent, /SCALAR, 'OBJREF') eq 0 then $
        message, 'PARENT must be a scalar object.'
    
    ;Get attribute info
    parentID = parent -> GetFileID()
    cdf_attinq,  parentID, attrName, theName, attrScope, maxEntry, maxZEntry
    
    
    ;Set properties
    self.name   = attrName
    self.number = cdf_attnum(parentID, attrName)
    self.parent = parent
    self.scope  = attrScope
    self.quiet  = n_elements(quiet) eq 0 ? 1 : keyword_set(quiet)
    if strpos(attrScope, 'GLOBAL') ne -1 $
        then self.global = 1B $
        else self.global = 0B
    
    return, 1
end


;+
;   The class definition.
;
; :Hidden:
;
; :Fields:
;       GLOBAL:         Indicates the attribute is global in scope (not variable).
;       NAME:           CDF attribute name.
;       NUMBER:         CDF attribute number.
;       PARENT:         CDF_File object.
;       SCOPE:          Scope of the attribute.
;-
pro MrCDF_Attribute__define
    compile_opt strictarr
    
    define = { MrCDF_Attribute, $
               inherits IDL_Object, $
               global:    0B, $
               name:      '', $
               number:    0L, $
               parent:    obj_new(), $
               quiet:     0B, $
               scope:     '' $
             }
end