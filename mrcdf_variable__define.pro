; docformat = 'rst'
;
; NAME:
;       MrCDF_Variable__Define
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
;       This is class for parsing variable information from CDF files. It is used as
;       a utility routine for the CDF_File object.
;
;       Information about CDF files can be found at the following site::
;           http://ppi.pds.nasa.gov/doc/cdf/CDF34-Users-Guide.pdf
;           http://spdf.gsfc.nasa.gov/istp_guide/istp_guide.html
;           http://cdaweb.gsfc.nasa.gov/pub/software/cdf/dist/cdf34_1/idl/
;           http://cdf.gsfc.nasa.gov/
;           http://cdf.gsfc.nasa.gov/html/leapseconds.html
;
; :Categories:
;       CDF Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMSG.pro
;       MrIsA.pro
;       
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
;       2014/03/22  -   AddAttr now tries to add an existing variable attribute first.
;                           It then checks if one is to be created. - MRA
;       2014/05/08  -   Added the Quiet property to suppress warnings from the CDF DLM. - MRA
;       2014/05/09  -   Added the _OverloadHelp method. - MRA
;       2014/05/17  -   Added the ToStruct method. - MRA
;       2014/01/30  -   Added the _OverloadBracketsRightSide method. - MRA
;       2015/02/06  -   _OverloadPrint/Help provide concise, well formatted output.
;                           Variable compression now possible. - MRA
;       2015/04/23  -   Moved compression information to from _OverloadHelp to _OverloadPrint. - MRA
;       2015/08/28  -   Turn off warning messages from CDF_VarGet. - MRA
;       2016/07/13  -   Variables with record variance of NOVARY read one record. - MRA
;-
;*****************************************************************************************
;+
;   Obtain a variable's object reference
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=string/integer
;                           If a string is given, it is the name of the attribute whose
;                               for which the object reference is to be retrieved. An
;                               integer value of 0 will return the variable's object
;                               reference.
;
; :Returns:
;       THEOBJ:             in, required, type=numeric array
;                           The variable attribute object identified by the subscripts.
;-
function MrCDF_Variable::_OverloadBracketsRightSide, isRange, subscript1
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, -1
    endif
    
    ;Pick the proper data set to alter
    if n_elements(subscript1) ne 1 then $
        message, 'The first subscript must be a scalar.'
    
    ;Only one subscript can be given    
    nSubs = n_elements(isRange)
    if nSubs ne 1 then message, 'Only one subscript is accepted.'

    ;Attribute name.    
    if MrIsA(subscript1, 'STRING') then begin
        tf_has = self -> HasAttr(subscript1, OBJECT=theObj)
        if tf_has eq 0 then message, 'Variable attribute "' + subscript1 + '" not found.'
    
    ;Attribute indices not allowed because 0 must return SELF otherwise object array
    ;indexing will not work.
    endif else if MrIsA(subscript1, /INTEGER) then begin
        ;If the scalar index 0 was given, return the SELF reference
        if subscript1 ne 0 then message, '0 is the only integer 0 is accepted as a subscript.'
        theObj = self
        
    endif else begin
        message, 'First subscript must be a string or 0.'
    endelse
    
    ;Return the variable object?
    return, theObj
end


;+
;   Provide information when the HELP procedure is called.
;-
function MrCDF_Variable::_OverloadHelp, varname
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, ''
    endif
    
    class  = obj_class(self)
    heapID = obj_valid(self, /GET_HEAP_IDENTIFIER)
    selfStr = class + '   <' + strtrim(heapID, 2) + '>'
    
    ;Dimensions
    ;   - Zero dimensional CDF files have DIMVAR=0
    if n_elements(*self.dimvar) eq 1 && *self.dimvar eq 0 $
        then dimStr = '[0, ' + strtrim(self.maxRec+1, 2) + ']' $
        else dimStr = '[' + strjoin(strtrim(*self.dim, 2), ', ') + ', ' + strtrim(self.maxRec+1, 2) + ']' 
    
    ;Variable type
    var_type = self.zvariable ? 'zVar' : 'rVar'
    
    ;Help string
    outStr = string(self.number, self.name, var_type, self.cdf_type, dimStr, $
                    FORMAT='(i3, 2x, a-0, 2x, a4, 2x, a14, 2x, a0)')
    
    return, [[selfStr], [outStr]]
end


;+
;   Provide information when the PRINT procedure is called.
;-
function MrCDF_Variable::_OverloadPrint
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, ''
    endif
    
    ;Dimensions
    ;   - Zero dimensional CDF files have DIMVAR=0
    if n_elements(*self.dimvar) eq 1 && *self.dimvar eq 0 $
        then dimStr = '[0, ' + strtrim(self.maxRec+1, 2) + ']' $
        else dimStr = '[' + strjoin(strtrim(*self.dim, 2), ', ') + ', ' + strtrim(self.maxRec+1, 2) + ']' 
    
    ;Variable type
    var_type = self.zvariable ? 'zVar' : 'rVar'
                    
    ;Get compression information
    self -> GetProperty, COMPRESSION=compression, GZIP_LEVEL=gzip_level
    compStr = '  Compression: ' + compression
    if compression eq 'GZIP' then compStr += ' at level ' + strtrim(gzip_level, 2)
    
    ;Help string
    outStr = string(self.number, self.name, var_type, self.cdf_type, dimStr, $
                    FORMAT='(i3, 2x, a-0, 2x, a4, 2x, a14, 2x, a0)')

    ;Allocate memory for the attribute help string    
    nAttrs      = self.attributes -> Count()
    attrHelp    = strarr(1, nAttrs+1)
    attrHelp[0] = 'ATTRIBUTES:'
    
    ;Get help from the attributes.
    allAttrs = self.attributes -> Get(/ALL)
    for i = 0, nAttrs - 1 do attrHelp[0,i+1] = '    ' + allAttrs[i] -> _OverloadHelp(VARIABLE=self.name)
    
    ;Concatenate
    outStr = [[outStr], [compStr], [attrHelp]]
    return, outStr
end


;+
;   Determin if the variable has a particular attribute.
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           Name of the attr
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           Attribute object associated with `ATTRNAME`.
;
; :Returns:
;       TF_HAS:             Returns true (1) if the variable has the attribute and
;                               false (0) if not.
;-
pro MrCDF_Variable::DelAttr, attrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Get the attribute object
    tf_has = self -> HasAttr(attrName, OBJECT=oAttrObj)
    if ~tf_has then message, 'Attribute does not exist. Cannot delete.'

    ;Delete from file
    fileID = self.parent -> GetFileID()
    cdf_attdelete, fileID, attrName, self.number, ZVARIABLE=self.zvariable

    ;Remove from container
    ;   - Do not destroy because the parent file is the
    ;     source of variable attributes.
    self.attributes -> Remove, oAttrObj
end


;+
;   Return the names of the attributes associated with this variable.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of variable attributes associated with `VARIABLE`.
;       ERROR:              out, optional, type=integer
;                           Named variable to recieve the error code. 0 indicates no
;                               error. If present, the error message will be suppressed.
;
; :Returns:
;       ATTRNAMES           Attribute name(s) associated with the variable
;-
function MrCDF_Variable::GetAttrNames, $
COUNT=count, $
ERROR=the_error, $
SHOW=show
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then MrPrintF, 'LogErr'
        return, ''
    endif
    
    ;Get all of the attributes
    count = self.attributes -> Count()
    if count eq 0 then return, ''
    
    ;Get all of the names
    attrNames = strarr(count)
    allAttrs = self.attributes -> Get(/ALL)
    for i = 0, count - 1 do attrNames[i] = allAttrs[i] -> GetName()
    
    ;Return a scalar
    if count eq 1 then attrNames = attrNames[0]
    
    ;Print the names?
    if keyword_set(show) then print, transpose(attrNames)
    
    return, attrNames
end


;+
;   Return the value of a particular variable attribute.
;
; :Params:
;       ATTRIBUTE:          in, required, type=object/string
;                           Name of an attribute or an attribute object for which the
;                               value is to be returned.
;
; :Keywords:
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of `ATTRVALUE`.
;       FOLLOW_PTR:         in, optional, type=boolean, default=0
;                           If `ATTRVALUE` points to a variable, then follow the pointer,
;                               read the variable's data, and return it.
;       PTR_VALUE:          out, optional, type=string, default=''
;                           If `FOLLOW_PTR` is set, this will return the name of the
;                               variable that serves as the pointer. If `ATTRVALUE` is
;                               not a pointer, then the empty string is returned.
;
; :Returns:
;       ATTRVALUE:          Value of `ATTRIBUTE`.
;-
function MrCDF_Variable::GetAttrValue, attribute, $
CDF_TYPE=cdf_type, $
FOLLOW_PTR=follow_ptr, $
PTR_VALUE=ptr_value
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, -1
    endif
    
    ;Get the attribute object.
    case size(attribute, /TNAME) of
        'OBJREF': attObj = attribute
        'STRING': begin
            attObj = self.attributes -> FindByName(attribute, COUNT=count)
            
            if count eq 0 then message, 'Attribute object "' + attribute + '" not found.'
            if obj_valid(attobj) eq 0 then message, 'Invalid attribute object with name "' + attribute + '".'
        endcase
        else: message, 'An attribute name or object is required.'
    endcase
    
    ;Get the value and data type.
    attrValue = attObj -> GetVarAttrValue(self.name, CDF_TYPE=cdf_type)
    
    ;Pointer to another variable
    if keyword_set(follow_ptr) && MrIsA(attrValue, /SCALAR, 'STRING') && attrValue ne self.name then begin
        ptr_value = attrValue
        attrValue = self.parent -> Read(ptr_value, CDF_TYPE=cdf_type)
    endif else begin
        ptr_value = ''
    endelse
    
    return, attrValue
end


;+
;   Return the name of the variable.
;
; :Returns:
;       VARNAME:            CDF variable name.
;-
function MrCDF_Variable::GetName
    return, self.name
end


;+
;   Return the name of the variable.
;
; :Keywords:
;       ISZVAR:             out, optional, type=boolean
;                           Returns 1 of the variable is a Z-Variable and 0 if it is an
;                               r-variable.
;
; :Returns:
;       VARNUM:             CDF variable number.
;-
function MrCDF_Variable::GetNumber, $
ISZVAR=isZvar
    isZvar = self.zvariable
    return, self.number
end


;+
;   Get variable data from the CDF file.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       OFFSET:             in, optional, type=intarr, default=0 for each dimension
;                           Array indices within the specified record(s) at which to
;                               begin reading. OFFSET is a 1-dimensional array
;                               containing one element per CDF dimension.
;       REC_COUNT:          in, optional, type=long, default=maxrec+1
;                           Number of records to be read. A REC_COUNT of 1 automatically
;                               sets `SINGLE_VALUE`=1.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records when reading multiple records.
;       REC_START:          in, optional, type=integer, default=0
;                           Record at which to begin reading data.
;       SINGLE_VALUE:       in, optional, type=boolean, default=0
;                           Read a single value via the CDF_VarGet1 procedure. The
;                               default is to read a full record via CDF_VarGet.
;       STRING:             in, optional, type=boolean, default=0
;                           If set, "CDF_CHAR" and "CDF_UCHAR" data will be converted
;                               to strings. The are read from the file as byte-arrays.
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of the variable being read. Possibilities are:
;                               'CDF_EPOCH', 'CDF_BYTE', 'CDF_CHAR', 'CDF_DOUBLE', 
;                               'CDF_REAL8', 'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4',
;                               'CDF_INT1', 'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR',
;                               'CDF_UINT1', 'CDF_UINT2', 'CDF_UINT4'.
;       FILLVALUE:          out, optional, type=any
;                           Value used as a filler for missing data.
;       NRECS:              out, optional, type=integer
;                           Total number of records read. This is useful, for instance,
;                               when no records have been written to a variable. In this
;                               case, a single `PADVALUE` is returned and NRECS=0.
;       PADVALUE:           out, optional, type=any
;                           Value used to pad the data when more data is read than what
;                               exists in the file. It is possible that this value does
;                               not exist.
;
; :Returns:
;       VALUE:              Value of the CDF variable.
;-
function MrCDF_Variable::GetValue, $
;INPUT
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
REC_COUNT=rec_count, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start, $
SINGLE_VALUE=single_value, $
STRING=string, $
;OUTPUT
CDF_TYPE=cdf_type, $
FILLVALUE=fillvalue, $
NRECS=nRecs, $
PADVALUE=padvalue
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        !Quiet = 0
        count  = 0
        void = cgErrorMSG(/QUIET)
        return, -1
    endif

    ;Defaults
    ;   - If no records have yet been written, then REC_COUNT<=0 will cause
    ;     an error. Leaving REC_COUNT undefined will quietly cause CDF_VarGet
    ;     to retrieve a pad value.
    single_value = keyword_set(single_value)
    if n_elements(string)    eq 0 then string       = 1
    rstart = n_elements(rec_start) gt 0 ? rec_start : 0
    rcount = n_elements(rec_count) gt 0 ? rec_count : self.maxrec + 1
    nRecs  = rcount

    ;If no records have been written, then undefine REC_COUNT
    ;   - Otherwise, will cause error "Array dimensions must be greater than 0."
    ;   - With REC_COUNT=0, a PAD_VALUE will be returned.
    if rcount eq 0 then begin
        MrPrintF, 'LogWarn', 'REC_COUNT=0. Returning PAD_VALUE.'
        void = temporary(rcount)
    endif
    
    ;If record variance is NOVARY, set REC_START=0 and REC_COUNT=1
    if self.recvar eq 'NOVARY' && (rstart ne 0 || rcount ne 1) then begin
        MrPrintF, 'LogWarn', self.name + ': Record variance is "NOVARY". Setting REC_COUNT=1 and REC_START=0'
        rstart = 0
        rcount = 1
    endif
    
    ;Get the file ID
    parentID = self.parent -> GetFileID()
    
    ;Turn off annoying error messages
    !Quiet = 1
    
    ;Get the value(s)
    if single_value then begin
        cdf_varget1, parentID, self.name, value, OFFSET=offset, REC_START=rstart, STRING=string
    endif else begin
        cdf_varget, parentID, self.name, value, COUNT=count, INTERVAL=interval, $
                    OFFSET=offset, REC_COUNT=rcount, REC_INTERVAL=rec_interval, $
                    REC_START=rstart, STRING=string
    endelse

    ;Turn warnings back on
    !Quiet = 0

    ;Output Keywords
    cdf_type  = self.cdf_type
    if ptr_valid(self.padvalue)      then padvalue  = *self.padvalue
    if self  -> HasAttr('FILLVAL') then fillvalue =  self -> GetAttrValue('FILLVAL')
    
    return, value
end


;+
;   Determin if the variable has a particular attribute.
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           Name of the attr
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           Attribute object associated with `ATTRNAME`.
;
; :Returns:
;       TF_HAS:             Returns true (1) if the variable has the attribute and
;                               false (0) if not.
;-
function MrCDF_Variable::HasAttr, attrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2
    
    ;Find the attribute
    object = self.attributes -> FindByName(attrName, COUNT=count)
    
    if count eq 0 $
        then return, 0 $
        else return, 1
end


;+
;   Get properties
;
; :Keywords:
;       CDF_TYPE:       out, optional, type=string
;                       CDF data type. Posibilities are: 'CDF_BYTE',
;                           'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                           'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                           'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                           'CDF_UINT2', 'CDF_UINT4'.
;       COMPRESSION:    out, optional, type=string
;                       Name of the compression algorighm used on the variable
;       DIMENSIONS:     out, optional, type=lonarr
;                       Dimension sizes of the data.
;       DIMVAR:         out, optional, type=bytarr
;                       Dimensional variance of the data.
;       GZIP_LEVEL:     out, optional, type=integer
;                       If `COMPRESSION` is 'GZIP', this is the level of gzip
;                           compression used.
;       MAXREC:         out, optional, type=integer
;                       Maximum number of records written to the variable (0-based).
;       NAME:           out, optional, type=string
;                       CDF variable name.
;       NELEMENTS:      out, optional, type=long
;                       Number of data elements. 
;       NUMBER:         out, optional, type=integer
;                       CDF variable number.
;       RECVAR:         out, optional, type=string
;                       Record variance of the data.
;       ZVARIABLE:      out, optional, type=boolean
;                       Indicates if the variable is a z-variable.
;-
pro MrCDF_Variable::GetProperty, $
CDF_TYPE=cdf_type, $
COMPRESSION=compression, $
DIMENSIONS=dimensions, $
DIMVAR=dimvar, $
GZIP_LEVEL=gzip_level, $
MAXREC=maxrec, $
NAME=name, $
NUMBER=number, $
NELEMENTS=nElements, $
RECVAR=recvar, $
ZVARIABLE=zvariable
    compile_opt strictarr
    on_error, 2
    
    ;Get Properties
    if arg_present(cdf_type)   then cdf_type   =  self.cdf_type
    if arg_present(dimensions) then dimensions = *self.dim
    if arg_present(dimvar)     then dimvar     = *self.dimvar
    if arg_present(gzip_level) then gzip_level = *self.gzip_level
    if arg_present(maxrec)     then maxrec     =  self.maxrec
    if arg_present(name)       then name       =  self.name
    if arg_present(nelements)  then nelements  =  self.nelements
    if arg_present(number)     then number     =  self.number
    if arg_present(recvar)     then recvar     =  self.recvar
    if arg_present(zvariable)  then zvariable  =  self.zvariable
    if arg_present(compression) then begin
        case self.compression of
            0: compression = 'None'
            1: compression = 'Run-Length Encoding'
            2: compression = 'Huffman'
            3: compression = 'Adaptive Huffman'
            5: compression = 'GZIP'
        endcase
    endif
end


;+
;   The purpose of this method is to load metadata associated with each variable in the
;   CDF file.
;-
pro MrCDF_Variable::Parse
    compile_opt strictarr
    
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if n_elements(thisQuiet) gt 0 then !quiet = thisQuiet
        MrPrintF, 'LogErr'
        return
    endif

;---------------------------------------------------------------------
; Variable Info //////////////////////////////////////////////////////
;---------------------------------------------------------------------
    
    ;Get the parent ID.
    parentID = self.parent -> GetFileID()
    
    ;Inquire about the variable to get its name
    varinq = cdf_varinq(parentID, self.number, ZVARIABLE=self.zvariable)
    self.cdf_type  = varinq.datatype
    self.nelements = varinq.numelem
    self.recvar    = varinq.recvar
    *self.dimvar   = varinq.dimvar
    *self.dim      = varinq.dim

    ;More info
    cdf_control, parentID, VARIABLE=self.name, ZVARIABLE=self.zvariable, GET_VAR_INFO=varinfo
    self.maxrec = varinfo.maxrec
    if max(tag_names(varinfo) eq 'PADVALUE') then self.padvalue = ptr_new(varinfo.padvalue)
    
    ;Compression
    cdf_compression, parentID, VARIABLE=self.name, ZVARIABLE=self.zvariable, $
                     GET_VAR_COMPRESSION=compression, GET_VAR_GZIP_LEVEL=gzip_level
    self.compression = compression
    if compression eq 5 then *self.gzip_level = gzip_level

;---------------------------------------------------------------------
; Variable Attribute Info ////////////////////////////////////////////
;---------------------------------------------------------------------
    ;Clear any attributes that exist. When the attribute object is created, it
    ;will be added to the parent's container. There is no need to destroy the
    ;old attributes here, as the parent will do that when it is destroyed.
    self.attributes -> Remove, /ALL

    ;Quiet
    thisQuiet = !quiet
    !quiet    = self.quiet

    ;Variable attributes *should* be /after/ global attributes, but I have seen
    ;a number of files where this is not the case. Thus, check each attribute.
    fileInfo = cdf_inquire(parentID) 
    for attrNum = 0, fileInfo.nAtts-1 do begin
        ;Check if the attribute exists for this variable.
        cdf_attinq, parentID, attrNum, attrName, attrScope, maxEntry
        cdf_attget_entry, parentID, attrName, self.name, AttEntryType, Value, status
        if status eq 0 then continue

        ;Has the attribute already been discovered? 
        tf_has = self.parent -> HasAttr(attrName, OBJECT=attrObj)
        if tf_has eq 0 then begin
            self.parent -> CreateAttrObj, attrName
            tf_has = self.parent -> HasAttr(attrName, OBJECT=attrObj)
        endif

        ;Add the attribute to the
        self.attributes -> Add, attrobj
    endfor
    
    ;Unquiet
    !quiet = thisQuiet
end


;+
;   Set the number of pre-allocated records for the variable. This
;   must be done before writing data to the variable.
;
; :Private:
;
; :Params:
;       NRECS:      in, required, type=integer
;                   Number of records to be initially written to the file.
;-
pro MrCDF_Variable::SetInitialRecs, nRecs
	compile_opt strictarr
	on_error, 2

	;Get the CDF ID
	fileID = self.parent -> GetFileID()

	;Set initial recs
	cdf_control, fileID, VARIABLE=self.name, SET_INITIALRECS=nrecs
end


;+
;   Set the compression type for the CDF variable. The file must be writable and compression
;   settings must be set before allocating or writing variable data. Individual
;   variables can be compressed differently from the rest of the CDF file.
;
; :Private:
;
; :Params:
;       COMPRESSION:        in, optional, type=integer, default='None'
;                           Type of compression to be performed on single-file CDFs::
;                               0 or 'None'
;                               1 or 'Run-Length Encoding'
;                               2 or 'Huffman'
;                               3 or 'Adaptive Huffman'
;                               5 or 'GZip' (See the `GZIP_LEVEL` keyword)
;
; :Keywords:
;       GZIP_LEVEL:         in, optional, type=byte, default=5
;                           Desired effort of GZip compression, from 1-9. Automatically
;                               sets `COMPRESSION`=5.
;-
pro MrCDF_Variable::SetCompression, compression, $
GZIP_LEVEL=gzip_level
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Check if the file is writable
    self.parent -> GetProperty, WRITEABLE=writeable
    if writeable eq 0 then $
        message, 'File is not writable. Cannot set compression.'

    ;GZIP_LEVEL sets COMPRESSION to 'GZIP'
    if n_elements(gzip_level)  gt 0 then compression = 'GZIP'
    if n_elements(compression) eq 0 then compression = 'NONE'

    ;Convert to a string if necessary
    comp = strtrim(compression, 2)
    case strupcase(comp) of
        '0':                   comp = 0
        '1':                   comp = 1
        '2':                   comp = 2
        '3':                   comp = 3
        '5':                   comp = 5
        'NONE':                comp = 0
        'RUN-LENGTH ENCODING': comp = 1
        'HUFFMAN':             comp = 2
        'ADAPTIVE HUFFMAN':    comp = 3
        'GZIP':                comp = 5
        else: message, 'Invalid compression: "' + comp + '".'
    endcase
    
    ;Default to GZIP_LEVEL=5
    if comp eq 5 && n_elements(gzip_level) eq 0 then gzip_level = 5
    
    ;Set the file compression
    fileID = self.parent -> GetFileID()
    cdf_compression, fileID, $
                     VARIABLE            = self.name, $
                     ZVARIABLE           = self.zvariable, $
                     SET_VAR_COMPRESSION = comp, $
                     SET_VAR_GZIP_LEVEL  = gzip_level
                     
    ;Set the object property
    self.compression = comp
    
    ;Reset the gzip_level pointer
    if comp ne 5 then begin
        ptr_free, self.gzip_level
        self.gzip_level = ptr_new(/ALLOCATE_HEAP)
    endif else begin
        *self.gzip_level = gzip_level
    endelse
end


;+
;   Set properties of the object.
;
; :Keywords:
;       QUIET:          in, optional, type=boolean
;                       If set, warning messages from the CDF DLM will not be output.
;-
pro MrCDF_Variable::SetProperty, $
QUIET=quiet
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    if n_elements(quiet) gt 0 then self.quiet = keyword_set(quiet)
end


;+
;   Parse variable information into a structure.
;-
function MrCDF_Variable::ToStruct, $
READ_DATA=read_data
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, -1
    endif
    
    ;Defaults
    read_data = keyword_set(read_data)
    
    ;Number of dimensions
    ;   - Zero dimensional CDF files have DIMVAR=0
    nDimensions = n_elements(*self.dimvar)
    if nDimensions eq 1 && *self.dimvar eq 0 then nDimensions = 0
    
    ;Get the data?
    if read_data $
        then data = self -> GetValue() $
        else data = '<NotRead>'
    
    ;Information about variable.
    var_struct = {_NAME:         self.name, $
                  _DATA:              data, $
                  _TYPE:              'VARIABLE', $
                  _CDF_TYPE:     self.cdf_type, $
                  _NELEMENTS:    self.maxrec + 1, $
                  _NDIMENSIONS:       nDimensions, $
                  _DIMENSIONS:  *self.dim, $
                  _ISZVAR:       self.zvariable, $
                  _RECVAR:       self.recvar, $
                  _DIMVAR:      *self.dimvar}
    
    ;Information about variable attributes.
    allAttrs = self.attributes -> Get(/ALL, COUNT=nAttrs)
    for i = 0, nAttrs - 1 do begin
        varAttr_struct = allAttrs[i] -> ToStruct(self.name)
    
        ;Create a structure tag out of the name.
        ;   Convert to uppercase.
        ;   Convert all non-alphanumeric characters to underscores.
        tag = varAttr_struct._NAME
        tag = strjoin(strsplit(strupcase(tag), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        
        ;Add the attribute to the variable structure
        var_struct = create_struct(var_struct, tag, varAttr_struct)
    endfor
    
    return, var_struct
end


;+
;   Write a value to an attribute.
;
; :Params:
;       ATTRNAME:           in, required, type=string/object
;                           The name of an attribute whose value is to be written.
;       VALUE:              in, required, type="CDF_DOUBLE"
;                           Value to be written to the attribute.
;
; :Keywords:
;       CREATE:             in, optional, type=boolean, default=0
;                           If set the attribute will be created. If the attribute already
;                               exists, this keyword has no effect.
;       CDF_EPOCH:          in, optional, type=boolean, default=0
;                           If set, `VALUE` will be written as a "CDF_EPOCH"
;                               (i.e. "CDF_FLOAT4"). The default is "CDF_DOUBLE"
;-
pro MrCDF_Variable::WriteAttrValue, attrName, value, $
CREATE=create, $
CDF_EPOCH=cdf_epoch
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return
    endif
    
    ;Is the file writeable?
    self.parent -> GetProperty, FILEID=parentID, WRITEABLE=writeable
    if writeable eq 0 then $
        message, 'File is not writable. Cannot write variable attribute data.'

    ;Does the variable have the attribute?
    if self -> HasAttr(attrName) eq 0 then begin
        
        ;Does the file have the attribute?
        tf_has = self.parent -> HasAttr(attrName, OBJECT=attrObj)
        if tf_has then begin
            self.attributes -> Add, attrObj
            
        ;Does the attribute need to be created?
        endif else if keyword_set(create) then begin
            self.parent -> CreateAttr, attrName, /VARIABLE_SCOPE
            tf_has = self.parent -> HasAttr(attrName, OBJECT=attrObj)
            self.attributes -> Add, attrObj
            
        ;Cannote write
        endif else begin
            message, 'Attribute "' + attrName + '" does not exist for variable "' + self.name + '".'
        endelse
    endif
    
    ;Write the value to the attribute
    cdf_attput, parentID, attrName, self.name, value, CDF_EPOCH=cdf_epoch
end


;+
;   Clean up after the object is destroyed
;-
pro MrCDF_Variable::cleanup
    ;Destroy objects
    obj_destroy, self.attributes
    
    ;Free pointers
    ptr_free, self.dimvar
    ptr_free, self.dim
    ptr_free, self.gzip_level
    ptr_free, self.padvalue
end


;+
;   Initialization method.
;
; :Params:
;       VARNAME:        in, required, type=string
;                       A valid CDF variable name contained within `PARENT`
;       PARENT:         in, required, type=object
;                       CDF_File object reference.
;
; :Keywords:
;       ISZVAR:         out, optional, type=boolean
;                       Named variable into which the zVariable state is return. 1 (0)
;                           indicates a zVariable (rVariable).
;-
function MrCDF_Variable::init, varname, parent, $
ISZVAR=isZVar, $
QUIET=quiet
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        MrPrintF, 'LogErr'
        return, 0
    endif
    
    ;Defaults
    if MrIsA(varname, /SCALAR, 'STRING') eq 0 then $
        message, 'VarName must be a scalar string.'
    
    if MrIsA(parent, /SCALAR, 'OBJREF') eq 0 then $
        message, 'Parent must be a scalar object.'
    
    ;Allocate Pointers
    self.attributes = obj_new('MrCDF_Container')
    self.dimvar     = ptr_new(/ALLOCATE_HEAP)
    self.dim        = ptr_new(/ALLOCATE_HEAP)
    self.gzip_level = ptr_new(/ALLOCATE_HEAP)
    self.padvalue   = ptr_new(/ALLOCATE_HEAP)
    self.quiet      = n_elements(quiet) eq 0 ? 1 : keyword_set(quiet)
    
    ;Get the variable number and Z-Variable state
    parentID = parent -> GetFileID()
    varnum = cdf_varnum(parentID, varname, isZvar)
    
    ;Set Properties
    self.name      = varname
    self.number    = varnum
    self.parent    = parent
    self.zvariable = isZvar
    
    ;Parse the file
    self -> Parse

    return, 1
end


;+
;   Class definition
;
; :Hidden:
;
; :Params:
;       CLASS:              out, optional, type=structure
;                           Class definition structure.
;
; :Fields:
;       ATTRIBUTES:     CDF_Container object for holding variable attributes.
;       CDF_TYPE:       CDF datatype.
;       COMPRESSION:    Type of variable compression.
;       DIMVAR:         Dimensional variance of the CDF variable data.
;       MAXREC:         Maximum number of records associated with the data (0-based).
;       NAME:           CDF variable name.
;       NELEMENTS:      Number of elements of the CDF variable data.
;       NUMBER:         CDF variable number.
;       PADVALUE:       Value used to pad data records.
;       PARENT:         CDF_File object.
;       QUIET:          Suppress warnings from the CDF DLM.
;       RECVAR:         Record variance of the CDF variable data.
;       ZVARIABLE:      Indicates that the variable is a z-variable, not an r-variable.
;-
pro MrCDF_Variable__define, class
    compile_opt strictarr
    
    class = { MrCDF_Variable, $
              inherits IDL_Object, $
              parent:      obj_new(), $
              attributes:  obj_new(), $
              name:        '', $
              number:      0L, $
              quiet:       0B, $
              compression: 0S, $
              gzip_level:  ptr_new(), $
              
              ;VAR_INQ
              cdf_type:   '', $
              nelements:  0L, $
              recvar:     '', $
              dimvar:     ptr_new(), $
              dim:        ptr_new(), $
              zvariable:  0B, $
              
              ;VAR_INFO:
              maxrec:     0L, $
              padvalue:   ptr_new() $
             }
end