; docformat = 'rst'
;
; NAME:
;       CDF_Variable__Define
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
;-
;*****************************************************************************************
;+
;   Provide information when the PRINT procedure is called.
;-
function CDF_Variable::_OverloadPrint
    on_error, 2
    
    nameStr   = string('Name:',      self.name,      FORMAT='(a-23, a0)')
    numberStr = string('Number:',    self.number,    FORMAT='(a-20, i0)')
    zVarStr   = string('ZVariable:', self.zvariable, FORMAT='(a-20, i0)')
    maxRecStr = string('MaxRec:',    self.maxRec,    FORMAT='(a-20, i0)')
    recVarStr = string('RecVar:',    self.RecVar,    FORMAT='(a-20, a0)')

    dimStr    = string('Dim:',    '[' + strjoin(string(*self.dim,    FORMAT='(i0)'), ', ') + ']', FORMAT='(a-20, a0)')
    dimVarStr = string('DimVar:', '[' + strjoin(string(*self.dimvar, FORMAT='(i1)'), ', ') + ']', FORMAT='(a-20, a0)')
    
    ;Append all of the strings together. Make a column so each is
    ;printed on its own line.
    output = [[nameStr], $
              [numberStr], $
              [zVarStr], $
              [MaxRecStr], $
              [recVarStr], $
              [dimStr], $
              [dimVarStr]]
    
    ;Variable Attributes
    varAttrStr = 'VARATTRS:'
    allVarAttrs = self.attributes -> Get(/ALL, COUNT=varAttrCount)
    for i = 0, varAttrCount-1 do begin
        thisVarAttr = allVarAttrs[i]
        attrName = thisVarAttr -> GetName()
        attrValue = thisVarAttr -> GetVarAttrValue(self.name, CDF_TYPE=cdf_type)
        
        attrNameStr = string('Name:', attrName, FORMAT='(a-17, a0)')
        attrTypeStr = string('CDF Type', cdf_type, FORMAT='(a-14, a0)')
        case 1 of
            MrIsA(attrValue, 'STRING'): begin
                if n_elements(attrValue) eq 1 $
                    then attrValueStr = string('Value', '"' + attrValue + '"', FORMAT='(a-14, a0)') $
                    else attrValueStr = string('Value', '["' + strjoin(attrValue, '", "') + '"]', FORMAT='(a-14, a0)')
            endcase
            
            MrIsA(attrValue, 'INTEGER'): begin
                if n_elements(attrValue) eq 1 $
                    then attrValueStr = string('Value', string(attrValue, FORMAT='(i0)'), FORMAT='(a-14, a0)') $
                    else attrValueStr = string('Value', '[' + strjoin(string(attrValue, FORMAT='(i0)'), ', ') + ']', FORMAT='(a-14, a0)')
            endcase
            
            else: begin
                if n_elements(attrValue) eq 1 $
                    then attrValueStr = string('Value', string(attrValue, FORMAT='(f0)'), FORMAT='(a-14, a0)') $
                    else attrValueStr = string('Value', '[' + strjoin(string(attrValue, FORMAT='(f0)'), ', ') + ']', FORMAT='(a-14, a0)')
            endcase
        endcase
        
        varAttrStr = [[varAttrStr], $
                      ['   ' + attrNameStr], $
                      ['      ' + attrTypeStr], $
                      ['      ' + attrValueStr]]
    endfor
    
    ;Join the variable with its attributes 
    output = [[output], [varAttrStr]]
    
    ;Offset everything form the variable name
    output[0,1:*] = '   ' + output[0,1:*]
    
    return, output
end


;+
;   Return the names of the attributes associated with this variable.
;
; :Returns:
;       ATTRNAMES           Attribute name(s) associated with the variable
;-
function CDF_Variable::GetAttrNames
    compile_opt strictarr
    on_error, 2
    
    ;Get all of the attributes
    count = self.attributes -> Count()
    if count eq 0 then return, ''
    
    ;Get all of the names
    attrNames = strarr(count)
    allAttrs = self -> Get(/ALL)
    for i = 0, count - 1 do attrNames[i] = allAttrs[i] -> GetName()
    
    ;Return a scalar
    if count eq 1 then attrNames = attrNames[0]
    
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
;       DATATYPE:           out, optional, type=string
;                           CDF datatype of `ATTRVALUE`. Posibilities are: 'CDF_BYTE',
;                               'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                               'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                               'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                               'CDF_UINT2', 'CDF_UINT4'.
;
; :Returns:
;       ATTRVALUE:          Value of `ATTRIBUTE`.
;-
function CDF_Variable::GetAttrValue, attribute, $
CDF_TYPE=cdf_type
    compile_opt strictarr
    on_error, 2
    
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
    
    return, attrValue
end


;+
;   Return the name of the variable.
;
; :Returns:
;       VARNAME:            CDF variable name.
;-
function CDF_Variable::GetName
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
function CDF_Variable::GetNumber, $
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
;                           Number of records to be read.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records when reading multiple records.
;       REC_START:          in, optional, type=integer, defualt=0
;                           Record at which to begin reading data.
;       STRING:             in, optional, type=boolean, default=0
;                           If set, "CDF_CHAR" and "CDF_UCHAR" data will be converted
;                               to strings. The are read from the file as byte-arrays.
;       DATATYPE:           out, optional, type=string
;                           CDF datatype of the variable being read. Possibilities are:
;                               'CDF_EPOCH', 'CDF_BYTE', 'CDF_CHAR', 'CDF_DOUBLE', 
;                               'CDF_REAL8', 'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4',
;                               'CDF_INT1', 'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR',
;                               'CDF_UINT1', 'CDF_UINT2', 'CDF_UINT4'.
;       FILLVALUE:          out, optional, type=any
;                           Value used as a filler for missing data.
;       PADVALUE:           out, optional, type=any
;                           Value used to pad the data when more data is read than what
;                               exists in the file. It is possible that this value does
;                               not exist.
;
; :Returns:
;       VALUE:              Value of the CDF variable.
;-
function CDF_Variable::GetValue, $
;INPUT
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
REC_COUNT=rec_count, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start, $
STRING=string, $
;OUTPUT
CDF_TYPE=cdf_type, $
FILLVALUE=fillvalue, $
PADVALUE=padvalue
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    single_value = keyword_set(single_value)
    if n_elements(string)    eq 0 then string    = 1
    if n_elements(rec_count) eq 0 then rec_count = self.maxrec + 1
    if n_elements(rec_start) eq 0 then rec_start = 0
    
    ;Get the file ID
    parentID = self.parent -> GetFileID()
    
    ;Get the value(s)
    if rec_count eq 1 then begin
        cdf_varget1, parentID, self.name, value, OFFSET=offset, REC_START=rec_start, STRING=string
    endif else begin
        cdf_varget, parentID, self.name, value, COUNT=count, INTERVAL=interval, $
                    OFFSET=offset, REC_COUNT=rec_count, REC_INTERVAL=rec_interval, $
                    REC_START=rec_start, STRING=string
    endelse
    
    ;Output Keywords
    cdf_type  = self.cdf_type
    if ptr_valid(self.padvalue)      then padvalue  = *self.padvalue
    if self  -> HasAttr('FILLVALUE') then fillvalue =  self -> GetAttrValue('FILLVAL')
    
    return, value
end


;+
;   Determin if the variable has a particular attribute.
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           Name of the attr
;-
function CDF_Variable::HasAttr, attrName, $
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
;       NAME:           out, optional, type=string
;                       CDF variable name.
;       NUMBER:         out, optional, type=integer
;                       CDF variable number.
;       DATATYPE:       out, optional, type=string
;                       CDF data type. Posibilities are: 'CDF_BYTE',
;                           'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                           'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                           'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                           'CDF_UINT2', 'CDF_UINT4'.
;       NELEMENTS:      out, optional, type=long
;                       Number of data elements.
;       RECVAR:         out, optional, type=string
;                       Record variance of the data.
;       DIMVAR:         out, optional, type=bytarr
;                       Dimensional variance of the data.
;       DIM:            out, optional, type=lonarr
;                       Dimension sizes of the data.
;-
pro CDF_Variable::GetProperty, $
NAME=name, $
NUMBER=number, $
CDF_TYPE=cdf_type, $
NELEMENTS=nElements, $
RECVAR=recvar, $
DIMVAR=dimvar, $
DIM=dim, $
MAXREC=maxrec
    compile_opt strictarr
    on_error, 2
    
    ;Get Properties
    if arg_present(cdf_type)  then cdf_type  =  self.cdf_type
    if arg_present(dim)       then dim       = *self.dim
    if arg_present(dimvar)    then dimvar    = *self.dimvar
    if arg_present(maxrec)    then maxrec    =  self.maxrec
    if arg_present(name)      then name      =  self.name
    if arg_present(nelements) then nelements =  self.nelements
    if arg_present(number)    then number    =  self.number
    if arg_present(recvar)    then recvar    =  self.recvar
end


;+
;   The purpose of this method is to load metadata associated with each variable in the
;   CDF file.
;-
pro CDF_Variable::ParseVariable
    compile_opt strictarr

;---------------------------------------------------------------------
;Catch Errors ////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Get the parent ID.
    parentID = self.parent -> GetFileID()
    
    ;Inquire about the variable to get its name
    varinq = cdf_varinq(parentID, self.number, ZVARIABLE=self.zvariable)
    self.cdf_type  = varinq.datatype
    self.nelements = varinq.numelem
    self.recvar    = varinq.recvar
    self.dimvar    = ptr_new(varinq.dimvar)
    self.dim       = ptr_new(varinq.dim)

    ;More info
    cdf_control, parentID, VARIABLE=self.name, ZVARIABLE=self.zvariable, GET_VAR_INFO=varinfo
    self.maxrec = varinfo.maxrec
    if max(tag_names(varinfo) eq 'PADVALUE') then self.padvalue = ptr_new(varinfo.padvalue)

;---------------------------------------------------------------------
;Step Through All of the Attributes //////////////////////////////////
;---------------------------------------------------------------------
    ;Clear any attributes that exist. When the attribute object is created, it
    ;will be added to the parent's container. There is no need to destroy the
    ;old attributes here, as the parent will do that when it is destroyed.
    self.attributes -> Remove, /ALL

    ;Variable attributes *should* be /after/ global attributes, but I have seen
    ;a number of files where this is not the case. Thus, check each attribute.
    fileInfo = cdf_inquire(parentID) 
    for attrNum = 0, fileInfo.nAtts-1 do begin
        ;Check if the attribute exists for this variable.
        cdf_attinq, parentID, attrNum, attrName, attrScope, maxEntry
        cdf_attget_entry, parentID, attrName, self.name, AttEntryType, Value, status
        if status eq 0 then continue

        ;Has the attribute already been discovered? 
        tf_has = self.parent -> HasVarAttr(attrName, OBJECT=attrObj)
        if tf_has eq 0 then begin
            self.parent -> CreateAttrObj, attrName, /VARIABLE
            tf_has = self.parent -> HasVarAttr(attrName, OBJECT=attrObj)
        endif

        ;Add the attribute to the         
        self.attributes -> Add, attrobj
    endfor
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
;                           If set, the attribute will be created.
;       CDF_EPOCH:          in, optional, type=boolean, default=0
;                           If set, `VALUE` will be written as a "CDF_EPOCH"
;                               (i.e. "CDF_FLOAT4"). The default is "CDF_DOUBLE"
;-
pro CDF_Variable::WriteAttrValue, attrName, value, $
CREATE=create, $
CDF_EPOCH=cdf_epoch
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Is the file writeable?
    self.parent -> GetProperty, FILEID=parentID, WRITEABLE=writeable
    if writeable eq 0 then $
        message, 'File is not writable. Cannot write variable attribute data.'

    ;Does the variable have the attribute?
    if self -> HasAttr(attrName) eq 0 then begin
        
        ;Does the file have the attribute?
        tf_has = self.parent -> HasVarAttr(attrName, OBJECT=attrObj)
        if tf_has then begin
            self.attributes -> Add, attrObj
            
        ;Does the attribute need to be created?
        endif else if keyword_set(create) then begin
            self.parent -> WriteVarAttrDef, attrName
            tf_has = self.parent -> HasVarAttr(attrName, OBJECT=attrObj)
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
pro CDF_Variable::cleanup
    ;Destroy objects
    obj_destroy, self.attributes
    
    ;Free pointers
    ptr_free, self.dimvar
    ptr_free, self.dim
    ptr_free, self.padvalue
end


;+
;   Initialization method.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           A valid CDF variable name contained within `PARENT`
;       PARENT:             in, required, type=object
;                           CDF_File object reference.
;-
function CDF_Variable::init, varname, parent, $
ISZVAR=isZVar
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif
    
    ;Defaults
    if MrIsA(varname, /SCALAR, 'STRING') eq 0 then $
        message, 'VarName must be a scalar string.'
    
    if MrIsA(parent, /SCALAR, 'OBJREF') eq 0 then $
        message, 'Parent must be a scalar object.'
    
    ;Allocate Pointers
    self.attributes = obj_new('CDF_Container')
    
    ;Get the variable number and Z-Variable state
    parentID = parent -> GetFileID()
    varnum = cdf_varnum(parentID, varname, isZvar)
    
    ;Set Properties
    self.name      = varname
    self.number    = varnum
    self.parent    = parent
    self.zvariable = isZvar
    
    ;Parse the file
    self -> ParseVariable

    return, 1
end


;+
; The init method for CDF_Info__DEFINE.PRO
;
; :Hidden:
;
; :Fields:
;       PARENT:         CDF_File object.
;       ATTRIBUTES:     CDF_Container object for holding variable attributes.
;       NAME:           CDF variable name.
;       NUMBER:         CDF variable number.
;       DATATYPE:       CDF datatype.
;       NELEMENTS:      Number of elements of the CDF variable data.
;       RECVAR:         Record variance of the CDF variable data.
;       DIMVAR:         Dimensional variance of the CDF variable data.
;       ZVARIABLE:      Indicates that the variable is a z-variable, not an r-variable.
;       MAXREC:         Maximum number of records associated with the data (0-based).
;       PADVALUE:       Value used to pad data records.
;-
pro CDF_Variable__define
    compile_opt strictarr
    
    define = { CDF_Variable, $
               parent:     obj_new(), $
               attributes: obj_new(), $
               name:       '', $
               number:     0L, $
               
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