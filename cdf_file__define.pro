; docformat = 'rst'
;
; NAME:
;       CDF_File__Define
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
;       This is class for reading CDF files. The CDF file is opened and Global and
;       Variable attributes are automatically loaded. Data (and metadata) can be read 
;       and accessed through the class methods.
;
;       Information about CDF files can be found at the following site::
;           http://spdf.gsfc.nasa.gov/istp_guide/istp_guide.html
;           http://cdaweb.gsfc.nasa.gov/pub/software/cdf/dist/cdf34_1/idl/
;           http://cdf.gsfc.nasa.gov/
;           http://cdf.gsfc.nasa.gov/html/leapseconds.html
;
; CDF_TYPE:
;   CDF data type names and their corresponding IDL type names::
;     ---CDF Type---        ---IDL Type--   -----Description-----
;       'CDF_BYTE'          -   INT     -   1-byte, signed integer
;       'CDF_CHAR'          -   BYTE    -   1-byte, unsigned integer
;       'CDF_DOUBLE'        -   DOUBLE  -   8-byte, double-precision floating point
;       'CDF_REAL8'         -   DOUBLE  -   8-byte, double-precision floating point
;       'CDF_EPOCH'         -   DOUBLE  -   8-byte, double-precision floating point (time)
;       'CDF_LONG_EPOCH'    -   DCOMPLEX-   two 8-byte, double-precision floating point (time)
;       'CDF_FLOAT'         -   FLOAT   -   4-byte, single-precision floating point
;       'CDF_REAL4'         -   FLOAT   -   4-byte, single-precision floating point
;       'CDF_INT1'          -   INT     -   1-byte, signed integer
;       'CDF_INT2'          -   LONG    -   2-byte, signed integer
;       'CDF_INT4'          -   LONG64  -   4-byte, signed integer
;       'CDF_INT8'          -   LONG64  -   8-byte, signed integer
;       'CDF_UCHAR'         -   STRING  -   1-byte, unsigned integer
;       'CDF_UINT1'         -   BYTE    -   1-byte, unsigned integer
;       'CDF_UINT2'         -   UINT    -   2-byte, unsigned integer
;       'CDF_UINT4'         -   ULONG   -   4-byte, unsigned integer
;
; :Examples:
;       See the CDF_File_Examples.pro program for a demonstration of how to create,
;       write, read, and copy CDF files.
;
; :Categories:
;       CDF Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg.pro
;       CDF_Attribute__Define.pro
;       CDF_CastDataType.pro
;       CDF_Container__Define.pro
;       CDF_File_Examples.pro
;       CDF_Variable__Define.pro
;       MrCmpVersion.pro
;       MrCDF_Epoch_Parse.pro
;       MrCDF_Epoch_Compare.pro
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
;       2014/03/23  -   Removed the Parse_Attributes and Parse_Variables methods. Renamed
;                           the Parse method to ParseFile. WriteGlobalAttr now accepts
;                           arrays of values. Added the _OverloadPrint and PrintFileInfo
;                           methods. - MRA
;       2014/03/27  -   Added ENCODING and DECODING keywords to the Open method. - MRA
;       2014/03/31  -   Split the ATTRIBUTES property into GATTRS and VATTRS. Also split
;                           the VARIABLES property into RVARS and ZVARS. - MRA
;-
;*****************************************************************************************
;+
;   Provide information when the PRINT procedure is called.
;-
function CDF_File::_OverloadPrint
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, ''
    endif
    
    ;Has the file been parsed?
    if self.isParsed eq 0 then self -> ParseFile
    
    ;Number of global attributes
    gAttrCount = self.gAttrs -> Count()
    vAttrCount = self.vAttrs -> Count()
    rVarCount  = self.rVars  -> Count()
    zVarCount  = self.zVars  -> Count()
    
    ;File information
    fileStr  = string('FILENAME:',              self.filename, FORMAT='(a-33, a0)')
    nGAttrStr = string('# Global Attributes:',   gAttrCount,    FORMAT='(a-30, i0)')
    nVAttrStr = string('# Variable Attributes:', vAttrCount,    FORMAT='(a-30, i0)')
    nRVarStr  = string('# R-Variables:',         rVarCount,     FORMAT='(a-30, i0)')
    nZVarStr  = string('# Z-Variables:',         zVarCount,     FORMAT='(a-30, i0)')
    
    ;Output string for the file
    fileOut = [[fileStr], $
               [nGAttrStr], $
               [nVAttrStr], $
               [nRVarStr], $
               [nZVarStr]]
               
    ;Offset everything from the file name
    fileOut[0,1:*] = '   ' + fileOut[0,1:*]
    
    ;Global attribute information
    gAttrStr = 'GLOBAL ATTRIBUTES'
    allGAttrs = self.gAttrs -> Get(/ALL, COUNT=gAttrCount)
    for i = 0, gAttrCount - 1 do begin
        gAttrInfo = allGAttrs[i] -> _OverloadPrint()
        gAttrInfo = '   ' + gAttrInfo
        
        ;Gather information from all attributes
        gAttrStr = [[gAttrStr], [gAttrInfo]]
    endfor
    
    zVarStr = 'Z-VARIABLES'
    allZVars = self.zVars -> Get(/ALL, COUNT=zVarCount)
    for i = 0, zVarCount - 1 do begin
        zVarInfo = allZVars[i] -> _OverloadPrint()
        zVarInfo = '   ' + zVarInfo
        
        ;Gather information from all z-variables
        zVarStr = [[zVarStr], [zVarInfo]]
    endfor
    
    ;Gather all of the output information
    output = [[fileOut], $
              [gAttrStr], $
              [zVarStr]]
    
    return, output
end


;+
;   Close the CDF file
;-
pro CDF_File::Close
    on_error, 2

    cdf_close, self.fileID
    self.fileID = 0
end


;+
;   The purpose of this method is to print CDF global attribute conventions
;-
pro CDF_File::ConventionsGlobalAttr
    compile_opt strictarr
    on_error, 2
    
    ;Print the global attribute conventsions.
    print, ''
    
    ;Print the URL
    print, 'http://spdf.gsfc.nasa.gov/istp_guide/gattributes.html'
end


;+
;   The purpose of this method is to print CDF variable attribute conventions
;-
pro CDF_File::ConventionsVarAttr
    compile_opt strictarr
    on_error, 2
    
    ;Print the variable attribute conventsions.
    print, [['CATDESC',          'Required'], $
            ['DEPEND_0',         'Required'], $
            ['DEPEND_1',         'Required'], $
            ['DEPEND_2',         'Required'], $
            ['DEPEND_3',         'Required'], $
            ['DISPLAY_TYPE',     'Required'], $
            ['FIELDNAM',         'Required'], $
            ['FILLVAL',          'Required'], $
            ['FORMAT',           'Required'], $
            ['FORM_PTR',         'Required'], $
            ['LABLAXIS',         'Required'], $
            ['LABL_PTR_1',       'Required'], $
            ['LABL_PTR_2',       'Required'], $
            ['LABL_PTR_3',       'Required'], $
            ['UNITS',            'Required'], $
            ['UNIT_PTR',         'Required'], $
            ['VALIDMIN',         'Required'], $
            ['VALIDMAX',         'Required'], $
            ['VAR_TYPE',         'Recommended'], $
            ['SCALETYP',         'Recommended'], $
            ['VAR_NOTES',        'Recommended'], $
            ['AVG_TYPE',         'Optional'], $
            ['DELTA_PLUS_TYPE',  'Optional'], $
            ['DELTA_MINUS_TYPE', 'Optional'], $
            ['DICT_KEY',         'Optional'], $
            ['MONOTON',          'Optional'], $
            ['SCALEMIN',         'Optional'], $
            ['SCALEMAX',         'Optional'], $
            ['V_PARENT',         'Optional'], $
            ['DERIVN',           'cluster required'], $
            ['sig_digits',       'cluster recommended'], $
            ['SI_conv',          'cluster recommended']]
    
    ;Print the URL
    print, 'See also: http://spdf.gsfc.nasa.gov/istp_guide/vattributes.html'
            
            
end


;+
;   The purpose of this method is to create an attribute object.
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           A valid CDF global attribute name.
;-
pro CDF_File::CreateAttrObj, attrName, $
VARIABLE=variable
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Create the attribute and add it to the container.
    attrObj = obj_new('CDF_Attribute', attrName, self)
    
    ;Add the attribute to the proper container
    if n_elements(variable) eq 0 $
        then self.gAttrs -> Add, attrObj $
        else self.vAttrs -> Add, attrObj
end


;+
;   The purpose of this method is to create a variable object.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           A valid CDF variable name.
;-
pro CDF_File::CreateVarObj, varName
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Create the attribute and add it to the container.
    varObj = obj_new('CDF_Variable', varName, self, isZVar=isZVar)
    
    ;Add to the proper container.
    if isZVar $
        then self.zVars -> Add, varObj $
        else self.rVars -> Add, varObj
end


;+
;   Get the value of a variable attribute.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       ATTRNAME:           in, required, type=string
;                           Name of the variable attribute whose value is to be returned.
;-
function CDF_File::CopyGlobalAttrTo, attrName, destObj
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    case size(gAttr, /TNAME) of
        'OBJREF': attrObj = gAttribute
        'STRING': begin
            attrObj = self.attributes -> FindByName(gAttribute, COUNT=gAttrCount)
            if gAttrCount eq 0 then $
                message, 'Cannot find global attribute with name "' + gAttribute + '".'
            if obj_valid(attrObj) eq 0 then $
                message, 'Attribute object invalid for "' + gAttribute + '".'
        endcase
        else: message, 'GATTRIBUTE must be an attribute name or object.'
    endcase

    ;Get the attribute's value
    attrValue = varObj -> GetAttrValue(attrName, DATATYPE=datatype)
    
    ;Write the variable attribute to the destination file
    destObj -> WriteGlobalAttr, attrName, attrValue, DATATYPE=datatype
end


;+
;   Get the value of a variable attribute.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       ATTRNAME:           in, required, type=string
;                           Name of the variable attribute whose value is to be returned.
;-
function CDF_File::CopyVarAttrTo, variable, attrName, destObj
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = varName
        
        'STRING': begin
            varObj = self.variables -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase

    ;Get the variable attribute's value
    attrValue = varObj -> GetAttrValue(attrName, DATATYPE=datatype)
    
    ;Write the variable attribute to the destination file
    destObj -> WriteVarAttr, varName, attrName, attrValue, DATATYPE=datatype
end


;+
;   Get the value of a variable attribute.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       ATTRNAME:           in, required, type=string
;                           Name of the variable attribute whose value is to be returned.
;-
function CDF_File::CopyVariableTo, variable, destObj
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = varName
        
        'STRING': begin
            varObj = self.variables -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase

    ;Does the variable exist in the destination? If not, create it.
    if ~destObj -> HasVar(varName) then self -> CopyVarDefTo, varName, destObj
    
    ;Does the variable have attributes? Copy them.
    varAttrNames = varObj -> GetAttrNames(COUNT=nVarAttrs)
    for i = 0, nVarAttrs - 1 do begin
        thisAttrName = varAttrNames[i]
        if ~destObj -> HasVarAttr(thisAttrName) then $
            self -> CopyVarAttrTo, varName, thisAttrName, destObj
    endfor
    
    ;Copy the variable's data
    self -> CopyVarDataTo, varName, destObj
end


;+
;   Get the value of a variable attribute.
;
;   NOTE:
;       The variable must already exist in the desination object.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       ATTRNAME:           in, required, type=string
;                           Name of the variable attribute whose value is to be returned.
;-
function CDF_File::CopyVarDataTo, variable, destObj, $
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = varName
        
        'STRING': begin
            varObj = self.variables -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase

    ;Get the data
    data = varObj -> GetValue()
    
    ;Write the information
    destObj -> WriteVarData
end


;+
;   Delete a global attribute from the file.
;
; :Params:
;       GATTRNAME:          in, required, type=string
;                           Name of the global attribute to be deleted from the CDF file.
;       ENTRYNUM:           in, optional, type=integer
;                           Entry number to be deleted. If not present, then entire
;                               attribute is deleted.
;-
function CDF_File::DelGlobalAttr, gAttrName, entrynNum
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.writeable eq 0 then $
        message, 'Cannot delete global attribute because file is READ-ONLY.'
        
    ;Check to see if the global attribute exists
    tf_Has = self -> HasGlobalAttr(gAttrName, OBJECT=object)
    if tf_has eq 0 then message, 'Global attribute name does not exist: "' + gAttrName + '".'
    
    ;Delete the attribute
    if n_elements(entryNum) gt 0 $
        then cdf_attdelete, self.fileID, gAttrName, entryNum $
        else cdf_attdelete, self.fileID, gAttrName
    
    ;Remove the attribute from the container
    self.attributes -> Remove, object, /DESTROY
end


;+
;   Get the compression type for the CDF file or variable.
;
; :Keywords:
;       VARIABLE:           in, optional, type=string
;                           Name of the variable whose compression settings are to
;                               be retrieved. The default is to retrieve the compression
;                               settings of the file.
;       GZIP_LEVEL:         out, optional, type=byte, default=5
;                           Desired effort of GZip compression, from 1-9. Automatically
;                               sets `COMPRESSION`=5.
;
; :Returns:
;       COMPRESSION:        Type of compression set for the file or `VARIABLE`::
;                               0 - No compression
;                               1 - Run-Length Encoding
;                               2 - Huffman
;                               3 - Adaptive Huffman
;                               5 - GZip (See the `GZIP_LEVEL` keyword)
;-
function CDF_File::GetCompression, $
VARIABLE=variable, $
GZIP_LEVEL=gzip_level
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif

    ;Defaults/dependencies
    if n_elements(compression) eq 0 then compression = 0
    if n_elements(gzip_level)  gt 0 then compression = 5
    
    ;Variable Compression
    if n_elements(variable) gt 0 then begin
        cdf_compression, self.fileID, GET_VAR_GZIP_LEVEL=gzip_level, $
                         GET_VAR_COMPRESSION=compression, VARIABLE=variable
    
    ;File Compression
    endif else begin
        cdf_compression, self.fileID, GET_COMPRESSION=compression, $
                         GET_GZIP_LEVEL=gzip_level
    endelse
    
    return, compression
end


;+
;   Get the names of the global attributes.
;
; :Returns:
;       GATTRNAMES:         Name(s) of the global attribute(s) within the CDF file.
;-
function CDF_File::GetGEntryMask, gAttrName, $
GENTRYCOUNT=gEntryCount, $
MAXGENTRY=maxGEntry
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    ;Get the global attribute object
    gAttrObj = self.attributes -> FindByName(gAttrName)
    if obj_valid(gAttrObj) eq 0 then $
        message, 'Could not find attribute with name "' + gAttrName + '".'
    
    gAttrObj
    nGAttrs = self.attributes -> Count()
    if nGAttrs eq 0 then return, ''

    ;Get all of the global attribute names
    gAttrNames = strarr(nGAttrs)
    allGAttrs  = self.attributes -> Get(/ALL)
    for i = 0, nGAttrs-1 do gAttrNames[i] = allGAttrs[i] -> GetName()

    ;Return a scalar?
    if nGAttrs eq 1 then gAttrNames = gAttrNames[0]
    return, gAttrNames
end


;+
;   Get the names of the global attributes.
;
; :Returns:
;       GATTRNAMES:         Name(s) of the global attribute(s) within the CDF file.
;-
function CDF_File::GetGlobalAttrNames, $
COUNT=nGAttrs
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    ;Count the number of global attributes
    nGAttrs = self.gAttrs -> Count()
    if nGAttrs eq 0 then return, ''

    ;Get all of the global attribute names
    gAttrNames = strarr(nGAttrs)
    allGAttrs  = self.gAttrs -> Get(/ALL)
    for i = 0, nGAttrs-1 do gAttrNames[i] = allGAttrs[i] -> GetName()

    ;Return a scalar?
    if nGAttrs eq 1 then gAttrNames = gAttrNames[0]
    return, gAttrNames
end


;+
;   Get the value of a global attributes.
;
; :Params:
;       GATTRIBUTE:         in, required, type=string/object
;                           Name or CDF_Attribute object of the global attribute whose
;                               value is to be returned.
;
; :Keywords:
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of `GATTRVALUE`.  Posibilities are: 'CDF_BYTE',
;                               'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                               'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                               'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                               'CDF_UINT2', 'CDF_UINT4'.
;       GENTRYNUM:          in, optional, type=long
;                           The global entry number of the value to be returned. It
;                               is an index into the list of values associated with
;                               `GATTRIBUTE`. The default is to return all values.
;
; :Returns:
;       GATTRVALUE:         Value of the global attribute.
;-
function CDF_File::GetGlobalAttrValue, gAttribute, $
CDF_TYPE=cdf_type
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    case size(gAttribute, /TNAME) of
        'OBJREF': attrObj = gAttribute
        
        'STRING': begin
            attrObj = self.gAttrs -> FindByName(gAttribute, COUNT=gAttrCount)
            if gAttrCount eq 0 then $
                message, 'Cannot find global attribute with name "' + gAttribute + '".'
            if obj_valid(attrObj) eq 0 then $
                message, 'Attribute object invalid for "' + gAttribute + '".'
        endcase
        
        else: message, 'GATTRIBUTE must be an attribute name or object.'
    endcase

    ;Get the value
    value = attrObj -> GetGlobalAttrValue(CDF_TYPE=cdf_type)
    
    return, value
end


;+
;   Get the names of the CDF variables.
;
; :Returns:
;       VARNAMES:           Name(s) of the variable(s) within the CDF file.
;-
function CDF_File::GetVarNames, $
COUNT=nVars
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    ;Count the number of global attributes
    nVars = self.variables -> Count()
    if nVars eq 0 then return, ''

    ;Get all of the global attribute names
    varNames = strarr(nVars)
    allVars  = self.variables -> Get(/ALL)
    for i = 0, nVars-1 do varNames[i] = allVars[i] -> GetName()

    ;Return a scalar?
    if nVars eq 1 then varNames = varNames[0]
    return, varNames
end


;+
;   Get the names of the CDF variable attributes for a particular variable.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;
; :Keywords:
;       COUNT:              out, optional, type=integer
;                           Number of variable attributes associated with `VARIABLE`.
;
; :Returns:
;       ATTRNAMES:          Name(s) of the variable attributes associated with `VARIABLE`
;-
function CDF_File::GetVarAttrNames, variable, $
COUNT=varAttrCount
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = variable
        
        'STRING': begin
            varObj = self.variables -> FindByName(variable, COUNT=vAttrCount)
            if vAttrCount eq 0 then $
                message, 'Cannot find global attribute with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Attribute object invalid for "' + variable + '".'
        endcase
        
        else: message, 'GATTRIBUTE must be an attribute name or object.'
    endcase

    ;Get the value
    varAttrNames = varObj -> GetAttrNames(COUNT=varAttrCount)
    
    return, varAttrNames
end


;+
;   Get the value of a variable attribute.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       ATTRNAME:           in, required, type=string
;                           Name of the variable attribute whose value is to be returned.
;
; :Keywords:
;       DATATYPE:           out, optional, type=string
;                           CDF datatype of `GATTRVALUE`.  Posibilities are: 'CDF_BYTE',
;                               'CDF_CHAR', 'CDF_DOUBLE', 'CDF_REAL8', 'CDF_EPOCH', 
;                               'CDF_LONG_EPOCH', 'CDF_FLOAT', 'CDF_REAL4', 'CDF_INT1',
;                               'CDF_INT2', 'CDF_INT4', 'CDF_UCHAR', 'CDF_UINT1',
;                               'CDF_UINT2', 'CDF_UINT4'.
;
; :Returns:
;       ATTRVALUE:          Value of the attribute.
;-
function CDF_File::GetVarAttrValue, variable, attrName, $
CDF_TYPE=cdf_type
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    case size(varName, /TNAME) of
        'OBJREF': varObj = variable
        
        'STRING': begin
            varObj = self.variables -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase

    ;Get the variable attribute's value
    value = varObj -> GetAttrValue(attrName, CDF_TYPE=cdf_type)
    
    return, value
end


;+
;   Get the data from a CDF variable.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
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
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of the variable being read. See the file header.
;       FILLVALUE:          out, optional, type=any
;                           Value used as a filler for missing data.
;       PADVALUE:           out, optional, type=any
;                           Value used to pad the data when more data is read than what
;                               exists in the file. It is possible that this value does
;                               not exist.
;
; :Returns:
;       DATA:               Data of the CDF variable specified.
;-
function CDF_File::GetVarData, variable, $
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

    ;File must be parsed first
    if self.isParsed eq 0 then ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = variable
        'STRING': begin
            varObj = self.zVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then varObj = self.rVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase
    
    ;Get the data
    data = varObj -> GetValue(INTERVAL=interval, $
                              OFFSET=offset, $
                              REC_COUNT=rec_count, $
                              REC_INTERVAL=rec_interval, $
                              REC_START=rec_start, $
                              STRING=string, $
                              CDF_TYPE=cdf_type, $
                              FILLVALUE=fillvalue, $
                              PADVALUE=padvalue)
    
    return, data
end


;+
;   Get properties
;
; :Keywords:
;       FILENAME:           out, optional, type=string
;                           Name of the CDF file.
;       FILEID:              out, optional, type=long
;                           CDF file identifier.
;       NATTS:              out, optional, type=integer
;                           Total number of attributes within the file
;       NGATTS:             out, optional, type=integer
;                           Number of global attributes within the file
;       NVATTS:             out, optional, type=integer
;                           Number of variable attributes within the file
;       NVARS:              out, optional, type=integer
;                           Total number of variables within the file
;       NRVARS:             out, optional, type=integer
;                           Number of regular (r-)variables within the file
;       NZVARS:             out, optional, type=integer
;                           Number of z-variables within the file
;       WRITEABLE:          out, optional, type=boolean
;                           Flag indicating that the file is writeable (1) or read-only (0).
;-
pro CDF_File::GetProperty, $
FILENAME=filename, $
fileID=fileID, $
NATTRS=nAttrs, $
NGATTRS=nGAttrs, $
NVATTRS=nVAttrs, $
NVARS=nVars, $
NRVARS=nRVars, $
NZVARS=nZVars, $
WRITEABLE=writeable
    compile_opt strictarr
    on_error, 2
    
    if arg_present(filename)  then filename  = self.filename
    if arg_present(fileID)    then fileID    = self.fileID
    if arg_present(ngattrs)   then ngattrs   = self.gAttrs -> Count()
    if arg_present(nvattrs)   then nvattrs   = self.vAttrs -> Count()
    if arg_present(nrvars)    then nrvars    = self.rVars -> Count()
    if arg_present(nzvars)    then nzvars    = self.zVars -> Count()
    if arg_present(writeable) then writeable = self.writeable
    
    if arg_present(nattrs) then begin
        nGAttrs = self.gAttrs -> Count()
        nVAttrs = self.vAttrs -> Count()
        nattrs = nGAttrs + nVAttrs
    endif
    
    if arg_present(nvars) then begin
        nRVars = self.rVars -> Count()
        nZVars = self.zVars -> Count()
        nvars = nRVars + nZVars
    endif
end


;+
;   Return the CDF ID for the file.
;
; :Returns:
;       FILEID:         Identifier number for the CDF file.
;-
function CDF_File::GetFileID
    return, self.fileID
end


;+
;   Determines if a global attribute is present in the file.
;
; :Params:
;       GATTRNAME:          in, required, type=string
;                           Name of the attribute to search for.
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           CDF_Attribute object for the desired attribute.
;
; :Returns:
;       TF_HAS:             Returns true (1) of the attribute name exists, false (0) otherwise.
;-
function CDF_File::HasGlobalAttr, gAttrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try to find the object
    object = self.gAttrs -> FindByName(gAttrName, COUNT=count)
    
    if count gt 0 $
        then return, 1 $
        else return, 0
end


;+
;   Determines if a variable attribute is contained within the file. To determine if
;   a specific variable as a particular attribute, use the VarHasAttr method.
;
; :Params:
;       VARATTRNAME:        in, required, type=string
;                           Name of the variable attribute.
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           CDF_Attribute object reference for the attribute identified
;                               by `VARATTRNAME`.
;
; :Returns:
;       TF_EXISTS:          Returns true (1) of the variable attribute exists, 
;                               false (0) otherwise.
;-
function CDF_File::HasVarAttr, varAttrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try to find the object
    object = self.vAttrs -> FindByName(varAttrName, COUNT=count)
    if count eq 0 $
        then tf_exists = 0 $
        else tf_exists = 1
    
    return, tf_exists
end


;+
;   Determines if a variable is present in the CDF file.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the attribute to search for.
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           CDF_Attribute object for the desired attribute.
;
; :Returns:
;       TF_HAS:             Returns true (1) of the attribute name exists, false (0) otherwise.
;-
function CDF_File::HasVar, varName, $
ISZVAR=isZVar, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try to find the object
    object = self.zVars -> FindByName(varName, COUNT=count)
    if count eq 0 $
        then object = self.rVars -> FindByName(varName, COUNT=count) $
        else isZVar = 1B
        
    ;Was the variable found?
    if count gt 0 $
        then return, 1 $
        else return, 0
end


;+
; Open the CDF file, get a CDF ID, and get the number of variables and
; attributes within the file
;
; :Params:
;       FILENAME:           in, out, optional, type=string
;                           The complete path to the CDF file to be accessed. If not
;                               provided, a dialog to allow the user to pick a file.
;                           If `FILENAME` is present but not defined, 1 will be returned
;                               if a file was chosen, 0 if not (i.e. the cancel button
;                               on the dialog box was pushed).
;
; :Keywords:
;       BACKWARD_COMPATIBLE:    in, optional, type=boolean, default=0
;                               If set, the CDF file will be backward compatible to
;                                   versions of IDL pre-6.3. Note that this keyword
;                                   is sticky and will persist until
;                                   CDF_SET_CDF27_BACKWARD_COMPATIBLE is called again.
;       CANCEL:             out, optional, type=boolean
;                           Returns 1 if the cancel button on the dialog_pickfile box
;                               was pressed. Returns 0 otherwise.
;       ROW_MAJOR:          in, optional, type=boolean, default=0
;                           If set, use row-major (C-like) ordering for variable
;                               storage. The default is column-major (IDL-like).
;       COMPRESSION:        in, optional, type=byte, default=0
;                           Type of compression to be performed on single-file CDFs::
;                               0 - No compression
;                               1 - Run-Length Encoding
;                               2 - Huffman
;                               3 - Adaptive Huffman
;                               5 - GZip (See the `GZIP_LEVEL` keyword)
;       CLOBBER:            in, optional, type=boolean, default=0
;                           If set, then if the CDF file being created already exists,
;                               it will be over-written. The default is to not over-write
;                               files. Ignored unless `CREATE` is set.
;       CREATE:             in, optional, type=boolean, default=0
;                           If set, a new CDF file will be created and available to write.
;       DECODING:           in, optional, type=string, default=`ENCODING`
;                           The type of data decoding. Options are::
;                               "ALPHAOSF1"
;                               "ALPHAVMSD"
;                               "ALPHAVMSG"
;                               "DECSTATION"
;                               "HOST"
;                               "HP"
;                               "IBMPC"
;                               "IBMRS"
;                               "MAC"
;                               "NETWORK" 
;                               "NEXT"
;                               "SGI"
;                               "SUN"
;       DIALOG_PARENT:      in, optional, type=integer
;                           The dialog parent of the file selection gui. Use only when
;                               `FILENAME` undefined.
;       DIRECTORY:          in, optional, type=string
;                           If FILENAME is not provided, then open the file-choosing
;                               dialog box in this directory.
;       ENCODING:           in, optional, type=string, default='HOST'
;                           The type of data decoding. Options are::
;                               "ALPHAOSF1"
;                               "ALPHAVMSD"
;                               "ALPHAVMSG"
;                               "DECSTATION"
;                               "HOST"
;                               "HP"
;                               "IBMPC"
;                               "IBMRS"
;                               "MAC"
;                               "NETWORK" 
;                               "NEXT"
;                               "SGI"
;                               "SUN"
;       GZIP_LEVEL:         in, optional, type=byte, default=5
;                           Desired effort of GZip compression, from 1-9. Automatically
;                               sets `COMPRESSION`=5.
;       MULTI_FILE:         in, optional, type=boolean, default=0
;                           If set, control and attribute data will be written into a
;                               primary ".cdf" file while each variable will be written
;                               to a separate (.r0) or (.z0) file.
;       MODIFY:             in, optional, type=boolean, default=1
;                           If set, the file will be opened for writing. The default is
;                               to open in a read-only mode.
;       VALIDATE:           in, optional, type=boolean, default=1
;                           Set equal to 0 to turn file validation off when opening a
;                               file. File validation only occurs in IDL 8.0+.
;       _REF_EXTRA:         in, optinal, type=any
;-
pro CDF_File::Open, filename, $
BACKWARD_COMPATIBLE = backward_compatible, $
CANCEL = cancel, $
ROW_MAJOR = row_major, $
COMPRESSION = compression, $
CLOBBER = clobber, $
CREATE = create, $
DECODING = decoding, $
DIALOG_PARENT = dialog_parent, $
DIRECTORY = directory, $
ENCODING = encoding, $
GZIP_LEVEL = gzip_level, $
MULTI_FILE = multi_file, $
MODIFY = modify, $
VALIDATE = validate
    compile_opt strictarr
    
    ;Catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        ;close the CDF file if it was opened.
        if n_elements(fileID) ne 0 then cdf_close, self.fileID
        void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    cancel    = 0B
    modify    = keyword_set(modify)
    create    = keyword_set(create)
    clobber   = keyword_Set(clobber)
    row_major = n_elements(row_major) eq 0 ? 0 : keyword_set(row_major)
    validate  = n_elements(validate)  eq 0 ? 1 : keyword_set(validate)
    backward_compatible = keyword_set(backward_compatible)
    col_major = ~row_major
    if n_elements(encoding) eq 0 then encoding = 'HOST'
    if n_elements(decoding) eq 0 then decoding = encoding
    
    ;Pick a file
    if n_elements(filename) eq 0 then begin
        filename = dialog_pickfile(FILTER='*.cdf', PATH=directory, $
                                   TITLE='Select a CDF File...', DIALOG_PARENT=dialog_parent)
        
        ;Check if the cancel button was pushed.
        if filename eq '' then begin
            cancel = 1B
            return
        endif
    endif
    
    ;Are we creating a new file or opening an existing file. The file is writeable
    ;if we are opening and modifying, or if we are creating a new one.
    mode = (create eq 1) ? 'CREATE' : 'OPEN'
    self.writeable = (mode eq 'OPEN' && modify eq 1) or (mode eq 'CREATE')

    ;If we are creating a new file, make sure it does not exist
    if mode eq 'CREATE' then begin
        if file_test(filename) then begin
            case clobber of
                1: file_delete, filename, /ALLOW_NONEXISTENT
                0: message, 'The specified CDF file already exists and cannot be ' + $
                            'over-written unless CLOBBER is set.'
            endcase
        endif
    endif

    ;If we are opening a file, make sure it exists
    if mode eq 'OPEN' then begin
        if file_test(filename, /READ) eq 0 then $
            message, 'The CDF file cannot be opened for reading: "' + filename + '".'
        if modify && file_test(filename, /WRITE) eq 0 then $
            message, 'The CDF file cannot be opened for writing: "' + filename + '".'
    endif
    
    ;Set the filename
    self.filename = filename
    
    ;Validate the file (IDL v8.0+)
    if validate eq 0 && MrCmpVersion('8.0') le 0 then cdf_set_validate, /NO
    
    ;Make backward compatible?
    if backward_compatible then cdf_set_cdf27_backward_compatible, /ON
    
    ;Encoding type
    case strupcase(encoding) of
        'ALPHAOSF1':  alphaosf1_encoding  = 1
        'ALPHAVMSD':  alphavmsd_encoding  = 1
        'ALPHAVMSG':  alphavmsg_encoding  = 1
        'DECSTATION': decstation_encoding = 1
        'HOST':       host_encoding       = 1
        'HP':         hp_encoding         = 1
        'IBMPC':      ibmpc_encoding      = 1
        'IBMRS':      ibmrs_encoding      = 1
        'MAC':        mac_encoding        = 1
        'NETWORK':    network_encoding    = 1 
        'NEXT':       next_encoding       = 1
        'SGI':        sgi_encoding        = 1
        'SUN':        sun_encoding        = 1
        else: message, 'Enconding "' + encoding + '" not recognized.'
    endcase
    
    ;Decoding type
    case strupcase(decoding) of
        'ALPHAOSF1':  alphaosf1_decoding  = 1
        'ALPHAVMSD':  alphavmsd_decoding  = 1
        'ALPHAVMSG':  alphavmsg_decoding  = 1
        'DECSTATION': decstation_decoding = 1
        'HOST':       host_decoding       = 1
        'HP':         hp_decoding         = 1
        'IBMPC':      ibmpc_decoding      = 1
        'IBMRS':      ibmrs_decoding      = 1
        'MAC':        mac_decoding        = 1
        'NETWORK':    network_decoding    = 1 
        'NEXT':       next_decoding       = 1
        'SGI':        sgi_decoding        = 1
        'SUN':        sun_decoding        = 1
        else: message, 'Deconding "' + decoding + '" not recognized.'
    endcase
    
    ;Open/Create the CDF file
    case mode of
        'OPEN':   fileID = cdf_open(filename, READONLY=~modify)
        'CREATE': begin
            fileID = cdf_create(filename, ROW_MAJOR=row_major, COL_MAJOR=col_major, $
                                MULTI_FILE=multi_file, $
                                ALPHAOSF1_ENCODING=alphaosf1_encoding, $
                                ALPHAVMSD_ENCODING=alphavmsd_encoding, $
                                ALPHAVMSG_ENCODING=alphavmsg_encoding, $
                                DECSTATION_ENCODING=decstation_encoding, $
                                HOST_ENCODING=host_encoding, $
                                HP_ENCODING=hp_encoding, $
                                IBMPC_ENCODING=ibmpc_encoding, $
                                IBMRS_ENCODING=ibmrs_encoding, $
                                MAC_ENCODING=mac_encoding, $
                                NETWORK_ENCODING=network_encoding, $
                                NEXT_ENCODING=next_encoding, $
                                SGI_ENCODING=sgi_encoding, $
                                SUN_ENCODING=sun_encoding, $
                                ALPHAOSF1_DECODING=alphaosf1_decoding, $
                                ALPHAVMSD_DECODING=alphavmsd_decoding, $
                                ALPHAVMSG_DECODING=alphavmsg_decoding, $
                                DECSTATION_DECODING=decstation_decoding, $
                                HOST_DECODING=host_decoding, $
                                HP_DECODING=hp_decoding, $
                                IBMPC_DECODING=ibmpc_decoding, $
                                IBMRS_DECODING=ibmrs_decoding, $
                                MAC_DECODING=mac_decoding, $
                                NETWORK_DECODING=network_decoding, $
                                NEXT_DECODING=next_decoding, $
                                SGI_DECODING=sgi_decoding, $
                                SUN_DECODING=sun_decoding)
        endcase
    endcase
    
    ;Turn file validation back on (IDL v8.0+)
    if validate eq 0 && MrCmpVersion('8.0' le 0) then cdf_set_validate, /ON
    
    ;Set the compression?
    if n_elements(compression) gt 0 || n_elements(gzip_level) gt 0 then begin
        self -> SetCompression, compression, GZIP_LEVEL=gzip_level
    endif

    ;update the object fields
    self.fileID = fileID
    self.filename = filename
end


;+
;   The purpose of this method is to load the Variable Attributes for each z-variable
;   of the CDf file.
;-
pro CDF_File::ParseFile
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Has the file already been parsed
    if self.isParsed eq 1 then return
    
    ;Make sure the attributes are empty
    self.gAttrs -> Remove, /ALL, /DESTROY
    self.vAttrs -> Remove, /ALL, /DESTROY
    self.zVars  -> Remove, /ALL, /DESTROY
    self.rVars  -> Remove, /ALL, /DESTROY

;-----------------------------------------------------
; Global Attribute and Variable Info \\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    fileInfo = cdf_inquire(self.fileID)
    cdf_control, self.fileID, GET_NUMATTRS=attrCounts
    
    ;Number of attributes
    globalAttrCount = attrCounts[0]
    attrCount       = fileInfo.nAtts
    
    ;Number of variables
    nRVars = fileInfo.nVars
    nZVars = fileInfo.nZVars
    nVars  = nRVars + nZVars

;-----------------------------------------------------
; Parse Global Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
;
;Parse each global attribute. Global attributes are suppose to be stored before
;variable attributes so that their index ranges from 0 to globalAttrCount-1, but
;sometimes they get appended to the end. Thus, loop through all attributes. Parse
;only the global ones. Variable attributes are taken care of below, when variables
;are parsed.
;
    nGAttrs = 0L
    attrNum = 0L
    while (attrNum lt fileInfo.natts) && (nGAttrs lt globalAttrCount) do begin
        ;Get the attribute name.
        cdf_attinq, self.fileID, attrNum, attrName, attrScope, maxentry
        
        ;Create global attributes.
        if strpos(attrScope, 'GLOBAL') ne -1 then begin
            self -> CreateAttrObj, attrName
            nGAttrs += 1
        endif
        
        attrNum += 1
    endwhile
    
    ;Make sure all of the global attributes were found
    if nGAttrs ne globalAttrCount then message, 'Could not find all global attributes.'

;-----------------------------------------------------
; Parse Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;zVariables are stored after rVariables.
    rCount = 0L
    zCount = 0L
    for varNum = 0, nVars-1 do begin
        ;Get the variable name
        zvariable = (varNum gt nRVars-1) ? 1 : 0
        varinq = cdf_varinq(self.fileID, varNum, ZVARIABLE=zvariable)

        ;Create a variable object
        self -> CreateVarObj, varinq.name
        
        if zvariable $
            then zCount++ $
            else rCount++
    endfor
    
    ;Were all r- and z-variables found?
    if rCount ne nRVars then message, 'Count not find all rVariables.'
    if zCount ne nZVars then message, 'Could not find all zVariables.'
    
    self.isParsed = 1B
end


;+
;   The purpose of this method is to print information about the file.
;-
pro CDF_File::PrintFileInfo
    compile_opt strictarr
    on_error, 2

    outInfo = self -> _OverloadPrint()
    print, outInfo
end


;+
;   A more robust method for obtaining variable data, when compared to the GetVarData
;   method.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable to which the
;                               attribute value will be written.
;       VALUE:              in, required, type="CDF_DOUBLE"
;                           Value to be written to the attribute.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       PATTERN:            in, optional, type=boolean, default=0
;                           If set, then `REC_START` and `REC_END` are that will be parsed
;                               into CDF epoch values. PATTERN describes how the times
;                               should be parsed and accepts any pattern recognized by
;                               MrTimeParser.pro. Automatically sets `TIME`=1.
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
;       REC_END:            in, optional, type=integer
;                           If set, the last record to read. `REC_COUNT` will be set
;                               as `REC_END` - `REC_START` + 1.
;       STRING:             in, optional, type=boolean, default=0
;                           If set, "CDF_CHAR" and "CDF_UCHAR" data will be converted
;                               to strings. The are read from the file as byte-arrays.
;       TIME:               in, optional, type=boolean, default=0
;                           If set, `REC_START` and `REC_END` represent the time interval,
;                               in CDF_EPOCH, CDF_EPOCH16, or CDF_TIME_TT2000 format
;                               over which data should be read. It is then assumed that
;                               a "DEPEND_0" variable attribute exists for `VARIABLE` and
;                               that its values are epoch times. The epoch type of TIME
;                               must match that of DEPEND_0.
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
;-
function CDF_File::Read, varName, depend_0, depend_1, depend_2, depend_3, $
;INPUT
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
PATTERN=pattern, $
REC_COUNT=rec_count, $
REC_END=rec_end, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start, $
STRING=string, $
TIME=time, $
;OUTPUT
DATATYPE=datatype, $
FILLVALUE=fillvalue, $
PADVALUE=padvalue
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Was a time range given?
    time = keyword_set(time)
    if n_elements(pattern) gt 0 then time = 1
    
    ;Get the variable object
    tf_has = self -> HasVar(varName, OBJECT=varObj)
    if tf_has eq 0 then $
        message, 'Variable "' + varName + '" is not found in the file.'

;-----------------------------------------------------
;Record Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    
    ;Time interval given?
    if time eq 1 then begin
        ;Get the name of the epoch variable
        tVarName = varObj -> GetAttrValue('DEPEND_0')
        
        ;Read the time and figure out its epoch type.
        depend_0 = self -> GetVarData(tVarName, CDF_TYPE=epoch_type)

        ;If time strings were given, convert them to epoch values
        if n_elements(pattern) gt 0 $
            then epoch_range = MrCDF_Epoch_Parse([rec_start, rec_end], PATTERN=pattern, EPOCH_TYPE=epoch_type) $
            else epoch_range = [rec_start, rec_end]

        ;Get the record range and record count
        comp = MrCDF_Epoch_Compare(depend_0, epoch_range[0], epoch_range[1])
        rec_start_out = min(where(comp, rec_count))
        if rec_count eq 0 then message, 'No records found between REC_START and REC_END.'

    ;Record interval given?
    endif else if n_elements(rec_end) gt 0 then begin
        rec_start_out = rec_start
        rec_count     = rec_end - rec_start + 1
    endif

;-----------------------------------------------------
;Get the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Default to reading all reacords
    if n_elements(rec_start_out) eq 0 then rec_start_out = 0
    if n_elements(rec_count) eq 0 then begin
        varObj -> GetProperty, MAXREC=maxrec
        rec_count = maxrec + 1 - rec_start_out
    endif

    ;Get the data
    data = varObj -> GetValue(COUNT=count, INTERVAL=interval, OFFSET=offset, $
                              REC_COUNT=rec_count, REC_INTERVAL=rec_interval, $
                              REC_START=rec_start_out, STRING=string, CDF_TYPE=cdf_type, $
                              FILLVALUE=fillvalue, PADVALUE=padvalue)
    
    ;DEPEND_0
    if arg_present(depend_0) then begin
        if n_elements(depend_0) gt 0 then begin
            depend_0 = depend_0[*,rec_start_out:rec_start_out+rec_count-1]
        endif else begin
            tf_dep0 = varObj -> HasAttr('DEPEND_0', OBJECT=attrObj)
            if tf_dep0 then begin 
                dep0VarName = attrObj -> GetValue()
                depend_0 = self -> GetVarData(dep0VarName, COUNT=count, INTERVAL=interval, $
                                              OFFSET=offset, REC_COUNT=rec_count, $
                                              REC_INTERVAL=rec_interval, REC_START=rec_start_out)
            endif
        endelse
    endif
    
    ;DEPEND_1
    if arg_present(depend_1) then if varObj -> HasAttr('DEPEND_1', OBJECT=attrObj) then begin
        dep1VarName = attrObj -> GetValue()
        depend_1 = self -> GetVarData(dep1VarName, COUNT=count, INTERVAL=interval, $
                                      OFFSET=offset, REC_COUNT=rec_count, $
                                      REC_INTERVAL=rec_interval, REC_START=rec_start_out)
    endif
    
    ;DEPEND_2
    if arg_present(depend_2) then if varObj -> HasAttr('DEPEND_2', OBJECT=attrObj) then begin
        dep2VarName = attrObj -> GetValue()
        depend_2 = self -> GetVarData(dep2VarName, COUNT=count, INTERVAL=interval, $
                                      OFFSET=offset, REC_COUNT=rec_count, $
                                      REC_INTERVAL=rec_interval, REC_START=rec_start_out)
    endif
    
    ;DEPEND_3
    if arg_present(depend_3) then if varObj -> HasAttr('DEPEND_3', OBJECT=attrObj) then begin
        dep3VarName = attrObj -> GetValue()
        depend_3 = self -> GetVarData(dep3VarName, COUNT=count, INTERVAL=interval, $
                                      OFFSET=offset, REC_COUNT=rec_count, $
                                      REC_INTERVAL=rec_interval, REC_START=rec_start_out)
    endif
    
    return, data
end


;+
;   Set the compression type for the CDF file. The file must be writable and compression
;   settings must be set before writing the data being compressed to the file. Individual
;   variables can be compressed differently from the rest of the CDF file.
;
;   Note that these settings can be set when creating the file (see the Open method) or
;   when adding data to a variable (see the WriteVarData method).
;
; :Params:
;       COMPRESSION:        in, optional, type=byte, default=0
;                           Type of compression to be performed on single-file CDFs::
;                               0 - No compression
;                               1 - Run-Length Encoding
;                               2 - Huffman
;                               3 - Adaptive Huffman
;                               5 - GZip (See the `GZIP_LEVEL` keyword)
;
; :Keywords:
;       VARIABLE:           in, optional, type=string
;                           Name of the variable whose compression settings are to
;                               be set. If given, but `COMPRESSION` and
;                               `GZIP_LEVEL` are undefined, then the variable
;                               compression settings will be made equal to the current
;                               file compression settings.
;       GZIP_LEVEL:         in, optional, type=byte, default=5
;                           Desired effort of GZip compression, from 1-9. Automatically
;                               sets `COMPRESSION`=5.
;-
pro CDF_File::SetCompression, compression, $
GZIP_LEVEL=gzip_level, $
VARIABLE=variable
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot set compression.'

    ;Defaults/dependencies
    if n_elements(compression) eq 0 then compression = 0
    if n_elements(gzip_level)  gt 0 then compression = 5
    
    ;Variable Compression
    ;   If a variable was given, but no compression settings, use the same settings
    ;   as the file.
    if n_elements(variable) gt 0 then begin
        ;Make sure a name was given
        if MrIsA(variable, 'STRING', /SCALAR) eq 0 then $
            message, 'VARIABLE must be a scalar string representing the CDF variable name.'
    
        ;GZip? If no compression, use the current file compression settings.
        if n_elements(gzip_level) gt 0 then begin
            compression = 5
        endif else if n_elements(var_compression) eq 0 then begin
            self -> GetCompression, COMPRESSION=var_compression, GZIP_LEVEL=gzip_level
            if var_compression eq 5 then var_gzip_level = gzip_level
        endif

        ;Set the compression
        cdf_compression, self.fileID, SET_VAR_GZIP_LEVEL=var_gzip_level, $
                         SET_VAR_COMPRESSION=var_compression, VARIABLE=variable
    
    ;File Compression
    endif else begin
        cdf_compression, self.fileID, SET_COMPRESSION=compression, $
                         SET_GZIP_LEVEL=gzip_level
    endelse
    
end


;+
;   Determines if a variable has a particular attribute.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the variable to which `ATTRNAME` belongs.
;       ATTRNAME:           in, required, type=string
;                           Name of the variable attribute to search for.
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           CDF_Attribute object for the desired attribute.
;
; :Returns:
;       TF_HAS:             Returns true (1) of the variable has the attribute identified, 
;                               by `ATTRNAME` and false (0) otherwise.
;-
function CDF_File::VarHasAttr, varName, attrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try to find the object
    varObj = self.variables -> FindByName(varName, COUNT=count)
    if count eq 0 then message, 'Cannot find variable "' + varName + '".'
    
    ;Is the attribute there?
    tf_has = varObj -> HasAttr(attrName, OBJECT=object)
    
    return, tf_has
end


;+
;   Write a value to a global attribute.
;
;   Values are stored at global entry numbers, which serve as an array index. The range
;   from 0 to maxGEntry, which can be obtained from CDF_Control's GET_ATTR_INFO property.
;   However, unlike array indices, gEntryNums do not have to be contiguous. Out of the
;   maxGEntry+1 entries, only numGEntries (again, obtained from CDF_Control) are defined.
;   Use the GetGEntryMask method to determine which are defined.
;
; :Params:
;       ATTRIBUTE:          in, required, type=string/object
;                           Name or CDF_Attribute object of an attribute whose value
;                               is to be written.
;       VALUE:              in, required, type=valid cdf type
;                           Value or vector values to be written to the attribute.
;
; :Keywords:
;       CREATE:             in, optional, type=boolean, default=0
;                           If set, the attribute will be created.
;       CDF_EPOCH:          in, optional, type=boolean, default=0
;                           If set, `VALUE` will be written as a datatype "CDF_EPOCH"
;                               (i.e. "CDF_FLOAT4"). The default is "CDF_DOUBLE" Can
;                               be a vector the same lenth as `VALUE`.
;       GENTRYNUM:          in, required, type=integer, defualt=maxGEntry+1
;                           Either a scalar or vector of global entry numbers. If a scalar,
;                               then the element(s) of `VALUE` will be stored contiguously
;                               starting at the given value. If a vector, it must be the
;                               same length as `VALUE`. If a value was already stored at
;                               GENTRYNUM, then it will be overwritten.
;-
pro CDF_File::WriteGlobalAttr, gAttrName, value, $
CREATE=create, $
CDF_EPOCH=cdf_epoch, $
GENTRYNUM=gEntryNum
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot add new global attribute.'
    
    ;Does the attribute exist?
    if self -> HasGlobalAttr(gAttrName) eq 0 then begin
        if keyword_set(create) $
            then self -> WriteGlobalAttrDef, gAttrName $
            else message, 'Global attribute "' + gAttrName + '" does not exist. ' + $
                          'Set the CREATE keyword or use the WriteGlobalAttrDef method to create it.'
    endif
    
    ;Check inputs
    nValues = n_elements(value)
    if n_elements(cdf_epoch) eq 0 then cdf_epoch = 0
    
    ;gEntryNumber
    if n_elements(gEntryNum) eq 0 then begin
        cdf_control, self.fileID, ATTRIBUTE=gAttrName, GET_ATTR_INFO=attr_info
        gEntryNum = attr_info.maxGEntry+1
    endif
    
    case n_elements(gEntryNum) of
        nValues: ;Do nothing
        1:       gEntryNum = lindgen(nValues) + gEntryNum
        else: message, 'GENTRYNUM must be a scalar or a vector the same length as VALUE'
    endcase
    
    ;CDF_Epoch
    case n_elements(cdf_epoch) of
        nValues: cdf_epoch = cdf_epoch ne 0
        1:       cdf_epoch = replicate(keyword_set(cdf_epoch), nValues)
        else: message, 'CDF_EPOCH must be a scalar or a vector the same length as VALUE'
    endcase

    ;Write single value
    if nValues eq 1 then begin
        cdf_attput, self.fileID, gAttrName, gEntryNum[0], value[0], CDF_EPOCH=cdf_epoch[0]
        
    ;Write array of values
    endif else begin
        for i=0, nValues-1 do $
            cdf_attput, self.fileID, gAttrName, gEntryNum[i], value[i], CDF_EPOCH=cdf_epoch[i]
    endelse
end


;+
;   Create a global attribute.
;
; :Params:
;       GATTRNAME:              in, required, type=string
;                               Name of the global attribute to be created.
;-
pro CDF_File::WriteGlobalAttrDef, gAttrName
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot add new global attribute.'
    
    ;Create the attribute
    gAttrID = cdf_attcreate(self.fileID, gAttrName, /GLOBAL_SCOPE)
    
    ;Create the attribute object.
    self -> CreateAttrObj, gAttrName
end


;+
;   Write a value to a variable attribute.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable to which the
;                               attribute value will be written.
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
pro CDF_File::WriteVarAttr, variable, attrName, value, $
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
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot add new variable.'

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = variable
        'STRING': begin
            varObj = self.zVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then varObj = self.rVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        else: message, 'VARIABLE must be a variable name or object.'
    endcase
    
    ;Create the variable attribute
    varObj -> WriteAttrValue, attrName, value, CREATE=create, CDF_EPOCH=cdf_epoch
end


;+
;   Create a variable attribute.
;
; :Params:
;       GATTRNAME:              in, required, type=string
;                               Name of the global attribute to be created.
;-
pro CDF_File::WriteVarAttrDef, attrName
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot add new variable.'
    
    ;Create the variable attribute
    attrID = cdf_attcreate(self.fileID, attrName, /VARIABLE_SCOPE)
    
    ;Store the variable attribute
    self -> CreateAttrObj, attrName, /VARIABLE
end


;+
;   The purpose of this method is to create a CDF variable and add it to the file.
;   Variables can also be added in a more natural way with the CREATE keyword in
;   the WriteVarData method. The default is to create a Z-variable.
;
;   NOTES
;       I. About Variable Creation
;           rVariables and zVariables must have unique names. However, they are numbered
;           independently, starting at 0. For this reason, they are kept in separate
;           containers.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the variable to be created.
;       DATATYPE:           in, required, type=string
;                           Either the IDL type-name (as returned by Size(/TNAME)) or
;                               the CDF datatype of the variable to be created. See the
;                               file header for CDF data types.
;       DIMVARY:            in, optional, type=bytarr/strarr
;                           A vector with one element per CDF dimension. Elements are
;                               either 'VARY' (or 1) or 'NOVARY' (or 0) to indicate
;                               variance in that dimension.
;       ALLOCATERECS:       in, optional, type=long
;                           Number of pre-allocated records in a single-file CDF file.
;                               Ensures that all data records are stored contiguously.
;       DIMENSIONS:         in, optional, type=lonarr, default=0
;                           Create a z-variable with the specified dimensions. If not
;                               set, a scalar is assumed. Automatically sets `ZVARIABLE`=1.
;       NUMELEM:            in, optional, type=long, default=1
;                           Number of elements of data at each variable value. Only valid
;                               if `DATATYPE` is "CDF_CHAR" or "CDF_UCHAR" and indicates
;                               the length of the string.
;       REC_NOVARY:         in, optional, type=boolean, default=0
;                           If set, all records will contain the same information. The
;                               default is "VARY"
;       ZVARIABLE:          in, optional, type=boolean, default=1
;                           If set, a z-variable will be created. Set equal to zero to
;                               create an r-variable.
;-
pro CDF_File::WriteVarDef, varName, datatype, dimVary, $
ALLOCATERECS=allocaterecs, $
DIMENSIONS=dimensions, $
NUMELEM=numelem, $
REC_NOVARY=rec_novary, $
ZVARIABLE=zvariable
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot add new variable.'
    
    ;Default to z-variables
    zvariable = (n_elements(zvariable) eq 0) ? 1 : keyword_set(zvariable)
    if n_elements(dimensions) gt 0 then zvariable = 1

    ;Get the CDF data type
    if strpos(datatype, 'CDF') eq -1 $
        then cdf_type = CDF_CastDataType(datatype, /TNAME) $
        else cdf_type = datatype
    
    ;Set the proper flag
    case cdf_type of
       'CDF_BYTE':        cdf_byte        = 1
       'CDF_CHAR':        cdf_char        = 1
       'CDF_DOUBLE':      cdf_double      = 1
       'CDF_EPOCH':       cdf_epoch       = 1
       'CDF_EPOCH16':     cdf_long_epoch  = 1
       'CDF_LONG_EPOCH':  cdf_long_epoch  = 1
       'CDF_FLOAT':       cdf_float       = 1
       'CDF_INT1':        cdf_int1        = 1
       'CDF_INT2':        cdf_int2        = 1
       'CDF_INT4':        cdf_int4        = 1
       'CDF_INT8':        cdf_int8        = 1
       'CDF_REAL4':       cdf_read4       = 1
       'CDF_REAL8':       cdf_real8       = 1
       'CDF_TIME_TT2000': cdf_time_tt2000 = 1
       'CDF_UCHAR':       cdf_uchar       = 1
       'CDF_UINT1':       cdf_uint1       = 1
       'CDF_UINT2':       cdf_uint2       = 1
       'CDF_UINT4':       cdf_uint4       = 1
       else: message, 'CDF datatype not recognized: "' + cdf_type + '".'
    endcase

    ;Write the data
    varID = cdf_varcreate(self.fileID, varName, dimVary, ALLOCATERECS=allocaterecs, $
                          DIMENSIONS=dimensions, NUMELEM=numelem, REC_NOVARY=rec_novary, $
                          ZVARIABLE=zvariable, CDF_BYTE=cdf_byte, CDF_CHAR=cdf_char, $
                          CDF_DOUBLE=cdf_double, CDF_EPOCH=cdf_epoch, $
                          CDF_LONG_EPOCH=cdf_long_epoch, CDF_FLOAT=cdf_float, $
                          CDF_INT1=cdf_int1, CDF_INT2=cdf_int2, CDF_INT4=cdf_int4, $
                          CDF_REAL4=cdf_real4, CDF_REAL8=cdf_real8, CDF_UCHAR=cdf_uchar, $
                          CDF_UINT1=cdf_uint1, CDF_UINT2=cdf_uint2, CDF_UINT4=cdf_uint4, $
                          CDF_INT8=cdf_int8, CDF_TIME_TT2000=cdf_time_tt2000)
    
    ;Add the variable object
    self -> CreateVarObj, varName
end


;+
;   Write variable data to the CDF file.
;
; NOTES:
;   Records come after dimensions, so the array::
;
;       data = bytarr(340, 440, 24)
;
;   has DIMENSIONS=[340,440] with 24 records.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       DATA:               in, required, type=any
;                           Data to be written.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr, defualt=dimesnions of `DATA`.
;                           Vector containing the counts to be used when writing each
;                               value. Does not have to be the same size as `DIMENSION`.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       OFFSET:             in, optional, type=intarr, default=0 for each dimension
;                           Array indices within the specified record(s) at which to
;                               begin writing. OFFSET is a 1-dimensional array
;                               containing one element per CDF dimension.
;       QUIET:              in, optional, type=boolean, default=1
;                           If set, warning messages from the CDF DLM will not be ouput
;                               to the command window. - MRA
;       REC_COUNT:          in, optional, type=long, default=maxrec+1
;                           Number of records to be read.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records being written when writing multiple records.
;       REC_START:          in, optional, type=integer, defualt=0
;                           Record at which to begin writing data.
;-
pro CDF_File::WriteVarData, variable, data, $
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
QUIET=quiet, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start, $
;Create Variable?
CREATE=create, $
CDF_TYPE=cdf_type, $
DIMS=dims, $
REC_NOVARY=rec_novary, $
ZVARIABLE=zvariable
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        if n_elements(quiet_in) gt 0 then !Quiet = quiet_in
        return
    endif
    
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot set compression.'
    
    ;Defaults
    create = keyword_set(create)
    quiet = (n_elements(quiet) eq 0) ? 1 : keyword_set(quiet)
    if n_elements(datatype) eq 0 then datatype = size(data, /TNAME)
    
;-----------------------------------------------------
; Create Variable \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if create then begin
        ;Must be a variable name
        if size(variable, /TNAME) ne 'STRING' then $
            message, 'To create a new variable, VARIABLE must be a scalar string.'

        ;Already exists?    
        tf_has = self -> HasVar(variable)
        if tf_has then message, 'Variable name already exists: "' + variable + '". Cannot create.'
        
        ;Get size
        zvariable = n_elements(zvariable) eq 0 ? 1: keyword_set(zvariable)
        rec_novary = keyword_set(rec_novary)
        if zvariable and n_elements(dimensions) eq 0 then begin
            nDims = size(data, /N_DIMENSIONS)
            dims  = size(data, /DIMENSIONS)
            
            if nDims gt 1 then dims = dims[0:nDims-2]
            if nDims eq 0 then rec_novary = 1
        endif
        
        ;Assume dimensional variance.
        if n_elements(dimVary) eq 0 then $
            if n_elements(dims) ne 0 then dimVary = bytarr(n_elements(dims)) + 1
        
        ;Get the data type
        if n_elements(cdf_type) eq 0 then begin
            cdf_type = size(data, /TNAME)
            cdf_type = cdf_castdatatype(cdf_type, /TNAME)
        endif

        ;Create the variable
        self -> WriteVarDef, variable, cdf_type, dimVary, ALLOCATERECS=allocaterecs, $
                             DIMENSIONS=dims, REC_NOVARY=rec_novary
    endif
    
;-----------------------------------------------------
; Get the Variable Object \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    case size(variable, /TNAME) of
        'OBJREF': varObj = variable
        'STRING': begin
            varObj = self.zVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then varObj = self.rVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase
    
;-----------------------------------------------------
; Write the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    varName = varObj -> GetName()
    
    ;Prevent warnings from the CDF DLM?
    quiet_in = !Quiet
    !Quiet = quiet

    ;Write the data
    cdf_varput, self.fileID, varName, data, COUNT=count, INTERVAL=interval, OFFSET=offset, $
                REC_INTERVAL=rec_interval, REC_START=rec_start
    
    !Quiet = quiet_in
end


;+
; Clean up after the object is destroyed
;
; :Hidden:
;-
pro CDF_File::cleanup
    compile_opt idl2

    ;close the CDF file
    self -> Close
    
    ;Destoy objects
    obj_destroy, self.gAttrs
    obj_destroy, self.vAttrs
    obj_destroy, self.rVars
    obj_destroy, self.zVars
end


;+
; The purpose of this method is to create an instance of the CDF class by opening
; the desired CDF file and loading its metadata.
;
; :Params:
;       FILENAME:           in, optional, type=string
;                           The complete path to the CDF file to be accessed. If not
;                               provided, a dialog box will appear from which a file
;                               can be chosen.
;
; :Keywords:
;       DIALOG_PARENT:      in, optional, type=integer
;                           The dialog parent of the file selection gui. Use only when
;                               `FILENAME` undefined.
;       DIRECTORY:          in, optional, type=string
;                           If FILENAME is not provided, then open the file-choosing
;                               dialog box in this directory.
;       FILE_STATUS:        out, optional, type=int
;                           Sucess status of the file being opened::
;                               0 - No file chosen (i.e. the cancel button on the dialog
;                                      box was pushed).
;                               1 - A file was chosen
;                               # - The index of the error returned by Catch
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the Open method is also accepted
;                               via keyword inheritance.
;
; :Returns:
;       SUCCESS:            If `FILENAME` was (chosen and) opened successfully, then a
;                           valid object reference is returned (1). Otherwise, an invalid
;                           object reference is returned (0).
;-
function CDF_File::init, filename, $
CREATE=create, $
DIALOG_PARENT = dialog_parent, $
DIRECTORY = directory, $
FILE_STATUS = file_status, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, 0
    endif

    ;Defaults
    create = keyword_set(create)
    
    ;Allocate Pointers
    self.rVars  = obj_new('CDF_Container')
    self.zVars  = obj_new('CDF_Container')
    self.gAttrs = obj_new('CDF_Container')
    self.vAttrs = obj_new('CDF_Container')

    ;Open the file and load the meta-data
    self -> open, filename, $
                  CREATE=create, $
                  CANCEL=cancel, $
                  DIALOG_PARENT=dialog_parent, $
                  DIRECTORY=directory, $
                  _STRICT_EXTRA=extra
    if cancel eq 1 then return, 0
    
    ;Parse the file if it existed previously
    if create eq 0 then self -> ParseFile

    return, 1
end


;+
; The init method for CDF_File__DEFINE.PRO
;
; :Hidden:
;
; :Fields:
;       FILENAME:           The name of the CDF file begin read.
;       CDF_ID:             The CDF ID number of the file being read.
;       ATTRIBUTES:         Each attribute and its meta-data.
;       VARIABLES:          Each variable and its meta-data.
;-
pro CDF_File__define
    compile_opt strictarr
    
    define = { CDF_File, $
               filename:  '', $
               fileID:    0L, $
               isParsed:  0B, $
               gAttrs:    obj_new(), $
               vAttrs:    obj_new(), $
               rVars:     obj_new(), $
               zVars:     obj_new(), $
               writeable: 0B $
             }
end