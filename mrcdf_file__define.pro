; docformat = 'rst'
;
; NAME:
;       MrCDF_File__Define
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
; ORGANIZATIONAL NOTES:
;   Variables:
;       rVariables and zVariables must have unique names. However, they are numbered
;       independently, starting at 0. For this reason, they are kept in separate
;       containers.
;                                           c.f. CDF User's Guide Section 2.3.5-6
;
;   Attributes:
;       Global and variable attributes all have unique names and are all numbered
;       sequentially. They are stored together in the same internal list. Thus, there is
;       a single container for all parsed attributes.
;                                           c.f. CDF User's Guide Section 2.4.1-2
;
;
; :Examples:
;       See the MrCDF_File_Examples.pro program for a demonstration of how to create,
;       write, read, and copy CDF files.
;
; :Categories:
;       CDF Utilities, File I/O
;
; :Uses:
;   Uses the following external programs::
;       cgErrorMsg.pro
;       MrCDF_Attribute__Define.pro
;       MrCDF_CastDataType.pro
;       MrCDF_Container__Define.pro
;       MrCDF_File_Examples.pro
;       MrCDF_Variable__Define.pro
;       MrCmpVersion.pro
;       MrCDF_Epoch_Parse.pro
;       MrCDF_Epoch_Compare.pro
;       MrIsA.pro
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
;       2014/05/07  -   Combined global and variable attributes into a single container
;                           to mimic CDF file structure. - MRA
;       2014/05/08  -   REC_START and REC_END can be given singley. Added the Quiet
;                           property to suppress warnings from the CDF DLM. - MRA
;       2014/05/09  -   Added DECODING, ENCODING, and MAJORITY properties. Added the
;                           _OverloadHelp method. - MRA
;       2014/05/17  -   Added the ToStruct method. - MRA
;       2014/06/05  -   CDF files are no longer validated by default when opened. - MRA
;       2015/02/06  -   _OverloadHelp/Print methods provide concise, well formatted
;                           information. File compression is now possible. Added Copy*To
;                           and Delete/Del* methods. The Close method now purges object
;                           properties. DEPEND_0 params made into keywords in Read method.
;                           Added SHOW keyword to Get*Names methods. - MRA
;       2015/04/27  -   Global and variable attributes can now be accessed via
;                           _OverloadBracketsRightSide. Added the BOUNDS keyword to ::READ. - MRA
;-
;*****************************************************************************************
;+
;   Obtain a attribute's or a variable's object reference
;
; :Params:
;       ISRANGE:            in, required, type=intarr
;                           A vector that has one element for each Subscript argument
;                               supplied by the user; each element contains a zero if the
;                               corresponding input argument was a scalar index value or
;                               array of indices, or a one if the corresponding input
;                               argument was a subscript range.
;       SUBSCRIPT1:         in, required, type=string/integer
;                           If a string is given, it is the name of the variable object whose
;                               for which the object reference is to be retrieved. An
;                               integer value of 0 will return the file's object reference.
;       SUBSCRIPT2:         in, required, type=string/integer
;                           If `SUBSCRIPT1` is a variable name, then set this equal to
;                               the name of one of its variable attributes. The object
;                               reference to the variable attribute will be returned.
;
; :Returns:
;       THEOBJ:             The object reference identified by the given subscripts.
;-
function MrCDF_File::_OverloadBracketsRightSide, isRange, subscript1, subscript2
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, -1
	endif

	;Pick the proper data set to alter
	if n_elements(subscript1) ne 1 then $
		message, 'The first subscript must be a scalar.'

	;Max of two subscripts allowed    
	nSubs = n_elements(isRange)
	if nSubs gt 2 then message, 'A maximum of two subscripts is accepted.'
	if nSubs eq 2 then begin
		if ~MrIsA(subscript2, 'String', /SCALAR) then $
			message, 'The second subscript must be a scalar string.'
	endif

;-----------------------------------------------------
; Name was Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if MrIsA(subscript1, 'STRING') then begin

	;-----------------------------------------------------
	; Variable Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		;Was a variable name given?
		tf_has = self -> HasVar(subscript1, OBJECT=theObj)

		;Variable exists
		if tf_has then begin
			;Variable attribute name also given?
			if nSubs eq 2 then begin
				tf_has = theObj -> HasAttr(subscript2, OBJECT=theObj)
				if ~tf_has then $
					message, string(FORMAT='(%"Variable \"%s\" does not have attribute \"%s\"")', $
					                subscript1, subscript2)
			endif
	
	;-----------------------------------------------------
	; Attribute Name \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		endif else begin
			tf_has = self -> HasAttr(subscript1, OBJECT=theObj)
			
			;Did we find the attribute?
			if tf_has && nSubs eq 2 then message, 'Second subscripts not allowed with attributes.'
			if ~tf_has then $
				message, 'No variable or attribute with name: "' + subscript1 + '".'
		endelse

;-----------------------------------------------------
; Integer Given \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if MrIsA(subscript1, /INTEGER) then begin
		;If the scalar index 0 was given, return the SELF reference
		if subscript1 ne 0 then message, '0 is the only integer accepted as a subscript.'
		theObj = self
	
	endif else begin
		message, 'First subscript must be a string or 0.'
	endelse

	;Return the variable object?
	return, theObj
end


;+
;   Provide information when the PRINT procedure is called.
;-
function MrCDF_File::_OverloadPrint
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

;-----------------------------------------------------
; File Properties \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Number of global attributes
    self -> GetProperty, NGATTRS=gAttrCount, NVATTRS=vAttrCount, $
                         NRVARS=rVarCount, NZVARS=zVarCount
    
    ;File information
    fileStr   = string('FILENAME:',              self.filename, FORMAT='(a-33, a0)')
    nGAttrStr = string('   # Global Attributes:',   gAttrCount,    FORMAT='(a-30, i0)')
    nVAttrStr = string('   # Variable Attributes:', vAttrCount,    FORMAT='(a-30, i0)')
    nRVarStr  = string('   # R-Variables:',         rVarCount,     FORMAT='(a-30, i0)')
    nZVarStr  = string('   # Z-Variables:',         zVarCount,     FORMAT='(a-30, i0)')
    
    ;Output string for the file
    textOut = [[fileStr], $
               [nGAttrStr], $
               [nVAttrStr], $
               [nRVarStr], $
               [nZVarStr]]
    
;-----------------------------------------------------
; Global Attribute \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if gAttrCount gt 0 then begin
        gAttrHelp    = strarr(1, gAttrCount+1)
        gAttrHelp[0] = 'GLOBAL ATTRIBUTES:'

        ;Get help from the attributes.
        allGAttrs = self.attrs -> Get(/ALL, COUNT=nAttrs)
        gCount    = 0L
        for i = 0, nAttrs - 1 do begin
            ;Check if it is a global attribute
            allGAttrs[i] -> GetProperty, SCOPE=scope
            if strpos(scope, 'GLOBAL') eq -1 then continue
            
            ;Get help text
            tempHelp = allGAttrs[i] -> _OverloadHelp()
            gAttrHelp[0,gCount+1] = '  ' + tempHelp
            gCount++
        endfor
        
        textOut = [[textOut], [gAttrHelp]]
    endif

;-----------------------------------------------------
; R-Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if rVarCount gt 0 then begin
        rVarPrint = 'R-VARIABLES:'
        
        ;Get help from the variables
        allRVars = self.rVars -> Get(/ALL, COUNT=rVarCount)
        for i = 0, rVarCount - 1 do begin
            rTemp           = allRVars[i] -> _OverloadHelp()
            rVarHelp = [[rVarPrint], ['  ' + rTemp]]
        endfor
        
        ;Concatenate
        textOut = [[textOut], [rVarHelp]]
    endif

;-----------------------------------------------------
; Z-Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if zVarCount gt 0 then begin
        zVarPrint = 'Z-VARIABLES:'

        ;Get help from the variables
        allZVars = self.zVars -> Get(/ALL, COUNT=zVarCount)
        for i = 0, zVarCount - 1 do begin
            ;Get the help text and take the 2nd element
            zTemp    = allZVars[i] -> _OverloadPrint()
            zVarPrint = [[zVarPrint], ['  ' + zTemp]]
        endfor
        
        ;Concatenate
        textOut = [[textOut], [zVarPrint]]
    endif
    
    return, textOut
end


;+
;   Provide information when the PRINT procedure is called.
;-
function MrCDF_File::_OverloadHelp, varname
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
    
    ;General file properties
    self -> GetProperty, NGATTRS=gAttrCount, NVATTRS=vAttrCount, $
                         NRVARS=rVarCount, NZVARS=zVarCount, MAJORITY=majority, $
                         ENCODING=encoding, DECODING=decoding, $
                         COMPRESSION=compression, GZIP_LEVEL=gzip_level
    
    ;GZip?
    gzip = ''
    if n_elements(gzip_level) gt 0 then gzip = ' at level ' + strtrim(gzip_level, 2)
    
    ;Variable information
    heapID  = obj_valid(self, /GET_HEAP_IDENTIFIER)
    selfStr = obj_class(self) + '  <' + strtrim(heapID, 2) + '>'
    
    ;File information
    fileStr   = string('FILENAME:',                self.filename, FORMAT='(a-30, a0)')
    nGAttrStr = string('  # Global Attributes:',   gAttrCount,    FORMAT='(a-30, i0)')
    nVAttrStr = string('  # Variable Attributes:', vAttrCount,    FORMAT='(a-30, i0)')
    nRVarStr  = string('  # R-Variables:',         rVarCount,     FORMAT='(a-30, i0)')
    nZVarStr  = string('  # Z-Variables:',         zVarCount,     FORMAT='(a-30, i0)')
    majorStr  = string('  Majority:',              majority,      FORMAT='(a-30, a0)')
    codeStr   = string('  Encoding/Decoding:',     encoding + '/' + decoding, FORMAT='(a-30, a0)')
    compStr   = string('  Compression: ',          compression + gzip,        FORMAT='(a-30, a0)')
    
    ;Output string for the file
    helpOut = [[selfStr], $
               [fileStr], $
               [nGAttrStr], $
               [nVAttrStr], $
               [nRVarStr], $
               [nZVarStr], $
               [majorStr], $
               [codeStr], $
               [compStr]]
        
    return, helpOut
end


;+
;   Close the CDF file
;-
pro MrCDF_File::Close

    ;Close the CDF file -- An error will occur if it has already been closed.
    catch, the_error
    if the_error eq 0 then begin
        cdf_close, self.fileID
        self.fileID = 0
    endif else begin
        if stregex(!error_state.msg, 'Invalid CDFid', /BOOLEAN) then return
        void = cgErrorMSG()
        return
    endelse
    catch, /CANCEL
    on_error, 2
    
    ;Reset properties.
    self.decoding  = ''
    self.encoding  = ''
    self.filename  = ''
    self.majority  = ''
    self.writeable = 0B
    self.isParsed  = 0B
    
    ;Remove all
    self.attrs -> Remove, /ALL, /DESTROY
    self.rVars -> Remove, /ALL, /DESTROY
    self.zVars -> Remove, /ALL, /DESTROY
end


;+
;   Create a global or variable attribute.
;
; :Params:
;       ATTRNAME:               in, required, type=string
;                               Name of the attribute to be created.
;
; :Keywords:
;       VARIABLE_SCOPE:         in, optional, type=boolean, default=0
;                               If set, a variable attribute is created. The default
;                                   is to create a global attribute.
;-
pro MrCDF_File::CreateAttr, attrName, $
VARIABLE_SCOPE=variable_scope
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
    
    ;Does the attribute already exist?
    if self -> HasAttr(attrName) then $
        message, 'Attribute name"' + attrName + '" already exists. Cannot create.'
    
    ;Defaults
    variable_scope = keyword_set(variable_scope)
    global_scope   = ~variable_scope
    
    ;Create the attribute
    attrID = cdf_attcreate(self.fileID, attrName, GLOBAL_SCOPE=global_scope, $
                           VARIABLE_SCOPE=variable_scope)
    
    ;Create the attribute object.
    self -> CreateAttrObj, attrName
end


;+
;   The purpose of this method is to create a CDF variable and add it to the file.
;   Variables can also be added in a more natural way with the CREATE keyword in
;   the WriteVarData method. The default is to create a Z-variable.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the variable to be created.
;       CDF_TYPE:           in, required, type=string
;                           Either the IDL type-name (as returned by Size(/TNAME)) or
;                               the CDF datatype of the variable to be created. See the
;                               file header for CDF data types.
;       DIMVARY:            in, optional, type=bytarr/strarr
;                           A vector with one element per CDF dimension. Elements are
;                               either 'VARY' (or 1) or 'NOVARY' (or 0) to indicate
;                               variance in that dimension.
;
; :Keywords:
;       ALLOCATERECS:       in, optional, type=long
;                           Number of pre-allocated records in a single-file CDF file.
;                               Ensures that all data records are stored contiguously.
;       DIMENSIONS:         in, optional, type=lonarr, default=0
;                           Create a z-variable with the specified dimensions. If not
;                               set, a scalar is assumed. Automatically sets `ZVARIABLE`=1.
;       COMPRESSION:        in, optional, type=string/integer, default='None'
;                           Type of variable compression to perform. Options are::
;                               0 or 'None'
;                               1 or 'Run-Length Encoding'
;                               2 or 'Huffman'
;                               3 or 'Adaptive Huffman'
;                               5 or 'GZIP'
;       GZIP_LEVEL:         in, optional, type=integer, default=5
;                           The desired effort for the GZIP compression. This effort must
;                               be expressed as a scalar in the range (1-9). If set, then
;                               `COMPRESSION` is set to 5 automatically.
;       NUMELEM:            in, optional, type=long, default=1
;                           Number of elements of data at each variable value. Only valid
;                               if `CDF_TYPE` is "CDF_CHAR" or "CDF_UCHAR" and indicates
;                               the length of the string.
;       REC_NOVARY:         in, optional, type=boolean, default=0
;                           If set, all records will contain the same information. The
;                               default is "VARY"
;       ZVARIABLE:          in, optional, type=boolean, default=1
;                           If set, a z-variable will be created. Set equal to zero to
;                               create an r-variable.
;-
pro MrCDF_File::CreateVar, varName, datatype, dimVary, $
ALLOCATERECS=allocaterecs, $
COMPRESSION=compression, $
DIMENSIONS=dimensions, $
GZIP_LEVEL=gzip_level, $
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

;-------------------------------------------------------
; Check Inputs /////////////////////////////////////////
;-------------------------------------------------------
    ;Check if the file is writable
    if self.writeable eq 0 then $
        message, 'File is not writable. Cannot add new variable.'
        
    ;Does the variable name already exist?
    if self -> HasVar(varName) then $
        message, 'Variable name "' + varName + '" already exists. Cannot create.'
    
    ;Default to z-variables
    zvariable = (n_elements(zvariable) eq 0) ? 1 : keyword_set(zvariable)
    if n_elements(dimensions) gt 0 then zvariable = 1

    ;Get the CDF data type
    if strpos(datatype, 'CDF') eq -1 $
        then cdf_type = MrCDF_CastDataType(datatype, /TNAME) $
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

;-------------------------------------------------------
; Write Data ///////////////////////////////////////////
;-------------------------------------------------------
    if n_elements(dimVary) eq 0 then begin
        varID = cdf_varcreate(self.fileID, varName, $
                              ALLOCATERECS    = allocaterecs, $
                              DIMENSIONS      = dimensions, $
                              NUMELEM         = numelem, $
                              REC_NOVARY      = rec_novary, $
                              ZVARIABLE       = zvariable, $
                              CDF_BYTE        = cdf_byte, $
                              CDF_CHAR        = cdf_char, $
                              CDF_DOUBLE      = cdf_double, $
                              CDF_EPOCH       = cdf_epoch, $
                              CDF_LONG_EPOCH  = cdf_long_epoch, $
                              CDF_FLOAT       = cdf_float, $
                              CDF_INT1        = cdf_int1, $
                              CDF_INT2        = cdf_int2, $
                              CDF_INT4        = cdf_int4, $
                              CDF_REAL4       = cdf_real4, $
                              CDF_REAL8       = cdf_real8, $
                              CDF_UCHAR       = cdf_uchar, $
                              CDF_UINT1       = cdf_uint1, $
                              CDF_UINT2       = cdf_uint2, $
                              CDF_UINT4       = cdf_uint4, $
                              CDF_INT8        = cdf_int8, $
                              CDF_TIME_TT2000 = cdf_time_tt2000)
    endif else begin
        varID = cdf_varcreate(self.fileID, varName, dimVary, $
                              ALLOCATERECS    = allocaterecs, $
                              DIMENSIONS      = dimensions, $
                              NUMELEM         = numelem, $
                              REC_NOVARY      = rec_novary, $
                              ZVARIABLE       = zvariable, $
                              CDF_BYTE        = cdf_byte, $
                              CDF_CHAR        = cdf_char, $
                              CDF_DOUBLE      = cdf_double, $
                              CDF_EPOCH       = cdf_epoch, $
                              CDF_LONG_EPOCH  = cdf_long_epoch, $
                              CDF_FLOAT       = cdf_float, $
                              CDF_INT1        = cdf_int1, $
                              CDF_INT2        = cdf_int2, $
                              CDF_INT4        = cdf_int4, $
                              CDF_REAL4       = cdf_real4, $
                              CDF_REAL8       = cdf_real8, $
                              CDF_UCHAR       = cdf_uchar, $
                              CDF_UINT1       = cdf_uint1, $
                              CDF_UINT2       = cdf_uint2, $
                              CDF_UINT4       = cdf_uint4, $
                              CDF_INT8        = cdf_int8, $
                              CDF_TIME_TT2000 = cdf_time_tt2000)
    endelse

;-------------------------------------------------------
; Add to Object ////////////////////////////////////////
;-------------------------------------------------------
    self -> CreateVarObj, varName

;-------------------------------------------------------
; Set Compression //////////////////////////////////////
;-------------------------------------------------------
    if (n_elements(compression) gt 0) || (n_elements(gzip_level) gt 0) then begin
        if n_elements(allocaterecs) gt 0 then begin
            message, 'Records have been allocated with ALLOCATERECS. Cannot set compression.', /INFORMATIONAL
        endif else begin
            tf_has = self -> HasVar(varName, OBJECT=varObj)
            varObj -> SetCompression, comp, GZIP_LEVEL=gzip_level
        endelse
    endif
end


;+
;   The purpose of this method is to create an attribute object.
;
; :Private:
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           A valid CDF attribute name.
;-
pro MrCDF_File::CreateAttrObj, attrName
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Create the attribute and add it to the container.
    attrObj = obj_new('MrCDF_Attribute', attrName, self, QUIET=self.quiet)
    
    ;Add the attribute to the proper container
    self.attrs -> Add, attrObj
end


;+
;   The purpose of this method is to create a variable object.
;
; :Private:
;
; :Params:
;       VARNAME:            in, required, type=string
;                           A valid CDF variable name.
;-
pro MrCDF_File::CreateVarObj, varName
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return
    endif
    
    ;Create the attribute and add it to the container.
    varObj = obj_new('MrCDF_Variable', varName, self, isZVar=isZVar, QUIET=self.quiet)
    
    ;Add to the proper container.
    if isZVar $
        then self.zVars -> Add, varObj $
        else self.rVars -> Add, varObj
end


;+
;   Get the value of a variable attribute.
;
; :Params:
;       GATTRIBUTE:         in, required, type=string/object
;                           Name or MrCDF_Attribute object of the global attribute to be
;                               copied. Must not already exist in `DESTOBJ`.
;       DESTOBJ:            in, required, type=string
;                           MrCDF_File object to which the global attribute is to be copied.
;-
pro MrCDF_File::CopyGlobalAttrTo, gAttribute, destObj
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    case size(gAttribute, /TNAME) of
        'OBJREF': gAttrObj = gAttribute
        'STRING': begin
            gAttrObj = self.attrs -> FindByName(gAttribute, COUNT=gAttrCount)
            if gAttrCount eq 0 then $
                message, 'Cannot find global attribute with name "' + gAttribute + '".'
            if obj_valid(gAttrObj) eq 0 then $
                message, 'Global attribute object invalid for "' + gAttribute + '".'
        endcase
        else: message, 'GATTRIBUTE must be an attribute name or object.'
    endcase

    ;Get the attribute's value.
    gAttrName  = gAttrObj -> GetName()
    
    ;Copy attribute
    destObj -> CreateAttr, gAttrName
    
    ;Find which entries have values
    mask = gAttrObj -> GetEntryMask(NUMGENTRIES=nGEntries)
    iGEntry = where(mask eq 1, nToCopy)
    if nToCopy eq 0 then return

    ;Copy values
    for i = 0, nToCopy - 1 do begin
        gAttrValue = gAttrObj -> GetGlobalAttrValue(iGEntry[i])
        destObj -> WriteGlobalAttr, gAttrName, gAttrValue
    endfor
end


;+
;   Copy a variable attribute from one CDF file to another.
;
; :Params:
;       VATTRIBUTE:         in, required, type=string/object
;                           Name or MrCDF_Attribute object of the variable attribute to be
;                               copied. Must not already exist in `DESTOBJ`.
;       DESTOBJ:            in, required, type=string
;                           MrCDF_File object to which the variable attribute is to be
;                               copied.
;
; :Keywords:
;       VARNAME:            in, optional, type=string
;                           Name of the variable for which the variable attribute value
;                               should be copied. If the variable does not exist in
;                               `DESTOBJ`, an error will occur.
;-
pro MrCDF_File::CopyVarAttrTo, vAttribute, destObj, $
varname=varname
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    ;Get the variable object
    case size(vAttribute, /TNAME) of
        'OBJREF': attrObj = vAttribute
        
        'STRING': begin
            attrObj = self.attrs -> FindByName(vAttribute, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable attribute with name "' + vAttribute + '".'
            if obj_valid(attrObj) eq 0 then $
                message, 'Variable attribute object invalid for "' + vAttribute + '".'
        endcase
        
        else: message, 'vAttribute must be an attribute name or object.'
    endcase

    ;Get the name
    vAttrName = attrObj -> GetName()
    
    ;Create the attribute if we need to
    if destObj -> HasAttr(vAttrName) eq 0 $
        then destObj -> CreateAttr, vAttrName, /VARIABLE_SCOPE

    ;Copy Attribute
    if n_elements(varname) gt 0 then begin
        ;Get the attribute value
        vAttrValue = attrObj -> GetVarAttrValue(varname, CDF_TYPE=cdf_type)
        
        ;Is it an epoch value?
        isEpoch = cdf_type eq 'CDF_EPOCH'   || $
                  cdf_type eq 'CDF_EPOCH16' || $
                  cdf_type eq 'CDF_TIME_TT2000'
        
        ;Copy Attribute value
        destObj -> WriteVarAttr, varname, vAttrName, vAttrValue, CDF_EPOCH=isEpoch
    endif
end


;+
;   Copy a variable, its data, and its variable attributes to another file.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired. Must not already
;                               exist in `DESTOBJ`.
;       DESTOBJ:            in, required, type=string
;                           MrCDF_File object to which the variable and its data is to be
;                               copied.
;-
pro MrCDF_File::CopyVariableTo, variable, destObj
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

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

    ;Make sure we have a variable name
    varname = varObj  -> GetName()
    
    ;Get the variable definition
    varObj -> GetProperty, CDF_TYPE    = cdf_type, $
                           COMPRESSION = compression, $
                           DIMVAR      = dimvary, $
                           DIMENSIONS  = dimensions, $
                           GZIP_LEVEL  = gzip_level, $
                           MAXREC      = maxrec, $
                           NELEMENTS   = numelem, $
                           RECVAR      = recvar, $
                           ZVARIABLE   = zvariable

    ;Does the variable exist in the destination? If not, create it.
    if destObj -> HasVar(varname) eq 0 then begin
        destObj -> CreateVar, varname, cdf_type, dimvary, $
                              ALLOCATERECS = maxrec+1, $
                              COMPRESSION  = compression, $
                              DIMENSIONS   = dimensions, $
                              GZIP_LEVEL   = gzip_level, $
                              NUMELEM      = numelem, $
                              REC_NOVARY   = ~recvar, $
                              ZVARIABLE    = zvariable
    endif
    
    ;Copy the variable's data
    self -> CopyVarDataTo, varName, destObj
    
    ;Copy the variable's attributes.
    varAttrNames = varObj -> GetAttrNames(COUNT=nVarAttrs)

    for i = 0, nVarAttrs - 1 $
        do self -> CopyVarAttrTo, varAttrNames[i], destObj, VARNAME=varname
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
;       DESTOBJ:            in, required, type=string
;                           The MrCDF_File object to which the variable's data is copied.
;-
pro MrCDF_File::CopyVarDataTo, variable, destObj, $
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = variable
        
        'STRING': begin
            varObj = self.zVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then varObj = self.zVars -> FindByName(variable, COUNT=varCount)
            if varCount eq 0 then $
                message, 'Cannot find variable with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Variable object invalid for "' + variable + '".'
        endcase
        
        else: message, 'VARIABLE must be an attribute name or object.'
    endcase

    ;Get the data
    data = varObj -> GetValue()
    name = varObj -> GetName()
    
    ;Write the information
    destObj -> WriteVar, name, data, $
                         COUNT        = count, $
                         INTERVAL     = interval, $
                         OFFSET       = offset, $
                         REC_INTERVAL = rec_interval, $
                         REC_START    = rec_start
end


;+
;   Delete the CDF file
;-
pro MrCDF_File::Delete
    compile_opt idl2
    on_error, 2
    
    ;Delete the file
    cdf_delete, self.fileID
    
    ;Close the file to flush object content
    self -> Close
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
pro MrCDF_File::DelGlobalAttr, gAttrName, entryNum
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.writeable eq 0 then $
        message, 'Cannot delete global attribute because file is READ-ONLY.'
        
    ;Check to see if the global attribute exists
    tf_Has = self -> HasAttr(gAttrName, OBJECT=gAttrObj)
    if tf_has eq 0 then message, 'Global attribute name does not exist: "' + gAttrName + '".'
    
    ;Remove value
    if n_elements(entryNum) gt 0 then begin
        cdf_attdelete, self.fileID, gAttrName, entryNum
        
    ;Delete Attribute
    endif else begin
        cdf_attdelete, self.fileID, gAttrName
        self.attributes -> Remove, gAttrObj, /DESTROY
    endelse
end


;+
;   Delete a global attribute from the file.
;
; :Params:
;       VATTRNAME:          in, required, type=string
;                           Name of the variable attribute to be deleted from the CDF file.
;       VARNAME:            in, optional, type=integer
;                           Name of the variable attribute from which the attribute is
;                               deleted. If not present, the entire attribute is deleted.
;-
pro MrCDF_File::DelVarAttr, vAttrName, varname
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.writeable eq 0 then $
        message, 'Cannot delete global attribute because file is READ-ONLY.'
        
    ;Check to see if the global attribute exists
    tf_Has = self -> HasAttr(gAttr, OBJECT=vAttrObj, /VARIABLE_SCOPE)
    if tf_has eq 0 then message, 'Variable attribute name does not exist: "' + vAttrName + '".'

;-------------------------------------------------------
; Delete from Variable /////////////////////////////////
;-------------------------------------------------------
    if n_elements(varname) gt 0 then begin
        ;Check if the variable exists
        tf_has = self -> HasVar(varname, OBJECT=varObj, ISZVAR=isZVar)
        if ~tf_has then message, 'Variable name does not exist: "' + varname + '".'
        
        ;Get the variable number
        varnum = varObj -> GetNumber()
    
        ;Delete the attribute from the variable
        cdf_attdelete, self.fileID, vAttrName, varnum

;-------------------------------------------------------
; Delete from File /////////////////////////////////////
;-------------------------------------------------------
    endif else begin
        cdf_attdelete, self.fileID, vAttrName
        self.attributes -> Remove, vAttrObj, /DESTROY
    endelse
end


;+
;   Delete a variable from the file.
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the variable to be deleted from the CDF file.
;-
pro MrCDF_File::DelVariable, varname
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.writeable eq 0 then $
        message, 'Cannot delete variable because file is READ-ONLY.'
        
    ;Check to see if the global attribute exists
    tf_Has = self -> HasVar(varname, ISZVAR=isZVar, OBJECT=varObj)
    if tf_has eq 0 then message, 'Variable name does not exist: "' + varname + '".'
    
    ;Delete the variable
    cdf_vardelete, self.fileID, varname, ZVARIABLE=isZVar
    
    ;Remove the variable from the container
    if isZVar $
        then self.zVars -> Remove, varObj, /DESTROY $
        else self.rVars -> Remove, varObj, /DESTROY
end


;+
;   The purpose of this method is to print information about the file.
;-
pro MrCDF_File::FileHelp
    compile_opt strictarr
    on_error, 2

    outHelp = self -> _OverloadHelp()
    print, outHelp
end


;+
;   The purpose of this method is to print information about the file.
;-
pro MrCDF_File::FilePrint
    compile_opt strictarr
    on_error, 2

    outInfo = self -> _OverloadPrint()
    print, outInfo
end


;+
;   Create a mask for the global entry numbers (gEntryNums) associated with a global
;   attribute. Global attribute values are array-like and are indexed by gEntryNums.
;   However, values do not need to exist at all gEntryNum. This method creates a mask
;   of 1s and 0s indicated if a value exists at the corresponding gEntryNum.
;
; :Params:
;       GATTRNAME:          in, required, type=string
;                           Name of the global attribute..
;
; :Returns:
;       GENTRYMASK:         Array of 1s and 0s indicating whether or not a value exists
;                               at the corresponding global entry number.
;-
function MrCDF_File::GetGEntryMask, gAttrName, $
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
    
    ;Get the mask
    gEntryMask = gAttrObj -> GetEntryMask()
    
    return, gEntryMask
end


;+
;   Get the names of all global and/or varaible attributes.
;
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           If set, variable attribute names will be included in the
;                               results.
;       COUNT:              out, optional, type=integer
;                           Number of attribute names returned.
;       VARIABLE_SCOPE:     in, optional, type=boolea, default=0
;                           If set, only variable attribute names will be returned.
;
; :Returns:
;       ATTRNAMES:          Name(s) of the attribute(s) within the CDF file. If no
;                               attributes are found, the empty string is returned.
;-
function MrCDF_File::GetAttrNames, $
ALL=all, $
COUNT=count, $
SHOW=show, $
VARIABLE_SCOPE=variable_scope
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile
    count = 0

    ;Defaults
    all            = keyword_set(all)
    show           = keyword_set(show)
    variable_scope = all ? 1 : keyword_set(variable_scope)
    global_scope   = all ? 1 : ~variable_scope

    ;Count the number of global attributes
    nAttrs = self.attrs -> Count()
    if nAttrs eq 0 then return, ''

    ;Get all of the global attribute names
    attrNames = strarr(nAttrs)
    allAttrs = self.attrs -> Get(/ALL)
    for i = 0, nAttrs-1 do begin
        allAttrs[i] -> GetProperty, NAME=name, SCOPE=scope
        
        ;Keep global attributes
        if global_scope && scope eq 'GLOBAL_SCOPE' then begin
            attrNames[count] = name
            count++
        endif
        
        ;Keep variable attributes
        if variable_scope && scope eq 'VARIABLE_SCOPE' then begin
            attrNames[count] = name
            count++
        endif
    endfor
    
    ;Return a scalar?
    if count eq 1 $
        then attrNames = attrNames[0] $
        else attrNames = attrNames[0:count-1]
    
    ;Print the names?
    if show then print, transpose(attrNames)

    ;Return a scalar?
    return, attrNames
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
;                           CDF datatype of `GATTRVALUE`.
;
; :Returns:
;       GATTRVALUE:         Value of the global attribute.
;-
function MrCDF_File::GetGlobalAttrValue, gAttribute, $
CDF_TYPE=cdf_type
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    case size(gAttribute, /TNAME) of
        'OBJREF': attrObj = gAttribute
        
        'STRING': begin
            attrObj = self.Attrs -> FindByName(gAttribute, COUNT=gAttrCount)
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
; :Keywords:
;       ALL:                in, optional, type=boolean, default=0
;                           If set, the names of both r- and z-variables are returned.
;       COUNT:              out, optional, type=integer
;                           Number of variable names returned.
;       RVARIABLE:          in, optional, type=boolean, default=0
;                           If set, r-variable names returned.
;       SHOW:               in, optional, type=boolean, default=0
;                           If set, variable names will be printed to the command window.
;
; :Returns:
;       VARNAMES:           Name(s) of the variable(s) within the CDF file. If no
;                               variable names are found, the empty string is returned.
;-
function MrCDF_File::GetVarNames, $
ALL=all, $
COUNT=nVars, $
RVARIABLE=rVariable, $
SHOW=show
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, ''
    endif

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile
    
    ;Defaults
    all  = keyword_set(all)
    show = keyword_set(show)
    rVariable = all eq 1 ? 1 : keyword_set(rVariable)
    zVariable = all eq 1 ? 1 : ~rVariable

    ;Count the number of r- and z-variables
    nRVars = (rVariable eq 0) ? 0 : self.rVars -> Count()
    nZVars = (zVariable eq 0) ? 0 : self.zVars -> Count()
    nVars  = nRVars + nZVars

    ;Get all of the global attribute names
    varNames = strarr(nVars)
    
    ;R-Variables
    if nRVars gt 0 then begin
        allR = self.rVars -> Get(/ALL)
        for i = 0, nRVars-1 do varNames[i] = allR[i] -> GetName()
    endif
    
    ;Z-Variables
    if nZVars gt 0 then begin
        allZ = self.zVars -> Get(/ALL)
        for i = 0, nZVars-1 do varNames[nRVars+i] = allZ[i] -> GetName()
    endif

    ;Print results?
    if show then print, transpose(varNames)

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
;       ERROR:              out, optional, type=integer
;                           Named variable to recieve the error code. 0 indicates no
;                               error. If present, the error message will be suppressed.
;       SHOW:               in, optional, type=boolean, default=0
;                           If set, variable attribute names are printed to the command window.
;
; :Returns:
;       ATTRNAMES:          Name(s) of the variable attributes associated with `VARIABLE`
;-
function MrCDF_File::GetVarAttrNames, variable, $
COUNT=varAttrCount, $
ERROR=the_error, $
SHOW=show
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, ''
    endif

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

    ;Get the variable object
    case size(variable, /TNAME) of
        'OBJREF': varObj = variable
        
        'STRING': begin
            varObj = self.zVars -> FindByName(variable, COUNT=vAttrCount)
            if vAttrCount eq 0 then varObj = self.rVars -> FindByName(variable, COUNT=vAttrCount)
            if vAttrCount eq 0 then $
                message, 'Cannot find global attribute with name "' + variable + '".'
            if obj_valid(varObj) eq 0 then $
                message, 'Attribute object invalid for "' + variable + '".'
        endcase
        
        else: message, 'VARIABLE must be an variable name or object.'
    endcase

    ;Get the value
    varAttrNames = varObj -> GetAttrNames(COUNT=varAttrCount, ERROR=the_error, SHOW=show)
    if the_error ne 0 then message, /REISSUE_LAST
    
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
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of `GATTRVALUE`.
;       FOLLOW_PTR:         in, optional, type=boolean, default=0
;                           If `ATTRVALUE` points to a variable, then follow the pointer,
;                               read the variable's data, and return it.
;       PTR_VALUE:          out, optional, type=string, default=''
;                           If `FOLLOW_PTR` is set, this will return the name of the
;                               variable that serves as the pointer. If `ATTRVALUE` is
;                               not a pointer, then the empty string is returned.
;
; :Returns:
;       ATTRVALUE:          Value of the attribute.
;-
function MrCDF_File::GetVarAttrValue, variable, attrName, $
CDF_TYPE=cdf_type, $
FOLLOW_PTR=follow_ptr, $
PTR_VALUE=ptr_value
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

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

    ;Get the variable attribute's value
    value = varObj -> GetAttrValue(attrName, CDF_TYPE   = cdf_type, $
                                             FOLLOW_PTR = follow_ptr, $
                                             PTR_VALUE  = ptr_value)
    
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
;       REC_START:          in, optional, type=integer, default=0
;                           Record at which to begin reading data.
;       SINGLE_VALUE:       in, optional, type=boolean, default=0
;                           Read a single value via the CDF_VarGet1 procedure. The
;                               default is to read a full record via CDF_VarGet.
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
function MrCDF_File::GetVarData, variable, $
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
PADVALUE=padvalue
    compile_opt strictarr
    on_error, 2

    ;File must be parsed first
    if self.isParsed eq 0 then self -> ParseFile

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
                              SINGLE_VALUE=single_value, $
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
;       DECODING:           out, optional, type=string
;                           Decoding type for the CDF file.
;       ENCODING:           out, optional, type=string
;                           Encoding type for the CDF file.
;       FILENAME:           out, optional, type=string
;                           Name of the CDF file.
;       FILEID:             out, optional, type=long
;                           CDF file identifier.
;       MAJORITY:           out, optional, type=string
;                           Majority using in the file: {"ROW_MAJOR" | "COL_MAJOR"}
;       NATTRS:             out, optional, type=integer
;                           Total number of attributes within the file
;       NGATTRS:            out, optional, type=integer
;                           Number of global attributes within the file
;       NVATTRS:            out, optional, type=integer
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
pro MrCDF_File::GetProperty, $
COMPRESSION=compression, $
DECODING=decoding, $
ENCODING=encoding, $
FILENAME=filename, $
FILEID=fileID, $
GZIP_LEVEL=gzip_level, $
MAJORITY=majority, $
NATTRS=nAttrs, $
NGATTRS=nGAttrs, $
NVATTRS=nVAttrs, $
NVARS=nVars, $
NRVARS=nRVars, $
NZVARS=nZVars, $
WRITEABLE=writeable
    compile_opt strictarr
    on_error, 2
    
    if arg_present(decoding)   then decoding   = self.decoding
    if arg_present(encoding)   then encoding   = self.encoding
    if arg_present(filename)   then filename   = self.filename
    if arg_present(fileID)     then fileID     = self.fileID
    if arg_present(gzip_level) then gzip_level = *self.gzip_level
    if arg_present(majority)   then majority   = self.majority
    if arg_present(nAttrs)     then nAttrs     = self.attrs -> Count()
    if arg_present(nGAttrs)    then void       = self -> GetAttrNames(COUNT=nGAttrs)
    if arg_present(nVAttrs)    then void       = self -> GetAttrNames(COUNT=nVAttrs, /VARIABLE_SCOPE)
    if arg_present(nRVars)     then nrvars     = self.rVars -> Count()
    if arg_present(nZVars)     then nzvars     = self.zVars -> Count()
    if arg_present(writeable)  then writeable  = self.writeable
    
    if arg_present(compression) then begin
        case self.compression of
            0: compression = 'None'
            1: compression = 'Run-Length Encoding'
            2: compression = 'Huffman'
            3: compression = 'Adaptive Huffman'
            5: compression = 'GZIP'
        endcase
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
function MrCDF_File::GetFileID
    return, self.fileID
end


;+
;   Determines if a specific attribute is present in the file.
;
; :Params:
;       ATTRNAME:           in, required, type=string
;                           Name of the attribute to search for.
;
; :Keywords:
;       OBJECT:             out, optional, type=object
;                           CDF_Attribute object for the desired attribute.
;
; :Returns:
;       TF_HAS:             Returns true (1) of the attribute name exists, false (0) otherwise.
;-
function MrCDF_File::HasAttr, attrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try to find the object
    object = self.attrs -> FindByName(attrName, COUNT=count)
    if count eq 0 then return, 0
    
    return, 1
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
function MrCDF_File::HasVar, varName, $
ISZVAR=isZVar, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try zVariables first
    isZVar = 1B
    object = self.zVars -> FindByName(varName, COUNT=count)
    
    ;Try rVariables
    if count eq 0 then begin
        object = self.rVars -> FindByName(varName, COUNT=count)
        isZVar = 0B
    endif
        
    ;Was the variable found?
    return, (count gt 0)
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
;       ERROR:              out, optional, type=integer
;                           Named variable into which the error code will be returned.
;                               0 indicates no error. If present, the dialog error message
;                               will be suppressed.
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
;       VALIDATE:           in, optional, type=boolean, default=0
;                           Set equal to 1 to turn file validation on when opening a
;                               file. File validation only occurs in IDL 8.0+.
;-
pro MrCDF_File::Open, filename, $
BACKWARD_COMPATIBLE = backward_compatible, $
CANCEL = cancel, $
COMPRESSION = compression, $
CLOBBER = clobber, $
CREATE = create, $
DECODING = decoding, $
DIALOG_PARENT = dialog_parent, $
DIRECTORY = directory, $
ERROR = the_error, $
ENCODING = encoding, $
GZIP_LEVEL = gzip_level, $
MULTI_FILE = multi_file, $
MODIFY = modify, $
ROW_MAJOR = row_major, $
VALIDATE = validate
    compile_opt strictarr
    
    ;Catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        ;close the CDF file if it was opened.
        if n_elements(fileID) ne 0 then cdf_close, self.fileID
        cdf_set_validate, /YES
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return
    endif
    
    ;Defaults
    cancel              = 0B
    backward_compatible = keyword_set(backward_compatible)
    clobber             = keyword_Set(clobber)
    create              = keyword_set(create)
    modify              = keyword_set(modify)
    row_major           = keyword_set(row_major)
    no_validate         = ~n_elements(validate)
    col_major           = ~row_major
    if n_elements(encoding) eq 0 then encoding = 'HOST'
    if n_elements(decoding) eq 0 then decoding = encoding
    
    ;Pick a file
    if n_elements(filename) eq 0 then begin
        filename = cgpickfile(FILTER='*.cdf', PATH=directory, $
                              TITLE='Select a CDF File...', DIALOG_PARENT=dialog_parent)
        
        ;Check if the cancel button was pushed.
        if filename eq '' then begin
            cancel = 1B
            return
        endif
    endif
    
    ;Are we creating a new file or opening an existing file? The file is writeable
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
            message, 'File does not exist. Cannot read: "' + filename + '".'
        if modify && file_test(filename, /WRITE) eq 0 then $
            message, 'Cannot write to file: "' + filename + '".'
    endif
    
    ;Validate the file (IDL v8.0+)
    if no_validate && MrCmpVersion('8.0') le 0 then cdf_set_validate, /NO
    
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
            fileID = cdf_create(filename, $
                                ROW_MAJOR           = row_major, $
                                COL_MAJOR           = col_major, $
                                MULTI_FILE          = multi_file, $
                                ALPHAOSF1_ENCODING  = alphaosf1_encoding, $
                                ALPHAVMSD_ENCODING  = alphavmsd_encoding, $
                                ALPHAVMSG_ENCODING  = alphavmsg_encoding, $
                                DECSTATION_ENCODING = decstation_encoding, $
                                HOST_ENCODING       = host_encoding, $
                                HP_ENCODING         = hp_encoding, $
                                IBMPC_ENCODING      = ibmpc_encoding, $
                                IBMRS_ENCODING      = ibmrs_encoding, $
                                MAC_ENCODING        = mac_encoding, $
                                NETWORK_ENCODING    = network_encoding, $
                                NEXT_ENCODING       = next_encoding, $
                                SGI_ENCODING        = sgi_encoding, $
                                SUN_ENCODING        = sun_encoding, $
                                ALPHAOSF1_DECODING  = alphaosf1_decoding, $
                                ALPHAVMSD_DECODING  = alphavmsd_decoding, $
                                ALPHAVMSG_DECODING  = alphavmsg_decoding, $
                                DECSTATION_DECODING = decstation_decoding, $
                                HOST_DECODING       = host_decoding, $
                                HP_DECODING         = hp_decoding, $
                                IBMPC_DECODING      = ibmpc_decoding, $
                                IBMRS_DECODING      = ibmrs_decoding, $
                                MAC_DECODING        = mac_decoding, $
                                NETWORK_DECODING    = network_decoding, $
                                NEXT_DECODING       = next_decoding, $
                                SGI_DECODING        = sgi_decoding, $
                                SUN_DECODING        = sun_decoding)
        endcase
    endcase
    
    ;Turn file validation back on (IDL v8.0+)
    if no_validate && MrCmpVersion('8.0' le 0) then cdf_set_validate, /YES

    ;update the object fields
    self.fileID   = fileID
    self.filename = filename
    
    ;Set the compression?
    if n_elements(compression) gt 0 || n_elements(gzip_level) gt 0 then begin
        self -> SetCompression, compression, GZIP_LEVEL=gzip_level
    endif
end


;+
;   The purpose of this method is to load the Variable Attributes for each z-variable
;   of the CDf file.
;-
pro MrCDF_File::ParseFile
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
    self.attrs -> Remove, /ALL, /DESTROY
    self.zVars -> Remove, /ALL, /DESTROY
    self.rVars -> Remove, /ALL, /DESTROY

;-----------------------------------------------------
; File Info \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    file_inq = cdf_inquire(self.fileID)
    self.encoding = file_inq.encoding
    self.decoding = file_inq.decoding
    self.majority = file_inq.majority

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
; Parse zVariables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;zVariables are stored after rVariables.
    for varNum = 0, nZVars-1 do begin
        varinq = cdf_varinq(self.fileID, varNum, /ZVARIABLE)

        ;Create a variable object
        self -> CreateVarObj, varinq.name
    endfor

;-----------------------------------------------------
; Parse rVariables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;zVariables are stored after rVariables.
    for varNum = 0, nRVars-1 do begin
        varinq = cdf_varinq(self.fileID, varNum)

        ;Create a variable object
        self -> CreateVarObj, varinq.name
    endfor
    
    self.isParsed = 1B
end


;+
;   A more robust method for obtaining variable data, when compared to the GetVarData
;   method.
;
; :Params:
;       VARNAME:            in, required, type=string/object
;                           Name or CDF_Variable object of the variable to which the
;                               attribute value will be written.
;
; :Keywords:
;       BOUNDS:             in, optional, type=string, default=''
;                           A string representing the array bounds to be returned. Each
;                               dimension of the returned data can have a
;                               [start:stop:stride] indicating the records to be read.
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       PATTERN:            in, optional, type=boolean, default="%Y-%M-%dT%H:%m:%S%z"
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
;       REC_START:          in, optional, type=integer, default=0
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
;       CDF_TYPE:           out, optional, type=string
;                           CDF datatype of the variable being read.
;       DEPEND_0:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_0` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       DEPEND_1:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_1` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       DEPEND_2:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_2` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       DEPEND_3:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_3` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       FILLVALUE:          out, optional, type=any
;                           Value used as a filler for missing data.
;       PADVALUE:           out, optional, type=any
;                           Value used to pad the data when more data is read than what
;                               exists in the file. It is possible that this value does
;                               not exist.
;-
function MrCDF_File::Read, varName, $
;INPUT
BOUNDS=bounds, $
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
CDF_TYPE=cdf_type, $
DEPEND_0=depend_0, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3, $
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
    if n_elements(bounds)  eq 0 then bounds  = ''
    if n_elements(pattern) eq 0 then pattern = ''
    
    ;Dependencies
    if pattern ne '' || MrIsA(rec_start, 'STRING') || MrIsA(rec_end, 'STRING') then time = 1
    if time && pattern eq '' then pattern = "%Y-%M-%dT%H:%m:%S%z"

    ;Get the variable object
    tf_has = self -> HasVar(varName, OBJECT=varObj)
    if tf_has eq 0 then $
        message, 'Variable "' + varName + '" is not found in the file.'

;-----------------------------------------------------
; Time Range? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Flag indicating that the variable given represents time.
    isTime = 0
    if time eq 1 then begin
    ;-----------------------------------------------------
    ; Get Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        varObj -> GetProperty, CDF_TYPE=cdf_type

        ;Time variable
        if max(cdf_type eq ['CDF_EPOCH', 'CDF_EPOCH16', 'CDF_TIME_TT2000']) eq 1 then begin
            isTime   = 1B
            depend_0 = self -> GetVarData(varName, CDF_TYPE=epoch_type)

        endif else begin
            ;Search for DEPEND_0
            tf_has = varObj -> HasAttr('DEPEND_0')
            if tf_has then begin
                ;Get the name of the epoch variable
                tVarName = varObj -> GetAttrValue('DEPEND_0')
        
                ;Read the time and figure out its epoch type.
                depend_0 = self -> GetVarData(tVarName, CDF_TYPE=epoch_type)
            endif else begin
                message, 'No DEPEND_0 attribute exists for variable "' + varName + '". Reading all records.', /INFORMATIONAL
            endelse
        endelse

        ;Is there a DEPEND_0 variable?
        if tf_has then begin
        ;-----------------------------------------------------
        ; Convert Interval to Epoch Values \\\\\\\\\\\\\\\\\\\
        ;-----------------------------------------------------
            if MrIsA(rec_start, 'STRING') $
                then epoch_start = MrCDF_Epoch_Parse(rec_start, PATTERN=pattern, EPOCH_TYPE=epoch_type) $
                else epoch_start = n_elements(rec_start) gt 0 ? rec_start : depend_0[0]
                
            if MrIsA(rec_end, 'STRING') $
                then epoch_end = MrCDF_Epoch_Parse(rec_end, PATTERN=pattern, EPOCH_TYPE=epoch_type) $
                else epoch_end = n_elements(rec_end) gt 0 ? rec_end : depend_0[n_elements(depend_0)-1]

        ;-----------------------------------------------------
        ; Find Record Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;-----------------------------------------------------
            ;REC_START and REC_COUNT are needed
            comp = MrCDF_Epoch_Compare(depend_0, epoch_start, epoch_end)
            iMatch = where(comp eq 1, rec_count)
            if rec_count eq 0 $
                then message, 'No records found between REC_START and REC_END.' $
                else rec_start_out = min(iMatch)
        endif

;-----------------------------------------------------
; Bounds? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if bounds ne '' then begin
        ;Form the dimension sizes of the variable
        varObj -> GetProperty, MAXREC=maxrec, DIMENSIONS=dim
        nDims = n_elements(dim)
        dims  = [dim, maxrec + 1]
        if nDims eq 1 && dim eq 0 then nDims = 0

        ;Get the records to be read.
        indexing      = MrArray_Bounds(dims, bounds, /DIMENSIONS, /COUNT, SINGLE=single)
        rec_start_out = reform(indexing[nDims, 0])
        rec_count     = reform(indexing[nDims, 1])
        rec_interval  = reform(indexing[nDims, 2])
        
        ;Cannot do single dimension indexing (have not tried).
        if single and nDims gt 0 then $
            message, 'Invalid number of dimensions in BOUNDS.' 
        
        ;Dimensions to read
        if nDims gt 0 then begin
            offset   = reform(indexing[0:nDims-1, 0])
            count    = reform(indexing[0:nDims-1, 1])
            interval = reform(indexing[0:nDims-1, 2])
        endif

;-----------------------------------------------------
; Record Range? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
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
    if n_elements(rec_interval) eq 0 then rec_interval  = 1

    ;Get the data
    if isTime eq 0 then begin
        data = varObj -> GetValue(COUNT=count, INTERVAL=interval, OFFSET=offset, $
                                  REC_COUNT=rec_count, REC_INTERVAL=rec_interval, $
                                  REC_START=rec_start_out, STRING=string, CDF_TYPE=cdf_type, $
                                  FILLVALUE=fillvalue, PADVALUE=padvalue)
    endif else begin
        data = depend_0[*,rec_start_out:rec_start_out+rec_count-1:rec_interval]
    endelse
    
    ;DEPEND_0
    if arg_present(depend_0) then begin
        if n_elements(depend_0) gt 0 then begin
            depend_0 = depend_0[*,rec_start_out:rec_start_out+rec_count-1:rec_interval]
        endif else begin
            tf_dep0 = varObj -> HasAttr('DEPEND_0', OBJECT=attrObj)
            if tf_dep0 then begin 
                dep0VarName = attrObj -> GetVarAttrValue(varName)
                depend_0 = self -> GetVarData(dep0VarName, COUNT=count, INTERVAL=interval, $
                                              OFFSET=offset, REC_COUNT=rec_count, $
                                              REC_INTERVAL=rec_interval, REC_START=rec_start_out)
            endif
        endelse
    endif
    
    ;DEPEND_1
    if arg_present(depend_1) then if varObj -> HasAttr('DEPEND_1', OBJECT=attrObj) then begin
        dep1VarName = attrObj -> GetVarAttrValue(varName)
        depend_1 = self -> GetVarData(dep1VarName, COUNT=count, INTERVAL=interval, $
                                      OFFSET=offset, REC_COUNT=rec_count, $
                                      REC_INTERVAL=rec_interval, REC_START=rec_start_out)
    endif
    
    ;DEPEND_2
    if arg_present(depend_2) then if varObj -> HasAttr('DEPEND_2', OBJECT=attrObj) then begin
        dep2VarName = attrObj -> GetVarAttrValue(varName)
        depend_2 = self -> GetVarData(dep2VarName, COUNT=count, INTERVAL=interval, $
                                      OFFSET=offset, REC_COUNT=rec_count, $
                                      REC_INTERVAL=rec_interval, REC_START=rec_start_out)
    endif
    
    ;DEPEND_3
    if arg_present(depend_3) then if varObj -> HasAttr('DEPEND_3', OBJECT=attrObj) then begin
        dep3VarName = attrObj -> GetVarAttrValue(varName)
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
pro MrCDF_File::SetCompression, compression, $
GZIP_LEVEL=gzip_level
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
    cdf_compression, self.fileID, SET_COMPRESSION=comp, $
                     SET_GZIP_LEVEL=gzip_level
                     
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
pro MrCDF_File::SetProperty, $
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
;   Parse file information into a structure.
;-
function MrCDF_File::ToStruct, $
READ_DATA=read_data
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        void = cgErrorMsg()
        return, -1
    endif
    
    ;Defaults
    read_data = keyword_set(read_data)
    
    self -> GetProperty, NGATTRS=nGAttrs, NVATTRS=nVAttrs, NRVARS=nRVars, NZVARS=nZVars
    
    ;Information about variable.
    file_struct = {_NAME:         self.filename, $
                   _ENCODING:     self.encoding, $
                   _DECODING:     self.decoding, $
                   _MAJORITY:     self.majority, $
                   _NGLOBALATTRS:      nGAttrs, $
                   _NVARATTRS:         nVAttrs, $
                   _NRVARS:            nRVars, $
                   _NZVARS:            nZVars}
    
;---------------------------------------------------------------------
; Global Attributes //////////////////////////////////////////////////
;---------------------------------------------------------------------
    allAttrs = self.attrs -> Get(/ALL, COUNT=nAttrs)
    for i = 0, nAttrs - 1 do begin
        thisAttr = allAttrs[i]
        thisAttr -> GetProperty, SCOPE=scope
        if strpos(scope, 'GLOBAL') eq -1 then continue
    
        gAttr_struct = thisAttr -> ToStruct()
    
        ;Create a structure tag out of the name.
        ;   Convert to uppercase.
        ;   Convert all non-alphanumeric characters to underscores.
        tag = gAttr_struct._NAME
        tag = strjoin(strsplit(strupcase(tag), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        
        ;Add the attribute to the variable structure
        file_struct = create_struct(file_struct, tag, gAttr_struct)
    endfor
    
;---------------------------------------------------------------------
; rVariables /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    allRVars = self.rVars -> Get(/ALL, COUNT=nRVars)
    for i = 0, nRVars - 1 do begin
        rVar_struct = allRVars[i] -> ToStruct(READ_DATA=read_data)
    
        ;Create a structure tag out of the name.
        ;   Convert to uppercase.
        ;   Convert all non-alphanumeric characters to underscores.
        tag = rVar_struct._NAME
        tag = strjoin(strsplit(strupcase(tag), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        
        ;Add the attribute to the variable structure
        file_struct = create_struct(file_struct, tag, rVar_struct)
    endfor
    
;---------------------------------------------------------------------
; zVariables /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
    allZVars = self.zVars -> Get(/ALL, COUNT=nZVars)
    for i = 0, nZVars - 1 do begin
        zVar_struct = allZVars[i] -> ToStruct(READ_DATA=read_data)
    
        ;Create a structure tag out of the name.
        ;   Convert to uppercase.
        ;   Convert all non-alphanumeric characters to underscores.
        tag = zVar_struct._NAME
        tag = strjoin(strsplit(strupcase(tag), '[^A-Z0-9]', /EXTRACT, /REGEX), '_')
        
        ;Add the attribute to the variable structure
        file_struct = create_struct(file_struct, tag, zVar_struct)
    endfor
    
    return, file_struct
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
function MrCDF_File::VarHasAttr, varName, attrName, $
OBJECT=object
    compile_opt strictarr
    on_error, 2

    ;Try to find the object
    varObj = self.zvars -> FindByName(varName, COUNT=count)
    if count eq 0 then begin
        varObj = self.rvars -> FindByName(varName, COUNT=count)
        if count eq 0 then message, 'Cannot find variable "' + varName + '".'
    endif
    
    ;Is the attribute there?
    tf_has = varObj -> HasAttr(attrName, OBJECT=object)
    
    return, tf_has
end


;+
;   Write a value to a global attribute.
;
;   Values are stored at global entry numbers, which serve as array indices. They range
;   from 0 to maxGEntry, which can be obtained from CDF_Control's GET_ATTR_INFO property.
;   However, unlike array indices, gEntryNums do not have to be contiguous. Out of the
;   maxGEntry+1 entries, only numGEntries (again, obtained from CDF_Control) are defined.
;   Use the GetGEntryMask method to determine which are defined.
;
; :Params:
;       GATTRNAME:          in, required, type=string/object
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
;       GENTRYNUM:          in, optional, type=integer, default=maxGEntry+1
;                           Either a scalar or vector of global entry numbers. If a scalar,
;                               then the element(s) of `VALUE` will be stored contiguously
;                               starting at the given value. If a vector, it must be the
;                               same length as `VALUE`. If a value was already stored at
;                               GENTRYNUM, then it will be overwritten.
;-
pro MrCDF_File::WriteGlobalAttr, gAttrName, value, $
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
    if self -> HasAttr(gAttrName) eq 0 then begin
        if keyword_set(create) $
            then self -> CreateAttr, gAttrName $
            else message, 'Global attribute "' + gAttrName + '" does not exist. ' + $
                          'Set the CREATE keyword or use the CreateAttr method to create it.'
    endif
    
    ;Check inputs
    nValues   = n_elements(value)
    cdf_epoch = keyword_set(cdf_epoch)
    
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
pro MrCDF_File::WriteVarAttr, variable, attrName, value, $
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
;   Write variable data to the CDF file.
;
; NOTES:
;   Records and dimensions should be ordered in the following manner::
;       [DEPEND_3, DEPEND_2, DEPEND_1, DEPEND_0, RECORDS]
;
;   with higher dependencies being optional.
;
; :Params:
;       VARIABLE:           in, required, type=string/object
;                           Name or CDF_Variable object of the variable for which the
;                               variable attribute names are desired.
;       DATA:               in, required, type=any
;                           Data to be written.
;
; :Keywords:
;       COMPRESSION:        in, optional, type=string/integer, default='None'
;                           Type of variable compression to perform. Options are::
;                               0 or 'None'
;                               1 or 'Run-Length Encoding'
;                               2 or 'Huffman'
;                               3 or 'Adaptive Huffman'
;                               5 or 'GZIP'
;       COUNT:              in, optional, type=intarr, default=dimesnions of `DATA`.
;                           Vector containing the counts to be used when writing each
;                               value. Does not have to be the same size as `DIMENSION`.
;       GZIP_LEVEL:         in, optional, type=integer, default=5
;                           The desired effort for the GZIP compression. This effort must
;                               be expressed as a scalar in the range (1-9). If set, then
;                               `COMPRESSION` is set to 5 automatically.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       OFFSET:             in, optional, type=intarr, default=0 for each dimension
;                           Array indices within the specified record(s) at which to
;                               begin writing. OFFSET is a 1-dimensional array
;                               containing one element per CDF dimension.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records being written when writing multiple records.
;       REC_START:          in, optional, type=integer, default=0
;                           Record at which to begin writing data.
;-
pro MrCDF_File::WriteVar, variable, data, $
COUNT=count, $
INTERVAL=interval, $
OFFSET=offset, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start, $
;Create Variable?
COMPRESSION=compression, $
CREATE=create, $
CDF_TYPE=cdf_type, $
DIMS=dims, $
GZIP_LEVEL=gzip_level, $
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
            if n_elements(dims) ne 0 then dimVary = dims gt 1
        
        ;Get the data type
        if n_elements(cdf_type) eq 0 then begin
            idl_type = size(data, /TNAME)
            cdf_type = MrCDF_CastDataType(idl_type, /TNAME)
        endif

        ;Create the variable
        self -> CreateVar, variable, cdf_type, dimVary, ALLOCATERECS=allocaterecs, $
                           COMPRESSION=compression, DIMENSIONS=dims, $
                           GZIP_LEVEL=gzip_level, REC_NOVARY=rec_novary
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
    
    ;Prevent warnings from the CDF DLM.
    quiet_in = !Quiet
    !Quiet   = self.quiet

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
pro MrCDF_File::cleanup
    compile_opt idl2
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        void = cgErrorMSG()
        return
    endif
    
    ;Close the file
    self -> Close
    
    ;Destoy objects
    obj_destroy, self.attrs
    obj_destroy, self.rVars
    obj_destroy, self.zVars
    
    ;Free heap variables
    ptr_free, self.gzip_level
    
    ;Superclasses
    self -> IDL_Object::Cleanup
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
;       ERROR:              out, optional, type=integer
;                           Named variable into which the error code will be returned.
;                               0 indicates no error. If present, the dialog error message
;                               will be suppressed.
;       QUIET:              in, optional, type=boolean, default=1
;                           If set, annoying warnings from the CDF DLM  will be suppressed.
;                               This is the default.
;       _REF_EXTRA:         in, optional, type=any
;                           Any keyword accepted by the Open method is also accepted
;                               via keyword inheritance.
;
; :Returns:
;       SUCCESS:            If `FILENAME` was (chosen and) opened successfully, then a
;                           valid object reference is returned (1). Otherwise, an invalid
;                           object reference is returned (0).
;-
function MrCDF_File::init, filename, $
CREATE=create, $
DIALOG_PARENT=dialog_parent, $
DIRECTORY=directory, $
ERROR=the_error, $
QUIET=quiet, $
_REF_EXTRA=extra
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        if arg_present(the_error) eq 0 then void = cgErrorMsg()
        return, 0
    endif

    ;Defaults
    create = keyword_set(create)
    
    ;Set Properties
    self.quiet = n_elements(quiet) eq 0 ? 1 : keyword_set(quiet)
    self.attrs = obj_new('MrCDF_Container')
    self.rVars = obj_new('MrCDF_Container')
    self.zVars = obj_new('MrCDF_Container')
    self.gzip_level = ptr_new(/ALLOCATE_HEAP)

    ;Open the file and load the meta-data
    self -> open, filename, $
                  CREATE=create, $
                  CANCEL=cancel, $
                  DIALOG_PARENT=dialog_parent, $
                  DIRECTORY=directory, $
                  ERROR=the_error, $
                  _STRICT_EXTRA=extra
    if cancel eq 1 then return, 0
    if the_error ne 0 then message, /REISSUE_LAST
    
    ;Parse the file if it existed previously
    if create eq 0 then self -> ParseFile

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
;       ATTRS:              Container of attribute objects.
;       DECODING:           Decoding type for the CDF file.
;       ENCODING:           Encoding type for the CDF file.
;       COMPRESSION:        Type of file compression.
;       FILENAME:           The name of the CDF file begin read.
;       FILEID:             The CDF ID number of the file being read.
;       ISPARSED:           Indicates that the file has been parsed.
;       MAJORITY:           Majority using in the file.
;       QUIET:              Suppress warnings from the CDF DLM.
;       RVARS:              Container of rVariables.
;       WRITEABLE:          Indicates that the file is writeable.
;       ZVARS:              Container of zVariables.
;-
pro MrCDF_File__define, class
    compile_opt strictarr
    
    define = { MrCDF_File, $
               inherits IDL_Object, $
               compression: 0S, $
               decoding:    '', $
               encoding:    '', $
               filename:    '', $
               fileID:      0L, $
               isParsed:    0B, $
               gzip_level:  ptr_new(), $
               majority:    '', $
               quiet:       0B, $
               attrs:       obj_new(), $
               rVars:       obj_new(), $
               zVars:       obj_new(), $
               writeable:   0B $
             }
end
