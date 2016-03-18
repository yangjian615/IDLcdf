;+
; NAME:
;       MrCDF_CastDataType
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
;
;       This is a utility routine to turn IDL data types into the equivalent
;       CDF data type. In other words, change 'STRING' to 'CDF_BYTE' and so on.
;
; :Categories:
;       CDF Utilities
;
; :Params:
;       VARIABLE:       in, required, type=any
;                       The IDL variable for which you want a netCDF data type.
;                           Or, if the TYPE keyword is set, the variable type index you wish
;                           to convert. Or, if the TNAME keyword is set, the variable type
;                           name you wish to convert.
;                      
; :Keywords:
;        TYPE:          in, optional, type=boolean, default=0
;                       If set, the positional argument is an IDL variable type of
;                           the sort returned by the SIZE function with the TYPE keyword set.
;        TNAME:         in, optional, type=boolean, default=0
;                       If set, the positional argument is an IDL variable type of
;                           the sort returned by the SIZE function with the TNAME keyword set.
;
; :Returns:
;       cdf_type:       Equivalent CDF variable type. Options are::
;                           'CDF_BYTE'
;                           'CDF_CHAR'
;                           'CDF_DOUBLE',    'CDF_REAL8'
;                           'CDF_EPOCH'
;                           'CDF_LONG_EPOCH' (i.e. CDF_EPOCH16)
;                           'CDF_FLOAT',     'CDF_REAL4'
;                           'CDF_INT1'
;                           'CDF_INT2'
;                           'CDF_INT4'
;                           'CDF_UCHAR'
;                           'CDF_UINT1'
;                           'CDF_UINT2'
;                           'CDF_UINT4'
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
;-
FUNCTION MrCDF_CastDataType, variable, $
TNAME=tname, $
TYPE=type
    On_Error, 2
    
    ;Only one keyword can be set at a time.
    IF (Keyword_Set(tname) + Keyword_Set(type)) EQ 2 THEN $
        Message, 'TNAME and TYPE are mutually exclusive.'
        
    ;Get the data type of the variable. This may be wrong, depending on which
    ;keywords are set, if any.
    idl_type = Size(variable, /TNAME)
    
    ;Is the TNAME keyword set?
    IF Keyword_Set(tname) THEN idl_type = variable
    
    ;Is the TYPE keyword set?
    IF Keyword_Set(type) THEN BEGIN
        CASE strupcase(variable) OF
             0: idl_type = 'UNDEFINED'
             1: idl_type = 'BYTE'
             2: idl_type = 'INT'
             3: idl_type = 'LONG'
             4: idl_type = 'FLOAT'
             5: idl_type = 'DOUBLE'
             6: idl_type = 'COMPLEX'
             7: idl_type = 'STRING'
             8: idl_type = 'STRUCT'
             9: idl_type = 'DCOMPLEX'
            10: idl_type = 'POINTER'
            11: idl_type = 'OBJREF'
            12: idl_type = 'UINT'
            13: idl_type = 'ULONG'
            14: idl_type = 'LONG64'
            15: idl_type = 'ULONG64'
        ENDCASE
    ENDIF
    
    ;Convert from IDL to CDF data type
    case idl_type of
        ;'UNDEFINED': 
        'BYTE':     cdf_type = 'CDF_UINT1'      ;NOT 'CDF_BYTE', which is a 1-byte signed integer
        'INT':      cdf_type = 'CDF_INT2'
        'LONG':     cdf_type = 'CDF_INT4'
        'FLOAT':    cdf_type = 'CDF_FLOAT'      ;=CDF_REAL4
        'DOUBLE':   cdf_type = 'CDF_DOUBLE'     ;=CDF_REAL8 (should be CDF_EPOCH if time)
        ;'COMPLEX':
        'STRING':   cdf_type = 'CDF_UCHAR'
        ;'STRUCT':
        'DCOMPLEX': cdf_type = 'CDF_LONG_EPOCH' ;i.e. CDF_EPOCH16 (time)
        ;'POINTER':
        ;'OBJREF':
        'UINT':     cdf_type = 'CDF_UINT2'
        'ULONG':    cdf_type = 'CDF_UINT4'
        'LONG64':   cdf_type = 'CDF_INT8'       ;should be CDF_EPOCH_TT2000 if time
        ;'ULONG64'
        else: message, 'IDL variable type "' + idl_type + '" cannot be converted to CDF type.'
    endcase

    return, cdf_type
END
