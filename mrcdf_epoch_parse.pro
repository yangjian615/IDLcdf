; docformat = 'rst'
;
; NAME:
;       MrCDF_Epoch_Parse
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
;   A wrapper for the CDF_PARSE_* procedures offered by IDL. This program converts a time
;   strings in any format to CDF_EPOCH, CDF_EPOCH16, or CDF_TIME_TT000 values.
;
;   Typically, the CDF epoch types require a specific format::
;       CDF_EPOCH       -   "DD-Mon-YYYY hh:mm:ss.ccc"
;       CDF_EPOCH16     -   "DD-Mon-YYYY hh:mm:ss.ccc.uuu.nnn.ppp"
;       CDF_TIME_TT2000 -   "YYYY-MM-DDTHH:MM:SS.mmmuuunnnppp"
;   where Y=year, M=month, D=day, H=hour, M=minute, S=second, m=millisecond, u=microsecond,
;   n=nanosecond, and p=picosecond.
;
;   However, with the PATTERN keyword, `EPOCH_STRINGS` can be given in nearly any foramt.
;   This is acheived through the MrTimeParser procedure. See it for details on how
;   `PATTERN` can be used to convert from one type to another.
;
; :Params:
;       EPOCH_STRING:       in, required, type=string/strarr
;                           A properly formatted epoch string. Choices are::
;                               CDF_EPOCH:          'DD-Mon-YYYY hh:mm:ss.ccc'
;                               CDF_EPOCH16:        'DD-Mon-YYYY hh:mm:ss.ccc.uuu.nnn.ppp'
;                               CDF_TIME_TT2000:    'yyyy-mm-ddThh:mm:ss.cccuuunnn'
;                           Note that all decimal places are required.
;
; :Keywords:
;       PATTERN:        in, optional, type=string/integer
;                       If `EPOCH_STRING` is not in one of the standard CDF formats, use
;                           this keyword to specify its format. PATTERN can be any of 
;                           the pre-defined patterns of MrTimeParser, or a pattern made
;                           up of recognized date and time tokens. `EPOCH_STRING` will
;                           then be convered to the proper format.
;       TO_CDF_EPOCH:   in, optional, type=boolean
;                       If set, `EPOCH_STRING` will be parsed into "CDF_EPOCH" values.
;       TO_EPOCH16:     in, optional, type=boolean
;                       If set, `EPOCH_STRING` will be parsed into "CDF_EPOCH16" values.
;       TO_TT2000:      in, optional, type=boolean
;                       If set, `EPOCH_STRING` will be parsed into "CDF_TIME_TT2000" values.
;       EPOCH_TYPE:     in, optional, type=string
;                       An alternate way of specifying the `TO_CDF_EPOCH`, `TO_EPOCH16`
;                           and `TO_TT2000` keywords. Possible values include::
;                               "CDF_EPOCH"
;                               "CDF_EPOCH16"
;                               "CDF_TIME_TT2000"
;
; :Returns:
;       EPOCH_TIME:     out, type=dblarr/dcomplex/lon64arr
;                       The EPOCH, EPOCH16, or TT2000 times of the converted DATE-TIME.
;                           Output has same number of elements as DATE and is of the
;                           type: EPOCH=dblarr, EPOCH16=dcomplex_arr, TT2000=lon64arr
;
; :Uses:
;   Uses the following external programs::
;       MrTimeParser.pro
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
;       2014/03/08  -   Written by Matthew Argall
;       2014/03/16  -   Renamed the ISO keyword to PATTERN and generalized the format
;                           of EPOCH_STRING by using MrTimeParser. - MRA
;-
function MrCDF_Epoch_Parse, epoch_string, $
PATTERN      = pattern, $
TO_CDF_EPOCH = to_epoch, $
TO_EPOCH16   = to_epoch16, $
TO_TT2000    = to_tt2000, $
EPOCH_TYPE   = epoch_type
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    to_epoch   = keyword_set(to_epoch)
    to_epoch16 = keyword_set(to_epoch16)
    to_tt2000  = keyword_set(to_tt2000)
    
    ;EPOCH_TYPE to specify conversion?
    if n_elements(epoch_type) gt 0 then begin
        case epoch_type of
            'CDF_EPOCH':       to_epoch   = 1
            'CDF_EPOCH16':     to_epoch16 = 1
            'CDF_TIME_TT2000': to_tt2000  = 1
            else: message, 'Epoch type "' + epoch_type + '" not recognized.'
        endcase
    endif
    
    ;Guess which epoch time is being created?
    if (to_epoch + to_epoch16 + to_tt2000 eq 0) && n_elements(pattern) eq 0 then begin
        to_epoch = stregex(epoch_string[0], '[0-9]{2}-[A-Z][a-z]{2}-[0-9]{4} ' + $
                                            '[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$', $
                                            /BOOLEAN)
        
        to_epoch16 = stregex(epoch_string[0], '[0-9]{2}-[A-Z][a-z]{2}-[0-9]{4} ' + $
                                              '[0-9]{2}:[0-9]{2}:[0-9]{2}\.' + $
                                              '[0-9]{3}\.[0-9]{3}\.[0-9]{3}\.[0-9]{3}', $
                                              /BOOLEAN)
        
        to_tt2000 = stregex(epoch_string[0], '[0-9]{4}-[0-9]{2}-[0-9]{2}T' + $
                                             '[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{9}', $
                                             /BOOLEAN)
    endif
    
    ;Only one can bese
    if to_epoch + to_epoch16 + to_tt2000 ne 1 then $
        message, 'Exactly one of TO_CDF_EPOCH, TO_EPOCH16, and TO_TT2000 can be set.'

;-----------------------------------------------------------------------------------------
; Time Strings ///////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    ;Was a pattern given?
    If n_elements(pattern) eq 1 then begin
        ;Figure out what we are converting to
        case 1 of
            to_epoch:   patternOut = 2
            to_epoch16: patternOut = 3
            to_tt2000:  patternOut = 4
        endcase

        ;Convert the time
        MrTimeParser, epoch_string, pattern, patternOut, epoch_out
    endif else begin
        epoch_out = epoch_string
    endelse

;-----------------------------------------------------------------------------------------
;Parse ///////////////////////////////////////////////////////////////////////////////////
;-----------------------------------------------------------------------------------------
    case 1 of
        to_epoch:   epoch_time = cdf_parse_epoch(temporary(epoch_out))
        to_epoch16: epoch_time = cdf_parse_epoch16(temporary(epoch_out))
        to_tt2000:  epoch_time = cdf_parse_tt2000(temporary(epoch_out))
    endcase
    
    ;Return the epoch time
    return, epoch_time
end



;-----------------------------------------------------------------------------------------
;%MAIN EXAMPLE DEMONSTRATION PROGRAM (.r datetime_to_epoch) //////////////////////////////
;-----------------------------------------------------------------------------------------
;Example with CDF_TIME_TT2000
test_string  = '2005-12-04T20:19:18.176214648' 
parsed_value = MrCDF_Epoch_Parse(test_string) 

;Print results
print, '---------------------------------------------------'
print, 'CDF_TIME_TT2000 Example'
print, FORMAT='(%"Test TT2000 String:  %s")', test_string
print, FORMAT='(%"String -> Epoch16:   %i")', parsed_value 
print, FORMAT='(%"Epoch16  -> String:  %s")', CDF_ENCODE_TT2000(parsed_value)
print, ''

;Example with CDF_EPOCH16
test_string = '04-Dec-2005 20:19:18.176.214.648.000' 
parsed_value = MrCDF_Epoch_Parse(test_string)

print, '---------------------------------------------------'
print, 'CDF_EPOCH16 Example'
print, FORMAT='(%"Test Epoch16 String:  %s")',    test_string
print, FORMAT='(%"String  -> Epoch:     %i.%i")', real_part(parsed_value), imaginary(parsed_value)
print, FORMAT='(%"Epoch16 -> String:    %s")',    CDF_ENCODE_EPOCH16(parsed_value)
print, ''

;Example with CDF_EPOCH
test_string  = '04-Dec-2005 20:19:18.176' 
parsed_value = MrCDF_Epoch_Parse(test_string)

print, '---------------------------------------------------'
print, 'CDF_EPOCH Example'
print, FORMAT='(%"Test Epoch String:  %s")', test_string
print, FORMAT='(%"String -> Epoch:    %i")', parsed_value 
print, FORMAT='(%"Epoch  -> String:   %s")', CDF_ENCODE_EPOCH(parsed_value)
print, ''


;Example with ISO-8601 String
test_string    = '2005-12-04T20:19:18.176214648Z' 
parsed_epoch   = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S.%1%2%3Z', /TO_CDF_EPOCH)
parsed_epoch16 = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S.%1%2%3Z', /TO_EPOCH16) 
parsed_tt2000  = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S.%1%2%3Z', /TO_TT2000) 

print, '---------------------------------------------------'
print, 'ISO-8601 Example'
print, FORMAT='(%"Test ISO-8601 String:  %s")',    test_string
print, FORMAT='(%"String  -> Epoch:      %i")',    parsed_epoch
print, FORMAT='(%"String  -> Epoch16:    %i.%i")', real_part(parsed_epoch16), imaginary(parsed_epoch16)
print, FORMAT='(%"String  -> TT2000:     %i")',    parsed_tt2000
print, FORMAT='(%"Epoch   -> String:     %s")',    CDF_ENCODE_TT2000(parsed_epoch)
print, FORMAT='(%"Epoch16 -> String:     %s")',    CDF_ENCODE_TT2000(parsed_epoch16)
print, FORMAT='(%"TT2000  -> String:     %s")',    CDF_ENCODE_TT2000(parsed_tt2000)
print, ''

;Array of strings
test_string    = ['2001-12-03T10:55:00', '2001-12-03T11:00:00']
parsed_epoch   = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S', /TO_CDF_EPOCH)
parsed_epoch16 = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S', /TO_EPOCH16)
parsed_tt2000  = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S', /TO_TT2000)

print, '---------------------------------------------------'
print, 'ISO-8601 Example'
print, FORMAT='(%"Test ISO-8601 String:  [\"%s\", \"%s\"]")', test_string
print, FORMAT='(%"String  -> Epoch:      [%i, %i]")',         parsed_epoch
print, FORMAT='(%"String  -> Epoch16:    [%i.%i, %i.%i]")',   [real_part(parsed_epoch16[0]), imaginary(parsed_epoch16[0]), $
                                                               real_part(parsed_epoch16[1]), imaginary(parsed_epoch16[1])]
print, FORMAT='(%"String  -> TT2000:     [%i, %i]")',         parsed_tt2000
print, FORMAT='(%"Epoch   -> String:     [\"%s\", \"%s\"]")', CDF_ENCODE_TT2000(parsed_epoch)
print, FORMAT='(%"Epoch16 -> String:     [\"%s\", \"%s\"]")', CDF_ENCODE_TT2000(parsed_epoch16)
print, FORMAT='(%"TT2000  -> String:     [\"%s\", \"%s\"]")', CDF_ENCODE_TT2000(parsed_tt2000)
print, ''
end
