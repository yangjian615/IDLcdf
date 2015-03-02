; docformat = 'rst'
;
; NAME:
;       MRCDF_EPOCH_ENCODE
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
;       The purpose of this program is to create a wrapper for the cdf_encode_epoch, 
;       cdf_encode_epoch16 and cdf_encode_tt2000 functions.
;
; :Categories:
;       Time Utility, CDF Utility, Time Conversion
;
; :Examples:
;   Try main level program at the end of this document::
;       IDL> .r MrCDF_Epoch_Encode
;
; :Params:
;   T_EPOCH:            in, required, type=double, dcomplex, long64
;                       A[n array of] CDF times to be turned into strings. Accepted types
;                           are::
;                               EPOCH               -- Double
;                               EPOCH16             -- DComplex
;                               CDF_TIME_TT2000     -- Long64
;
; :Keywords:
;   EPOCH:              in. optional, type=int, default=0
;                       The format of the output::
;                           0 - dd-mmm-yyyy hh:mm:ss.ccc                -- CDF_EPOCH
;                             - dd-mmm-yyyy hh:mm:ss.ccc.uuu.nnn.ppp    -- CDF_EPOCH16
;                             - dd-mmm-yyyy hh:mm:ss.ccccccccc          -- CDF_TIME_TT2000
;                           1 - yyyymmdd.ttttttt                        -- CDF_EPOCH
;                             - yyyymmdd.ttttttttttttttt                -- CDF_EPOCH16
;                             - yyyymmdd.tttttttttt                     -- CDF_TIME_TT2000
;                           2 - yyyymmddss                              -- [ALL]
;                           3 - yyyy-mm-ddThh:mm:ss.cccZ                -- CDF_EPOCH
;                             - yyyy-mm-ddThh:mm:ss.ccc.uuu.nnn.pppZ    -- CDF_EPOCH16
;                             - yyyy-mm-ddThh:mm:ss.ccccccccc           -- CDF_TIME_TT2000
;   PATTERN:            in, optional, type=string/integer, default=''
;                       An alternative to the `EPOCH` keyword (ignored if `EPOCH`is given).
;                           A string of tokens or the index of a pre-defined pattern
;                           known to MrTimeParser.pro describing how `ENCODED_STRING`
;                           should be encoded.
;
; :Returns:
;   ENCODED_STRING:     A date string representing each of `T_EPOCH`.
;
; :Uses:
;   Uses the following external programs::
;       MrTimeParser.pro
;       MrCDF_Epoch_Type.pro
;       MrCDF_Epoch_Parse.pro (for example)
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;	Modification History::
;       Written by  -   Matthew Argall 12 February 2012
;       2014/02/03  -   Renamed to MrCDF_Epoch_Encode from CDF_Epoch_Encode. - MRA
;-
function MrCDF_Epoch_Encode, t_epoch, $
EPOCH = epoch, $
PATTERN = pattern
    compile_opt strictarr
    on_error, 2
    
    ;Defaults
    doPattern = 0
    if n_elements(epoch) eq 0 then begin
        epoch = 0
        if n_elements(pattern) gt 0 then doPattern = 1
    endif
    if epoch lt 0 || epoch gt 3 then message, 'EPOCH must be {0 | 1 | 2 | 3}.'

    ;Determine the type of CDF epoch value given
    epoch_type = MrCDF_Epoch_Type(t_epoch)

    ;Encode epoch values
    case epoch_type of
        'CDF_EPOCH':       epoch_string = cdf_encode_epoch(t_epoch, EPOCH=epoch)
        'CDF_EPOCH16':     epoch_string = cdf_encode_epoch16(t_epoch, EPOCH=epoch)
        'CDF_TIME_TT2000': epoch_string = cdf_encode_tt2000(t_epoch, EPOCH=epoch)
    endcase
    
    ;Re-encode the output?
    if doPattern then begin
        case epoch_type of
            'CDF_EPOCH':       patternIn = '%d-%c-%Y %H:%m:%S.%1'
            'CDF_EPOCH16':     patternIn = '%d-%c-%Y %H:%m:%S.%1.%2.%3.%4'
            'CDF_TIME_TT2000': patternIn = '%d-%c-%Y %H:%m:%S.%1%2%3'
        endcase

        ;Re-encode
        MrTimeParser, epoch_string, patternIn, pattern, epoch_string
    endif
        
    return, epoch_string
end


;-----------------------------------------------------------------------------------------
;%MAIN EXAMPLE DEMONSTRATION PROGRAM (.r datetime_to_epoch) //////////////////////////////
;-----------------------------------------------------------------------------------------
;Example with CDF_TIME_TT2000
;   Convert a properly formatted CDF_TIME_TT2000 string to a CDF_TIME_TT2000 time,
;   then convert it back.
test_string  = '2005-12-04T20:19:18.176214648' 
parsed_value = MrCDF_Epoch_Parse(test_string) 

print, '---------------------------------------------------'
print, 'CDF_TIME_TT2000 Example'
print, FORMAT='(%"Test TT2000 String:  %s")', test_string
print, FORMAT='(%"String -> Epoch16:   %i")', parsed_value 
print, FORMAT='(%"Epoch16  -> String:  %s")', MrCDF_Epoch_Encode(parsed_value)
print, ''

;Example with CDF_EPOCH16
;   Convert a properly formatted CDF_EPOCH16 string to a CDF_EPOCH16 time,
;   then convert it back.
test_string = '04-Dec-2005 20:19:18.176.214.648.000' 
parsed_value = MrCDF_Epoch_Parse(test_string)

print, '---------------------------------------------------'
print, 'CDF_EPOCH16 Example'
print, FORMAT='(%"Test Epoch16 String:  %s")',    test_string
print, FORMAT='(%"String  -> Epoch:     %i.%i")', real_part(parsed_value), imaginary(parsed_value)
print, FORMAT='(%"Epoch16 -> String:    %s")',    MrCDF_Epoch_Encode(parsed_value)
print, ''

;Example with CDF_EPOCH
;   Convert a properly formatted CDF_EPOCH string to a CDF_EPOCH time,
;   then convert it back.
test_string  = '04-Dec-2005 20:19:18.176' 
parsed_value = MrCDF_Epoch_Parse(test_string)

print, '---------------------------------------------------'
print, 'CDF_EPOCH Example'
print, FORMAT='(%"Test Epoch String:  %s")', test_string
print, FORMAT='(%"String -> Epoch:    %i")', parsed_value 
print, FORMAT='(%"Epoch  -> String:   %s")', MrCDF_Epoch_Encode(parsed_value)
print, ''


;Example using PATTERN
;   Convert a string with a custom format into a CDF_TIME_TT2000 time, then convert
;   it to all of the patterns known by MrTimeParser.pro.
test_string    = '2005-12-04T20:19:18.176214648Z' 
test_tt2000  = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S.%1%2%3Z', /TO_TT2000)

;Convert to the 26 different patterns recognized by MrTimeParser
encoded_string = strarr(32)
for i = 0, 31 do encoded_string[i] = MrCDF_Epoch_Encode(test_tt2000, PATTERN=i+1)

print, '---------------------------------------------------'
print, 'Test ISO-8601 String:'
print, '    ' + test_string
print, 'Encoded Results'
print, '    ' + transpose(encoded_string)
print, ''


;Array of strings
;   Convert a string with a custom format into CDF_EPOCH16 times, then convert
;   it to a custom pattern recognized by MrTimeParser.pro.
test_string    = ['2001-12-03T10:55:00', '2001-12-03T11:00:00']
parsed_epoch16 = MrCDF_Epoch_Parse(test_string, PATTERN='%Y-%M-%dT%H:%m:%S', /TO_EPOCH16)
patternOut = '%M/%d/%Y %h:%m:%S %A'

print, '---------------------------------------------------'
print, 'ISO-8601 Example'
print, FORMAT='(%"Test ISO-8601 String:  [\"%s\", \"%s\"]")', test_string
print, FORMAT='(%"String  -> Epoch16:    [%i.%i, %i.%i]")',   [real_part(parsed_epoch16[0]), imaginary(parsed_epoch16[0]), $
                                                               real_part(parsed_epoch16[1]), imaginary(parsed_epoch16[1])]
print, FORMAT='(%"Epoch16 -> String:     [\"%s\", \"%s\"]")', MrCDF_Epoch_Encode(parsed_epoch16, PATTERN=patternOut)
print, ''
end
