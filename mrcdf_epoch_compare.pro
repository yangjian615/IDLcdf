; docformat = 'rst'
;
; NAME:
;       DATATIME_TO_EPOCH
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
;   Compare CDF epoch times.
;
; :Params:
;       EPOCH:          in, required, type="CDF_EPOCH"\, "CDF_EPOCH16"\, "CDF_TIME_TT2000"
;                       Epoch time to be compared against `BASE_EPOCH`.
;       BASE_EPOCH:     in, required, type="CDF_EPOCH"\, "CDF_EPOCH16"\, "CDF_TIME_TT2000"
;                       Check to see if `EPOCH` is less than, equal to, or greater than
;                           this value.
;       END_EPOCH:      in, optional, type=same as `BASE_EPOCH`
;                       If given, comparison is `BASE_EPOCH` <= `EPOCH` <= `END_EPOCH`.
;                           END_EPOCH must be same type and dimensionality as `EPOCH`.
;
; :Returns:
;       RESULT:         If `END_EPOCH` was given::
;                            1 - base_epoch <= epoch <= end_epoch
;                            0 - otherwise
;                       If `END_EPOCH` is /not/ given::
;                            1 - epoch > base_epoch
;                            0 - epoch = base_epoch
;                           -1 - epoch < base_epoch
;
; :Uses:
;   Uses the following external programs::
;       MrCDFCmpVersion.pro
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
;-
function MrCDF_Epoch_Compare, epoch, base_epoch, end_epoch
    compile_opt strictarr
    on_error, 2
    
    if MrCDFCmpVersion('3.4') le 0 then begin
        if n_elements(end_epoch) eq 0 $
            then result = cdf_epoch_compare(epoch, base_epoch) $
            else result = cdf_epoch_compare(epoch, base_epoch, end_epoch)
    endif else begin
        ;TO DO: add functionality for versions < 3.4
        message, "For better performance, install the NASA's official CDF path for IDL.", /INFORMATIONAL
        message, 'http://cdf.gsfc.nasa.gov/html/cdf_patch_for_idl.html', /INFORMATIONAL
    endelse
    
    ;Return the epoch time
    return, result
end