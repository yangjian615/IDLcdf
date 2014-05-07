; docformat = 'rst'
;
; NAME:
;       MrCDF_Epoch_Type
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
;       The purpose of this program is to determine the CDF epoch type of a given input.
;       Known `CDF epoch types http://cdf.gsfc.nasa.gov/html/leapseconds.html` include::
;           EPOCH
;           EPOCH16
;           CDF_TIME_TT2000
;
; :Categories:
;       Time Utility
;
; :Params:
;   T_EPOCH:        in, required, type=double/dcomplex/long64
;                   A[n array of] CDF times, either EPOCH, EPOCH16, or CDF_TIME_TT2000.
;
; :Returns:
;   EPOCH_TYPE:     Will return the type of the given CDF epoch time contained in `T_EPOCH`
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
;       2014/01/19  -   For readability, use type names to check epoch values. - MRA
;       2014/02/03  -   Renamed to MrCDF_Epoch_Type from MrCDF_Epoch. - MRA
;-
function MrCDF_Epoch_Type, t_epoch
    compile_opt strictarr
    on_error, 2

    ;Get the data-type of T_EPOCH
    t_type = size(t_epoch, /TNAME)
    
    ;Pick the epoch type.
    case t_type of
        'DOUBLE': epoch_type = 'CDF_EPOCH'
        'DCOMPLEX': epoch_type = 'CDF_EPOCH16'
        'LONG64': epoch_type = 'CDF_TIME_TT2000'
        else: message, 'Unknown Epoch Type: "' + t_type + '".'
    endcase
    
    return, epoch_type
end