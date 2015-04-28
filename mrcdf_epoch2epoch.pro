; docformat = 'rst'
;
; NAME:
;       MrCDF_epoch2epoch
;
;*****************************************************************************************
;   Copyright (c) 2015, Matthew Argall                                                   ;
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
;   Convert from one CDF epoch type to another.
;
; :Categories:
;   CDF, Time Utility
;
; :Params:
;   T_EPOCH:        in, required, type=double/dcomplex/long64
;                   CDF time(s), either EPOCH, EPOCH16, or CDF_TIME_TT2000.
;   DEST_TYPE:      in, required, type=string
;                   CDF epoch datatype to which `T_EPOCH` is converted. Choices are
;                     EPOCH, EPOCH16, or CDF_TIME_TT2000.
;
; :Returns:
;   T_OUT:          `T_EPOCH` converted to the requested CDF epoch type.
;
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2015-04-29  -   Written by Matthew Argall
;-
function MrCDF_epoch2epoch, t_epoch, dest_type
	compile_opt strictarr
	on_error, 2

	;Determine the epoch type
	;  - If source and destination types are the same, return input.
	src_type = MrCDF_Epoch_Type(t_epoch)
	if strupcase(dest_type) eq src_type then return, t_epoch

	;Breakdown the input.
	case src_type of
		'CDF_EPOCH':       cdf_epoch,   t_epoch, yr, mo, day, hr, mnt, sec, milli, /BREAKDOWN_EPOCH
		'CDF_EPOCH16':     cdf_epoch16, t_epoch, yr, mo, day, hr, mnt, sec, milli, micro, nano, pico, /BREAKDOWN_EPOCH
		'CDF_TIME_TT2000': cdf_tt2000,  t_epoch, yr, mo, day, hr, mnt, sec, milli, micro, nano, /BREAKDOWN_EPOCH
		else: message, 'Unknown input epoch type: "' + src_type + '".'
	endcase
	
	;Make sure micro, nono, and pico are defined
	if n_elements(micro) eq 0 then micro = 0
	if n_elements(nano)  eq 0 then nano  = 0
	if n_elements(pico)  eq 0 then pico  = 0
	
	;Build up the output
	case dest_type of
		'CDF_EPOCH':   cdf_epoch,   t_out, mo, day, hr, mnt, sec, milli, /COMPUTE_EPOCH
		'CDF_EPOCH16': cdf_epoch16, t_out, mo, day, hr, mnt, sec, milli, micro, nono, pico, /COMPUTE_EPOCH
		'CDF_TT2000':  cdf_tt2000,  t_out, mo, day, hr, mnt, sec, milli, micro, nono, /COMPUTE_EPOCH
		else: message, 'Unknown output epoch type: "' + dest_type + '".'
	endcase

	return, t_out
end
