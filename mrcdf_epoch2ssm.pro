; docformat = 'rst'
;
; NAME:
;       MrCDF_epoch2ssm
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
;   Convert CDF epoch time to seconds since midnight on a given epoch time. 
;
; :Categories:
;   CDF, Time Utility
;
; :Params:
;   T_EPOCH:        in, required, type=double/dcomplex/long64
;                   CDF time(s), either EPOCH, EPOCH16, or CDF_TIME_TT2000.
;   T_REF:          in, required, type=double/dcomplex/long64, default=T_EPOCH[0]
;                   The reference epoch time.
;
; :Keywods:
;   EPOCH_TYPE:     in, optional, type=string, default=MrCDF_Epoch_Type(`T_EPOCH`)
;                   CDF epoch type of `T_EPOCH`. Options are::
;                     'CDF_EPOCH'
;                     'CDF_EPOCH16'
;                     'CDF_TIME_TT2000'
;
; :Returns:
;   T_SSM:          Number of seconds elapsed since midnight on `T_REF`.
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
;       2015-08-26  -   Written by Matthew Argall
;-
function MrCDF_epoch2ssm, t_epoch, t_ref, $
EPOCH_TYPE=epoch_type
	compile_opt strictarr
	on_error, 2

	;Determine reference epoch
	if n_elements(t_ref)      eq 0 then t_ref      = t_epoch[0]
	if n_elements(epoch_type) eq 0 then epoch_type = MrCDF_Epoch_Type(t_epoch)
	
	;Make sure t0 and T_EPOCH are the same CDF type
	if MrCDF_Epoch_Type(t_ref) ne epoch_type then $
		message, 'T_REF must be the same epoch type as T_EPOCH'

	;Find midnight on T_REF
	MrCDF_Epoch_Breakdown, t_ref, yr, mo, day
	MrCDF_Epoch_Compute,   t0,    yr, mo, day

	;Convert to seconds.
	case epoch_type of
		'CDF_EPOCH':       t_ssm = ( t_epoch - t0 ) * 1d-3
		'CDF_EPOCH16':     t_ssm = ( real_part(t_epoch) - real_part(t0) ) + $
		                           ( imaginary(t_epoch) - imaginary(t0) ) * 1d-12
		'CDF_TIME_TT2000': t_ssm = double(t_epoch - t0) * 1d-9
		else: message, 'Unknown Epoch Type: "' + epoch_type + '".'
	endcase

	return, t_ssm
end
