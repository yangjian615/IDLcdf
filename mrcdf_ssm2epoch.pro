; docformat = 'rst'
;
; NAME:
;       MrCDF_sse2epoch
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
;   Convert CDF epoch time to seconds since a specific epoch time.
;
; :Categories:
;   CDF, Time Utility
;
; :Examples:
;   See the example program at the end of this document
;     IDL> .r MrCDF_sse2epoch
;
; :Params:
;   T_SSM:          in, required, type=double/dcomplex/long64
;                   Time, in seconds since `T0`.
;   T0:             in, required, type=double/dcomplex/long64
;                   The reference epoch time. `T_SSM` are with respect
;                       to midnight on the date of this epoch value. It
;                       is not necessary for T0 to be a midnight value,
;                       any value from the desired date will do.
;
; :Keywods:
;   EPOCH_TYPE:     in, optional, type=string, default=MrCDF_Epoch_Type(`T0`)
;                   CDF epoch type of the desired output. Options are::
;                     'CDF_EPOCH'
;                     'CDF_EPOCH16'
;                     'CDF_TIME_TT2000'
;
; :Returns:
;   T_EPOCH:        CDF epoch, epoch16, or tt2000 times
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
function MrCDF_ssm2epoch, t_ssm, t_ref, $
EPOCH_TYPE=epoch_type
	compile_opt strictarr
	on_error, 2

	;Determine the epoch type
	if n_elements(epoch_type) eq 0 then epoch_type = MrCDF_Epoch_Type(t_ref)

	;Compute midight
	MrCDF_Epoch_Breakdown, t_ref, yr, mo, day
	MrCDF_Epoch_Compute,   t0,    yr, mo, day

	;Convert to epoch.
	case epoch_type of
		'CDF_EPOCH':       t_epoch = (t_ssm * 1e3) + t0
		'CDF_EPOCH16':     t_epoch = dcomplex( fix(t_ssm, TYPE=14)    + real_part(t0), $
		                                       (t_ssm mod 1.0) * 1d12 + imaginary(t0) )
		'CDF_TIME_TT2000': t_epoch = fix(t_ssm * 1d9, TYPE=14) + t0
		else: message, 'Unknown Epoch Type: "' + epoch_type + '".'
	endcase

	return, t_epoch
end
