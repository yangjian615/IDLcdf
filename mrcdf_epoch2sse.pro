; docformat = 'rst'
;
; NAME:
;       MrCDF_epoch2sse
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
;   Convert CDF epoch time to seconds since a specific epoch time.
;
; :Categories:
;   CDF, Time Utility
;
; :Params:
;   T_EPOCH:        in, required, type=double/dcomplex/long64
;                   CDF time(s), either EPOCH, EPOCH16, or CDF_TIME_TT2000.
;   T0:             in, required, type=double/dcomplex/long64, default=T_EPOCH[0]
;                   The reference epoch time.
;
; :Returns:
;   T_SSE:            Number of seconds elapsed since `T0`.
;
; :Examples:
;   Try the main level program at the end of this document.
;     IDL> .run MrCDF_epoch2sse
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
;       Written by  -   Matthew Argall 12 February 2012
;-
function MrCDF_epoch2sse, t_epoch, t0
	compile_opt strictarr
	on_error, 2

	;Determine reference epoch
	if n_elements(t0) eq 0 then t0 = t_epoch[0]

	;Determine the epoch type
	epoch_type = MrCDF_Epoch_Type(t_epoch)
	
	;Make sure t0 and T_EPOCH are the same CDF type
	if MrCDF_Epoch_Type(t0) ne epoch_type then $
		message, 'T0 must be the same epoch type as T_EPOCH'

	;Convert to seconds.
	case epoch_type of
		'CDF_EPOCH':       t_sse = ( t_epoch - t0 ) * 1d-3
		'CDF_EPOCH16':     t_sse = ( real_part(t_epoch) - real_part(t0) ) + $
		                           ( imaginary(t_epoch) - imaginary(t0) ) * 1d-12
		'CDF_TIME_TT2000': t_sse = double(t_epoch - t0) * 1d-9
		else: message, 'Unknown Epoch Type: "' + epoch_type + '".'
	endcase

	return, t_sse
end


;-------------------------------------------------------
; Main Level Program (.r MrCDF_epoch2sse) //////////////
;-------------------------------------------------------

;CDF_EPOCH
;  - Refrence Epoch: 27-Apr-2015 00:00:00.000
;  - Epoch time:     27-Apr-2015 08:51:42.068
ref_epoch    = '27-Apr-2015 00:00:00.000'
target_epoch = '27-Apr-2015 08:51:42.068'
t0           = cdf_parse_epoch(ref_epoch)
t_epoch      = cdf_parse_epoch(target_epoch)
t_sse        = MrCDF_epoch2sse(t_epoch, t0)
t_hr         = fix(t_sse / 3600)
t_min        = fix( (t_sse mod 3600) / 60 )
t_sec        = t_sse mod 60

;Print results
print, FORMAT='(%"%s")', 'CDF_EPOCH'
print, FORMAT='(%"  Ref Epoch:  %s")',               ref_epoch
print, FORMAT='(%"  Target:     %s")',               target_epoch
print, FORMAT='(%"  T_SSE:      %9.3f")',            t_sse
print, FORMAT='(%"  Time:       %02i:%02i:%06.3f")', t_hr, t_min, t_sec
print, '-------------------------------------------'
print, ''


;CDF_EPOCH16
;  - Refrence Epoch: 27-Apr-2015 00:00:00.000.000.000.000
;  - Epoch time:     27-Apr-2015 08:51:42.068.000.000.000
ref_epoch    = '27-Apr-2015 00:00:00.000.000.000.000'
target_epoch = '27-Apr-2015 08:51:42.068.000.000.000'
t0           = cdf_parse_epoch16(ref_epoch)
t_epoch      = cdf_parse_epoch16(target_epoch)
t_sse        = MrCDF_epoch2sse(t_epoch, t0)
t_hr         = fix(t_sse / 3600)
t_min        = fix( (t_sse mod 3600) / 60 )
t_sec        = t_sse mod 60

;Print results
print, FORMAT='(%"%s")', 'CDF_EPOCH16'
print, FORMAT='(%"  Ref Epoch:  %s")',               ref_epoch
print, FORMAT='(%"  Target:     %s")',               target_epoch
print, FORMAT='(%"  T_SSE:      %9.3f")',            t_sse
print, FORMAT='(%"  Time:       %02i:%02i:%06.3f")', t_hr, t_min, t_sec
print, '-------------------------------------------'
print, ''


;CDF_TIME_TT2000
;  - Refrence Epoch: 2015-04-27T00:00:00.000000000
;  - Epoch time:     2015-04-27T08:51:42.068000000
ref_epoch    = '2015-04-27T00:00:00.000000000'
target_epoch = '2015-04-27T08:51:42.068000000'
t0           = cdf_parse_tt2000(ref_epoch)
t_epoch      = cdf_parse_tt2000(target_epoch)
t_sse        = MrCDF_epoch2sse(t_epoch, t0)
t_hr         = fix(t_sse / 3600)
t_min        = fix( (t_sse mod 3600) / 60 )
t_sec        = t_sse mod 60

;Print results
print, FORMAT='(%"%s")', 'CDF_TIME_TT2000'
print, FORMAT='(%"  Ref Epoch:  %s")',               ref_epoch
print, FORMAT='(%"  Target:     %s")',               target_epoch
print, FORMAT='(%"  T_SSE:      %9.3f")',            t_sse
print, FORMAT='(%"  Time:       %02i:%02i:%06.3f")', t_hr, t_min, t_sec
print, '-------------------------------------------'
print, ''

end
