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
;   T_SSE:          in, required, type=double/dcomplex/long64
;                   Time, in seconds since `T0`.
;   T0:             in, required, type=double/dcomplex/long64
;                   The reference epoch time.
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
function MrCDF_sse2epoch, t_sse, t0
	compile_opt strictarr
	on_error, 2

	;Determine the epoch type
	epoch_type = MrCDF_Epoch_Type(t0)

	;Convert to epoch.
	case epoch_type of
		'CDF_EPOCH':       t_epoch = (t_sse * 1e3) + t0
		'CDF_EPOCH16':     t_epoch = dcomplex( fix(t_sse, TYPE=14)    + real_part(t0), $
		                                       (t_sse mod 1.0) * 1d12 + imaginary(t0) )
		'CDF_TIME_TT2000': t_epoch = fix(t_sse * 1d9, TYPE=14) + t0
		else: message, 'Unknown Epoch Type: "' + epoch_type + '".'
	endcase

	return, t_epoch
end


;-------------------------------------------------------
; Main Level Program (.r MrCDF_sse2epoch) //////////////
;-------------------------------------------------------

;CDF_EPOCH
;  - Refrence Epoch: 27-Apr-2015 00:00:00.000
;  - Epoch time:     27-Apr-2015 08:51:42.068
ref_epoch    = '27-Apr-2015 00:00:00.000'
target_epoch = '27-Apr-2015 08:51:42.068'
t0           = cdf_parse_epoch(ref_epoch)
t_sse        = 8 * 3600.0D + 51 * 60.0D + 42.068
t_epoch      = MrCDF_sse2epoch(t_sse, t0)
t_str        = cdf_encode_epoch(t_epoch)

;Print results
print, FORMAT='(%"%s")', 'CDF_EPOCH'
print, FORMAT='(%"  Ref Epoch:  %s")', ref_epoch
print, FORMAT='(%"  Target:     %s")', target_epoch
print, FORMAT='(%"  T_SSE:      %9.3f")', t_sse
print, FORMAT='(%"  T_Epoch:    %f")', t_epoch
print, FORMAT='(%"  Date:       %s")', t_str
print, '-------------------------------------------'
print, ''

;CDF_EPOCH16
;  - Refrence Epoch: 27-Apr-2015 00:00:00.000.000.000.000
;  - Epoch time:     27-Apr-2015 08:51:42.068.000.000.000
ref_epoch    = '27-Apr-2015 00:00:00.000.000.000.000'
target_epoch = '27-Apr-2015 08:51:42.068.000.000.000'
t0      = cdf_parse_epoch16(ref_epoch)
t_sse   = 8 * 3600.0D + 51 * 60.0D + 42.068
t_epoch = MrCDF_sse2epoch(t_sse, t0)
t_str   = cdf_encode_epoch16(t_epoch)

;Print results
print, FORMAT='(%"%s")', 'CDF_EPOCH16'
print, FORMAT='(%"  Ref Epoch:  %s")', ref_epoch
print, FORMAT='(%"  Target:     %s")', target_epoch
print, FORMAT='(%"  T_SSE:      %9.3f")', t_sse
print, FORMAT='(%"  T_Epoch:    %f")', t_epoch
print, FORMAT='(%"  Date:       %s")', t_str
print, '-------------------------------------------'
print, ''


;CDF_TIME_TT2000
;  - Refrence Epoch: 2015-04-27T00:00:00.000000000
;  - Epoch time:     2015-04-27T08:51:42.068000000
ref_epoch    = '2015-04-27T00:00:00.000000000'
target_epoch = '2015-04-27T08:51:42.068000000'
t0      = cdf_parse_tt2000(ref_epoch)
t_sse   = 8 * 3600.0D + 51 * 60.0D + 42.068
t_epoch = MrCDF_sse2epoch(t_sse, t0)
t_str   = cdf_encode_tt2000(t_epoch)

;Print results
print, FORMAT='(%"%s")', 'CDF_TIME_TT2000'
print, FORMAT='(%"  Ref Epoch:  %s")', ref_epoch
print, FORMAT='(%"  Target:     %s")', target_epoch
print, FORMAT='(%"  T_SSE:      %9.3f")', t_sse
print, FORMAT='(%"  T_Epoch:    %i")', t_epoch
print, FORMAT='(%"  Date:       %s")', t_str
print, '-------------------------------------------'
print, ''

end
