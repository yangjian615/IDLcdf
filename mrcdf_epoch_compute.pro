; docformat = 'rst'
;
; NAME:
;       MRCDF_EPOCH_COMPUTE
;
; PURPOSE:
;+
;    Compute a CDF_EPOCH, CDF_EPOCH16, or CDF_TIME_TT2000 times. This is a
;    wrapper for
;        `cdf_epoch',   /COMPUTE_EPOCH
;        `cdf_epoch16', /COMPUTE_EPOCH
;        `cdf_tt2000',  /COMPUTE_EPOCH
;
; :Examples:
;   Compute a single CDF time in CDF_TIME_TT2000 format::
;       IDL> MrCDF_Epoch_Compute, epoch, 2015, 03, 12
;       IDL> print, epoch
;           479390467184000000
;
; :Categories:
;       Time Utility, CDF Utility, Time Conversion
;
; :Uses:
;   Uses the following external programs::
;       MrCDF_Epoch_Type.pro
;       MrCDFCmpVersion.pro
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
;       2015-08-22  -   Written by Matthew Argall
;-
;*****************************************************************************************
;+
;   The purpose of this program is to compute CDF Epoch times.
;
; :Params:
;       T_EPOCH:            in, out, required, type=double/dblarr
;                           An array of cdf_epoch times.
;       YEAR:               in, out, required, type=intarr
;                           Year of each epoch time.
;       MONTH:              in, out, optional, type=intarr
;                           Mont of each epoch time.
;       DAY:                in, out, optional, type=intarr
;                           Day of each epoch time.
;       HOUR:               in, out, optional, type=intarr
;                           Hour of each epoch time.
;       MINUTE:             in, out, optional, type=intarr
;                           Minute of each epoch time.
;       SECOND:             in, out, optional, type=intarr
;                           Second of each epoch time.
;       MILLI:              in, out, optional, type=intarr
;                           Milli-second of each epoch time: 000-999.
;       MICRO:              in, out, optional, type=intarr
;                           Micro-second of each epoch time: 000-999. Ignored.
;       NANO:               in, out, optional, type=intarr
;                           Nano-second of each epoch time: 000-999. Ignored.
;       PICO:               in, out, optional, type=intarr
;                           Pico-second of each epoch time: 000-999. Ignored.
;-
pro MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
	compile_opt strictarr
	on_error, 2

	;Number if iterations
	nTimes = n_elements(year)

	;Allocate memory
	t_epoch = dblarr(nTimes)
	
	;Number if inputs given
	nParts = n_elements(year)       eq 0 ? 0 : $
	             n_elements(month)  eq 0 ? 1 : $
	             n_elements(day)    eq 0 ? 2 : $
	             n_elements(hour)   eq 0 ? 3 : $
	             n_elements(minute) eq 0 ? 4 : $
	             n_elements(second) eq 0 ? 5 : $
	             n_elements(milli)  eq 0 ? 6 : $
	             n_elements(micro)  eq 0 ? 7 : $
	             n_elements(nano)   eq 0 ? 8 : $
	             n_elements(pico)   eq 0 ? 9 : $
	             10

	;Compute each time
	for i = 0L, nTimes - 1 do begin
		case nParts of
			10: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], minute[i], second[i], milli[i], /COMPUTE_EPOCH
			 9: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], minute[i], second[i], milli[i], /COMPUTE_EPOCH
			 8: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], minute[i], second[i], milli[i], /COMPUTE_EPOCH
			 7: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], minute[i], second[i], milli[i], /COMPUTE_EPOCH
			 6: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], minute[i], second[i], /COMPUTE_EPOCH
			 5: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], minute[i], /COMPUTE_EPOCH
			 4: cdf_epoch, tTmp, year[i], month[i], day[i], hour[i], /COMPUTE_EPOCH
			 3: cdf_epoch, tTmp, year[i], month[i], day[i], /COMPUTE_EPOCH
			 2: cdf_epoch, tTmp, year[i], month[i], /COMPUTE_EPOCH
			 1: cdf_epoch, tTmp, year[i], /COMPUTE_EPOCH
			else: ;Do nothing
		endcase
		
		;Store the epoch time.
		t_epoch[i] = tTmp
	endfor

	;Return a scalar
	if nTimes eq 1 then t_epoch = t_epoch[0]

end


;+
;   The purpose of this program is to compute CDF Epoch times.
;
; :Params:
;       T_EPOCH:            in, out, required, type=dcomplex/dcomplexarr
;                           An array of cdf_epoch16 times.
;       YEAR:               in, out, required, type=intarr
;                           Year of each epoch time.
;       MONTH:              in, out, optional, type=intarr
;                           Mont of each epoch time.
;       DAY:                in, out, optional, type=intarr
;                           Day of each epoch time.
;       HOUR:               in, out, optional, type=intarr
;                           Hour of each epoch time.
;       MINUTE:             in, out, optional, type=intarr
;                           Minute of each epoch time.
;       SECOND:             in, out, optional, type=intarr
;                           Second of each epoch time.
;       MILLI:              in, out, optional, type=intarr
;                           Milli-second of each epoch time: 000-999.
;       MICRO:              in, out, optional, type=intarr
;                           Micro-second of each epoch time: 000-999.
;       NANO:               in, out, optional, type=intarr
;                           Nano-second of each epoch time: 000-999.
;       PICO:               in, out, optional, type=intarr
;                           Pico-second of each epoch time: 000-999.
;-
pro MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
	compile_opt strictarr
	on_error, 2

	;Number if iterations
	nTimes = n_elements(year)
	
	;Allocate memory
	t_epoch = dcomplexarr(nTimes)
	
	;Compute here once so we do not have to compute it many times in the loop.
	nParts = n_elements(year)       eq 0 ? 0 : $
	             n_elements(month)  eq 0 ? 1 : $
	             n_elements(day)    eq 0 ? 2 : $
	             n_elements(hour)   eq 0 ? 3 : $
	             n_elements(minute) eq 0 ? 4 : $
	             n_elements(second) eq 0 ? 5 : $
	             n_elements(milli)  eq 0 ? 6 : $
	             n_elements(micro)  eq 0 ? 7 : $
	             n_elements(nano)   eq 0 ? 8 : $
	             n_elements(pico)   eq 0 ? 9 : $
	             10
	
	;Compute each time
	for i = 0L, nTimes - 1 do begin
		case nParts of
			10: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], minute[i], $
			                 second[i], milli[i], micro[i], nano[i], pico[i], /COMPUTE_EPOCH
			 9: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], minute[i], $
			                 second[i], milli[i], micro[i], nano[i], /COMPUTE_EPOCH
			 8: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], minute[i], $
			                 second[i], milli[i], micro[i], /COMPUTE_EPOCH
			 7: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], minute[i], $
			                 second[i], milli[i], /COMPUTE_EPOCH
			 6: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], minute[i], $
			                 second[i], /COMPUTE_EPOCH
			 5: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], minute[i], /COMPUTE_EPOCH
			 4: cdf_epoch16, tTmp, year[i], month[i], day[i], hour[i], /COMPUTE_EPOCH
			 3: cdf_epoch16, tTmp, year[i], month[i], day[i], /COMPUTE_EPOCH
			 2: cdf_epoch16, tTmp, year[i], month[i], /COMPUTE_EPOCH
			 1: cdf_epoch16, tTmp, year[i], /COMPUTE_EPOCH
			else: ;Do nothing
		endcase

		;Store the epoch time.
		t_epoch[i] = tTmp
	endfor

	;Must return a scalar if a scalar was given
	if nTimes eq 1 then t_epoch = t_epoch[0]
end


;+
;   The purpose of this program is to compute or break-down CDF Epoch times.
;
; :Params:
;       YEAR:               in, out, required, type=intarr
;                           Year of each epoch time.
;       MONTH:              in, out, required, type=intarr
;                           Mont of each epoch time.
;       DAY:                in, out, optional, type=intarr
;                           Day of each epoch time.
;       HOUR:               in, out, optional, type=intarr
;                           Hour of each epoch time.
;       MINUTE:             in, out, optional, type=intarr
;                           Minute of each epoch time.
;       SECOND:             in, out, optional, type=intarr
;                           Second of each epoch time.
;       MILLI:              in, out, optional, type=intarr
;                           Milli-second of each epoch time: 000-999.
;       MICRO:              in, out, optional, type=intarr
;                           Micro-second of each epoch time: 000-999. Ignored for
;                               CDF_EPOCH times.
;       NANO:               in, out, optional, type=intarr
;                           Nano-second of each epoch time: 000-999. Ignored for
;                               CDF_EPOCH times.
;       PICO:               in, out, optional, type=intarr
;                           Pico-second of each epoch time: 000-999. Ignored for
;                               CDF_EPOCH and CDF_TIME_TT2000 times.
;
; :Keywords:
;       CDF_EPOCH:          in, optional, type=boolean
;                           If `COMPUTE_EPOCH` is performed, indicate that CDF_EPOCH times
;                               are to be generated. This is the default for CDF versions
;                               less than 3.4.
;       EPOCH_TYPE:         in, optional, type=string
;                           An alternate method of specifying `CDFEPOCH`, `EPOCH16` or
;                               `TT2000`. Choices are::
;                                   "CDF_EPOCH"
;                                   "CDF_EPOCH16"
;                                   "CDF_TIME_TT2000"
;       EPOCH16:            in, optional, type=boolean
;                           If `COMPUTE_EPOCH` is performed, indicate that CDF_EPOCH16
;                               times are to be generated.
;       TT2000:             in, optional, type=boolean
;                           If `COMPUTE_EPOCH` is performed, indicate that CDF_TIME_TT2000
;                               times are to be generated. This is the default for CDF
;                               versions greater than or equal to 3.4.
;
; :Params:
;       T_EPOCH:            out, required, type=dblarr/dcomplexarr/lon64arr
;                           An array of computed epoch times. Accepted epoch types are::
;-
function MrCDF_Epoch_Compute, year, month, day, hour, minute, second, milli, micro, nano, pico, $
CDF_EPOCH=cdfepoch, $
EPOCH16=epoch16, $
TT2000=tt2000, $
EPOCH_TYPE=epoch_type
	compile_opt strictarr
	on_error, 2
	
	;Determine the type of CDF epoch value given
	if n_elements(epoch_type) gt 0 then begin
		eType = strupcase(epoch_type)
	endif else begin
		eType = keyword_set(TT2000)                   ? 'CDF_TIME_TT2000' : $
		            keyword_set(EPOCH16)              ? 'CDF_EPOCH16'     : $
		            keyword_set(CDFEPOCH)             ? 'CDF_EPOCH'       : $
		            (MrCDFCmpVersion('3.4.0.0') le 0) ? 'CDF_TIME_TT2000' : $
		            'CDF_EPOCH'
	endelse

;---------------------------------------------------------------------
;Vectorized Versions /////////////////////////////////////////////////
;---------------------------------------------------------------------
	if MrCDFCmpVersion('3.4.0.0') le 0 then begin

		;Define each input to prevent "BAD Epoch Type" error.
		if n_elements(year)   eq 0 then year   = 0
		if n_elements(month)  eq 0 then month  = 0
		if n_elements(day)    eq 0 then day    = 0
		if n_elements(hour)   eq 0 then hour   = 0
		if n_elements(minute) eq 0 then minute = 0
		if n_elements(second) eq 0 then second = 0
		if n_elements(milli)  eq 0 then milli  = 0
		if n_elements(micro)  eq 0 then micro  = 0
		if n_elements(nano)   eq 0 then nano   = 0
		if n_elements(pico)   eq 0 then pico   = 0

		;Compute/Breakdown the epoch value
		case eType of
			'CDF_EPOCH':       cdf_epoch,   t_epoch, year, month, day, hour, minute, second, milli, /COMPUTE_EPOCH
			'CDF_EPOCH16':     cdf_epoch16, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico, /COMPUTE_EPOCH
			'CDF_TIME_TT2000': cdf_tt2000,  t_epoch, year, month, day, hour, minute, second, milli, micro, nano, /COMPUTE_EPOCH
		endcase
	
		return, t_epoch
	endif


;=====================================================================
;Use a loop for versions < 3.4 ///////////////////////////////////////
;=====================================================================
	message, 'Vectorized CDF_EPOCH, CDF_EPOCH16 and CDF_TT2000 procedures were ', /INFORMATIONAL
	print, '               introduced in CDF v3.4. It is suggested that you upgrade to that or '
	print, '               a higher version. See the official patch on the NASA webpage '
	print, '               http://cdf.gsfc.nasa.gov/html/cdf_patch_for_idl.html'

;---------------------------------------------------------------------
;Compute Epoch ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Use CASE statement to prevent returning unwanted pieces with /COMPUTE_EPOCH
	
	;CDF_EPOCH
	if eType eq 'CDF_EPOCH' then begin
		case n_params() of
			11: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
			10: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano
			 9: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro
			 8: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli
			 7: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute, second
			 6: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour, minute
			 5: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day, hour
			 4: MrCDF_Epoch_Compute_Loop, t_epoch, year, month, day
			 3: MrCDF_Epoch_Compute_Loop, t_epoch, year, month
			 2: MrCDF_Epoch_Compute_Loop, t_epoch, year
			else: message, 'Incorrect number of parameters.'
		endcase
	
	;CDF_EPOCH16
	endif else if eType eq 'CDF_EPOCH16' then begin
		case n_params() of
			11: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
			10: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano
			 9: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro
			 8: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute, second, milli
			 7: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute, second
			 6: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour, minute
			 5: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day, hour
			 4: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month, day
			 3: MrCDF_Epoch16_Compute_Loop, t_epoch, year, month
			 2: MrCDF_Epoch16_Compute_Loop, t_epoch, year
			else: message, 'Incorrect number of parameters.'
		endcase
	
	;CDF_TIME_TT2000
	endif else if eType eq 'CDF_TIME_TT2000' then begin
		message, 'CDF_TIME_TT2000 times were not introduced until CDf v3.4.', /INFORMATIONAL
	endif else begin
		message, 'Unknown CDF Epoch type: "' + eType + '".'
	endelse
	
	return, t_epoch
end