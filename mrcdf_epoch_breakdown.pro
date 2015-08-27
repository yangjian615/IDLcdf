; docformat = 'rst'
;
; NAME:
;       MRCDF_EPOCH_BREAKDOWN
;
; PURPOSE:
;+
;    Breakdown CDF_EPOCH, CDF_EPOCH16, or CDF_TIME_TT2000 times. This is a
;    wrapper for::
;        `cdf_epoch',   /BREAKDOWN_EPOCH
;        `cdf_epoch16', /BREAKDOWN_EPOCH
;        `cdf_tt2000',  /BREAKDOWN_EPOCH
;
; :Examples:
;   See the main-level program at the end of this routine::
;       IDL> .r MrCDF_Epoch_Breakdown
;
;   Breakdown a single CDF time in CDF_TIME_TT2000 format::
;       IDL> MrCDF_Epoch, 479390467184000000LL, yr, mo, day, hr, mn, sec, /TOINTEGER
;       IDL> print, yr, mo, day, hr, mn, sec
;           2015       3      12       0       0       0
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
;   The purpose of this program is to break-down CDF Epoch times.
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
;                           Micro-second of each epoch time: 000-999.
;       NANO:               in, out, optional, type=intarr
;                           Nano-second of each epoch time: 000-999.
;       PICO:               in, out, optional, type=intarr
;                           Pico-second of each epoch time: 000-999.
;-
pro MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
	compile_opt strictarr
	on_error, 2

	;Number if iterations
	nTimes = n_elements(t_epoch)

	;Number of outputs expected
	nPrams = n_params()

	;Allocate memory
	switch nPrams of
		11: pico   = intarr(nTimes)
		10: nano   = intarr(nTimes)
		 9: micro  = intarr(nTimes)
		 8: milli  = intarr(nTimes)
		 7: second = intarr(nTimes)
		 6: minute = intarr(nTimes)
		 5: hour   = intarr(nTimes)
		 4: day    = intarr(nTimes)
		 3: month  = intarr(nTimes)
		 2: year   = intarr(nTiems)
	endswitch

	;Breakdown each time.
	for i = 0L, nTimes - 1 do begin
		case nPrams of
			11: cdf_epoch, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, /BREAKDOWN_EPOCH
			10: cdf_epoch, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, /BREAKDOWN_EPOCH
			 9: cdf_epoch, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, /BREAKDOWN_EPOCH
			 8: cdf_epoch, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, /BREAKDOWN_EPOCH
			 7: cdf_epoch, t_epoch[i], yr, mo, dy, hr, mn, sec, /BREAKDOWN_EPOCH
			 6: cdf_epoch, t_epoch[i], yr, mo, dy, hr, mn, /BREAKDOWN_EPOCH
			 5: cdf_epoch, t_epoch[i], yr, mo, dy, hr, /BREAKDOWN_EPOCH
			 4: cdf_epoch, t_epoch[i], yr, mo, dy, /BREAKDOWN_EPOCH
			 3: cdf_epoch, t_epoch[i], yr, mo, /BREAKDOWN_EPOCH
			 2: cdf_epoch, t_epoch[i], yr, /BREAKDOWN_EPOCH
			else: ;Do nothing
		endcase
	
		;Store the values
		switch nPrams of
			11: ;Do nothing
			10: ;Do nothing
			 9: ;Do nothing
			 8: milli[i]  = mil
			 7: second[i] = sec
			 6: minute[i] = mn
			 5: hour[i]   = hr
			 4: day[i]    = dy
			 3: month[i]  = mo
			 2: year[i]   = yr
		endswitch
	endfor

	;Return scalars
	if nTimes eq 1 then begin
		switch nPrams of
			11: ;Do nothing
			10: ;Do nothing
			 9: ;Do nothing
			 8: milli[i]  = milli[0]
			 7: second[i] = second[0]
			 6: minute[i] = minute[0]
			 5: hour[i]   = hour[0]
			 4: day[i]    = day[0]
			 3: month[i]  = month[0]
			 2: year[i]   = year[0]
		endswitch
	endif
end


;+
;   The purpose of this program is to break-down CDF Epoch16 times.
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
pro MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
	compile_opt strictarr
	on_error, 2

	;Number if iterations
	nTimes = n_elements(t_epoch)

	;Number of outputs expected
	nPrams = n_params()

	;Allocate memory
	switch nPrams of
		11: pico   = intarr(nTimes)
		10: nano   = intarr(nTimes) 
		 9: micro  = intarr(nTimes)
		 8: milli  = intarr(nTimes)
		 7: second = intarr(nTimes)
		 6: minute = intarr(nTimes)
		 5: hour   = intarr(nTimes)
		 4: day    = intarr(nTimes)
		 3: month  = intarr(nTimes)
		 2: year   = intarr(nTiems)
	endswitch
	
	;Breakdown each time.
	for i = 0L, nTimes - 1 do begin
		case nPrams of
			11: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, mic, nan, pic, /BREAKDOWN_EPOCH
			10: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, mic, nan, /BREAKDOWN_EPOCH
			 9: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, mic, /BREAKDOWN_EPOCH
			 8: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, mn, sec, milli, /BREAKDOWN_EPOCH
			 7: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, mn, sec, /BREAKDOWN_EPOCH
			 6: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, mn, /BREAKDOWN_EPOCH
			 5: cdf_epoch16, t_epoch[i], yr, mo, dy, hr, /BREAKDOWN_EPOCH
			 4: cdf_epoch16, t_epoch[i], yr, mo, dy, /BREAKDOWN_EPOCH
			 3: cdf_epoch16, t_epoch[i], yr, mo, /BREAKDOWN_EPOCH
			 2: cdf_epoch16, t_epoch[i], yr, /BREAKDOWN_EPOCH
			else: ;Do nothing
		endcase
	
		;Store the values
		switch nPrams of
			11: pico[i]   = pic
			10: nano[i]   = nan
			 9: micro[i]  = mic
			 8: milli[i]  = mil
			 7: second[i] = sec
			 6: minute[i] = mn
			 5: hour[i]   = hr
			 4: day[i]    = dy
			 3: month[i]  = mo
			 2: year[i]   = yr
		endswitch
	endfor

	;Return scalars
	if nTimes eq 1 then begin
		switch nPrams of
			11: pico[i]   = pico[0]
			10: nano[i]   = nano[0]
			 9: micro[i]  = micro[0]
			 8: milli[i]  = milli[0]
			 7: second[i] = second[0]
			 6: minute[i] = minute[0]
			 5: hour[i]   = hour[0]
			 4: day[i]    = day[0]
			 3: month[i]  = month[0]
			 2: year[i]   = year[0]
		endswitch
	endif
end


;+
;   The purpose of this program is to compute or break-down CDF Epoch times.
;
; :Params:
;       T_EPOCH:            in, out, required, type=dblarr/dcomplexarr/lon64arr
;                           An array of epoch times. Accepted epoch types are::
;                               CDF_EPOCH
;                               CDF_EPOCH16
;                               CDF_TIME_TT2000
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
;       BREAKDOWN_EPOCH:    in, optional, type=boolean
;                           If set, `T_EPOCH` will be broken into elements which are
;                               returned through the remaining parameters. If not provided,
;                               a guess will be determined based on the input parameters.
;       COMPUTE_EPOCH:      in, optional, type=boolean
;                           If set, `T_EPOCH` will be computed using the values supplied
;                               in the other parameters. If not provided, a guess will be
;                               made based on the input parameters.
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
;-
pro MrCDF_Epoch_Breakdown, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico, $
TOINTEGER=toInteger
	compile_opt strictarr
	on_error, 2
	

	;Determine the type of CDF epoch value given
	eType = MrCDF_Epoch_Type(t_epoch)

;---------------------------------------------------------------------
;Vectorized Versions /////////////////////////////////////////////////
;---------------------------------------------------------------------
	if MrCDFCmpVersion('3.4.0.0') le 0 then begin

		;Breakdown the epoch value
		case eType of
			'CDF_EPOCH':       cdf_epoch,   t_epoch, year, month, day, hour, minute, second, milli, $
			                                /BREAKDOWN_EPOCH
			'CDF_EPOCH16':     cdf_epoch16, t_epoch, year, month, day, hour, minute, second, $
			                                milli, micro, nano, pico, $
			                                /BREAKDOWN_EPOCH
			'CDF_TIME_TT2000': cdf_tt2000,  t_epoch, year, month, day, hour, minute, second, $
			                                milli, micro, nano, TOINTEGER=toInteger, $
			                                /BREAKDOWN_EPOCH
		endcase
	
		return
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
			11: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
			10: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano
			 9: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro
			 8: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli
			 7: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second
			 6: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour, minute
			 5: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day, hour
			 4: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month, day
			 3: MrCDF_Epoch_Breakdown_Loop, t_epoch, year, month
			 2: MrCDF_Epoch_Breakdown_Loop, t_epoch, year
			else: message, 'Incorrect number of parameters.'
		endcase
	
	;CDF_EPOCH16
	endif else if eType eq 'CDF_EPOCH16' then begin
		case n_params() of
			11: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico
			10: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano
			 9: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro
			 8: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second, milli
			 7: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute, second
			 6: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour, minute
			 5: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day, hour
			 4: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month, day
			 3: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year, month
			 2: MrCDF_Epoch16_Breakdown_Loop, t_epoch, year
			else: message, 'Incorrect number of parameters.'
		endcase
	
	;CDF_TIME_TT2000
	endif else if eType eq 'CDF_TIME_TT2000' then begin
		message, 'CDF_TIME_TT2000 times were not introduced until CDf v3.4.', /INFORMATIONAL
	endif else begin
		message, 'Unknown CDF Epoch type: "' + eType + '".'
	endelse
end