; docformat = 'rst'
;
; NAME:
;       MRCDF_EPOCH
;
; PURPOSE:
;+
;       The purpose of this program is to create a wrapper for the `cdf_epoch`, 
;       `cdf_epoch16` and `cdf_tt2000` functions.
;
; :Examples:
;   See the main-level program at the end of this routine::
;       IDL> .r MrCDF_Epoch
;
;   Compute a single CDF time in CDF_TIME_TT2000 format::
;       IDL> MrCDF_Epoch, epoch, 2015, 03, 12
;       IDL> print, epoch
;           479390467184000000
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
;       Morse Hall, Room 113
;       8 Collge Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       Written by  -   Matthew Argall 12 February 2012
;       2014/02/03  -   Added MrCDF_Epoch_Loop and MrCDF_Epoch16_Loop routines to make
;                           versions of the CDF DLL earlier than 3.4 work. Added the
;                           CDFEPOCH, EPOCH16 and TT2000 keywords. - MRA
;       2014/03/03  -   Added the EPOCH_TYPE keyword. - MRA.
;       2015/02/16  -   Made code fragments more concise. Renamed CDFEPOCH keyword to
;                           CDF_EPOCH. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this program is to compute or break-down CDF Epoch times.
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
;
; :Keywords:
;       BREAKDOWN_EPOCH:    in, optional, type=boolean
;                           If set, `T_EPOCH` will be broken into elements which are
;                               returned through the remaining parameters. Either
;                               BREAKDOWN_EPOCH or `COMPUTE_EPOCH` must be specified.
;       COMPUTE_EPOCH:      in, optional, type=boolean
;                           If set, `T_EPOCH` will be computed using the values supplied
;                               in the other parameters. Either `BREAKDOWN_EPOCH` or
;                               COMPUTE_EPOCH must be specified.
;-
pro MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico, $
COMPUTE_EPOCH=compute_epoch, $
BREAKDOWN_EPOCH=breakdown_epoch
	compile_opt strictarr
	on_error, 2

	;Defaults
	compute_epoch   = keyword_set(compute_epoch)
	breakdown_epoch = keyword_set(breakdown_epoch)
	if compute_epoch + breakdown_epoch ne 1 then $
		message, 'Exactly one of COMPUTE_EPOCH and BREAKDOWN_EPOCH must be specified.'

	;Number if iterations
	nTimes = (compute_epoch eq 1) ? n_elements(year) : n_elements(t_epoch)

;---------------------------------------------------------------------
; Compute Epoch //////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if compute_epoch eq 1 then begin
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

;---------------------------------------------------------------------
;Breakdown Epoch /////////////////////////////////////////////////////
;---------------------------------------------------------------------
    endif else begin
		;Number of outputs expected
		nParts = n_params()
	
		;Allocate memory
		switch nParts of
			11: ;Do nothing
			10: ;Do nothing
			 9: ;Do nothing
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
	endelse
end


;+
;   The purpose of this program is to compute or break-down CDF Epoch times.
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
;
; :Keywords:
;       BREAKDOWN_EPOCH:    in, optional, type=boolean
;                           If set, `T_EPOCH` will be broken into elements which are
;                               returned through the remaining parameters. Either
;                               BREAKDOWN_EPOCH or `COMPUTE_EPOCH` must be specified.
;       COMPUTE_EPOCH:      in, optional, type=boolean
;                           If set, `T_EPOCH` will be computed using the values supplied
;                               in the other parameters. Either `BREAKDOWN_EPOCH` or
;                               COMPUTE_EPOCH must be specified.
;-
pro MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico, $
COMPUTE_EPOCH=compute_epoch, $
BREAKDOWN_EPOCH=breakdown_epoch
	compile_opt strictarr
	on_error, 2

	;Defaults
	compute_epoch  = keyword_set(compute_epoch)
	breakdown_epoch = keyword_set(breakdown_epoch)
	if compute_epoch + breakdown_epoch ne 1 then $
		message, 'Exactly one of COMPUTE_EPOCH and BREAKDOWN_EPOCH must be specified.'

	;Number if iterations
	nTimes = (compute_epoch eq 1) ? n_elements(year) : n_elements(t_epoch)

;---------------------------------------------------------------------
;Compute Epoch ///////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if compute_epoch eq 1 then begin
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

;---------------------------------------------------------------------
;Breakdown Epoch /////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
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
	endelse
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
pro MrCDF_Epoch, t_epoch, year, month, day, hour, minute, second, milli, micro, nano, pico, $
COMPUTE_EPOCH=compute_epoch, $
BREAKDOWN_EPOCH=breakdown_epoch, $
TOINTEGER=toInteger, $
CDF_EPOCH=cdfepoch, $
EPOCH16=epoch16, $
TT2000=tt2000, $
EPOCH_TYPE=epoch_type
	compile_opt strictarr
	on_error, 2
	
	;Defaults?
	compute_epoch   = keyword_set(compute_epoch)
	breakdown_epoch = keyword_set(breakdown_epoch)

	;Cannot supply both keywords
	if compute_epoch + breakdown_epoch gt 1 then $
		message, 'Exactly one of COMPUTE_EPOCH and BREAKDOWN_EPOCH may be set.'
	
	;If neither keyword was given, try to determine action.
	if compute_epoch + breakdown_epoch eq 0 then begin
		case 1 of
			n_elements(t_epoch) eq 0 && n_elements(year) gt 0: compute_epoch   = 1
			n_elements(t_epoch) gt 0 && n_elements(year) eq 0: breakdown_epoch = 1
			else: message, 'Confusing inputs. Must specify /BREAKDOWN_EPOCH, or /COMPUTE_EPOCH.'
		endcase
	endif

	;Determine the type of CDF epoch value given
	case 1 of
		n_elements(epoch_type) gt 0: eType = strupcase(epoch_type)
		breakdown_epoch eq 1:        eType = MrCDF_Epoch_Type(t_epoch)
		else:                        eType = keyword_set(TT2000)                   ? 'CDF_TIME_TT2000' : $
		                                         keyword_set(EPOCH16)              ? 'CDF_EPOCH16'     : $
		                                         keyword_set(CDFEPOCH)             ? 'CDF_EPOCH'       : $
		                                         (MrCDFCmpVersion('3.4.0.0') le 0) ? 'CDF_TIME_TT2000' : $
		                                        'CDF_EPOCH'
	endcase

;---------------------------------------------------------------------
;Vectorized Versions /////////////////////////////////////////////////
;---------------------------------------------------------------------
	if MrCDFCmpVersion('3.4.0.0') le 0 then begin

		;Define each input to prevent "BAD Epoch Type" error.
		if keyword_set(compute_epoch) then begin
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
		endif

		;Compute/Breakdown the epoch value
		case eType of
			'CDF_EPOCH': cdf_epoch, t_epoch, year, month, day, hour, minute, second, milli, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			'CDF_EPOCH16': cdf_epoch16, t_epoch, year, month, day, hour, minute, second, $
			                            milli, micro, nano, pico, $
			                            BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			'CDF_TIME_TT2000': cdf_tt2000, t_epoch, year, month, day, hour, minute, second, $
			                                milli, micro, nano, TOINTEGER=toInteger, $
			                                BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
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
			11: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, $
			                      second, milli, micro, nano, pico, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			10: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, $
			                      second, milli, micro, nano, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 9: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, $
			                      second, milli, micro, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 8: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, $
			                      second, milli, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 7: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, $
			                      second, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 6: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, minute, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 5: MrCDF_Epoch_Loop, t_epoch, year, month, day, hour, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 4: MrCDF_Epoch_Loop, t_epoch, year, month, day, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 3: MrCDF_Epoch_Loop, t_epoch, year, month, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 2: MrCDF_Epoch_Loop, t_epoch, year, $
			                      BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			else: message, 'Incorrect number of parameters.'
		endcase
	
	;CDF_EPOCH16
	endif else if eType eq 'CDF_EPOCH16' then begin
		case n_params() of
			11: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, $
			                        second, milli, micro, nano, pico, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			10: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, $
			                        second, milli, micro, nano, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 9: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, $
			                        second, milli, micro, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 8: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, $
			                        second, milli, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 7: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, $
			                        second, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 6: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, minute, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 5: MrCDF_Epoch16_Loop, t_epoch, year, month, day, hour, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 4: MrCDF_Epoch16_Loop, t_epoch, year, month, day, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 3: MrCDF_Epoch16_Loop, t_epoch, year, month, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			 2: MrCDF_Epoch16_Loop, t_epoch, year, $
			                        BREAKDOWN_EPOCH=breakdown_epoch, COMPUTE_EPOCH=compute_epoch
			else: message, 'Incorrect number of parameters.'
		endcase
	
	;CDF_TIME_TT2000
	endif else if eType eq 'CDF_TIME_TT2000' then begin
		message, 'CDF_TIME_TT2000 times were not introduced until CDf v3.4.', /INFORMATIONAL
	endif else begin
		message, 'Unknown CDF Epoch type: "' + eType + '".'
	endelse
end



;---------------------------------------------------------------------
;Main-Level Program (IDL> .r MrCDF_Epoch) ////////////////////////////
;---------------------------------------------------------------------


;============
; EXAMPLE 1 |
;============
; COMPUTE_EPOCH Using a Scalar Value
;   1. Create dates to be converted to epoch times
;   2. Convert it to an epoch value using cdf_epoch and cdf_epoch16
;   3. Convert it to an epoch value using MrCDF_Epoch
;   4. Compare the values
time = '2012-05-24T06:21:43.333666999112'
year   = 2012
month  = 05
day    = 24
hour   = 06
minute = 21
second = 43
milli  = 333
micro  = 666
nano   = 999
pico   = 112

;Compute using CDF routines
if MrCDFCmpVersion('3.4') eq 1 then begin
	cdf_epoch,     t_epoch, year, month, day, hour, minute, second, milli, /COMPUTE_EPOCH
	cdf_epoch16, t_epoch16, year, month, day, hour, minute, second, milli, micro, nano, pico, /COMPUTE_EPOCH

	;Compute using MrCDF_Epoch
	MrCDF_Epoch,   MrEpoch, year, month, day, hour, minute, second, milli, /CDFEPOCH, /COMPUTE_EPOCH
	MrCDF_Epoch, MrEpoch16, year, month, day, hour, minute, second, milli, micro, nano, pico, /EPOCH16, /COMPUTE_EPOCH

	;Compare the values
	epoch_match   = cdf_epoch_compare(MrEpoch, t_epoch)
	epoch16_match = cdf_epoch_compare(MrEpoch16, t_epoch16)
	results_tt2000 = 'CDF v3.4 or greater is requied. Current version: ' + MrCDFCmpVersion()

endif else begin
	cdf_epoch,   t_epoch, year, month, day, hour, minute, second, milli, /COMPUTE_EPOCH
	cdf_epoch16, t_epoch16, year, month, day, hour, minute, second, milli, micro, nano, pico, /COMPUTE_EPOCH
	cdf_tt2000,  t_epoch_tt, year, month, day, hour, minute, second, milli, micro, nano, /COMPUTE_EPOCH

	;Compute using MrCDF_Epoch
	MrCDF_Epoch, MrEpoch, year, month, day, hour, minute, second, milli, /CDFEPOCH, /COMPUTE_EPOCH
	MrCDF_Epoch, MrEpoch16, year, month, day, hour, minute, second, milli, micro, nano, pico, /EPOCH16, /COMPUTE_EPOCH
	MrCDF_Epoch, MrEpochTT, year, month, day, hour, minute, second, milli, micro, nano, /COMPUTE_EPOCH

	;Compare the values
	epoch_match   = cdf_epoch_compare(MrEpoch, t_epoch)
	epoch16_match = cdf_epoch_compare(MrEpoch16, t_epoch16)
	epochTT_match = cdf_epoch_compare(MrEpochTT, t_epoch_tt)
	results_tt2000 = (epochTT_match eq 0) ? 'Yes' : 'No'
endelse

print, '---------------------------------------------'
print, "Compare MrCDF_Epoch to IDL's CDF_* rountines."
print, '---------------------------------------------'

;Print the results
print, 'Test using a scalar value:'
print, FORMAT='(%"  Time              =        %s")', time
print, FORMAT='(%"  CDF_EPOCH       match?     %s")', (epoch_match   eq 0) ? 'Yes' : 'No'
print, FORMAT='(%"  CDF_EPOCH16     match?     %s")', (epoch16_match eq 0) ? 'Yes' : 'No'
print, FORMAT='(%"  CDF_TIME_TT2000 match?     %s")', results_tt2000
print, ''

;============
; EXAMPLE 2 |
;============
; COMPUTE_EPOCH Using Arrays
;   1. Create dates to be converted to epoch times
;   2. Convert it to an epoch value using cdf_epoch and cdf_epoch16
;   3. Convert it to an epoch value using MrCDF_Epoch
;   4. Compare the values
time = ['2012-05-24T06:21:43.333666999112Z', $
        '2011-07-13T18:59:31.444123876754Z', $
        '2013-01-31T00:42:21.555234347486Z', $
        '2000-11-09T04:56:45.666345845678Z']
year   = [2012, 2011, 2013, 2000]
month  = [  05,   07,   01,   11]
day    = [  24,   13,   31,   09]
hour   = [  06,   18,   00,   04]
minute = [  21,   59,   42,   56]
second = [  43,   31,   21,   45]
milli  = [ 333,  444,  555,  666]
micro  = [ 666,  123,  234,  345]
nano   = [ 999,  876,  347,  845]
pico   = [ 112,  754,  485,  678]

;Compute using CDF routines
if MrCDFCmpVersion('3.4') eq 1 then begin
	t_epoch         = dblarr(4)
	t_epoch16       = dcomplexarr(4)
	epoch_match     = intarr(4)
	epoch16_match   = intarr(4)
	epochTT_results = strarr(4)

	MrCDF_Epoch, MrEpoch,   year, month, day, hour, minute, second, milli, /COMPUTE_EPOCH
	MrCDF_Epoch, MrEpoch16, year, month, day, hour, minute, second, milli, micro, nano, pico, /EPOCH16, /COMPUTE_EPOCH

	for i = 0, 3 do begin
		cdf_epoch,   tmp,   year[i], month[i], day[i], hour[i], minute[i], second[i], milli[i], /COMPUTE_EPOCH
		cdf_epoch16, tmp16, year[i], month[i], day[i], hour[i], minute[i], second[i], milli[i], micro[i], nano[i], pico[i], /COMPUTE_EPOCH
	
		epoch_match[i]   = cdf_epoch_compare(MrEpoch[i],   tmp)
		epoch16_match[i] = cdf_epoch_compare(MrEpoch16[i], tmp16)
	
		t_epoch[i]   = tmp
		t_epoch16[i] = tmp16
		epochTT_results[i] = 'CDF v3.4 or greater is requied. Current version: ' + MrCDFCmpVersion()
	endfor
    
;Vectorized
endif else begin
	cdf_epoch,   t_epoch,    year, month, day, hour, minute, second, milli, /COMPUTE_EPOCH
	cdf_epoch16, t_epoch16,  year, month, day, hour, minute, second, milli, micro, nano, pico, /COMPUTE_EPOCH
	cdf_tt2000,  t_epoch_tt, year, month, day, hour, minute, second, milli, micro, nano, /COMPUTE_EPOCH

	MrCDF_Epoch, MrEpoch, year, month, day, hour, minute, second, milli, /CDFEPOCH, /COMPUTE_EPOCH
	MrCDF_Epoch, MrEpoch16, year, month, day, hour, minute, second, milli, micro, nano, pico, /EPOCH16, /COMPUTE_EPOCH
	MrCDF_Epoch, MrEpochTT, year, month, day, hour, minute, second, milli, micro, nano, /COMPUTE_EPOCH

	epoch_match   = cdf_epoch_compare(MrEpoch, t_epoch)
	epoch16_match = cdf_epoch_compare(MrEpoch16, t_epoch16)
	epochTT_match = cdf_epoch_compare(MrEpochTT, t_epoch_tt)

	epochTT_results = strarr(4)
	for i = 0, 3 do epochTT_results[i] = (epochTT_match[i] eq 0) ? 'Yes' : 'No'
endelse

;Print the results
print, 'Test using arrays:'
for i = 0, 3 do begin
	print, FORMAT='(%"  Time              =        %s")', time[i]
	print, FORMAT='(%"  CDF_EPOCH       match?     %s")', (epoch_match[i]   eq 0) ? 'Yes' : 'No'
	print, FORMAT='(%"  CDF_EPOCH16     match?     %s")', (epoch16_match[i] eq 0) ? 'Yes' : 'No'
	print, FORMAT='(%"  CDF_TIME_TT2000 match?     %s")', epochTT_results[i]
	print, ''
endfor

end

