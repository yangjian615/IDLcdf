; docformat = 'rst'
;
; NAME:
;       MrCDF_toASCII
;
;*****************************************************************************************
;   Copyright (c) 2016, Matthew Argall                                                   ;
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
;   Convert CDF files to ASCII. One file will be created per Epoch variable. Only
;   variables with DISPLAY_TYPE of 'time_series' will be considered.
;
; :Categories:
;   CDF
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
;       2016-05-16  -   Written by Matthew Argall
;       2016-08-22  -   Write epoch times as ISO date-time strings. - MRA
;-
;*****************************************************************************************
;+
;   Format variable information and write header to file.
;
; :Params:
;       LUN:        in, required, type=long
;                   Logical unit number of the file being written.
;       OCDF:       in, required, type=object
;                   MrCDF_File object of the CDF file from which data is extracted.
;       VNAME:      in, required, type=string
;                   Name of the CDF variable from which info is taken.
;       VARLEN:     in, required, type=integer
;                   Length of the longest variable name in the CDF file.
;
; :Keywords:
;       VALUE:      out, optional, type=depends
;                   Representative value of the data associated with the variable.
;       VARFMT:     out, optional, type=string
;                   Format string describing how a single value should be written.
;
; :Returns:
;       DATAFMT:    out, required, type=string
;                   Format string describing how a complete record should be written.
;-
function MrCDF_toASCII_header, lun, oCDF, vname, varlen, $
VALUE=value, $
VARFMT=varFMT
	compile_opt idl2
	on_error, 2
	
	;Write the header information for the epoch variable
	;   - CDF_EPOCH_LONG (15 characters) is the longest CDF_TYPE
	;   - Format as: Variable  Type  Format  Size
	
	;Get variable properties
	oCDF[vname] -> GetProperty, DIMENSIONS = dims, $
	                            MAXREC     = maxrec, $
	                            CDF_TYPE   = cdf_type

;-----------------------------------------------------
; Variable Format \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;EPOCH data is converted to string
	if stregex(cdf_type, '(EPOCH|TT2000)', /BOOLEAN) then begin
		case 1 of
			cdf_type eq 'CDF_EPOCH':       varFMT = 'a33'
			cdf_type eq 'CDF_EPOCH_LONG':  varFMT = 'a24'
			cdf_type eq 'CDF_TIME_TT2000': varFMT = 'a29'
			else: message, 'Unexpected epoch type: "' + cdf_type + '".'
		endcase
		dataFMT = varFMT
	
	;FORMAT given as attribute
	endif else if oCDF -> VarHasAttr(vname, 'FORMAT') then begin
		;Get the format code
		fmt = oCDF -> GetVarAttrValue(vname, 'FORMAT')
	
		;Dissect format code
		datatype = strmid(fmt, 0, 1)
		datalen  = strsplit( strmid(fmt, 1), '.', /EXTRACT, COUNT=nDatalen)
		width    = fix(datalen[0])
		decimal  = nDataLen eq 2 ? fix(datalen[1]) : 0
		
		;Format code for header
		;   - The width is often 1 number too short for IDL formatting
		varFMT = string(datatype, width+1, decimal, FORMAT='(%"%s%i.%i")')
		
		;Format code for data
		dataFMT = dims[0] eq 0 ? string(varFMT, FORMAT='(%", 2x, %s")') : $
		                         string(dims, varFMT, FORMAT='(%", %i(2x, %s)")')
	
	;Guess formatting
	endif else begin
		;Get a test value
		case 1 of
			oCDF -> VarHasAttr(vname, 'VALIDMIN'): test = oCDF -> GetVarAttrValue(vname, 'VALIDMIN')
			oCDF -> VarHasAttr(vname, 'VALIDMAX'): test = oCDF -> GetVarAttrValue(vname, 'VALIDMAX')
			oCDF -> VarHasAttr(vname, 'FILLVAL'):  test = oCDF -> GetVarAttrValue(vname, 'FILLVAL')
			else: message, 'Cannot determine variable format.'
		endcase
		
		;What type is the test value
		case 1 of 
			MrIsA(test, /INTEGER): varFMT = 'i' + string(alog10(test), FORMAT='(i0)')
			MrIsA(test, 'FLOAT'):  varFMT = 'G12.4'
			MrIsA(test, 'DOUBLE'): varFMT = 'G17.6'
			else: message, 'Cannot determine variable format code.'
		endcase
	endelse

;-----------------------------------------------------
; Dimensions \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Dimensions of variable
	dimSize = [dims, maxrec+1]
	dimStr  = '[' + strjoin(string(dimSize, FORMAT='(i0)'), ', ') + ']'

;-----------------------------------------------------
; Representative Value \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Read a representative value from the data file
	value = oCDF -> GetVarData(vname, REC_COUNT=1)
	if MrIsA(value, /NUMBER) then value *= 0B

;-----------------------------------------------------
; Write Header & Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Create the header string
	hdrFMT = '(4x, a-' + strtrim(varlen, 2) + ', 2x, a-15, 2x, a-7, 2x, a0)'
	printf, lun, vname, cdf_type, varFMT, dimStr, FORMAT=hdrFMT
	
	return, dataFMT
end


;+
;   Convert a CDF file to ASCII data files.
;
; :Params:
;       FILENAME:       in, required, type=string
;                       Name of the CDF file to be converted.
;
; :Keywords:
;       DIRECTORY:      in, optional, type=string, default=pwd
;                       Directory in which to save the output files.
;       VARFORMAT:      in, optional, type=string/strarr
;                       Filter for variable names. Only those variables that match
;                           the filter will be written to the ASCII file.
;
; :Returns:
;       F_OUT:          out, required, type=string/strarr
;                       Name(s) of the output data files. One file per Epoch variable.
;-
function MrCDF_toASCII, filename, $
DIRECTORY=directory, $
VARFORMAT=varformat
	compile_opt strictarr
	
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		obj_destroy, oCDF
		if n_elements(lun) gt 0 then free_lun, lun
		MrPrintF, 'LogErr'
		return, ''
	endif
	
	if n_elements(directory) eq 0 then cd, CURRENT=directory
	if n_elements(varformat) eq 0 then varformat = '*'

;-----------------------------------------------------
; Search for Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Open the file
	oCDF = MrCDF_File(filename)
	
	;Get variable names
	varnames = oCDF -> GetVarNames(COUNT=nVars)
	
	;Filter variables
	if varformat[0] ne '*' then begin
		;Filter mask
		tf_var = bytarr(nVars)
		for i = 0, n_elements(varformat)-1 do tf_var or= strmatch(varnames, varformat[i])

		;Variables that passed filter
		iPass = where(tf_var, nVars)
		if nVars eq 0 then message, 'No variables pass VARFILTER.'
		varnames = varnames[iPass]
	endif

;-----------------------------------------------------
; Organize by Epoch Variable \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	theEpoch = strarr(nVars)
	iData    = intarr(nVars)
	nData    = 0
	
	;Organize variables by type
	for i = 0, nVars - 1 do begin
		;Skip variables that are not of type "data"
		if oCDF -> GetVarAttrValue(varnames[i], 'VAR_TYPE') eq 'data' then begin
			;Get the epoch variable
			theEpoch[nData] = oCDF -> GetVarAttrValue(varnames[i], 'DEPEND_0')

			;Increase count
			iData[nData] = i
			nData       += 1
		endif
	endfor

	;Select only the data variables
	iData    = iData[0:nData-1]
	theEpoch = theEpoch[0:nData-1]
	varNames = varNames[iData]

;-----------------------------------------------------
; New File for Each Epoch \\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Find names of Epoch variables
	iSort = sort(theEpoch)
	iUniq = uniq(theEpoch, iSort)
	nUniq = n_elements(iUniq)
	f_out = strarr(nUniq)
	
	;Output directory
	fbase   = file_basename(filename)
	fparts  = strsplit(fbase, '.', /EXTRACT, COUNT=count)
	if count gt 1 then begin
		fbase = strjoin(fparts[0:count-2], '.')
		ext   = fparts[count-1]
	endif else begin
		ext = ''
	endelse

	;Create output file names
	if nUniq eq 1 then begin
		f_out = filepath(fbase + '.dat', ROOT_DIR=directory)
	endif else begin
		for i = 0, nUniq - 1 do f_out[i] = filepath(fbase + '_' + strtrim(i,2) + '.dat', ROOT_DIR=directory)
	endelse

;-----------------------------------------------------
; Create Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	
	;Step through each epoch variable
	for i = 0, nUniq - 1 do begin
		;Files for this epoch
		iMatch  = where(theEpoch eq theEpoch[iUniq[i]], nMatch)
		theVars = varNames[iMatch]
		varLen  = max(strlen(theVars))

		;Open the output file and write beginning of header
		;   - One line per variable
		;   - One line for the Epoch variable
		;   - Three extra lines for header formatting
		;   - One extra variable for the Epoch variable
		openw, lun, f_out[i], /GET_LUN
		printf, lun, nMatch+4, nMatch+1, FORMAT='(%"NHEADER=%i, NVARS=%i")'
		printf, lun, 'VARIABLES:'

		;Read the Epoch data and create a data structure
		struct = create_struct( theEpoch[iUniq[i]], '' )
		
		;Write the header information for the epoch variable
		;   - Epoch format codes can sometimes be not large enough (e.g. E15.8)
		;   - Re-define the format code to be wide enough
		hdrFMT = MrCDF_toASCII_header(lun, oCDF, theEpoch[iUniq[i]], varlen, VALUE=value)
		hdrFMT = 'a0'

		;Step through each variable
		for j = 0, nMatch - 1 do begin
			;Name of the variable
			theName = varnames[iMatch[j]]
			
			;Skip non-time series variables for now
			;   - Print a blank line to keep header info accurate
			dispType = oCDF -> GetVarAttrValue(theName, 'DISPLAY_TYPE')
			if dispType ne 'time_series' then begin
				printf, lun, ''
				continue
			endif
			
			;Write header info
			dataFMT = MrCDF_toASCII_header(lun, oCDF, theName, varlen, $
			                               VALUE = value)
			
			;Add the variable and value to the data
			struct  = create_struct(struct, theName, value)
			hdrFMT += dataFMT
		endfor
		
		;Read the Epoch data and expand the data structure
		epoch_temp = oCDF -> Read(theEpoch[iUniq[i]])
		epoch_temp = MrCDF_Epoch_Encode(epoch_temp, EPOCH=3)
		nPts       = n_elements(epoch_temp)
		struct     = replicate(struct, nPts)
		struct.(0) = reform(temporary(epoch_temp))

		;Read the variable data
		for j = 0, nMatch - 1 do struct.(j+1) = reform( oCDF -> Read(varNames[iMatch[j]]) )

		;Write the variable data
		printf, lun, 'DATA:'
		for i = 0, nPts-1 do printf, lun, struct[i], FORMAT= '(' + hdrFMT + ')'
		free_lun, lun
	endfor

;-----------------------------------------------------
; Create Files \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Close the CDF file
	obj_destroy, oCDF

	return, f_out
end
