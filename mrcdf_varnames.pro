; docformat = 'rst'
;
; NAME:
;       MrCDF_VarNames
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
;   Return the names of all variables within the given CDF file.
;
; :Categories:
;       CDF Utilities
;
; :Params:
;       FILENAME:       in, required, type=string
;                       Name of the CDF file for which variable names are to be returned.
;
; :Keywords:
;       COUNT:          out, optional, type=bytarr
;                       Returns the number of variable names.
;       ISZVAR:         out, optional, type=bytarr
;                       Returns 1 if the corresponding variable name is associated with
;                           a z-variable and 0 for r-variables.
;       VALIDATE:       in, optional, type=boolean, default=0
;                       If set, the CDF file will be validated (takes time).
;
; :Returns:
;       VARNAMES:       Array of variable names contained within the CDF file.
;       
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@unh.edu
;
; :History:
;   Modification History::
;       2014/08/22  -   Written by Matthew Argall
;       2016/02/14  -   Added the COUNT keyword. Fixed logic. - MRA
;-
function MrCDF_VarNames, filename, $
COUNT=count, $
ISZVAR=isZVar, $
VALIDATE=validate
	compile_opt strictarr

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		
		;No variables found
		count = 0
	
		;Close the file
		if n_elements(cdfID) gt 0 then cdf_close, cdfID

		;Turn file validation back on
		if MrCmpVersion('8.0') le 0 then $
			if validate eq 0 then cdf_set_validate, /YES
		
		MrPrintF, 'LogErr'
		return, ''
	endif

	;Defaults
	validate = keyword_set(validate)
	if file_test(filename) eq 0 then message, 'File not found: ' + filename

;-----------------------------------------------------
; Open File & Get Variable Names \\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Validate the file?
	if MrCmpVersion('8.0') le 0 then begin
		if validate $
			then cdf_set_validate, /YES $
			else cdf_set_validate, /NO
	endif

	;Open the file and get info
	cdfID    = cdf_open(filename)
	cdf_info = cdf_inquire(cdfID)
	count    = cdf_info.nvars + cdf_info.nzvars

	;Allocate memory
	varnames = strarr(count)
	isZVar   = bytarr(count)

	;R-Variables
	for i = 0, cdf_info.nvars - 1 do begin
		var_inq = cdf_varinq(cdfID, i)
		varnames[i] = var_inq.name
	endfor

	;Z-Variables
	for i = 0, cdf_info.nzvars - 1 do begin
		var_inq                    = cdf_varinq(cdfID, i, /ZVARIABLE)
		varnames[cdf_info.nvars+i] = var_inq.name
		isZvar[cdf_info.nvars+i]   = 1B
	endfor

	;Close the file
	cdf_close, cdfID

	;Turn file validation back on
	if MrCmpVersion('8.0') le 0 then $
		if validate eq 0 then cdf_set_validate, /YES

	return, varnames
end
