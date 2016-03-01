; docformat = 'rst'
;
; NAME:
;       MrCDF_nRead
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
;   Read data and its dependencies from multiple CDF data files.
;
;   NOTES:
;       - Untested on variables with dimensional variance
;       - If DEPEND_# has no record variance, the values are assumed to be
;         uniform across files and data from the last file is returned.
;       - `FILES` are assumed to be in chronological order. If not, data
;         will also not be in chronological order.
;       - The variable identified by `VARNAME` is assume to be conistent
;         across data files.
;
; :Categories:
;       CDF Utilities
;
; :Examples:
;    Given the files and variable name::
;      IDL> files = ['mms1_edi_slow_l1a_efield_20150424_v0.1.0.cdf', $
;                    'mms1_edi_slow_l1a_efield_20150425_v0.1.0.cdf']
;      IDL> varname = 'mms1_edi_phi_gd12'
;
;    Read data from the first file::
;      IDL> data = MrCDF_nRead(files[0], varname, DEPEND_0=depend_0)
;      IDL> help, data
;        DATA            FLOAT     = Array[1, 339680]
;        DEPEND_0        LONG64    = Array[1, 339680]
;
;    Read data from both files (the seconds has data from 13:16:55 to 17:59:23)::
;      IDL> data = MrCDF_nRead(files, varname, DEPEND_0=depend_0)
;      IDL> help, data
;        DATA            FLOAT     = Array[1, 475264]
;        DEPEND_0        LONG64    = Array[1, 475264]
;
;    Select a time interval::
;      IDL> tstart = '2015-04-24T12:00:00Z'
;      IDL> tend   = '2015-04-25T17:20:00Z'
;      IDL> data = MrCDF_nRead(files, varname, DEPEND_0=depend_0, TSTART=tstart, TEND=tend)
;      IDL> help, data
;        DATA            FLOAT     = Array[1, 292374]
;        DEPEND_0        LONG64    = Array[1, 292374]
;      IDL> print, MrCDF_Epoch_Encode(depend_0[[0,292373]])
;        24-Apr-2015 12:00:00.004246218
;        25-Apr-2015 17:19:59.879286343
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
;       2014/04/30  -   Written by Matthew Argall
;       2016/02/09  -   Handle cases when zero records were written to file. - MRA
;-
;*****************************************************************************************
;+
;    Helper function for MrCDF_nRead. Read and append data.
;
; :Params:
;       DATA:               in, required, type=any
;                           Data from last call to MrCDF_nRead_GetData. New data will
;                               be appended. If this is the first call to _GetData, then
;                               set equal to a named variable that will receive data.
;       CDFID:              in, required, type=string/long
;                           CDF identifier of the CDF file being read.
;       VARNAME:            in, required, type=string/object
;                           Name of the variable whose data will be read.
;       ATTRNAME:           in, required, type=string, default=''
;                           If a dependent variable's data should be read, then specify
;                               this parameter as 'DEPEND_[0,1,2,3]'.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       OFFSET:             in, optional, type=intarr, default=0 for each dimension
;                           Array indices within the specified record(s) at which to
;                               begin reading. OFFSET is a 1-dimensional array
;                               containing one element per CDF dimension.
;       REC_COUNT:          in, optional, type=long, default=maxrec+1
;                           Number of records to be read.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records when reading multiple records.
;       STRING:             in, optional, type=boolean, default=0
;                           If set, "CDF_CHAR" and "CDF_UCHAR" data will be converted
;                               to strings. The are read from the file as byte-arrays.
;       VARINQ:             out, optional, type=structure
;                           Result of CDF_VarInq() on the variable for which data is
;                               returned.
;-
pro MrCDF_nRead_GetData, data, cdfID, varname, attrname, $
REC_INTERVAL = rec_interval, $
OFFSET = offset, $
COUNT = count, $
INTERVAL = interval, $
NRECS = nrecs, $
STRING = string, $
VARINQ = varinq
	compile_opt idl2
	on_error, 2
	
	;See if the variable has DEPEND_# as an attribute
	if attrname ne '' then begin
		cdf_attget_entry, cdfID, attrname, varname, entryType, theName, tf_has
	endif else begin
		tf_has  = 1B
		theName = varname
	endelse

	;If the variable exists
	if tf_has then begin
		;Number of records -- read all
		if n_elements(nrecs) eq 0 then nrecs = 0
		cdf_control, cdfID, GET_VAR_INFO=var_info, VARIABLE=theName
		rec_count = var_info.maxrec + 1
		
		;If no records have been written, then MAXREC=-1 and
		;REC_COUNT=0. CDF_VarGet will complain when reading 0
		;records, so undefine REC_COUNT. In this case,
		;CDF_VarGet will return a pad value.
		nrecs += rec_count
		
		;Get the data
		if rec_count gt 0 then begin
			;Do not show annoying cdf_varget warnings
			!Quiet = 1
			
			;Get its data
			cdf_varget, cdfID, theName, temp_data, $
			            REC_COUNT    = rec_count, $
			            REC_INTERVAL = rec_interval, $
			            OFFSET       = offset, $
			            COUNT        = count, $
			            INTERVAL     = interval, $
			            STRING       = string
			
			;Turn on normal warnings.
			!Quiet = 0
			
			;Append it to other data?
			varinq = cdf_varinq(cdfID, theName)
			nDims  = n_elements(varinq.dim)
	
			;Can only append if there is record variance
			if varinq.recvar eq 'VARY' $
				then data = MrConcatenate(data, temporary(temp_data), nDims+1) $
				else data = temporary(temp_data)
		endif else begin
			if n_elements(data) eq 0 then data = -1
		endelse
	endif else begin
		message, string(FORMAT='(%"Variable \"%s\" does not have attribute \"%s\". Cannot read data")', $
		                varname, depend)
	endelse
end


;+
;   A more robust method for obtaining variable data, when compared to the GetVarData
;   method.
;
; :Params:
;       FILE:               in, required, type=string/long
;                           The CDF identifier retured by CDF_Open() of the name of a CDF
;                               file from which data is read.
;       VARNAME:            in, required, type=string/object
;                           Name of the variable whose data will be read.
;
; :Keywords:
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       DATATYPE:           out, optional, type=string
;                           CDF datatype of the variable being read.
;       DEPEND_0:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_0` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       DEPEND_1:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_1` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       DEPEND_2:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_2` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       DEPEND_3:           out, optional, type=any
;                           If the variable `VARNAME` has a `DEPEND_3` attribute, then
;                               set this keyword equal to a named variable into which the
;                               data for that attribute is returned.
;       FILLVALUE:          out, optional, type=any
;                           Value used as a filler for missing data.
;       PADVALUE:           out, optional, type=any
;                           Value used to pad the data when more data is read than what
;                               exists in the file. It is possible that this value does
;                               not exist.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       NRECS:              out, optional, type=intarr, default=1 for each dimension
;                           Number of records read. If no records were written to the
;                               file, then NRECS=0 and `DATA`=-1.
;       OFFSET:             in, optional, type=intarr, default=0 for each dimension
;                           Array indices within the specified record(s) at which to
;                               begin reading. OFFSET is a 1-dimensional array
;                               containing one element per CDF dimension.
;       PATTERN:            in, optional, type=boolean, default="%Y-%M-%dT%H:%m:%S%z"
;                           If set, then `REC_START` and `REC_END` are times that will be
;                               parsed into CDF epoch values. PATTERN describes how the
;                               times should be parsed and accepts any pattern recognized
;                               by MrTimeParser.pro. Automatically sets `TIME`=1.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records when reading multiple records.
;       STATUS:             out, optional, type=integer
;                           Named variable to receive the error status. If present, no
;                               error is issued and a value of -1 is returned.
;                                    0  -  No Error
;                                    1  -  Unexpected error
;                                    2  -  Trapped error
;                                    3  -  No records found
;                                    4  -  No records in time interval
;       STRING:             in, optional, type=boolean, default=0
;                           If set, "CDF_CHAR" and "CDF_UCHAR" data will be converted
;                               to strings. The are read from the file as byte-arrays.
;       TSTART:             in, optional, type=string, default=''
;                           An ISO-8601 date-time string specifying the start time of
;                               the data interval to be read.
;       TSTART:             in, optional, type=string, default=''
;                           An ISO-8601 date-time string specifying the end time of
;                               the data interval to be read.
;       VALIDATE:           in, optional, type=boolean, default=0
;                           If set, the CDF file will be validated when opened.
;
; :Returns:
;       DATA:               Data associated with `VARNAME`. If zero records were
;                               written to the file, then DATA=-1 and `NRECS`=0.
;-
function MrCDF_nRead, files, varName, $
;INPUT
COUNT=count, $
COL_MAJOR=col_major, $
INTERVAL=interval, $
OFFSET=offset, $
PATTERN=pattern, $
REC_INTERVAL=rec_interval, $
TSTART=tstart, $
TEND=tend, $
STRING=string, $
VALIDATE=validate, $
;OUTPUT
DEPEND_0=depend_0, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3, $
DATATYPE=datatype, $
FILLVALUE=fillvalue, $
NRECS=nRecs, $
PADVALUE=padvalue, $
STATUS=status
	compile_opt strictarr

	;catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
	
		;Check that warnings are turned back on
		if !Quiet eq 1 then !Quiet = 0
		if status eq 0 then status = 1
	
		;Close the file
		if n_elements(cdfID) gt 0 && tf_open then cdf_close, cdfID

		;Turn file validation back on
		if MrCmpVersion('8.0') le 0 then $
			if validate eq 0 then cdf_set_validate, /YES
		
		;Issue error
		if ~arg_present(status) then MrPrintF, 'LogErr'
		return, -1
	endif

	;Defaults
	status    = 0
	col_major = keyword_set(col_major)
	validate  = keyword_set(validate)
	if n_elements(pattern) eq 0 then pattern = '%Y-%M-%dT%H:%m:%S%z'
	if n_elements(tstart)  eq 0 then tstart  = ''
	if n_elements(tend)    eq 0 then tend    = ''
	
	;Make sure a variable name was given, not a variable index number
	if MrIsA(varname, 'STRING') eq 0 then message, 'VARNAME must be a string.'

;-----------------------------------------------------
; Open File & Get MetaData \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;CDF IDs given?
	if size(files, /TNAME) eq 'LONG' then begin
		tf_IDs = 1B
	endif else begin
		tf_IDs = 0B
	
		;Validate the files?
		if MrCmpVersion('8.0') le 0 then begin
			if validate $
				then cdf_set_validate, /YES $
				else cdf_set_validate, /NO
		endif
	endelse

;-----------------------------------------------------
;Get the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

	;Step through each file
	for i = 0, n_elements(files) - 1 do begin
		;Open the file
		if tf_IDs then begin
			cdfID   = files[i]
			tf_open = 0B
		endif else begin
			cdfID   = cdf_open(files[i])
			tf_open = 1B
		endelse

		;Get information about the variable
		if i eq 0 then begin
			;Get CDF type
			var_inq  = cdf_varinq(cdfID, varName)
			cdf_type = var_inq.datatype
			
			;Get pad value
			cdf_control, cdfID, GET_VAR_INFO=var_info, VARIABLE=varname
			if has_tag(var_info, 'PADVALUE') then padvalue = var_info.padvalue

			;Is the variable an epoch time?
			isTime = max(cdf_type eq ['CDF_EPOCH', 'CDF_EPOCH16', 'CDF_TIME_TT2000'])
		endif

		;Read the variable data
		MrCDF_nRead_GetData, data, cdfID, varname, '', $
		                     REC_INTERVAL = rec_interval, $
		                     OFFSET       = offset, $
		                     COUNT        = count, $
		                     INTERVAL     = interval, $
		                     NRECS        = nRecs, $
		                     STRING       = string, $
		                     VARINQ       = data_inq

		;DEPEND_0
		if arg_present(depend_0) || ( ~isTime && (tstart ne '' || tend ne '') ) then begin
			MrCDF_nRead_GetData, depend_0, cdfID, varname, 'DEPEND_0', $
			                     REC_INTERVAL = rec_interval, $
			                     OFFSET       = offset, $
			                     COUNT        = count, $
			                     INTERVAL     = interval, $
			                     STRING       = string, $
			                     VARINQ       = dep0_inq
		endif

		;DEPEND_1
		if arg_present(depend_1) then begin
			MrCDF_nRead_GetData, depend_1, cdfID, varname, 'DEPEND_1', $
			                     REC_INTERVAL = rec_interval, $
			                     OFFSET       = offset, $
			                     COUNT        = count, $
			                     INTERVAL     = interval, $
			                     STRING       = string, $
			                     VARINQ       = dep1_inq
		endif
	
		;DEPEND_2
		if arg_present(depend_2) then begin
			MrCDF_nRead_GetData, depend_2, cdfID, varname, 'DEPEND_2', $
			                     REC_INTERVAL = rec_interval, $
			                     OFFSET       = offset, $
			                     COUNT        = count, $
			                     INTERVAL     = interval, $
			                     STRING       = string, $
			                     VARINQ       = dep2_inq
		endif
	
		;DEPEND_3
		if arg_present(depend_3) then begin
			MrCDF_nRead_GetData, depend_3, cdfID, varname, 'DEPEND_3', $
			                     REC_INTERVAL = rec_interval, $
			                     OFFSET       = offset, $
			                     COUNT        = count, $
			                     INTERVAL     = interval, $
			                     STRING       = string, $
			                     VARINQ       = dep3_inq
		endif
	
		;Close the data file
		if tf_open then begin
			cdf_close, cdfID
			tf_open = 0B
		endif
	endfor

;-----------------------------------------------------
; Select Time Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	if tstart ne '' || tend ne '' then begin
		;Make sure we have DEPEND_0
		if isTime then depend_0 = data
		epoch_type = MrCDF_Epoch_Type(depend_0)
	
		;Convert TSTART to an epoch
		if tstart ne '' $
			then t0 = MrCDF_Epoch_Parse(tstart, PATTERN=pattern, EPOCH_TYPE=epoch_type) $
			else t0 = depend_0[0]
			
		;Convert TEND to an epoch
		if tend ne '' $
			then t1 = MrCDF_Epoch_Parse(tend, PATTERN=pattern, EPOCH_TYPE=epoch_type) $
			else t1 = depend_0[n_elements(depend_0)-1]
		
		;Select a subset of time
		iselect = where(depend_0 ge t0 and depend_0 lt t1, nselect)
		if nselect eq 0 then begin
			;Set status
			status = 4
		
			;Create message
			epoch_range = MrCDF_Epoch_Encode([depend_0[0], depend_0[n_elements(depend_0)-1]], $
			                                 PATTERN='%Y-%M-%dT%H:%m:%S')
			time_range  = MrCDF_Epoch_Encode([t0, t1], PATTERN='%Y-%M-%dT%H:%m:%S')
			message, string(FORMAT='(%"No records found in time interval %s to %s")', time_range) + $
			         string(10B) + string(FORMAT='(%"Data ranges from %s to %s")', epoch_range)
		endif

	;-----------------------------------------------------
	; Select Subset \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	;-----------------------------------------------------
		if isTime then begin
			data = data[0, iselect]
		endif else begin
			depend_0 = depend_0[0, iselect]

			;Record variance is always along the last dimensions
			case n_elements(data_inq.dim) of
				0: data = data[iselect]
				1: data = data[*, iselect]
				2: data = data[*, *, iselect]
				3: data = data[*, *, *, iselect]
				else: message, 'Data has an unexpected number of dimensions.'
			endcase
			
			;DEPEND_1
			if arg_present(depend_1) && dep1_inq.recvar eq 'VARY' then begin
				case n_elements(data_inq.dim) of
					0: depend_1 = depend_1[iselect]
					1: depend_1 = depend_1[*, iselect]
					2: depend_1 = depend_1[*, *, iselect]
					3: depend_1 = depend_1[*, *, *, iselect]
					else: message, 'DEPEND_1 has an unexpected number of dimensions.'
				endcase
			endif
			
			;DEPEND_2
			if arg_present(depend_2) && dep2_inq.recvar eq 'VARY' then begin
				case n_elements(data_inq.dim) of
					0: depend_2 = depend_2[iselect]
					1: depend_2 = depend_2[*, iselect]
					2: depend_2 = depend_2[*, *, iselect]
					3: depend_2 = depend_2[*, *, *, iselect]
					else: message, 'DEPEND_2 has an unexpected number of dimensions.'
				endcase
			endif
			
			;DEPEND_3
			if arg_present(depend_2) && dep3_inq.recvar eq 'VARY' then begin
				case n_elements(data_inq.dim) of
					0: depend_3 = depend_3[iselect]
					1: depend_3 = depend_3[*, iselect]
					2: depend_3 = depend_3[*, *, iselect]
					3: depend_3 = depend_3[*, *, *, iselect]
					else: message, 'DEPEND_3 has an unexpected number of dimensions.'
				endcase
			endif
		endelse
	endif

;-----------------------------------------------------
; Return \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
	;Return column-major?
	if col_major then begin
		ndims = size(data, /N_DIMENSIONS)
		if ndims gt 1 then data = transpose(data, reverse(indgen(ndims)))
	endif

	;Turn file validation back on
	if MrCmpVersion('8.0') le 0 then $
		if validate eq 0 then cdf_set_validate, /YES

	return, data
end
