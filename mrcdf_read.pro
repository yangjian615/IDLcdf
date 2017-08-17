; docformat = 'rst'
;
; NAME:
;       MrCDF_Read
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
;   Read data and its dependencies from a CDF data file.
;
; :Categories:
;       CDF Utilities
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
;       2015/04/30  -   A CDF ID can be provided instead of a file name. - MRA
;       2015/05/13  -   Epoch type determined VARNAME is not the Epoch variable. All
;                           DEPEND_0 records read when searching for time interval. - MRA
;       2016/07/13  -   Read only one record for DEPEND_N variables if their record
;                           variance is 'NOVARY'. - MRA
;       2016/07/21  -   DEPEND_0 is overwritten if passed in as a defined variable.
;                           Fixes bug when matching values to a time interval. - MRA
;       2017/03/16  -   Update to account for name changes in program library. - MRA
;-
;*****************************************************************************************
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
;       BOUNDS:             in, out, optional, type=string, default=''
;                           A string if the form of IDL's array indexing that specifies
;                               the elements within `DATA` to be read. E.g.::
;                                   `DATA` = bytarr(10, 10)
;                                   BOUNDS = '[0:10:2, 5:*]'
;                               is the same as `DATA`[0:10:2, 5:*]. See MrArray_Bounds for
;                               more examples. If BOUNDS is provided, `OFFSET`, `COUNT`
;                               and `INTERVAL`, `REC_START`, `REC_COUNT` and `REC_INTERVAL`
;                               will be set automatically and any inputs will be ignored
;                               and overwritten. If `TIME` is set, this keyword is ignored.
;       COUNT:              in, optional, type=intarr
;                           Vector containing the counts to be used when reading each
;                               `VALUE`. The default is to read each record, taking
;                               into account `INTERVAL` and `OFFSET`.
;       INTERVAL:           in, optional, type=intarr, default=1 for each dimension
;                           Interval between values in each dimension.
;       PATTERN:            in, optional, type=boolean, default="%Y-%M-%dT%H:%m:%S%z"
;                           If set, then `REC_START` and `REC_END` are times that will be
;                               parsed into CDF epoch values. PATTERN describes how the
;                               times should be parsed and accepts any pattern recognized
;                               by MrTimeParser.pro. Automatically sets `TIME`=1.
;       OBOUNDS:            out, optional, type=lonarr
;                           A [nDims, 3] array where "nDims" is the number of
;                               dimensions of `DATA` and the rows will have the form:
;                               [offset, count, interval]. It serves as a compact manner
;                               of returning `REC_START`, `REC_COUNT`, `REC_INTERVAL`, 
;                               `OFFSET`, `COUNT` and `INTERVAL`, if they are not given.
;       OFFSET:             in, optional, type=intarr, default=0 for each dimension
;                           Array indices within the specified record(s) at which to
;                               begin reading. OFFSET is a 1-dimensional array
;                               containing one element per CDF dimension.
;       REC_COUNT:          in, optional, type=long, default=maxrec+1
;                           Number of records to be read.
;       REC_INTERVAL:       in, optional, type=integer, default=1
;                           Interval between records when reading multiple records.
;       REC_START:          in, optional, type=integer, default=0
;                           Record at which to begin reading data.
;       REC_END:            in, optional, type=integer
;                           If set, the last record to read. `REC_COUNT` will be set
;                               as `REC_END` - `REC_START` + 1.
;       STATUS:             out, optional, type=integer
;                           Returns 1 if successful and 0 otherwise. If present and
;                               reading is unsuccessful, the error message will not
;                               be issued.
;       STRING:             in, optional, type=boolean, default=0
;                           If set, "CDF_CHAR" and "CDF_UCHAR" data will be converted
;                               to strings. The are read from the file as byte-arrays.
;       TIME:               in, optional, type=boolean, default=0
;                           If set, `REC_START` and `REC_END` represent the time interval,
;                               in CDF_EPOCH, CDF_EPOCH16, or CDF_TIME_TT2000 format
;                               over which data should be read. It is then assumed that
;                               a "DEPEND_0" variable attribute exists for `VARIABLE` and
;                               that its values are epoch times. The epoch type of TIME
;                               must match that of DEPEND_0. If either `REC_START` or
;                               `REC_END` are strings, `TIME` is automatically set.
;       DATATYPE:           out, optional, type=string
;                           CDF datatype of the variable being read.
;       FILLVALUE:          out, optional, type=any
;                           Value used as a filler for missing data.
;       PADVALUE:           out, optional, type=any
;                           Value used to pad the data when more data is read than what
;                               exists in the file. It is possible that this value does
;                               not exist.
;       VALIDATE:           in, optional, type=boolean, default=0
;                           If set, the CDF file will be validated when opened.
;-
function MrCDF_Read, file, varName, $
;INPUT
BOUNDS=bounds, $
COUNT=count, $
INTERVAL=interval, $
OBOUNDS=oBounds, $
OFFSET=offset, $
PATTERN=pattern, $
REC_COUNT=rec_count, $
REC_END=rec_end, $
REC_INTERVAL=rec_interval, $
REC_START=rec_start, $
STATUS=status, $
STRING=string, $
TIME=time, $
VALIDATE=validate, $
;OUTPUT
DEPEND_0=depend_0, $
DEPEND_1=depend_1, $
DEPEND_2=depend_2, $
DEPEND_3=depend_3, $
DATATYPE=datatype, $
FILLVALUE=fillvalue, $
PADVALUE=padvalue
    compile_opt strictarr

    ;catch errors
    catch, the_error
    if the_error ne 0 then begin
        catch, /CANCEL
        
        ;Close the file
        if n_elements(cdfID) gt 0 && tf_opened then cdf_close, cdfID
    
        ;Turn file validation back on
        if MrCmpVersion('8.0') le 0 then $
            if validate eq 0 then cdf_set_validate, /YES
        
        ;Log the error
        status = 0
        if arg_present(status) then MrPrintF, 'LogErr'
        return, -1
    endif
    
    ;Defaults
    status   = 1
    oBounds  = arg_present(oBounds)
    time     = keyword_set(time)
    validate = keyword_set(validate)
    if n_elements(bounds)  eq 0 then bounds  = ''
    if n_elements(pattern) eq 0 then pattern = ''
    
    ;Dependencies
    if pattern ne '' || MrIsA(rec_start, 'STRING') || MrIsA(rec_end, 'STRING') then time = 1
    if time && pattern eq '' then pattern = "%Y-%M-%dT%H:%m:%S%z"
    
    ;Make sure a variable name was given, not a variable index number
    if MrIsA(varname, 'STRING') eq 0 then message, 'VARNAME must be a string.'
    
    ;DEPEND_0 must be undefined when passed in
    if arg_present(depend_0) && n_elements(depend_0) gt 0 then begin
        depend_0 = 0B
        void     = temporary(depend_0)
    endif

;-----------------------------------------------------
; Open File & Get MetaData \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------

    ;Was a CDF ID given
    if MrIsA(filename, 'LONG') then begin
        tf_opened = 0B
        cdfID     = file
    
    ;Filename given
    endif else begin
        ;Validate the file?
        if validate then if MrCmpVersion('8.0') le 0 $
            then cdf_set_validate, /YES $
            else cdf_set_validate, /NO

        ;Open the file, if it exists
        if file_test(file) eq 0 then message, 'File not found: ' + file
        cdfID = cdf_open(file)
        
        ;Indicate file was opened here
        tf_opened = 1B
    endelse
    
    ;Get information about the variable
    var_inq  = cdf_varinq(cdfID, varName)
    cdf_control, cdfID, GET_VAR_INFO=var_info, VARIABLE=varname
    datatype = var_inq.datatype
    if MrStruct_HasTag(var_info, 'PADVALUE') then padvalue = var_info.padvalue

;-----------------------------------------------------
; Time Range? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Flag indicating that the variable given represents time.
    isTime = 0
    if time eq 1 then begin
    ;-----------------------------------------------------
    ; Get Time \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    ;-----------------------------------------------------
        ;Is the requested variable that of time?
        if max(datatype eq ['CDF_EPOCH', 'CDF_EPOCH16', 'CDF_TIME_TT2000']) eq 1 then begin
            isTime   = 1B
            tf_has   = 0B
            cdf_varget, cdfID, varname, depend_0, REC_COUNT=var_info.maxrec+1
            epoch_type = datatype
        
        endif else begin
            ;Get the name of the epoch variable
            cdf_attget_entry, cdfID, 'DEPEND_0', varname, entryType, tVarName, tf_has
            
            ;Does the attribute exist?
            if ~tf_has then $
                message, 'No DEPEND_0 attribute exists for variable "' + varname + '". Reading all records.', /INFORMATIONAL
            
            ;Get the epoch type
            dep0_inq   = cdf_varinq(cdfID, tVarName)
            epoch_type = dep0_inq.datatype
            
            ;Get the number of records
            cdf_control, cdfID, GET_VAR_INFO=dep0_info, VARIABLE=tVarName
            
            ;Get the data
            cdf_varget, cdfID, tVarName, depend_0, REC_COUNT=dep0_info.maxrec+1
        endelse

        ;Is there a DEPEND_0 variable?
        if tf_has then begin
        ;-----------------------------------------------------
        ; Convert Interval to Epoch Values \\\\\\\\\\\\\\\\\\\
        ;-----------------------------------------------------
            if MrIsA(rec_start, 'STRING') $
                then epoch_start = MrCDF_Epoch_Parse(rec_start, PATTERN=pattern, EPOCH_TYPE=epoch_type) $
                else epoch_start = n_elements(rec_start) gt 0 ? rec_start : depend_0[0]
                
            if MrIsA(rec_end, 'STRING') $
                then epoch_end = MrCDF_Epoch_Parse(rec_end, PATTERN=pattern, EPOCH_TYPE=epoch_type) $
                else epoch_end = n_elements(rec_end) gt 0 ? rec_end : depend_0[n_elements(depend_0)-1]

        ;-----------------------------------------------------
        ; Find Record Range \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
        ;-----------------------------------------------------
            ;REC_START and REC_COUNT are needed
            comp = MrCDF_Epoch_Compare(depend_0, epoch_start, epoch_end)
            iMatch = where(comp eq 1, rec_count)
            if rec_count eq 0 then begin
                t0 = MrCDF_Epoch_Encode(depend_0[0])
                t1 = MrCDF_Epoch_Encode(depend_0[n_elements(depend_0)-1])
                msg = string(FORMAT='(%"No records found between REC_START and REC_END. ' + $
                                    'Data range %s - %s.")', t0, t1)
                message, msg
            endif else begin
                rec_start_out = min(iMatch)
            endelse
        endif

;-----------------------------------------------------
; Bounds? \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if bounds ne '' then begin
        ;Form the dimension sizes of the variable
        nDims = n_elements(var_inq.dim)
        dims = [var_inq.dim, var_info.maxrec + 1]
        if nDims eq 1 && var_inq.dim eq 0 then nDims = 0

        ;Get the records to be read.
        indexing      = MrArray_Bounds(dims, bounds, /DIMENSIONS, /COUNT, SINGLE=single)
        rec_start_out = reform(indexing[nDims, 0])
        rec_count     = reform(indexing[nDims, 1])
        rec_interval  = reform(indexing[nDims, 2])
        
        ;Cannot do single dimension indexing (have not tried).
        if single and nDims gt 0 then $
            message, 'Invalid number of dimensions in BOUNDS.' 
        
        ;Dimensions to read
        if nDims gt 1 then begin
            offset   = reform(indexing[0:nDims-1, 0])
            count    = reform(indexing[0:nDims-1, 1])
            interval = reform(indexing[0:nDims-1, 2])
        endif
;-----------------------------------------------------
; Record Interval \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    endif else if n_elements(rec_end) gt 0 then begin
        rec_start_out = rec_start
        rec_count     = rec_end - rec_start + 1
    endif

;-----------------------------------------------------
;Get the Data \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Default to reading all reacords
    if n_elements(rec_start_out) eq 0 then rec_start_out = 0
    if n_elements(rec_count)     eq 0 then rec_count     = var_info.maxrec + 1 - rec_start_out
    if n_elements(rec_interval)  eq 0 then rec_interval  = 1

    ;Get the data
    if isTime eq 0 then begin
        cdf_varget, cdfID, varname, data, $
                    REC_START=rec_start_out, REC_COUNT=rec_count, REC_INTERVAL=rec_interval, $
                    OFFSET=offset, COUNT=count, INTERVAL=interval, STRING=string
    endif else begin
        data = depend_0[*,rec_start_out:rec_start_out+rec_count-1:rec_interval]
    endelse

;-----------------------------------------------------
; DEPEND_0 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if arg_present(depend_0) then begin
        ;Was the data read to get the record interval?
        if n_elements(depend_0) gt 0 then begin
            depend_0 = depend_0[*,rec_start_out:rec_start_out+rec_count-1:rec_interval]
        
        ;Read the data
        endif else begin
            ;Get the variable name of the DEPEND_0 attribute
            cdf_attget_entry, cdfID, 'DEPEND_0', varname, entryType, dep0VarName, has_dep0

            ;Does it exist?
            if has_dep0 then begin
                ;Does it have record variance?
                varinq = cdf_varinq(cdfID, dep0VarName)
                rstart = varinq.recvar eq 'VARY' ? rec_start_out : 0
                rcount = varinq.recvar eq 'VARY' ? rec_count     : 1
                
                ;Get the data
                cdf_varget, cdfID, dep0VarName, depend_0, $
                            REC_START=rstart, REC_COUNT=rcount, REC_INTERVAL=rec_interval, $
                            OFFSET=offset, COUNT=count, INTERVAL=interval
            endif
        endelse
    endif

;-----------------------------------------------------
; DEPEND_1 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if arg_present(depend_1) then begin
        ;Get the variable name of the DEPEND_1 attribute
        cdf_attget_entry, cdfID, 'DEPEND_1', varname, entryType, dep1VarName, has_dep1
        
        ;Does it exist?
        if has_dep1 then begin
            ;Does it have record variance?
            varinq = cdf_varinq(cdfID, dep1VarName)
            rstart = varinq.recvar eq 'VARY' ? rec_start_out : 0
            rcount = varinq.recvar eq 'VARY' ? rec_count     : 1
                
            ;Get the data
            cdf_varget, cdfID, dep1VarName, depend_1, $
                        REC_START=rstart, REC_COUNT=rcount, REC_INTERVAL=rec_interval, $
                        OFFSET=offset, COUNT=count, INTERVAL=interval
        endif
    endif

;-----------------------------------------------------
; DEPEND_2 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if arg_present(depend_2) then begin
        ;Get the variable name of the DEPEND_2 attribute
        cdf_attget_entry, cdfID, 'DEPEND_2', varname, entryType, dep2VarName, has_dep2
        
        ;Does it exist?
        if has_dep2 then begin
            ;Does it have record variance?
            varinq = cdf_varinq(cdfID, dep2VarName)
            rstart = varinq.recvar eq 'VARY' ? rec_start_out : 0
            rcount = varinq.recvar eq 'VARY' ? rec_count     : 1
                
            ;Get the data
            cdf_varget, cdfID, dep2VarName, depend_2, $
                        REC_START=rstart, REC_COUNT=rcount, REC_INTERVAL=rec_interval, $
                        OFFSET=offset, COUNT=count, INTERVAL=interval
        endif
    endif

;-----------------------------------------------------
; DEPEND_3 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    if arg_present(depend_3) then begin
        ;Get the variable name of the DEPEND_3 attribute
        cdf_attget_entry, cdfID, 'DEPEND_3', varname, entryType, dep3VarName, has_dep3
        
        ;Does it exist?
        if has_dep3 then begin
            ;Does it have record variance?
            varinq = cdf_varinq(cdfID, dep3VarName)
            rstart = varinq.recvar eq 'VARY' ? rec_start_out : 0
            rcount = varinq.recvar eq 'VARY' ? rec_count     : 1
                
            ;Get the data
            cdf_varget, cdfID, dep3VarName, depend_3, $
                        REC_START=rstart, REC_COUNT=rcount, REC_INTERVAL=rec_interval, $
                        OFFSET=offset, COUNT=count, INTERVAL=interval
        endif
    endif
    
    ;Close the data file
    if tf_opened then cdf_close, cdfID
    
    ;Turn file validation back on
    if MrCmpVersion('8.0') le 0 then $
        if validate eq 0 then cdf_set_validate, /YES
    
    return, data
end
