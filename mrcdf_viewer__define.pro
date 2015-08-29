; docformat = 'rst'
;
; NAME:
;       MrCDF_Viewer__DEFINE
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
;       A class for plotting variable data from CDF files.
;
; :Author:
;       Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 348
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :Categories:
;       CDF Utilities, File I/O
;
; :History:
;   Modification History::
;       2015/03/03  -   Written by Matthew Argall
;       2015/08/28  -   Make use of the BOUNDS keyword to MrCDF_File::Read. - MRA
;-
;*****************************************************************************************
;+
;   The purpose of this method is to create a structure containing the properly defined
;   variable attributes. If an older or non-standard CDF file has used different variable,
;   attribute names, these can be changed here without having to alter the program.
;
;   Definitions of variable attributes can be found `here <http://spdf.gsfc.nasa.gov/istp_guide/istp_guide.html>`
;
; :Private:
;
; :Keywords:
;       CLUSTER:            in, optional, type=boolean, default=0
;                           If set, use Cluster mission configuration.
;-
pro MrCDF_Viewer::Configure, $
CLUSTER=cluster
	compile_opt idl2
	on_error, 2

	self.config = ptr_new({CATDESC:         'CATDESC', $
	                       DELTA_MINUS_VAR: 'DELTA_MINUS_VAR', $
	                       DELTA_PLUS_VAR:  'DELTA_PLUS_VAR', $
	                       DEPEND_0:        'DEPEND_0', $
	                       DEPEND_1:        'DEPEND_1', $
	                       DEPEND_2:        'DEPEND_2', $
	                       DEPEND_3:        'DEPEND_3', $
	                       DISPLAY_TYPE:    'DISPLAY_TYPE', $
	                       FIELDNAM:        'FIELDNAM', $
	                       FILLVAL:         'FILLVAL', $
	                       LABLAXIS:        'LABLAXIS', $
	                       LABL_PTR_1:      'LABL_PTR_1', $
	                       LABL_PTR_2:      'LABL_PTR_2', $
	                       LABL_PTR_3:      'LABL_PTR_3', $
	                       UNITS:           'UNITS', $
	                       UNIT_PTR:        'UNIT_PTR', $
	                       VALIDMIN:        'VALIDMIN', $
	                       VALIDMAX:        'VALIDMAX', $
	                       VAR_NOTES:       'VAR_NOTES', $
	                       VAR_TYPE:        'VAR_TYPE', $
	                       SCALETYP:        'SCALETYP', $
	                       SCAL_PTR:        'SCAL_PTR', $
	                       SCALEMIN:        'SCALEMIN', $
	                       SCALEMAX:        'SCALEMAX'})

	;Configuration for the CLUSTER satellite mission
	if keyword_set(cluster) then begin
		(*self.config).delta_minus_var = 'DELTA_MINUS'
		(*self.config).delta_plus_var  = 'DELTA_PLUS'
		(*self.config).display_type    = 'DISPLAYTYPE'
		(*self.config).labl_ptr_1      = 'REPRESENTATION_1'
		(*self.config).labl_ptr_2      = 'REPRESENTATION_2'
		(*self.config).labl_ptr_3      = 'REPRESENTATION_3'
		(*self.config).var_type        = 'PARAMETER_TYPE'
	endif
end


;+
;   The purpose of this method is to destroy the object.
;-
pro MrCDF_Viewer::Destroy
    obj_destroy, self
end


;+
;   The purpose of this method is to get the names of the variable attributes in the
;   current configuration. Definitions of variable attributes can be found at the NASA
;   Goddard's Space Flight Data Facility  `International Solar-Terrestrial Physics 
;   <http://spdf.gsfc.nasa.gov/istp_guide/istp_guide.html>` webpage.
;
;   Some CDF files do not follow the attribute naming conventions found on the webpage,
;   so this method offers a simple way of changing them.
;
; :Private:
;
; :Keywords:
;       CATDESC:            in, optional, type=string
;                           Name being used for the 'CATDESC' variable attribute.
;       DEPEND_0:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_0'.
;       DEPEND_1:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_0'.
;       DEPEND_2:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_1'.
;       DEPEND_3:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_2'.
;       DELTA_MINUS_VAR:    in, optional, type=string
;                           Name being used for the 'DELTA_MINUS_VAR' variable attribute.
;       DELTA_PLUS_VAR:     in, optional, type=string
;                           Name being used for the 'DELTA_PLUS_VAR' variable attribute.
;       DISPLAY_TYPE:       in, optional, type=string
;                           Name being used for the 'DISPLAY_TYPE' variable attribute.
;       FIELDNAM:           in, optional, type=string
;                           Name being used for the 'FIELDNAM' variable attribute.
;       FILLVAL:            in, optional, type=string
;                           Name being used for the 'FILLVAL' variable attribute.
;       LABLAXIS:           in, optional, type=string
;                           Name being used for the 'LABLAXIS' variable attribute.
;       LABL_PTR_1:         in, optional, type=string
;                           Name of the attribute that is a substitute for 'LABL_PTR_1'.
;       LABL_PTR_2:         in, optional, type=string
;                           Name being used for the 'LABLAXIS' variable attribute.
;       LABL_PTR_3:         in, optional, type=string
;                           Name of the attribute that is a substitute for 'LABL_PTR_3'.
;       UNITS:              in, optional, type=string
;                           Name being used for the 'UNITS' variable attribute.
;       UNIT_PTR:           in, optional, type=string
;                           Name being used for the 'UNIT_PTR' variable attribute.
;       VALIDMIN:           in, optional, type=string
;                           Name being used for the 'VALIDMIN' variable attribute.
;       VALIDMAX:           in, optional, type=string
;                           Name being used for the 'VALIDMAX' variable attribute.
;       VAR_NOTES:           in, optional, type=string
;                           Name being used for the 'VAR_NOTES' variable attribute.
;       VAR_TYPE:           in, optional, type=string
;                           Name being used for the 'VAR_TYPE' variable attribute.
;       SCALETYP:           in, optional, type=string
;                           Name being used for the 'SCALETYP' variable attribute.
;       SCAL_PTR:           in, optional, type=string
;                           Name being used for the 'SCAL_PTR' variable attribute.
;       SCALEMIN:           in, optional, type=string
;                           Name being used for the 'SCALEMIN' variable attribute.
;       SCALEMAX:           in, optional, type=string
;                           Name being used for the 'SCALEMAX' variable attribute.
;-
pro MrCDF_Viewer::GetConfig, $
CATDESC = catdesc, $
DELTA_MINUS_VAR = delta_minus_var, $
DELTA_PLUS_VAR = delta_plus_var, $
DEPEND_0 = depend_0, $
DEPEND_1 = depend_1, $
DEPEND_2 = depend_2, $
DEPEND_3 = depend_3, $
DISPLAY_TYPE = display_type, $
FIELDNAM = fieldnam, $
FILLVAL = fillval, $
LABLAXIS = lablaxis, $
LABL_PTR_1 = labl_ptr_1, $
LABL_PTR_2 = labl_ptr_2, $
LABL_PTR_3 = labl_ptr_3, $
UNITS = units, $
UNIT_PTR = unit_ptr, $
VALIDMIN = validmin, $
VALIDMAX = validmax, $
VAR_NOTES = var_notes, $
VAR_TYPE = var_type, $
SCALETYP = scaletyp, $
SCAL_PTR = scal_ptr, $
SCALEMIN = scalemin, $
SCALEMAX = scalemax
    compile_opt idl2
    on_error, 2

    ;Set the variable attribute names.
    if arg_present(catdesc)         ne 0 then catdesc         = (*self.config).catdesc
    if arg_present(delta_minus_var) ne 0 then delta_minus_var = (*self.config).delta_minus_var
    if arg_present(delta_plus_var)  ne 0 then delta_plus_var  = (*self.config).delta_plus_var
    if arg_present(depend_0)        ne 0 then depend_0        = (*self.config).depend_0
    if arg_present(depend_1)        ne 0 then depend_1        = (*self.config).depend_1
    if arg_present(depend_2)        ne 0 then depend_2        = (*self.config).depend_2
    if arg_present(depend_3)        ne 0 then depend_3        = (*self.config).depend_3
    if arg_present(display_type)    ne 0 then display_type    = (*self.config).display_type
    if arg_present(fieldnam)        ne 0 then fieldnam        = (*self.config).fieldnam
    if arg_present(fillval)         ne 0 then fillval         = (*self.config).fillval
    if arg_present(lablaxis)        ne 0 then lablaxis        = (*Self.config).lablaxis
    if arg_present(labl_ptr_1)      ne 0 then labl_ptr_1      = (*self.config).labl_ptr_1
    if arg_present(labl_ptr_2)      ne 0 then labl_ptr_2      = (*self.config).labl_ptr_2
    if arg_present(labl_ptr_3)      ne 0 then labl_ptr_3      = (*self.config).labl_ptr_3
    if arg_present(units)           ne 0 then units           = (*self.config).units
    if arg_present(unit_ptr)        ne 0 then unit_ptr        = (*self.config).unit_ptr
    if arg_present(validmin)        ne 0 then validmin        = (*self.config).validmin
    if arg_present(validmax)        ne 0 then validmax        = (*self.config).validmax
    if arg_present(var_notes)       ne 0 then var_notes       = (*self.config).var_notes
    if arg_present(var_type)        ne 0 then var_type        = (*self.config).var_type
    if arg_present(scaletyp)        ne 0 then scaletyp        = (*self.config).scaletyp
    if arg_present(scal_ptr)        ne 0 then scal_ptr        = (*self.config).scal_ptr
    if arg_present(scalemin)        ne 0 then scalemin        = (*self.config).scalemin
    if arg_present(scalemax)        ne 0 then scalemax        = (*self.config).scalemax
end


;+
;   The purpose of this method is to create a time series plot object based on the data
;   returned from PLOT_CDF.
;
; :Private:
;
; :Params:
;       VARNAME:            in, required, type=structure
;                           Name of the variable to be displayed.
;       DISPLAY_DEP:        in, required, type=string
;                           DEPEND_[123] variable to display.
;
; :Keywords:
;       NAME:               in, optional, type=string/strarr(2), default=[`VARNAME`\, 'CB: `VARNAME`']
;                           Name of the image and colorbar.
;       SUM_PAGES:          in, optional, type=boolean, default=0
;                           If set, dimensions 3+ of the image data will be summed.
;
; :Returns:
;       gImage:             A MrImage graphics object.
;-
function MrCDF_Viewer::Image, varname, display_dep, $
NAME=name, $
SUM_PAGES=sum_pages
	compile_opt strictarr
	on_error, 2

	sum_pages = keyword_set(sum_pages)
	
	;Name of the object
	if n_elements(name) eq 0 $
		then name = [varname, 'Legend: ' + varname] $
		else if n_elements(name) eq 1 then name = [name[0], 'Legend: ' + name[0]]

;---------------------------------------------------------------------
; Data ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get the dependent data
	case display_dep of
		'DEPEND_1': begin
			img       = self.oCDF -> Read(varname, DEPEND_0=x, DEPEND_1=y, _STRICT_EXTRA=extra)
			y_varname = self.oCDF -> GetVarAttrValue(varname, 'DEPEND_1')
		endcase
		'DEPEND_2': begin
			img       = self.oCDF -> Read(varname, DEPEND_0=x, DEPEND_2=y, _STRICT_EXTRA=extra)
			y_varname = self.oCDF -> GetVarAttrValue(varname, 'DEPEND_2')
		endcase
		'DEPEND_3': begin
			img       = self.oCDF -> Read(varname, DEPEND_0=x, DEPEND_3=y, _STRICT_EXTRA=extra)
			y_varname = self.oCDF -> GetVarAttrValue(varname, 'DEPEND_3')
		endcase
		else: message, 'Unknown depend variable "' + depend_dim + '". Choose DEPEND_[123].'
	endcase
	
	;Re-order the image to be [x, y, ...]
	nDims = size(img, /N_DIMENSIONS)
	case size(img, /N_DIMENSIONS) of
		2: img = transpose(img, [0,1])
		3: img = display_dep eq 'DEPEND_1' ? transpose(img, [2, 1, 0])    : transpose(img, [2, 0, 1])
		4: img = display_dep eq 'DEPEND_1' ? transpose(img, [3, 2, 0, 1]) : $
		         display_dep eq 'DEPEND_2' ? transpose(img, [3, 1, 0, 2]) : transpose(img, [3, 0, 1, 2])
		else: message, 'Image has unexpected number of dimensions (' + strtrim(nDims, 2) + ').'
	endcase
	

;---------------------------------------------------------------------
; Image Properties ///////////////////////////////////////////////////
;---------------------------------------------------------------------
	self -> ReadMetadata, y_varname, $
	                      AXTITLE     = cb_title, $
	                      FILLVAL     = missing_value, $
	                      LABEL       = label, $
	                      LOG         = cb_log, $
	                      MAXVALUE    = cb_maxvalue, $
	                      MINVALUE    = cb_minvalue, $
	                      TITLE       = img_title, $
	                      UNITS       = cb_units, $
	                     _STRICT_EXTA = extra
	
	;Make corrections
	if cb_title eq '' then cb_title = title
	if cb_units ne '' then cb_title = cb_title eq '' ? cb_units : cb_title + '!C(' + cb_units + ')'

	;Was the fill value a lie?
	;   - If the minimum is zero, then count 0 as the fill value
	iBad = where(img eq missing_value, nBad)
	if nBad eq 0 then if min(img) eq 0 then missing_value = 0

	;Log-scale?
	;   - Log-scale if the data range is greater than two orders of magnitude.
	if n_elements(cb_scale) eq 0 then begin
		iGood     = where( (finite(img) eq 1) and (img ne missing_value), nGood)
		img_range = [min(img[iGood], MAX=imMax), imMax]
		
		;Can only log-scale if the data range is entirely > 0.
		if img_range[0] gt 0 && img_range[1] gt 0 $
			then cb_log = ( (alog10(img_range[1]) - alog10(img_range[0]) ) gt 2 )
	endif
	
;---------------------------------------------------------------------
; X-Axis Properties //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Variable name
	x_varname = self.oCDF -> GetVarAttrValue(varname, 'DEPEND_0')

	;Metadata
	self -> ReadMetadata, x_varname, $
	                      AXTITLE      = xtitle, $
	                      ERROR_LOW    = x_delta_minus, $
	                      ERROR_HIGH   = x_delta_plus, $
	                      LABEL        = label, $
	                      LOG          = xlog, $
	                      TITLE        = title, $
	                      UNITS        = xunits, $
	                     _STRICT_EXTRA = extra
	
	;Make corrections
	if xtitle eq '' then xtitle = title
	if xunits ne '' then xtitle = xtitle eq '' ? xunits : xtitle + ' (' + xunits + ')'
	
	;Convert from epoch to seconds
	;   - Determine the day of the first data point.
	epoch_type = MrCDF_Epoch_Type(x[0])
	MrCDF_Epoch, x[0], yr, mo, day, /BREAKDOWN_EPOCH, EPOCH_TYPE=epoch_type
	MrCDF_Epoch, t0, yr, mo, day, /COMPUTE_EPOCH, EPOCH_TYPE=epoch_type

	;Extract the YYYY-MM-DD portion of the data
	epoch_string = MrCDF_Epoch_Encode(t0, EPOCH=3)
	date         = strmid(epoch_string, 0, 10)
	
	;Convert to seconds
	case epoch_type of
		'CDF_EPOCH':       x = (x - t0) * 1d-3
		'CDF_EPOCH16':     x = (real_part(x) - real_part(t0)) + imaginary(x) * 1d-12
		'CDF_TIME_TT2000': x = (x - t0) * 1d-9
		else: message, 'Invalid epoch type: "' + epoch_type + '".'
	endcase

	;Reset the xtitle
	xtitle = 'Time Since ' + date


;---------------------------------------------------------------------
; Y-Axis Properties //////////////////////////////////////////////////
;---------------------------------------------------------------------
	self -> ReadMetadata, y_varname, $
	                      AXTITLE     = ytitle, $
	                      ERROR_LOW   = y_delta_minus, $
	                      ERROR_HIGH  = y_delta_plus, $
	                      FILLVAL     = fillval, $
	                      LABEL       = label, $
	                      LOG         = ylog, $
	                      TITLE       = title, $
	                      UNITS       = yunits, $
	                     _STRICT_EXTA = extra
	
	;Make corrections
	if ytitle eq '' then ytitle = title
	if yunits ne '' then ytitle = ytitle eq '' ? yunits : ytitle + '!C(' + yunits + ')'
	
	;Ensure it is monotonically increasing
	if n_elements(fillval) eq 0 then fillval = missing_value
	iGood = where(y ne fillval, nGood)
	if nGood gt 0 then if y[iGood[0]] gt y[iGood[nGood-1]] then begin
		y = reverse(y)
		if n_elements(y_delta_minus) gt 0 then y_delta_minus = reverse(y_delta_minus, 1)
		if n_elements(y_delta_plus)  gt 0 then y_delta_plus  = reverse(y_delta_plus,  1)
		img = reverse(img, 2)
	endif

;---------------------------------------------------------------------
;Manipulate the Image ////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Sum over the extra dimensions?
	if sum_pages then if nDims gt 2 $
		then for i = nDims, 3, -1 do img = total(img, i, /NAN)

;---------------------------------------------------------------------
;Create the Image ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Increase the window margins
	win = GetMrWindows(/CURRENT)
	win -> GetProperty, OXMARGIN=oxmargin
	if oxmargin[1] lt 15 then win -> SetProperty, OXMARGIN=[oxmargin[0], 15]

	;Create the image
	gImage = MrImage(img, x, y, x_delta_minus, y_delta_minus, x_delta_plus, y_delta_plus, $
	                 /AXES, $
	                 /CURRENT, $
	                 /SCALE, $
	                 CTINDEX       = 13, $
	                 MISSING_COLOR = 'Antique White', $
	                 MISSING_VALUE = missing_value, $
;	                 NAME=onames[0], $
	                 LOG           = cb_log, $
	                 TITLE         = img_title, $
	                 XTICKFORMAT   = 'time_labels', $
	                 XTITLE        = xtitle, $
	                 XLOG          = xlog, $
	                 YTITLE        = ytitle, $
	                 YLOG          = ylog)

	;Put the colorbar in the same window as the image
	gColorbar = MrColorbar(TARGET = gImage, $
	                       TITLE  = cb_title, $
;	                       NAME   = onames[1], $
	                       YLOG   = cb_log)

    return, gImage
end


;+
;   The purpose of this method is to plot data from a CDF object.
;
; :Params:
;       VARNAME:                in, optional, type=string
;                               The name of the variable whose data is to be plotted. If
;                                   not provided, a dialog box will appear from which one
;                                   can be selected.
;
; :Keywords:
;       CURRENT:                in, optional, type=boolean, default=0
;                               If set, graphics are added to the current MrWindow
;                                   graphics window.
;       DISPLAY_DEP:            in, optional, type=string, default='DEPEND_1'
;                               The dependent variable to be displayed. Choices are
;                                   'DEPEND_1', 'DEPEND_2', or 'DEPEND_3'.
;       FORCE_IMAGE:            in, optional, type=boolean, default=0
;                               If set, time-series data will be plotted as an image, not
;                                   as a line plot. This requires `VARIABLE` to have a
;                                   DEPEND_1 attribute and for its "Display Type" to be
;                                   "Time Series".
;       GROUP_LEADER:           in, optional, type=integer
;                               The group leader of the variable selection gui. Use only when
;                                   `VARIABLE` is undefined.
;       NAME:                   in, optional, type=string, default=`VARIABLE`
;                               A scalar string or a 2-element array of strings indicating
;                                   the name of the graphic[, annotation] created. By
;                                   default, the variable name is used. In the case of
;                                   annotations, it is '[Annotation type]: `VARIABLE`'.
;       SUM_PAGES:              in, optional, type=boolean, default=0
;                               If set, then images with more than two dimensions will
;                                   have their extra dimension summed. This option is
;                                   is ignored if `DISPLAY_TYPE` is not "3D_SPECTROGRAM".
;       _REF_EXTRA:             in, optional, type=structure
;                               Any keyword accepted by the MrCDF_File::READ method.
;
; :Returns:
;       GFX:                    A MrGraphics window object in which the data is displayed.
;-
function MrCDF_Viewer::View, varname, $
 BOUNDS = bounds, $
 CURRENT = current, $
 DISPLAY_DEP = display_dep, $
 FORCE_IMAGE = force_image, $
 GROUP_LEADER = group_leader, $
 NAME = name, $
 SUM_PAGES = sum_pages, $
_REF_EXTRA = extra
	compile_opt strictarr

	;Error handling
	catch, the_error
	if the_error ne 0 then begin
		catch, /CANCEL
		if current eq 0 then if obj_valid(gWin) then obj_destroy, gWin
		void = cgErrorMsg()
		return, obj_new()
	endif

	current     = keyword_set(current)
	force_image = keyword_set(force_image)
	if n_elements(display_dep) eq 0 then display_dep = 'DEPEND_1'
	
	;Was a variable name given?
	if n_elements(varname) eq 0 then varname = self -> QueryVarname(BOUNDS=bounds, DEPEND=depend)
	if varname eq '' then return, obj_new()
	
	;What type of plot?
	if self.oCDF -> HasAttr('DISPLAY_TYPE') then begin
		display_type = self.oCDF -> GetVarAttrValue(varname, 'DISPLAY_TYPE')
	endif else begin
		case 1 of
			self.oCDF -> HasAttr('DEPEND_3'): display_type = 'SPECTROGRAM'
			self.oCDF -> HasAttr('DEPEND_2'): display_type = 'SPECTROGRAM'
			self.oCDF -> HasAttr('DEPEND_1'): display_type = 'SPECTROGRAM'
			self.oCDF -> HasAttr('DEPEND_0'): display_type = 'TIME_SERIES'
			else: message, 'Display type could not be determined.'
		endcase
	endelse

;---------------------------------------------------------------------
;Force an Image Plot? ////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Changing from a line plot to an image?
	if force_image then begin
		;Can we force it to be an image?
		if strupcase(display_type) eq 'TIME_SERIES' then begin
			display_dep = 'DEPEND_1'
		endif else begin
			message, 'Data is not a time series. Setting FORCE_IMAGE=0.', /INFORMATIONAL
			force_image = 0
		endelse
	endif

;---------------------------------------------------------------------
;Create Plot /////////////////////////////////////////////////////////
;---------------------------------------------------------------------

	;Create a new window, if need be.
	;   - Determine if the window will be refreshed afterward.
	if current eq 0 then begin
		gWin       = MrWindow(REFRESH=0)
		refresh_in = 1
	endif else begin
		gWin       = GetMrWindows(/CURRENT)
		refresh_in = gWin -> GetRefresh()
	endelse

	;Create an object.
	case strupcase(display_type) of
		'TIME_SERIES': gfx = self -> Plot(varname, BOUNDS=bounds)
		'SPECTROGRAM': gfx = self -> Image(varname, display_dep, SUM_PAGES=sum_pages)
		else: message, 'Display type "' + display_type + '" not recognized.'
	endcase

	;If CURRENT was not set, then draw the plot
	if refresh_in then gWin -> Refresh
	return, gfx
end


;+
;   The purpose of this method is to create a time series plot object based on the data
;   returned from PLOT_CDF.
;
; :Private:
;
; :Params:
;       VARNAME:            in, required, type=string
;                           Name of the variable to be plotted.
;
; :Returns:
;       GPLOT:              A MrPlot graphics object.
;-
function MrCDF_Viewer::Plot, varname, $
BOUNDS=bounds, $
NAME=name
	compile_opt strictarr
	on_error, 2
	
	;Name of the object
	if n_elements(name) eq 0 $
		then name = [varname, 'Legend: ' + varname] $
		else if n_elements(name) eq 1 then name = [name[0], 'Legend: ' + name[0]]

;---------------------------------------------------------------------
; Data ///////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Get the dependent data
	y = self.oCDF -> Read(varname, $
	                      BOUNDS        = bounds, $
	                      DEPEND_0      = x, $
	                      _STRICT_EXTRA = extra)

;---------------------------------------------------------------------
; X-Axis Properties //////////////////////////////////////////////////
;---------------------------------------------------------------------
	;Variable name
	x_varname = self.oCDF -> GetVarAttrValue(varname, 'DEPEND_0')

	;Metadata
	self -> ReadMetadata, x_varname, $
	                      AXTITLE     = xtitle, $
	                      LABEL       = label, $
	                      LOG         = xlog, $
	                      TITLE       = title, $
	                      UNITS       = xunits
	
	;Make corrections
	if xtitle eq '' then xtitle = title
	if xunits ne '' then xtitle = xtitle eq '' ? xunits : xtitle + ' (' + xunits + ')'

;---------------------------------------------------------------------
; Y-Axis Properties //////////////////////////////////////////////////
;---------------------------------------------------------------------
	self -> ReadMetadata, varname, $
	                      AXTITLE     = ytitle, $
	                      ERROR_LOW   = error_low, $
	                      ERROR_HIGH  = error_high, $
	                      FILLVAL     = fillval, $
	                      LABEL       = label, $
	                      LOG         = ylog, $
	                      MAXVALUE    = maxvalue, $
	                      MINVALUE    = minvalue, $
	                      TITLE       = title, $
	                      UNITS       = yunits, $
	                     _STRICT_EXTA = extra
	
	;Make corrections
	if ytitle eq '' then ytitle = title
	if yunits ne '' then ytitle = ytitle eq '' ? yunits : ytitle + '!C(' + yunits + ')'
	
	;Axis range
	if n_elements(fillval) gt 0 then begin
		isdata  = where(y ne fillval)
		yrange = [min((y)[isdata], max=max_data), max_data]
	endif else begin
		yrange = [min(y, max=max_data), max_data]
	endelse

;---------------------------------------------------------------------
;Convert from Epoch //////////////////////////////////////////////////
;---------------------------------------------------------------------
	t_ssm = MrCDF_epoch2ssm(x)
	
;---------------------------------------------------------------------
;Create the Plot Object //////////////////////////////////////////////
;---------------------------------------------------------------------
	gPlot = MrPlot(t_ssm, y, /CURRENT, $
	               DIMENSION   = size(y, /N_DIMENSIONS), $ 
	               MIN_VALUE   = minvalue[0], $
	               MAX_VALUE   = maxvalue[0], $
	               NAME        = name[0], $
	               TITLE       = title, $
	               XLOG        = xlog, $
	               XTICKFORMAT = 'time_labels', $
	               XTITLE      = xtitle, $
	               YLOG        = ylog, $
	               YRANGE      = yrange, $
	               YTITLE      = ytitle)
	               
	return, gPlot
end


;+
;   Query the user for a variable name.
;
; :Private:
;
; :Keywords:
;       DEPEND:             out, optional, type=string
;                           The DEPEND_[123] variable to be displayed.
;-
function MrCDF_Viewer::QueryVarname, $
BOUNDS = bounds, $
DEPEND = depend
	compile_opt idl2
	on_error, 2

	;Browse for a variable
	;   - A GUI will pop up and the user will select a variable
	oBrowser = obj_new('MrCDF_Browser', self.oCDF, /BLOCK)
	if obj_valid(oBrowser) eq 0 then return, ''
	
	;Get info on selected item
	name = oBrowser -> GetSelection(TYPE=type, SCOPE=scope, VARNAME=varname, BOUNDS=bounds)
	if n_elements(name) eq 0 then return, ''
	obj_destroy, oBrowser

	;Check what was given.
	if type eq 'ATTRIBUTE' then begin
		if scope eq 'GLOBAL' then message, 'Global Attributes cannot be plotted.'
		if MrIsMember(['DEPEND_1', 'DEPEND_1', 'DEPEND_2', 'DEPEND_4'], name) eq 0 $
			then message, 'Select only DEPEND_[1234] variable attributes or variable names.'
		
		;Swap names
		depend = name
		name   = varname
	endif

	return, name
end


;+
;   The purpose of this method is to retrieve and organize time series-related meta-data
;   from a CDF variable into a graphics keywords structure.
;
; :Private:
;
; :Params:
;       VARNAME:            in, required, type=string/int
;                           Variable name for which metadata is to be extracted.
;
; :Keywords:
;       AXTITLE:            out, optional, type=string
;                           Axis title determined from the LABLAXIS or FIELDNAM attribute.
;       ERROR_LOW:          out, optional, type=numeric/array
;                           Lower range of the variable data, determined from the 
;                               DELTA_MINUS_VAR attribute.
;       ERROR_HIGH:         out, optional, type=numeric/array
;                           Lower range of the variable data, determined from the 
;                               DELTA_PLUS_VAR attribute.
;       FILLVAL:            out, optional, type=number
;                           Value used to replace bad data, from the FILLVAL attribute.
;       LABEL:              out, optional, type=string/strarr
;                           A legend label, from the LABL_PTR_1 attribute.
;       LOG:                out, optional, type=boolean
;                           If set, log-scale the axis. From the SCALTYPE attribute.
;       MINVALUE:           out, optional, type=numeric/array
;                           Minimum data value, from the MINVALUE attribute.
;       MAXVALUE:           out, optional, type=numeric/array
;                           Maximum data value, from the MAXVALUE attribute.
;       TITLE:              out, optional, type=string
;                           Graphic title, from the FIELDNAM attribute.
;       UNITS;              out, optional, type=string
;                           Physical units of the data, from the UNITS attribute.
;       _REF_EXTRA          in, optional, type=any
;                           Any keyword accepted by MrCDF_File::Read is also accepted
;                               for keyword inheritance. This is only used if the
;                               DELTA_*_VAR variable attributes have a record variance
;                               of `VARY`.
;-
pro MrCDF_Viewer::ReadMetadata, varname, $
AXTITLE=axtitle, $
ERROR_LOW=error_low, $
ERROR_HIGH=error_high, $
FILLVAL=fillval, $
LABEL=label, $
LOG=log, $
MAXVALUE=maxvalue, $
MINVALUE=minvalue, $
TITLE=title, $
UNITS=units, $
_REF_EXTRA=extra
	compile_opt strictarr
	on_error, 2
	
	self -> GetConfig, DELTA_MINUS_VAR = delta_minus_var, $
	                   DELTA_PLUS_VAR  = delta_plus_var, $
	                   FIELDNAM        = fieldnam, $
	                   FILLVAL         = fillval, $
	                   LABLAXIS        = lablaxis, $
	                   LABL_PTR_1      = labl_ptr_1, $
	                   LABL_PTR_2      = labl_ptr_2, $
	                   LABL_PTR_3      = labl_ptr_3, $
	                   UNITS           = units, $
	                   UNIT_PTR        = unit_ptr, $
	                   VALIDMIN        = validmin, $
	                   VALIDMAX        = validmax, $
	                   SCALETYP        = scaletyp, $
	                   SCAL_PTR        = scal_ptr, $
	                   SCALEMIN        = scalemin, $
	                   SCALEMAX        = scalemax

	;Get the variable object
	tf_has = self.oCDF -> HasVar(varname, OBJECT=oVar)
	if tf_has eq 0 then message, 'VARNAME "' + varname  + '" is not a valid variable name.'

	;TITLE
	if arg_present(title) then if oVar -> HasAttr(fieldnam) $
		then title = oVar -> GetAttrValue(fieldnam) $
		else title = ''

	;Axis title
	if arg_present(axtitle) then if oVar -> HasAttr(lablaxis) $
		then axtitle = oVar -> GetAttrValue(lablaxis) $
		else axtitle = ''

	;Units
	if arg_present(units) then if oVar -> HasAttr(units) $
		then units = oVar -> GetAttrValue(units) $
		else units = ''

	;Scaletyp
	if arg_present(log) then if oVar -> HasAttr(scaletyp) then begin
		scaletype = oVar -> GetAttrValue(scaletyp)
		log       = strupcase(scaletype) eq 'LOG'
	endif

	;Fillval
	if arg_present(fillval) then if oVar -> HasAttr(fillval) $
		then fillval = oVar -> GetAttrValue(fillval)

	;Legend label
	if arg_present(legend_label) then if oVar -> HasAttr(labl_ptr_1) $
		then label = oVar -> GetAttrValue(varname, labl_ptr_1) $
		else label = ''

	;MIN_VALUE
	if arg_present(minvalue) then if oVar -> HasAttr(validmin) $
		then minvalue = oVar -> GetAttrValue(validmin)

	;MAX_VALUE
	if arg_present(maxvalue) then if oVar -> HasAttr(validmax) $
		then maxvalue = oVar -> GetAttrValue(validmax)

	;DELTA_MINUS_VAR -- value and fillvalue
	if arg_present(error_low) then if oVar -> HasAttr(delta_minus_var) then begin
		error_low = oVar -> GetAttrValue(delta_minus_var)

		;Points to a variable name
		if size(error_low, /TNAME) eq 'STRING' then begin
			;Check for record variance
			oVar -> GetProperty, RECVAR=recvar
			if recvar eq 'VARY' $
				then error_low = self.oCDF -> Read(error_low, _EXTRA=extra) $
				else error_low = self.oCDF -> Read(error_low)
		endif
	endif

	;DELTA_PLUS_VAR -- value and fillvalue
	if arg_present(error_high) then if oVar -> HasAttr(delta_plus_var) then begin
		error_high = oVar -> GetAttrValue(delta_plus_var)

		;Points to a variable name
		if size(error_high, /TNAME) eq 'STRING' then begin
			;Check for record variance
			oVar -> GetProperty, RECVAR=recvar
			if recvar eq 'VARY' $
				then error_high = self.oCDF -> Read(error_high, _EXTRA=extra) $
				else error_high = self.oCDF -> Read(error_high)
		endif
	endif
end


;+
;   The purpose of this method is to set the names of the variable attributes.
;   Definitions of variable attributes can be found at the NASA Goddard's
;   Space Flight Data Facility  `International Solar-Terrestrial Physics 
;   <http://spdf.gsfc.nasa.gov/istp_guide/istp_guide.html>` webpage.
;
;   Some CDF files do not follow the attribute naming conventions found on the webpage,
;   so this method offers a simple way of changing them.
;
; :Private:
;
; :Keywords:
;       CATDESC:            in, optional, type=string
;                           Name of the attribute that is a substitute for 'CATDESC'.
;       DEPEND_0:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_0'.
;       DEPEND_1:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_0'.
;       DEPEND_2:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_1'.
;       DEPEND_3:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'DEPEND_2'.
;       DELTA_MINUS_VAR:    in, optional, type=string
;                           Name of the attribute that is a substitute for 'DELTA_MINUS_VAR'.
;       DELTA_PLUS_VAR:     in, optional, type=string
;                           Name of the attribute that is a substitute for 'DELTA_PLUS_VAR'.
;       DISPLAY_TYPE:       in, optional, type=string
;                           Name of the attribute that is a substitute for 'DISPLAY_TYPE'.
;       FIELDNAM:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'FIELDNAM'.
;       FILLVAL:            in, optional, type=string
;                           Name of the attribute that is a substitute for 'FILLVAL'.
;       LABLAXIS:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'LABLAXIS'.
;       LABL_PTR_1:         in, optional, type=string
;                           Name of the attribute that is a substitute for 'LABL_PTR_1'.
;       LABL_PTR_2:         in, optional, type=string
;                           Name of the attribute that is a substitute for 'LABLAXIS'.
;       LABL_PTR_3:         in, optional, type=string
;                           Name of the attribute that is a substitute for 'LABL_PTR_3'.
;       UNITS:              in, optional, type=string
;                           Name of the attribute that is a substitute for 'UNITS'.
;       UNIT_PTR:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'UNIT_PTR'.
;       VALIDMIN:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'VALIDMIN'.
;       VALIDMAX:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'VALIDMAX'.
;       VAR_NOTES:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'VAR_NOTES'.
;       VAR_TYPE:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'VAR_TYPE'.
;       SCALETYP:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'SCALETYP'.
;       SCAL_PTR:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'SCAL_PTR'.
;       SCALEMIN:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'SCALEMIN'.
;       SCALEMAX:           in, optional, type=string
;                           Name of the attribute that is a substitute for 'SCALEMAX'.
;-
pro MrCDF_Viewer::SetConfig, $
CATDESC = catdesc, $
DEPEND_0 = depend_0, $
DEPEND_1 = depend_1, $
DEPEND_2 = depend_2, $
DEPEND_3 = depend_3, $
DELTA_MINUS_VAR = delta_minus_var, $
DELTA_PLUS_VAR = delta_plus_var, $
DISPLAY_TYPE = display_type, $
FIELDNAM = fieldnam, $
FILLVAL = fillval, $
LABLAXIS = lablaxis, $
LABL_PTR_1 = labl_ptr_1, $
LABL_PTR_2 = labl_ptr_2, $
LABL_PTR_3 = labl_ptr_3, $
UNITS = units, $
UNIT_PTR = unit_ptr, $
VALIDMIN = validmin, $
VALIDMAX = validmax, $
VAR_NOTES = var_notes, $
VAR_TYPE = var_type, $
SCALETYP = scaletyp, $
SCAL_PTR = scal_ptr, $
SCALEMIN = scalemin, $
SCALEMAX = scalemax
    compile_opt idl2
    on_error, 2

    ;Set the variable attribute names.
    if n_elements(catdesc)         ne 0 then (*self.config).catdesc         = catdesc
    if n_elements(depend_0)        ne 0 then (*self.config).depend_0        = depend_0
    if n_elements(depend_1)        ne 0 then (*self.config).depend_1        = depend_1
    if n_elements(depend_2)        ne 0 then (*self.config).depend_2        = depend_2
    if n_elements(depend_3)        ne 0 then (*self.config).depend_3        = depend_3
    if n_elements(delta_minus_var) ne 0 then (*self.config).delta_minus_var = delta_minus_var
    if n_elements(delta_plus_var)  ne 0 then (*self.config).delta_plus_var  = delta_plus_var
    if n_elements(display_type)    ne 0 then (*self.config).display_type    = display_type
    if n_elements(fieldnam)        ne 0 then (*self.config).fieldnam        = fieldnam
    if n_elements(fillval)         ne 0 then (*self.config).fillval         = fillval
    if n_elements(labl_ptr_1)      ne 0 then (*self.config).labl_ptr_1      = labl_ptr_1
    if n_elements(labl_ptr_2)      ne 0 then (*self.config).labl_ptr_2      = labl_ptr_2
    if n_elements(labl_ptr_3)      ne 0 then (*self.config).labl_ptr_3      = labl_ptr_3
    if n_elements(units)           ne 0 then (*self.config).units           = units
    if n_elements(unit_ptr)        ne 0 then (*self.config).unit_ptr        = unit_ptr
    if n_elements(validmin)        ne 0 then (*self.config).validmin        = validmin
    if n_elements(validmax)        ne 0 then (*self.config).validmax        = validmax
    if n_elements(var_notes)       ne 0 then (*self.config).var_notes       = var_notes
    if n_elements(var_type)        ne 0 then (*self.config).var_type        = var_type
    if n_elements(scaletyp)        ne 0 then (*self.config).scaletyp        = scaletyp
    if n_elements(scal_ptr)        ne 0 then (*self.config).scal_ptr        = scal_ptr
    if n_elements(scalemin)        ne 0 then (*self.config).scalemin        = scalemin
    if n_elements(scalemax)        ne 0 then (*self.config).scalemax        = scalemax
end


;+
;   The purpose of this method is to clean up after the object is destroyed. If a MrCDF_Viewer
;   GUI is open, it will also be destroyed.
;-
pro MrCDF_Viewer::Cleanup
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return
	endif
	
	;Destroy pointers
	ptr_free, self.config

	;Destroy object references
	if obj_valid(self.oCDF) then obj_destroy, self.oCDF
end


;+
;   The purpose of this method is to initialize the CDF_Read object reference.
;
; :Params:
;       thisCDF:        in, optional, type=string/object
;                       Either the file name of the CDF data file from which data is to
;                           be opened and plotted, or a valid MrCDF_File object reference.
;                           If not provided, a file selection dialog box will appear.
;
; :Keywords:
;       GROUP_LEADER:   in, optional, type=integer
;                       The group leader of the file selection gui. Use only when
;                           `FILENAME` undefined.
;       _REF_EXTRA:     in, optional, type=any
;                       All keyword accepted by the SET_CONFIG method are also excepted
;                           for keyword inheritance.
;
; :Returns:
;       object reference
;-
function MrCDF_Viewer::Init, thisCDF, $
GROUP_LEADER = group_leader, $
_REF_EXTRA = extra
	compile_opt strictarr

	;Catch errors
	catch, the_error
	if the_error ne 0 then begin
		catch, /cancel
		void = cgErrorMsg()
		return, 0
	endif

	type = size(thisCDF, /TNAME)

;---------------------------------------------------------------------
;OBJECT REFERENCE ////////////////////////////////////////////////////
;---------------------------------------------------------------------
	if type eq 'OBJREF' then begin
		;Is the object valid?
		if obj_valid(thisCDF) eq 0 $
			then message, 'thisCDF is not a valid object reference.' $
		
			;Is it a MrCDF_File object?
			else if obj_class(thisCDF) eq 'MRCDF_FILE' $
				then self.oCDF = thisCDF $
				else message, 'thisCDF must be a CDF_File object.'
	
;---------------------------------------------------------------------
;FILENAME ////////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else if type eq 'STRING' then begin
		oCDF = obj_new('MrCDF_File', thisCDF, _EXTRA=extra)
		if obj_valid(oCDF) $
			then self.oCDF = oCDF $
			else return, 0
	
;---------------------------------------------------------------------
;SELECT FILE /////////////////////////////////////////////////////////
;---------------------------------------------------------------------
	endif else begin
		oCDF = obj_new('MrCDF_File', _EXTRA=extra)
		if obj_valid(oCDF) $
			then self.oCDF = oCDF $
			else return, 0 
	endelse
	
	;Set the variable attribute name configuration
	oCDF -> GetProperty, FILENAME=filename
	tf_cluster = stregex(filename, 'C[1-4]_CP_.*__[0-9]{8}_[0-9]{6}_[0-9]{8}_[0-9]{6}_V[0-9]{6}.cdf', /BOOLEAN)
	self -> Configure, CLUSTER=tf_cluster

	return, 1
end


;+
;   Define the MrCDF_Viewer class
;
; :Params:
;       CLASS:          out, optional, type=structure
;                       The class definition structure.
;
; :Fields:
;       OCDF:               An object reference to a CDF_Read object
;       CONFIG:             Satellite mission-specific set of variable attribute names.
;-
pro MrCDF_Viewer__define, class
	compile_opt strictarr
	
	class = { MrCDF_Viewer, $
	          oCDF:         obj_new(), $
	          config:       ptr_new() $
	        }
end
