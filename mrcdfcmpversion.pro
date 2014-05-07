;+
; NAME:
;       MrCDFCmpVersion
;
; PURPOSE:
;
;   The purpose of this program is to return the current version of the CDF DLL is
;   in use or to test if the given version is older, newer, or the same as the current
;   version.
;
;   To obtain the CDF version
;
; :Examples:
;   Print the current version::
;       print, MrCDFCmpVersion()
;
;   Compare version 8.0 to the current version::
;       print, MrCDFCmpVersion('3.4')
;
;   Compare version 3.4.0 to version 3.3.1::
;       print, MrCDFCmpVersion('3.4.0', '3.3.1')
;
;   For more examples, run the main level program::
;       IDL> .r MrCDFCmpVersion
;
; :Categories:
;       Utility
;
; :Params:
;       VERSION1:           in, required, type=string
;                           The version of CDF to be compared against `VERSION2`.
;       VERSION2:           in, optional, type=string, default=!version.release
;                           The version if CDF to be compared against `VERSION1`. If not
;                               provided, but `VERSION1` is, then the current version will
;                               be used.
;                           
; :Returns:
;       RESULT:             Return::
;                               -1  - if `VERSION1` < `VERSION2`
;                                0  - if `VERSION1` = `VERSION2`
;                                1  - if `VERSION1` > `VERSION2`
;       
; :Author:
;   Matthew Argall::
;       University of New Hampshire
;       Morse Hall, Room 113
;       8 College Rd.
;       Durham, NH, 03824
;       matthew.argall@wildcats.unh.edu
;
; :History:
;   Modification History::
;       2014/02/03  -   Written by Matthew Argall
;-
function MrCDFCmpVersion, version1, version2
	compile_opt strictarr
	on_error, 2
	
	;If VERSION2 was not given, use the current version
	if n_elements(version2) eq 0 then begin
	    cdf_lib_info, VERSION=version, RELEASE=release, INCREMENT=increment, SUBINCREMENT=subincrement
	    version2 = string(FORMAT='(%"%i.%i.%i.%s")', version, release, increment, subincrement)
	endif
	
	;Return the current version?
	if n_elements(version1) eq 0 then return, version2
	
	;Extract the version numbers
	v1 = fix(stregex(version1, '([0-9]+)\.([0-9]+)\.?([0-9]+)?', /EXTRACT, /SUBEXP))
	v2 = fix(stregex(version2, '([0-9]+)\.([0-9]+)\.?([0-9]+)?', /EXTRACT, /SUBEXP))
	
	;Version1 < Version2
	if (v1[1] lt v2[1]) || (v1[1] eq v2[1] and v1[2] lt v2[2]) || $
	   (v1[1] eq v2[1] and v1[2] eq v2[2] and v1[3] lt v2[3]) then return, -1
	
	;Version1 = Version2
	if v1[1] eq v2[1] and v1[2] eq v2[2] and v1[3] eq v2[3] then return, 0
	
	;Version1 > Version2
	if (v1[1] gt v2[1]) || (v1[1] eq v2[1] and v1[2] gt v2[2]) || $
	   (v1[1] eq v2[1] and v1[2] eq v2[2] and v1[3] gt v2[3]) then return, 1
end


;-----------------------------------------------------
;Main Level Example Program: IDL> .r MrCDFCmpVersion) 
;-----------------------------------------------------
;Run a couple of tests
result1 = MrCDFCmpVersion('1.2', '1.2.1')
result2 = MrCDFCmpVersion('1.2', '1.3')
result3 = MrCDFCmpVersion('1.2', '1.2')
result4 = MrCDFCmpVersion('1.2', '1.1.2')
result5 = MrCDFCmpVersion('1.2.1', '1.2')
result6 = MrCDFCmpVersion('3.4.0.0')
result7 = MrCDFCmpVersion()

;Print the Results
print, FORMAT='(%"Compare v1.2     to v1.2.1   %2i")', result1
print, FORMAT='(%"Compare v1.2     to v1.3     %2i")', result2
print, FORMAT='(%"Compare v1.2     to v1.2     %2i")', result3
print, FORMAT='(%"Compare v1.2     to v1.1.2   %2i")', result4
print, FORMAT='(%"Compare v1.2.1   to v1.2     %2i")', result5
print, FORMAT='(%"Compare v3.4.0.0 to Current  %2i")', result6
print, FORMAT='(%"Current Version:              %s")', result7

end