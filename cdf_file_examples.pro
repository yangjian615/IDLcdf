; docformat = 'rst'
;
; NAME:
;       CDF_File_Examples
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
;   A program for demonstrating how to read, write, and copy information in CDF files
;   using the CDF_File object.
;-
pro CDF_File_Examples
    compile_opt strictarr
    
    ;Error handling
    catch, the_error
    if the_error ne 0 then begin
        catch, /cancel
        obj_destroy, fileObj
        void = cgErrorMsg()
        return
    endif
    
    ;Initial object definitions -- used for proper cleanup
    fileObj = obj_new()

;-----------------------------------------------------
; Create a New File for Writing \\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    fileObj = obj_new('CDF_File', '/Users/argall/Desktop/cdf_example.cdf', $
                      /CREATE, /CLOBBER)
    if obj_valid(fileObj) eq 0 then $
        message, 'Unable to create CDF file.'

;-----------------------------------------------------
; Create Global Attributes \\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;
    ; About global attribute entries::
    ;   - EntryNum is the index at which the value is stored.
    ;   - They do not have to be the same type
    ;   - They do not have to stored at contiguous locations.
    ;

    ;Add the definition
    fileObj -> WriteGlobalAttrDef, 'Title'
    fileObj -> WriteGlobalAttrDef, 'Description'
    fileObj -> WriteGlobalAttrDef, 'Created'
    
    ;Write global attribute data
    fileObj -> WriteGlobalAttr, 'Title', 'CDF Example File'
    fileObj -> WriteGlobalAttr, 'Description', $
        'An example of how to create a CDF file using the CDF_File object.'
    fileObj -> WriteGlobalAttr, 'Created', systime()
    
    ;Experiment writing values
    ;   - Create and write in one step
    ;   - Change the value
    ;   - Add a value of a different type
    ;   - Add an array of values at a discontiguous location
    fileObj -> WriteGlobalAttr, 'Author', 'You!', /CREATE
    fileObj -> WriteGlobalAttr, 'Author', 'no, ME!', GENTRYNUM=0
    fileObj -> WriteGlobalAttr, 'Author', 156
    fileObj -> WriteGlobalAttr, 'Author', ['String', 'Array'], GENTRYNUM=3

;-----------------------------------------------------
; Create Z-Variables \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Create some data
    data1 = cgDemoData(12)              ;M51 Whirlpool Galaxy
    data2 = cgDemoData(6)               ;Heart Gated Blood Pool Study
    dims1 = size(data1, /DIMENSIONS)
    dims2 = size(data2, /DIMENSIONS)
    nPts = n_elements(data)
    colortable = 34
    
    ;Create times
    seconds = fltarr(24)
    times = '01-Jan-2000 00:00:' + string(seconds, FORMAT='(i02)') + '.000'
    t_epoch = cdf_parse_epoch(times)
    
    ;
    ; Records come after dimensions, so the array
    ;   data = bytarr(340, 440, 24)
    ; has DIMENSIONS=[340,440] with 24 records.
    ;
    ; We will use the M51 Whirlpool Galaxy image to create a dataset of 24 frames.
    ; Each frame (or record) will be taken at a different time, but will contain the
    ; same image. Thus, while the dimensions of the image have a dimensional variance
    ; of 'VARY', meaning the values in each dimension are different from one another,
    ; the record variance is 'NOVARY', meaning each record holds the same information --
    ; all frames are the same.
    ;
    ; Because the record variance is "NOVARY", only one record will be physically
    ; written to the file and MAXREC, as reported by the GET_VAR_INFO keyword to
    ; CDF_CONTROL, will be 0. However, there are actually 24 frames. How do we know that
    ; there are really 24 frames?
    ;
    ; Variables have variable attributes, and one standard attribute is DEPEND_0, which
    ; gives the independent variable data, and is usually time. If we use the DEPEND_0
    ; variable attribute to point to a time array with 24 records, we can associate one
    ; frame with each of the 24 times.
    ;
    fileObj -> WriteVarDef, 'M51_Whilrpool_Galaxy', 'CDF_INT1', ['VARY', 'VARY'], $
                            DIMENSIONS=[dims1[0], dims1[1]], /REC_NOVARY
    fileObj -> WriteVarDef, 'Time', 'CDF_EPOCH', 'NOVARY', ALLOCATERECS=24

    ;Write the data. This generates the following warning:
    ;   % CDF_VARPUT: Function completed but: VIRTUAL_RECORD_DATA: One or more of the records are virtual.
    fileObj -> WriteVarData, 'M51_Whilrpool_Galaxy', rebin(data1, dims1[0], dims1[1], 24)
    fileObj -> WriteVarData, 'Time', t_epoch

    ;Create and write in one step.
    ;   By default, this creates a zvariable with record and dimension variance of "VARY"
    fileObj -> WriteVarData, 'Blood_Study', data2, /CREATE
    
;-----------------------------------------------------
; Create Variable Attributes \\\\\\\\\\\\\\\\\\\\\\\\\
;-----------------------------------------------------
    ;Create a couple of variable attributes
    ;   - Global and Variable attribute names are grouped together and must be unique.
    fileObj -> WriteVarAttrDef, 'DEPEND_0'
    fileObj -> WriteVarAttrDef, 'Caption'
    
    ;Assign variables and values to the attributes
    ;   - Here we make a note that Whirlpool Galaxy data depends on the Time variable
    ;   - Variables share variable attributes
    fileObj -> WriteVarAttr, 'Time', 'Caption', 'Time at which each frame of M51 was taken.'
    fileObj -> WriteVarAttr, 'M51_Whilrpool_Galaxy', 'DEPEND_0', 'Time'
    fileObj -> WriteVarAttr, 'M51_Whilrpool_Galaxy', 'Caption', 'Images of M51.'
    fileObj -> WriteVarAttr, 'Blood_Study', 'Caption', 'Images of blood in a heart.'
    
    ;Create and write in one step
    fileObj -> WriteVarAttr, 'Blood_Study', 'Frame Order', [0,4,2,3,10,15,13,12,6,9,1,11,7,5,8], /CREATE
    fileObj -> WriteVarAttr, 'M51_Whilrpool_Galaxy', 'Frame Order', indgen(24)

    ;Destroy the object (writes file to disk and closes the file).
    obj_destroy, fileObj
    
    
    ;Read the data
    fileObj = obj_new('cdf_file', '/Users/argall/Desktop/cdf_example.cdf')
    fileObj -> PrintFileInfo
    data1 = fileObj -> Read('M51_Whilrpool_Galaxy')
    data2 = fileObj -> Read('Blood_Study')
    help, data1, data2
    obj_destroy, fileObj
end