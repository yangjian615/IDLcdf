#!/bin/bash

#Common Directory
root=/Users/argall/Documents

#Destination directories
dest_CDF="/Users/argall/Google Drive/Work/Programs/cdf_file/"  # CDF File

#Source directories
src_Coyote=$root/External_Libraries/coyote
src_Proj=$root/Work/Programs/Projects             # Project Directory
src_Lib=$root/Work/Programs/MyLibraryIDL          # My Personal Library

#Sources: CDF_File
CDF_File[0]=$src_Proj/data_readers/cdf_attribute__define.pro
CDF_File[1]=$src_Proj/data_readers/cdf_castdatatype.pro
CDF_File[2]=$src_Proj/data_readers/cdf_container__define.pro
CDF_File[3]=$src_Proj/data_readers/cdf_file__define.pro
CDF_File[4]=$src_Proj/data_readers/cdf_file_examples.pro
CDF_File[5]=$src_Proj/data_readers/cdf_variable__define.pro

#Sources: Utilities
CDF_File[6]=$src_Coyote/cgerrormsg.pro
CDF_File[7]=$src_Coyote/cgdemodata.pro
CDF_File[8]=$src_Coyote/linkedlist__define.pro
CDF_File[9]=$src_Lib/array_utils/ismember.pro
CDF_File[10]=$src_Lib/sys_utils/mrcmpversion.pro
CDF_File[11]=$src_Lib/time_utils/dissectdate.pro
CDF_File[12]=$src_Lib/time_utils/dissectdatetime.pro
CDF_File[13]=$src_Lib/time_utils/dissecttime.pro
CDF_File[14]=$src_Lib/time_utils/monthnametonumber.pro
CDF_File[15]=$src_Lib/time_utils/monthnumbertoname.pro
CDF_File[16]=$src_Lib/time_utils/mrcdf_epoch.pro
CDF_File[17]=$src_Lib/time_utils/mrcdf_epoch_compare.pro
CDF_File[18]=$src_Lib/time_utils/mrcdf_epoch_encode.pro
CDF_File[19]=$src_Lib/time_utils/mrcdf_epoch_parse.pro
CDF_File[20]=$src_Lib/time_utils/mrcdf_epoch_type.pro
CDF_File[21]=$src_Lib/time_utils/mrcdfcmpversion.pro
CDF_File[22]=$src_Lib/time_utils/mrtimeparser.pro
CDF_File[23]=$src_Lib/time_utils/mrtimetokenstoregex.pro
CDF_File[24]=$src_Lib/time_utils/year_day.pro
CDF_File[25]=$src_Lib/type_utils/mrisa.pro
CDF_File[26]=$src_Lib/type_utils/mrobj_class.pro
CDF_File[27]=$src_Proj/mrwindow/grManager/mridl_container__define.pro


#------------------------------------
# Copy the Files
#------------------------------------
cp -av "${CDF_File[@]}" "$dest_CDF"



