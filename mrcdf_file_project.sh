#!/bin/bash

#Common Directory
root=/Users/argall/Documents

#Destination directories
dest_CDF="/Users/argall/Dropbox/Work/Programs/MrCDF/"  # CDF File

#Source directories
src_Coyote=$root/External_Libraries/coyote
src_Proj=$root/IDL/Files/CDF                      # CDF Directory
src_Lib=$root/IDL/IDLlib                          # My Personal Library

#Sources: CDF_File
CDF_File[0]=$src_Proj/mrcdf_attribute__define.pro
CDF_File[1]=$src_Proj/mrcdf_browser__define.pro
CDF_File[2]=$src_Proj/mrcdf_castdatatype.pro
CDF_File[3]=$src_Proj/mrcdf_compile_all.pro
CDF_File[4]=$src_Proj/mrcdf_container__define.pro
CDF_File[5]=$src_Proj/mrcdf_file__define.pro
CDF_File[6]=$src_Proj/mrcdf_file_examples.pro
CDF_File[7]=$src_Proj/mrcdf_variable__define.pro

#Sources: Other
CDF_File[8]=$src_Proj/mrcdf_epoch_compare.pro
CDF_File[9]=$src_Proj/mrcdf_epoch_encode.pro
CDF_File[10]=$src_Proj/mrcdf_epoch_parse.pro
CDF_File[11]=$src_Proj/mrcdf_epoch_type.pro
CDF_File[12]=$src_Proj/mrcdf_epoch.pro
CDF_File[13]=$src_Proj/mrcdfcmpversion.pro
CDF_File[14]=$src_Proj/mrcdf_read.pro
CDF_File[15]=$src_Proj/mrcdf_varnames.pro


CDF_File[15]=$src_Proj/mrtimeparser.pro

#Sources: Utilities
CDF_File[16]=$src_Coyote/cgerrormsg.pro
CDF_File[17]=$src_Coyote/cgdemodata.pro
CDF_File[19]=$src_Lib/array_utils/mrismember.pro
CDF_File[20]=$src_Lib/sys_utils/mrcmpversion.pro
CDF_File[21]=$src_Lib/time_utils/dissectdate.pro
CDF_File[22]=$src_Lib/time_utils/dissectdatetime.pro
CDF_File[23]=$src_Lib/time_utils/dissecttime.pro
CDF_File[24]=$src_Lib/time_utils/monthnametonumber.pro
CDF_File[25]=$src_Lib/time_utils/monthnumbertoname.pro
CDF_File[26]=$src_Lib/time_utils/mrtimeparser.pro
CDF_File[27]=$src_Lib/time_utils/mrtimetokenstoregex.pro
CDF_File[28]=$src_Lib/time_utils/year_day.pro
CDF_File[29]=$src_Lib/type_utils/mrisa.pro
CDF_File[30]=$src_Lib/type_utils/mrobj_class.pro
CDF_File[31]=$src_Proj/mrwindow/grManager/mridl_container__define.pro


#------------------------------------
# Copy the Files
#------------------------------------
# a -- archive (preserve attributes)
# v -- verbose
rm "$dest_CDF"*.pro
cp -a "${CDF_File[@]}" "$dest_CDF"

# Create a zip archive with the date and time appended.
zip -qj "$dest_CDF"mrcdf_file_$(date +"%Y%m%d_%H%M%S") "$dest_CDF"*.pro


