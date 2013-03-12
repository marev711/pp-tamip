

 PROGRAM CMORIZETAMIP
     USE netcdf
     USE cmor_users_functions
     USE NDATASET
     USE CMOR_TAMIP_ROUTINES

     IMPLICIT NONE
     INTEGER :: error_flag

     inpath="44.128_tmp_mean.nc"
     error_flag = cmor_setup(inpath=inpath, netcdf_file_action=CMOR_APPEND)
END PROGRAM CMORIZETAMIP
