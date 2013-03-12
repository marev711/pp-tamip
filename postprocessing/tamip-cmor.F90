

 PROGRAM CMORIZETAMIP
     USE netcdf
     USE NDATASET
     USE CMOR_TAMIP_ROUTINES

     IMPLICIT NONE
     inpath="44.128_tmp_mean.nc"
     error_flag = cmor_setup(inpath=inpath, netcdf_file_action=CMOR_APPEND)
END PROGRAM CMORIZETAMIP
