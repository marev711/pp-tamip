 PROGRAM CMORIZETAMIP
     USE netcdf
     USE cmor_users_functions
     USE NDATASET
     USE CMOR_TAMIP_ROUTINES

     IMPLICIT NONE
     INTEGER :: error_flag, ncid, rhVarId, &
                lonDimID, latDimId, timeDimId, &
                numLons, numLats, numTimes, numLength, &
                status , id, ndims
     integer, dimension(nf90_max_var_dims) :: dimIDs
     character(len=128) :: dim_name
     real, dimension(:, :, :), allocatable :: rhValues
     INTEGER, PARAMETER :: lon = 512     ! number of longitude grid cells
     INTEGER, PARAMETER :: lat = 256     ! number of latitude grid cells
     INTEGER, PARAMETER :: levs = 16     ! number of standard pressure

     call read_nml()
     INPATH = "44.128_tmp_mean.nc"
     status = cmor_setup(inpath=INPATH, netcdf_file_action=CMOR_APPEND)
     IF (status .ne. 0) call handle_err(status, "CMOR_SETUP")

     status = nf90_open(path = "44.128_tmp_mean.nc", mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_OPEN")

     status = nf90_inq_varid(ncid, "ES", rhVarId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID")

     status = nf90_inquire_variable(ncid, rhVarId, dimids = dimIDs, ndims=ndims)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQUIRE_VARIABLE")
     do id=1, ndims
         write(*,*) "size(dimIDs)",size(dimIDs, 1)
         status = nf90_inquire_dimension(ncid, dimIDs(id), name=dim_name, len = numLength)
         if(status /= nf90_NoErr) call handle_err(status, "NF90_INQUIRE_DIMENSION")
         write(*,'(A, I2, 1X, A, A, I3)') "dim_id=",id, dim_name, " length=", numLength
     end do
call abort("exiting...")

     allocate(rhValues(numLons, numLats, numTimes))
     status = nf90_get_var(ncid, rhVarId, rhValues)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_VAR")

     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_CLOSE")
END PROGRAM CMORIZETAMIP
