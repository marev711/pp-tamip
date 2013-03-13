 PROGRAM CMORIZETAMIP
     USE netcdf
     USE cmor_users_functions
     USE NDATASET
     USE CMOR_TAMIP_ROUTINES

     IMPLICIT NONE
     INTEGER :: error_flag, ncid, rhVarId, timeVarId, &
                lonDimID, latDimId, timeDimId, &
                numLons, numLats, numTimes, numLength, &
                status , id, ndims, tableId
     integer, dimension(nf90_max_var_dims) :: dimIDs
     character(len=128) :: dim_name, calendar_read
     real, dimension(:, :, :), allocatable :: rhValues
     DOUBLE PRECISION, dimension(:), allocatable :: alats, alons, time
     INTEGER, PARAMETER :: lon = 512     ! number of longitude grid cells
     INTEGER, PARAMETER :: lat = 256     ! number of latitude grid cells
     INTEGER, PARAMETER :: levs = 16     ! number of standard pressure


     call read_nml()
     status = cmor_setup(inpath=INPATH, netcdf_file_action=CMOR_APPEND)
     IF (status .ne. 0) call handle_err(status, "CMOR_SETUP")

     tableId = cmor_load_table("TAMIP_3hrSlev")
     call cmor_set_table(tableId)

     status = nf90_open(path = curr_file, mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_OPEN")

     status = nf90_inq_varid(ncid, "ES", rhVarId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID")

     status = nf90_inq_varid(ncid, "time", timeVarId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID")

     status = nf90_get_att(ncid, timeVarId, "calendar", calendar_read)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_ATT")

     status = cmor_dataset(outpath=outpath,                           &
                           experiment_id=experiment_id,               &
                           institution=institution,                   &
                           source=source,                             &
                           calendar=calendar_read,                    &
                           realization=realization,                   &
                           contact=contact,                           &
                           history=history,                           &
                           comment=comment,                           &
                           references=references,                     &
                           model_id=model_id,                         &
                           forcing=forcing,                           &
                           institute_id=institute_id,                 &
                           parent_experiment_id=parent_experiment_id, &
                           branch_time=branch_time,                   &
                           parent_experiment_rip=parent_experiment_rip)


     call read_1d_coord(ncid, rhVarId, "lat", alats)
     call read_1d_coord(ncid, rhVarId, "lon", alons)
     call read_1d_coord(ncid, rhVarId, "time", time)

     allocate(rhValues(size(alons), size(alats), size(time)))
     status = nf90_get_var(ncid, rhVarId, rhValues)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_VAR")

     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_CLOSE")
END PROGRAM CMORIZETAMIP
