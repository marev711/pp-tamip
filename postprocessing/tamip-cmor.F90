 PROGRAM CMORIZETAMIP
     USE netcdf
     USE cmor_users_functions
     USE NDATASET
     USE CMOR_TAMIP_ROUTINES

     IMPLICIT NONE
     INTEGER :: error_flag, ncid, rhVarId, timeVarId, &
                lonDimID, latDimId, timeDimId, &
                numLons, numLats, numTimes, numLength, &
                status , id, ndims, tableId, ilon, ilat, itime, i, &
                cvar
     integer, dimension(nf90_max_var_dims) :: dimIDs
     character(len=128) :: dim_name, calendar_read, units_read
     real, dimension(:, :, :), allocatable :: rhValues
     DOUBLE PRECISION, dimension(:), allocatable :: alats, alons, time
     DOUBLE PRECISION, dimension(2)  :: time2
     DOUBLE PRECISION, dimension(2,2)  :: time2bnds
     DOUBLE PRECISION, dimension(:, :), allocatable :: alats_bounds, alons_bounds, time_bounds

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

     status = nf90_get_att(ncid, timeVarId, "units", units_read)
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


     call read_1d_coord(ncid, rhVarId, "lat", alats, alats_bounds)
     call read_1d_coord(ncid, rhVarId, "lon", alons, alons_bounds)
     call read_1d_coord(ncid, rhVarId, "time", time, time_bounds)

     allocate(rhValues(size(alons), size(alats), size(time)))
     status = nf90_get_var(ncid, rhVarId, rhValues)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_VAR")

     ilon = cmor_axis(table_entry='longitude', &
                      units='degrees_east',    &
                      coord_vals=alons,        &
                      cell_bounds=alons_bounds)
     if (ilon .lt. 0) call handle_err(status, "CMOR_AXIS_LON")

     ilat = cmor_axis(table_entry='latitude', &
                      units='degrees_north',  &
                      coord_vals=alats,       &
                      cell_bounds=alats_bounds)
     if (ilat .lt. 0) call handle_err(status, "CMOR_AXIS_LAT")

! Write the time bounds
write(*,*) "units_read=", trim(units_read)
do i=1,size(time)
  write(*,'(I5,A,F8.3,A,F8.3,A,F8.3,A)') i, ": ", time(i), " (", time_bounds(1, i), ", ", time_bounds(2, i), ")"
end do
     itime = cmor_axis(table_entry='time',     &
                       units="hours since 2009-07-20 00:00:00",       &
                       coord_vals=time,        &
                       interval="180 minutes", &
                       cell_bounds=time_bounds)
     if (itime .lt. 0) call handle_err(status, "CMOR_AXIS_TIME")

     cvar = cmor_variable(table_entry="evspsbl",                               &
                          units="kg m-2 s-1",                            &
                          axis_ids=(/ilon, ilat, itime/), &
                          missing_value=1.0e20)

     status = cmor_write(var_id = cvar,    &
                         data   = rhValues)
     if (status /= 0) call handle_err(status, "CMOR_WRITE")


     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_CLOSE")
END PROGRAM CMORIZETAMIP
