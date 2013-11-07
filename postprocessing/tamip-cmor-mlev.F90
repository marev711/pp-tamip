 PROGRAM CMORIZETAMIP
     USE netcdf
     USE level_coefficients
     USE cmor_users_functions
     USE NCMORIO
     USE NATTRIBUTES
     USE CMOR_TAMIP_ROUTINES

     IMPLICIT NONE
     INTEGER :: error_flag, ncid, ncid2, rhVarId, timeVarId, &
                lonDimID, latDimId, timeDimId, lev, &
                numLons, numLats, numTimes, numLength, &
                status , id, ndims, tableId, ilon, ilat, itime, i, &
                cvar, counter, hyaiId, hybiId, ilev, hyamId, hybmId, &
                zaxis_id, zfactor_id
     integer, dimension(nf90_max_var_dims) :: dimIDs

     double precision, dimension(91) :: levels
     double precision, dimension(2, 92) :: levels_bounds

     character(len=128) :: dim_name, calendar_read, units_read
     character(len=1024) :: history
     character (len=*) , parameter :: end_of_line = char(13)//char(10)
     real, dimension(:, :, :), allocatable :: ps_val
     real, dimension(:, :, :, :), allocatable :: rhValues
     DOUBLE PRECISION, dimension(:), allocatable :: alats, alons, time
     DOUBLE PRECISION, dimension(2)  :: time2
     DOUBLE PRECISION, dimension(2,2)  :: time2bnds
     DOUBLE PRECISION, dimension(:, :), allocatable :: alats_bounds, alons_bounds, time_bounds
     DOUBLE PRECISION, dimension(:, :), allocatable :: hyai_bounds, hybi_bounds
     DOUBLE PRECISION, dimension(:, :), allocatable :: hyam_bounds, hybm_bounds

     call read_nml()
     status = cmor_setup(inpath=INPATH, netcdf_file_action=CMOR_APPEND)
     IF (status .ne. 0) call handle_err(status, "CMOR_SETUP")

     tableId = cmor_load_table(table_id)
     call cmor_set_table(tableId)

     status = nf90_open(path = curr_file, mode = nf90_nowrite, ncid = ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_OPEN")

     status = nf90_inq_varid(ncid, model_varname, rhVarId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (model_varname)")

     status = nf90_inq_varid(ncid, "time", timeVarId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (time)")

     status = nf90_inq_varid(ncid, "hyai", hyaiId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (hyai)")

     status = nf90_inq_varid(ncid, "hybi", hybiId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (hybi)")

     status = nf90_inq_varid(ncid, "hyam", hyamId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (hyam)")

     status = nf90_inq_varid(ncid, "hybm", hybmId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (hybm)")

     status = nf90_get_att(ncid, timeVarId, "calendar", calendar_read)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_ATT")

     status = nf90_get_att(ncid, NF90_GLOBAL, "history", history)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_ATT")
     history=trim(history)//end_of_line

     status = nf90_get_att(ncid, timeVarId, "units", units_read)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_ATT")

     status = cmor_dataset(branch_time=branch_time,                   &
                           calendar=calendar_read,                    &
                           comment=comment,                           &
                           contact=contact,                           &
                           experiment_id=experiment_id,               &
                           forcing=forcing,                           &
                           history=history,                          &
                           institute_id=institute_id,                 &
                           institution=institution,                   &
                           model_id=model_id,                         &
                           parent_experiment_id=parent_experiment_id, &
                           parent_experiment_rip=parent_experiment_rip, &
                           realization=realization,                   &
                           references=references,                     &
                           source=source,                             &
                           outpath=outpath)


     call read_1d_coord(ncid, rhVarId, "lat", alats, alats_bounds)
     call read_1d_coord(ncid, rhVarId, "lon", alons, alons_bounds)
     call read_1d_coord(ncid, rhVarId, "time", time, time_bounds)
     do lev = 1, size(hyam_val)
       levels(lev) = hyam(lev)/ref_pressure + hybm(lev)
       levels_bounds(1, lev) = hyai(lev)/ref_pressure + hybi(lev)
       levels_bounds(2, lev) = hyai(lev+1)/ref_pressure + hybi(lev+1)
     end do
     allocate(rhValues(size(alons), size(alats), size(levels), size(time)))
     status = nf90_get_var(ncid, rhVarId, rhValues)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_VAR")

     status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status, "NF90_CLOSE")

     status = nf90_open(path = sp_file, mode = nf90_nowrite, ncid = ncid2)
     if (status /= nf90_noerr) call handle_err(status, "NF90_OPEN")

     status = nf90_inq_varid(ncid2, 'SP', rhVarId)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_INQ_VARID (SP)")

     allocate(ps_val(size(alons), size(alats), size(time)))
     status = nf90_get_var(ncid2, rhVarId, ps_val)
     if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_VAR")

     status = nf90_close(ncid2)
     if (status /= nf90_noerr) call handle_err(status, "NF90_CLOSE")


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


     itime = cmor_axis(table_entry='time',     &
                       units=units_read,       &
                       coord_vals=time,        &
                       interval="180 minutes", &
                       cell_bounds=time_bounds)
     if (itime .lt. 0) call handle_err(status, "CMOR_AXIS_TIME")

     ilev = cmor_axis(table_entry='alternate_hybrid_sigma',     &
                       units='1',       &
                       coord_vals=levels, &
                       cell_bounds=levels_bounds)
     if (ilev .lt. 0) call handle_err(status, "CMOR_AXIS_LEVEL")

     zfactor_id = cmor_zfactor(zaxis_id=ilev,     &
                               zfactor_name='ap', &
                               axis_ids=(/ilev/),    &
                               units='Pa',         &
                               zfactor_values=hyam_val, &
                               zfactor_bounds=hyai_val)
     if (zfactor_id .lt. 0) call handle_err(status, "CMOR_ZFACTOR_AP")

     zfactor_id = cmor_zfactor(zaxis_id=ilev,     &
                               zfactor_name='b', &
                               axis_ids=(/ilev/),    &
                               units='1',         &
                               zfactor_values=hybm_val, &
                               zfactor_bounds=hybi_val)
     if (zfactor_id .lt. 0) call handle_err(status, "CMOR_ZFACTOR_B")

     zfactor_id = cmor_zfactor(zaxis_id=ilev,     &
                               zfactor_name='ps', &
                               axis_ids=(/ilon, ilat, itime/),    &
                               units='Pa')
     if (zfactor_id .lt. 0) call handle_err(status, "CMOR_ZFACTOR_PS")

     cvar = cmor_variable(table_entry=cmor_varname,       &
                          units=model_units,              &
                          axis_ids=(/ilon, ilat, ilev, itime/), &
                          missing_value=1.0e20,           &
                          positive=positive,              &
                          original_name=original_name)

      status = cmor_write(var_id = cvar,    &
                          data   = rhValues)
     if (status /= 0) call handle_err(status, "CMOR_WRITE")

     status = cmor_write(var_id = zfactor_id,    &
                         store_with=cvar, &
                         data   = ps_val)


     status = cmor_close()
     if (status /= 0) call handle_err(status, "CMOR_CLOSE")
END PROGRAM CMORIZETAMIP
