 PROGRAM GRIBTOCMOR
     CHARACTER(256) :: inpath, outpath, experiment_id, institution, source, &
     & calendar, contact, history, comment, references, model_id, &
     & forcing, institute_id, parent_experiment_id, parent_experiment_rip
     INTEGER :: realization
     DOUBLE PRECISION :: branch_time
     error_flag = cmor_setup(inpath=inpath, netcdf_file_action=CMOR_APPEND)
END PROGRAM GRIBTOCMOR
