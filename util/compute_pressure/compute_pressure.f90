!
!  Description: 1) Compute the pressure on half and full model levels
!
!                    pr_full = hyam(lev) + hybm(lev) * surface_pressure
!                    pr_half = hyai(lev) + hybi(lev) * surface_pressure
!
program compute_pressure
  use level_coefficients
  use grib_api
  implicit none
  integer                              :: err, i, ii, idx, no_levels, time_step
  integer                              :: no_messages, typeOfLevelSize
  integer                              :: field_size, curr_level, levelSize
  integer                              :: sp_infile_idx, ml_infile_idx
  integer                              :: pressure_outfile_idx, hour
  integer                              :: sp_grib_idx, ml_grib_idx
  integer                              :: Ni, Nj
  logical                              :: compute_full_levels
  real                                 :: missingValue
  real, dimension(:), allocatable      :: sp
  real, dimension(:), allocatable      :: curr_pressure
  character(len=20)                    :: typeOfLevel

  integer,dimension(:),allocatable     :: pressure_grib_idx
  character(len=256)                   :: pressure_outfile
  character(len=256)                   :: surface_pressure_file
  character(len=256)                   :: ml_ref_grib_file ! The ml_ref used is 157.128.grb

  namelist /indata/ surface_pressure_file, ml_ref_grib_file, &
                    compute_full_levels, pressure_outfile
                    
  open(7, FILE='indata.nml')
  read(7, NML=indata)
  close(7)

  surface_pressure_file = TRIM(surface_pressure_file)
  pressure_outfile = TRIM(pressure_outfile)

  missingValue = 9999

  if (compute_full_levels) then 
      no_levels = 91
  else
      no_levels = 92
  end if
  call grib_open_file(sp_infile_idx, surface_pressure_file, 'r')
  call grib_open_file(pressure_outfile_idx, pressure_outfile, 'w')

  ! Get total field size (using surface pressure field)
  call grib_new_from_file(sp_infile_idx, sp_grib_idx, err)
  call check_exit_status(err, "Couldnt read surface pressure infile")
  call grib_get(sp_grib_idx, 'Ni', Ni)
  call grib_get(sp_grib_idx, 'Nj', Nj)
  field_size = Ni * Nj

  write(*,*) "field_size=", field_size

  call grib_release(sp_grib_idx)
  call grib_close_file(sp_infile_idx)

  ! Allocate input/output fields
  allocate(sp(field_size), stat=err)
  call check_exit_status(err, "Failed to allocate values for sp")

  allocate(curr_pressure(field_size),stat=err)
  call check_exit_status(err, "Failed to allocate values for curr_pressure")

  ! Count the messages in the file
  call grib_open_file(sp_infile_idx, surface_pressure_file, 'r')
  call grib_count_in_file(sp_infile_idx, no_messages)
  write(*,*) "no_messages=", no_messages

  allocate(pressure_grib_idx(no_messages), stat=err)
  call check_exit_status(err, "Failed to allocate values for pressure_grib_idx")
  pressure_grib_idx = -1

  ! Open model level file used as template
  call grib_open_file(ml_infile_idx, ml_ref_grib_file, 'r')

  ! Compute the pressure
  DO time_step = 1, no_messages
      call grib_new_from_file(sp_infile_idx, sp_grib_idx, err)
      call check_exit_status(err, "Failed to read current surface pressure field")
      call grib_get(sp_grib_idx, 'values', sp)
      write(*,*) "time_step=", time_step

      DO idx = 1, no_levels
          if (idx .le. 91) then  ! Half levels have 93 levels, reuse the 91th
                                 ! level in the reference file
              call grib_new_from_file(ml_infile_idx, ml_grib_idx, err)
          end if
          call grib_clone(ml_grib_idx, pressure_grib_idx(idx))
          call grib_set(pressure_grib_idx(idx), 'indicatorOfParameter', 54)
          call grib_set(pressure_grib_idx(idx), 'level', idx)
          call grib_set(pressure_grib_idx(idx), 'missingValue', missingValue)
          call grib_set(pressure_grib_idx(idx), 'bitmapPresent', 1)
          do ii=1, field_size
              if (compute_full_levels) then 
                  curr_pressure(ii) = hyam(idx) + hybm(idx) * sp(ii)
              else
                  curr_pressure(ii) = hyai(idx) + hybi(idx) * sp(ii)
              end if
          end do
          call grib_set(pressure_grib_idx(idx), 'values', pack(curr_pressure, mask=.true.))
          call grib_write(pressure_grib_idx(idx), pressure_outfile_idx, err)
      END DO
      call grib_release(sp_grib_idx)
  END DO

  ! Finalize
  deallocate(sp)
  deallocate(curr_pressure)
  call grib_release(ml_grib_idx)
  DO i=1, no_messages
    call grib_release(pressure_grib_idx(i))
  END DO

  call grib_close_file(sp_infile_idx)
  call grib_close_file(pressure_outfile_idx)

contains
!======================================
subroutine check_exit_status(err, err_message)
  implicit none
  integer               :: err
  character(len=*)      :: err_message
  if (err .ne. 0) then
    print*, err_message
    STOP
  end if
end subroutine check_exit_status
!======================================
end program compute_pressure
