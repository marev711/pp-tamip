!
!  Description: 1) Compute the convective cloud base/top (ccb/cct)
!                  The ccb/t is output in variable 96 where level one
!                  is the model level for cct and level two is the
!                  ditto for ccb. The ccb/t are then computed as,
!
!                    ccb/t = A(var96(lev)) + B(var96(lev)) * surface_pressure
!
program compute_ccbt
  use level_coefficients
  use grib_api
  implicit none
  integer                              :: err, i, ii
  integer                              :: no_messages, counter, no_level1_entries
  integer                              :: field_size, curr_level, levelSize
  integer                              :: sp_infile_idx, var96_infile_idx
  integer                              :: ccbt_outfile_idx
  integer                              :: var96fields_grib_idx, sp_fields_grib_idx
  integer                              :: var96_index_idx, sp_grib_idx
  integer                              :: PLPresent, nb_pl
  integer                              :: ccbt_level_in_var96
  real                                 :: missingValue
  real, dimension(:), allocatable      :: sp, pl
  real, dimension(:), allocatable      :: var96field, curr_ccbt

  integer,dimension(:),allocatable     :: ccbtfields_grib_idx
  character(len=256)                   :: var96_file, ccbt_outfile
  character(len=256)                   :: surface_pressure_file

  namelist /indata/ var96_file, surface_pressure_file, ccbt_outfile, ccbt_level_in_var96
  open(7, FILE='indata.nml')
  read(7, NML=indata)
  close(7)

  var96_file = TRIM(var96_file)
  surface_pressure_file = TRIM(surface_pressure_file)
  ccbt_outfile = TRIM(ccbt_outfile)

  missingValue = 9999

  call grib_open_file(sp_infile_idx, surface_pressure_file, 'r')
  call grib_open_file(var96_infile_idx, var96_file, 'r')
  call grib_open_file(ccbt_outfile_idx, ccbt_outfile, 'w')

  ! Get the number of levels
  call grib_index_create(var96_index_idx,var96_file, 'level')
  call grib_index_get_size(var96_index_idx, 'level', levelSize)

  ! Get total field size (using surface pressure field)
  call grib_new_from_file(sp_infile_idx, sp_grib_idx, err)
  call check_exit_status(err, "Couldnt read surface pressure infile")
  call grib_get(sp_grib_idx, 'PLPresent', PLPresent)
  if (PLPresent == 1) then
    call grib_get_size(sp_grib_idx,'pl', nb_pl)
    allocate(pl(nb_pl), stat=err)
    call check_exit_status(err, 'Failed to allocate values for pl')

    call grib_get(sp_grib_idx, 'pl', pl)
  end if
  field_size = sum(pl)
  deallocate(pl)
  call grib_release(sp_grib_idx)
  call grib_close_file(sp_infile_idx)

  ! Reopen file to read in the loop further down
  call grib_open_file(sp_infile_idx, surface_pressure_file, 'r')

  ! Allocate input/output fields
  allocate(sp(field_size), stat=err)
  call check_exit_status(err, "Failed to allocate values for sp")
  !call grib_get(sp_grib_idx,'values' ,sp)

  allocate(var96field(field_size),stat=err)
  call check_exit_status(err, "Failed to allocate values for var96field")

  allocate(curr_ccbt(field_size),stat=err)
  call check_exit_status(err, "Failed to allocate values for curr_ccbt")


  ! Count the messages in the file
  call grib_count_in_file(var96_infile_idx, no_messages)
  write(*,*) "no_messages=", no_messages
  no_level1_entries = no_messages / levelSize

  allocate(ccbtfields_grib_idx(no_level1_entries), stat=err)
  call check_exit_status(err, "Failed to allocate values for ccbtfields_grib_idx")
  ccbtfields_grib_idx = -1

  write(*,*) "number of level1 messages", no_level1_entries

  ! Compute the ccb/t
  counter = 1
  DO i = 1, no_messages
    call grib_new_from_file(var96_infile_idx, var96fields_grib_idx, err)
    call check_exit_status(err, "Failed to read current var96 field")

    call grib_get(var96fields_grib_idx, 'level', curr_level)
    if (curr_level .eq. ccbt_level_in_var96) then
      call grib_new_from_file(sp_infile_idx, sp_fields_grib_idx, err)
      call check_exit_status(err, "Failed to read current surface pressure field")

      call grib_get(sp_fields_grib_idx,'values', sp)
      call grib_clone(sp_fields_grib_idx, ccbtfields_grib_idx(counter))
      call grib_set(ccbtfields_grib_idx(counter), 'indicatorOfParameter', 118)  ! Any experimental product no
      call grib_set(ccbtfields_grib_idx(counter), 'missingValue', missingValue)
      call grib_set(ccbtfields_grib_idx(counter), 'bitmapPresent', 1)
      call grib_get(var96fields_grib_idx, 'values', var96field)
      do ii=1, field_size
        if (var96field(ii) .eq. -1) then
          curr_ccbt(ii) = missingValue
        else
          curr_ccbt(ii) = hyam(INT(var96field(ii))) + hybm(INT(var96field(ii))) * sp(ii)
        end if
      end do
      call grib_set(ccbtfields_grib_idx(counter), 'values', pack(curr_ccbt, mask=.true.))
      call grib_write(ccbtfields_grib_idx(counter), ccbt_outfile_idx, err)
      counter = counter + 1
    end if
    call grib_release(var96fields_grib_idx)
    call grib_release(sp_fields_grib_idx)
  END DO

  ! Finalize
  DO i=1, no_level1_entries
    call grib_release(ccbtfields_grib_idx(i))
  END DO

  call grib_close_file(var96_infile_idx)
  call grib_close_file(sp_infile_idx)
  call grib_close_file(ccbt_outfile_idx)

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
end program compute_ccbt
