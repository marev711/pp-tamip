!
!  Description: 1) Compute the convective cloud base/top (ccb/cct)
!                  The ccb/t is output in variable 96 where level one
!                  is the model level for cct and level two is the
!                  ditto for ccb. The ccb/t are then computed as, 
!
!                    ccb/t = A(var96(lev)) + B(var96(lev)) * surface_pressure
!
program clone
  use grib_api
  implicit none
  integer                              :: err, i, ii
  integer                              :: no_messages, counter, no_level1_entries
  integer                              :: field_size, curr_level, levelSize
  integer                              :: sp_infile_idx, var96_infile_idx
  integer                              :: ccbt_outfile_idx
  integer                              :: var96_index_idx, sp_grib_idx
  integer                              :: PLPresent, nb_pl
  integer                              :: ccbt_level
  real                                 :: missingValue
  real, dimension(:), allocatable      :: sp, pl
  real, dimension(:), allocatable      :: var96field, curr_ccbt

  integer,dimension(:),allocatable     :: var96fields_grib_idx
  integer,dimension(:),allocatable     :: ccbtfields_grib_idx
  character(len=256)                   :: var96_file, ccbt_outfile
  character(len=256)                   :: surface_pressure_file

  namelist /indata/ var96_file, surface_pressure_file, ccbt_outfile, ccbt_level
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


  ! Allocate input/output fields
  allocate(sp(field_size), stat=err)
  call check_exit_status(err, "Failed to allocate values for sp")
  call grib_get(sp_grib_idx,'values' ,sp)

  allocate(var96field(field_size),stat=err)
  call check_exit_status(err, "Failed to allocate values for var96field")

  allocate(curr_ccbt(field_size),stat=err)
  call check_exit_status(err, "Failed to allocate values for curr_ccbt")


  ! Count the messages in the file
  call grib_count_in_file(var96_infile_idx, no_messages)
  write(*,*) "no_messages=", no_messages
  no_level1_entries = no_messages / levelSize

  ! Allocate arrays to hold all grib messages
  allocate(var96fields_grib_idx(no_messages), stat=err)
  call check_exit_status(err, "Failed to allocate values for var96fields_grib_idx")
  var96fields_grib_idx = -1

  allocate(ccbtfields_grib_idx(no_level1_entries), stat=err)
  call check_exit_status(err, "Failed to allocate values for ccbtfields_grib_idx")
  ccbtfields_grib_idx = -1
 
  write(*,*) "number of level1 messages", no_level1_entries

  ! Compute the ccb/t
  counter = 1
  DO i = 1, no_messages
    call grib_new_from_file(var96_infile_idx, var96fields_grib_idx(i), err)
    call grib_get(var96fields_grib_idx(i), 'level', curr_level)
    if (curr_level .eq. ccbt_level) then
      call grib_clone(var96fields_grib_idx(i), ccbtfields_grib_idx(counter))
      call grib_set(ccbtfields_grib_idx(counter), 'indicatorOfParameter', 118)  ! Any experimental product no
      call grib_set(ccbtfields_grib_idx(counter), 'missingValue', missingValue)
      call grib_set(ccbtfields_grib_idx(counter), 'bitmapPresent', 1)
      call grib_get(var96fields_grib_idx(i), 'values', var96field)
      do ii=1, field_size
        if (var96field(ii) .eq. -1) then
          curr_ccbt(ii) = missingValue
        else
          curr_ccbt(ii) = A(INT(var96field(ii))) + B(INT(var96field(ii))) * sp(ii)
        end if
      end do
      call grib_set(ccbtfields_grib_idx(counter), 'values', pack(curr_ccbt, mask=.true.))
      call grib_write(ccbtfields_grib_idx(counter), ccbt_outfile_idx, err)
      counter = counter + 1
    end if
  END DO

  ! Finalize
  DO i=1, no_messages
    call grib_release(var96fields_grib_idx(i))
  END DO
  DO i=1, no_level1_entries
    call grib_release(ccbtfields_grib_idx(i))
  END DO

  call grib_close_file(var96_infile_idx)
  call grib_close_file(sp_infile_idx)
  call grib_close_file(ccbt_outfile_idx)

contains
!======================================
real function A(idx)
  integer   :: idx
  real, parameter, dimension(91)  ::  A_val=(/    0.0000,        2.0000,        3.9808,        7.3872,       12.9080, &
                                                 21.4140,       33.9530,       51.7470,       76.1680,      108.7200, &
                                                150.9900,      204.6400,      271.3600,      352.8200,      450.6900, &
                                                566.5200,      701.8100,      857.9500,     1036.2000,     1237.6000, &
                                               1463.2000,     1713.7000,     1989.9000,     2292.2000,     2620.9000, &
                                               2976.3000,     3358.4000,     3767.2000,     4202.4000,     4663.8000, &
                                               5150.9000,     5663.2000,     6199.8000,     6759.7000,     7341.5000, &
                                               7942.9000,     8564.6000,     9208.3000,     9873.6000,    10559.0000, &
                                              11262.0000,    11983.0000,    12714.0000,    13453.0000,    14192.0000, &
                                              14923.0000,    15638.0000,    16330.0000,    16991.0000,    17613.0000, &
                                              18191.0000,    18717.0000,    19185.0000,    19588.0000,    19920.0000, &
                                              20175.0000,    20349.0000,    20434.0000,    20426.0000,    20319.0000, &
                                              20107.0000,    19785.0000,    19349.0000,    18799.0000,    18141.0000, &
                                              17386.0000,    16545.0000,    15634.0000,    14666.0000,    13653.0000, &
                                              12608.0000,    11543.0000,    10471.0000,     9405.2000,     8356.3000, &
                                               7335.2000,     6353.9000,     5422.8000,     4550.2000,     3743.5000, &
                                               3010.1000,     2356.2000,     1784.9000,     1297.7000,      895.1900, &
                                                576.3100,      336.7700,      162.0400,       54.2080,        6.5756, &
                                                  0.31600E-02/)
  A = A_val(idx)
  return
end function A

real function B(idx)
  integer   :: idx
  real, parameter, dimension(91)  ::  B_val=(/0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.0000,      &
                                              0.0000,        0.0000,        0.0000,        0.0000,        0.27240E-06, &
                                              0.13912E-04,   0.54667E-04,   0.13136E-03,   0.27888E-03,   0.54838E-03, &
                                              0.10001E-02,   0.17011E-02,   0.27647E-02,   0.42670E-02,   0.63222E-02, &
                                              0.90350E-02,   0.12508E-01,   0.16860E-01,   0.22189E-01,   0.28610E-01, &
                                              0.36227E-01,   0.45146E-01,   0.55474E-01,   0.67316E-01,   0.80777E-01, &
                                              0.95964E-01,   0.11298,       0.13193,       0.15293,       0.17609,     &
                                              0.20152,       0.22931,       0.25955,       0.29199,       0.32633,     &
                                              0.36220,       0.39920,       0.43691,       0.47502,       0.51328,     &
                                              0.55146,       0.58932,       0.62656,       0.66293,       0.69822,     &
                                              0.73222,       0.76468,       0.79538,       0.82419,       0.85095,     &
                                              0.87552,       0.89777,       0.91765,       0.93516,       0.95027,     &
                                              0.96301,       0.97347,       0.98224,       0.98915,       0.99420,     &
                                              0.99763/)


  B = B_val(idx)
  return
end function B

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
end program clone
