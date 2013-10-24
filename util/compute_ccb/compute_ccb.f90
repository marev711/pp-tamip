!
!  Description: 1) and the pressure on the model levels
!               2) use the information in 1 to compute the ccb 
!
!
!
program clone
  use grib_api
  implicit none
  integer                                       :: err,i,j,iret, plx,ply, ii
  integer                                       :: no_messages, counter
  integer                                       :: ny, field_size, no_levels, curr_level
  integer                                       :: sp_infile_idx,p_ml_outfile_idx, sp_grib_idx
  integer                                       :: var96_infile_idx, var96_idx 
  integer                                       :: igrib_source, ccb_outfile_idx
  integer                                       :: igrib_sp
  integer                                       :: igrib_out
  integer                                       :: PLPresent, nb_pl
  real, dimension(:), allocatable               :: sp
  real, dimension(:), allocatable               :: pl
  integer, dimension(:), allocatable            :: var96
  real, dimension(:), allocatable               :: p_ml_output_field
  real, dimension(:), allocatable               :: var96field, curr_ccb

  integer            :: ifile
  real(kind=8),dimension(:),allocatable     :: lats,lons,values
  integer(4)        :: numberOfPoints
  real(8)  :: missingValue=9999
  integer           :: count=0, no_level1_entries
  character(len=256) :: filename
  integer            :: levelSize
  integer,dimension(:),allocatable   ::  i_all_var96fields, i_ccbfields
  character(len=256)   ::  var96_file, p_ml_outfile, ccb_outfile
  character(len=256)   ::  surface_pressure_file
  
  var96_file = '/nobackup/rossby15/rossby/joint_exp/tamip/TMIP_2009-01-15/96.128.grb'
  var96_file = TRIM(var96_file)

  surface_pressure_file = '/nobackup/rossby15/rossby/joint_exp/tamip/TMIP_2009-01-15/134.128.grb'
  surface_pressure_file = TRIM(surface_pressure_file)

  p_ml_outfile = '/nobackup/rossby15/rossby/joint_exp/tamip/TMIP_2009-01-15/p_ml.grib1'
  p_ml_outfile = TRIM(p_ml_outfile)

  ccb_outfile = '/nobackup/rossby15/rossby/joint_exp/tamip/TMIP_2009-01-15/ccb.grib1'
  ccb_outfile = TRIM(ccb_outfile)

  call grib_open_file(sp_infile_idx, surface_pressure_file,'r')
  call grib_open_file(var96_infile_idx, var96_file,'r')
  call grib_open_file(p_ml_outfile_idx,p_ml_outfile,'w')
  call grib_open_file(ccb_outfile_idx,ccb_outfile,'w')
!
  call grib_index_create(var96_idx,var96_file,'level')

  ! get the number of distinct values of level in the index
  call grib_index_get_size(var96_idx,'level',levelSize)
  ! allocate the array to contain the list of distinct levels
  write(*,'(a,i3)') 'levelSize=',levelSize

  !     a new grib message is loaded from file
  !     igrib is the grib id to be used in subsequent calls
  call grib_new_from_file(sp_infile_idx, sp_grib_idx, err)
  call grib_get(sp_grib_idx,'PLPresent',PLPresent)
  if (PLPresent == 1) then
     call grib_get_size(sp_grib_idx,'pl',nb_pl)
     allocate(pl(nb_pl), stat=err)
     if (err .ne. 0) then
        print*, 'Failed to allocate ', field_size, ' values for pl'
        STOP
     end if
     call grib_get(sp_grib_idx,'pl',pl)
  end if
  field_size=sum(pl)
  deallocate(pl)
  allocate(sp(field_size), stat=err)
  if (err .ne. 0) then
     print*, 'Failed to allocate ', field_size, ' values for sp'
     STOP
  end if
  call grib_get(sp_grib_idx,'values',sp)


  allocate(p_ml_output_field(field_size),stat=err)
  if (err .ne. 0) then
     print*, 'Failed to allocate ', field_size, ' values for the p_ml_output_field'
     STOP
  end if

  allocate(var96field(field_size),stat=err)
  if (err .ne. 0) then
     print*, 'Failed to allocate ', field_size, ' values for the var96field'
     STOP
  end if

  allocate(curr_ccb(field_size),stat=err)
  if (err .ne. 0) then
     print*, 'Failed to allocate ', field_size, ' values for the curr_ccb'
     STOP
  end if


! count the messages in the file
  call grib_count_in_file(var96_infile_idx,no_messages)
  write(*,*) "no_messages=", no_messages
  write(*,*) "levelSize=", levelSize
  no_level1_entries = no_messages / levelSize
  allocate(i_all_var96fields(no_messages))
  allocate(i_ccbfields(no_level1_entries))
  i_all_var96fields=-1
  i_ccbfields=-1
 
  write(*,*) "number of level1 messages",no_level1_entries
  ! Load the messages from the file.
  counter = 1
  DO i=1,no_messages
     call grib_new_from_file(var96_infile_idx,i_all_var96fields(i), iret)
     call grib_get(i_all_var96fields(i), 'level', curr_level)
     if (curr_level .eq. 1) then
        call grib_clone(i_all_var96fields(i), i_ccbfields(counter))
        call grib_get(i_all_var96fields(i),'values',var96field)
        do ii=1, field_size
           curr_ccb(ii) = A(ABS(INT(var96field(ii)))) + B(ABS(INT(var96field(ii)))) * sp(ii)
        end do
        call grib_set(i_ccbfields(counter),'values',pack(curr_ccb, mask=.true.))
        call grib_write(i_ccbfields(counter), ccb_outfile_idx, err)
        counter = counter + 1
     end if
     if (mod(i, 100) .eq. 0) then
        write(*,*) i
     end if
  END DO

    call grib_clone(i_all_var96fields(1), igrib_out)
    call grib_set(igrib_out,'level',i)
    call grib_set(igrib_out,'indicatorOfParameter',54)

    ! use pack to create 1D values
    call grib_set(igrib_out,'values',pack(p_ml_output_field, mask=.true.))

    ! write cloned messages to a file
    call grib_write(igrib_out, p_ml_outfile_idx)

    call grib_release(igrib_out)
   call grib_release(i_all_var96fields(1))
   call grib_release(igrib_sp)

   call grib_close_file(var96_infile_idx)
   call grib_close_file(sp_infile_idx)
   call grib_close_file(p_ml_outfile_idx)
   deallocate(p_ml_output_field)

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
!======================================
end program clone
