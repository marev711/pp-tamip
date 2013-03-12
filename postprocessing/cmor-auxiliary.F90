MODULE NDATASET
     CHARACTER(256) :: inpath, outpath, experiment_id, institution, source, &
     & calendar, contact, history, comment, references, model_id, &
     & forcing, institute_id, parent_experiment_id, parent_experiment_rip
     INTEGER :: realization
     DOUBLE PRECISION :: branch_time
END MODULE NDATASET

MODULE NCTL
  INTEGER :: NYEAR0, NYEAR, NYEARS, NMONTH, NMDAYS, NMININT, NPRINTLEV
  LOGICAL :: LWGRIB
  CHARACTER(256) :: suffix_mon_sfc, suffix_mon_plev, suffix_mon_mlev
  CHARACTER(256) :: suffix_day_sfc, suffix_day_plev
  CHARACTER(256) :: suffix_3h_sfc, suffix_6h_plev, suffix_6h_mlev
END MODULE NCTL

MODULE CMOR_TAMIP_ROUTINES

  USE cmor_users_functions
    PRIVATE
      PUBLIC read_nam, read_coords, read_coords_vert, read_time, &
      read_2d_input_files_mon, read_2d_input_files_day, read_2d_input_files_6h, &
      read_2d_input_files_3h, handle_err
      CONTAINS
  SUBROUTINE read_nam
    USE NCTL
    USE NDATASET

    IMPLICIT NONE

    INTEGER :: NULNAM

#include "namctl.h"
#include "namdataset.h"

!   default values overwritten by namelist NAMCTL read
!   NYEAR0     : year since we count the days passed
!   NYEAR      : year of the monthly output file
!   NMONTH     : month of the monthly output file
!   NMDAYS     : length of month (in days)
!   NMININT    : minimum interval for the input data
!   NPRINTLEV  : print level (=0 reduced log output, 1 and 2 more log output)
!   LWGRIB     : optional write grib output (in addition to cmor netcdf files)
!   suffix_mon_sfc : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_mon_plev : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_mon_mlev : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_day_sfc : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_day_plev : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_3h_sfc : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_6h_plev : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
!   suffix_6h_mlev : file_suffix for cmor_write (if "" then open a new file, if not then append to the previous file)
    NYEAR0=1850
    NYEAR=1850
    NMONTH=01
    NMDAYS=31
    NMININT=3
    NPRINTLEV=0
    LWGRIB=.FALSE.
    suffix_mon_sfc=""
    suffix_mon_plev=""
    suffix_mon_mlev=""
    suffix_day_sfc=""
    suffix_day_plev=""
    suffix_3h_sfc=""
    suffix_6h_plev=""
    suffix_6h_mlev=""

!   default values overwritten by namelist NAMDATASET read
    inpath="/perm/rd/neu/GRIBTOCMOR/CMIP5Tables"
    outpath="/c1a/tmp/rd/neu/GRIBTOCMOR/N80reg"
    experiment_id="historical"
    institution="EC-Earth (European Earth System Model)"
    source="EC-Earth 2.3 (2011); atmosphere: IFS (cy31R1+modifications, T159L62); ocean: NEMO (version2+modifications, ORCA1-42lev); sea ice: LIM2; land: HTessel"
    calendar="gregorian"
    realization=1
    contact="xxx"
    history="Output from archive/historical"
    comment="Equilibrium reached after preindustrial spin-up after which data were output starting with nominal date of January 1850"
    references="Model described by Hazeleger et al. (Bull. Amer. Meteor. Soc., 2010, 91, 1357-1363). Also see http://ecearth.knmi.nl."
    model_id="EC-Earth"
    forcing="Nat"
    institute_id ="EC-Earth"
    parent_experiment_id="N/A"
    branch_time=0.
    parent_experiment_rip="N/A"

    NULNAM=4

    OPEN(NULNAM,FILE="nam",DELIM="QUOTE")
    READ(NULNAM,NAMCTL)
    READ(NULNAM,NAMDATASET)
    CLOSE(NULNAM)

    WRITE(6,*) 'NYEAR0=',NYEAR0
    WRITE(6,*) 'NYEAR=',NYEAR
    NYEARS=NYEAR-(NYEAR0-1)
    WRITE(6,*) 'NYEARS=',NYEARS
    WRITE(6,*) 'NMONTH=',NMONTH
    WRITE(6,*) 'NMDAYS=',NMDAYS
    WRITE(6,*) 'NMININT=',NMININT
    WRITE(6,*) 'NPRINTLEV=',NPRINTLEV
    WRITE(6,*) 'LWGRIB=',LWGRIB
    WRITE(6,*) 'suffix_mon_sfc=',suffix_mon_sfc
    WRITE(6,*) 'suffix_mon_plev=',suffix_mon_plev
    WRITE(6,*) 'suffix_mon_mlev=',suffix_mon_mlev
    WRITE(6,*) 'suffix_day_sfc=',suffix_day_sfc
    WRITE(6,*) 'suffix_day_plev=',suffix_day_plev
    WRITE(6,*) 'suffix_3h_sfc=',suffix_3h_sfc
    WRITE(6,*) 'suffix_6h_plev=',suffix_6h_plev
    WRITE(6,*) 'suffix_6h_mlev=',suffix_6h_mlev

    RETURN
  END SUBROUTINE read_nam

  SUBROUTINE read_coords(alats, alons, plevs, plev8, plev3, bnds_lat, bnds_lon)

    USE NCTL

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alats
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alons
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plevs
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plev8
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plev3
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lat
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lon
    DOUBLE PRECISION, DIMENSION(SIZE(alats)) :: lats

    INTEGER :: i

101 FORMAT(I5,3F10.5)
    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, alons(i), bnds_lon(1,i), bnds_lon(2,i)'
    do i=1,SIZE(alons)
       alons(i)=(i-1)*360./SIZE(alons)
       bnds_lon(1,i) = alons(i) - 180./SIZE(alons)
       bnds_lon(2,i) = alons(i) + 180./SIZE(alons)
       IF (NPRINTLEV.GT.0) WRITE(6,101) i, alons(i), bnds_lon(1,i), bnds_lon(2,i)
    enddo

100 FORMAT(50X,F9.5)
    OPEN(1,FILE="N80Gaussian",FORM='formatted')
    do i=1,4
      READ(1,*)
    enddo
    do i=1,SIZE(alats)
      READ(1,100) lats(i)
! reorder from S to N
!      alats(SIZE(alats)-i+1)=lats(i)
! reordering not needed, done authomatically by CMOR
      alats(i)=lats(i)
    enddo
    CLOSE(1)

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, alats(i), bnds_lat(1,i), bnds_lat(2,i)'
    do i=1,SIZE(alats)
      if (i.eq.1) then
!        bnds_lat(1,i)=-90. ! when reordering from S to N
        bnds_lat(1,i)=90.
      else
        bnds_lat(1,i)=alats(i)-(alats(i)-alats(i-1))/2
      endif
      if (i.eq.SIZE(alats)) then
!        bnds_lat(2,i)=90. ! when reordering from S to N
        bnds_lat(2,i)=-90.
      else
        bnds_lat(2,i)=alats(i)+(alats(i+1)-alats(i))/2
      endif
      IF (NPRINTLEV.GT.0) WRITE(6,101) i, alats(i), bnds_lat(1,i), bnds_lat(2,i)
    enddo

102 FORMAT(I5,F20.5)

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, plevs(i)'
    plevs = (/100000., 92500., 85000., 70000.,&
     60000., 50000., 40000., 30000., 25000., 20000.,&
     15000., 10000., 7000., 5000., 3000., 2000. /)
    IF (NPRINTLEV.GT.0) THEN
      do i = 1, SIZE(plevs)
        WRITE(6,102) i, plevs(i)
      enddo
    ENDIF

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, plev8(i)'
    plev8 = (/100000., 85000., 70000.,&
     50000., 25000., 10000., 5000. /)
    IF (NPRINTLEV.GT.0) THEN
      do i = 1, SIZE(plev8)
        WRITE(6,102) i, plev8(i)
      enddo
    ENDIF

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, plev3(i)'
    plev3 = (/85000., 50000., 25000. /)
    IF (NPRINTLEV.GT.0) THEN
      do i = 1, SIZE(plev3)
        WRITE(6,102) i, plev3(i)
      enddo
    ENDIF

    RETURN
  END SUBROUTINE read_coords

  SUBROUTINE read_coords_vert(zlevs, a_coeff, b_coeff, zlev_bnds, a_coeff_bnds, b_coeff_bnds, p0)

    USE NCTL

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: zlevs
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: zlev_bnds
    REAL, INTENT(OUT), DIMENSION(:) :: a_coeff
    REAL, INTENT(OUT), DIMENSION(:) :: a_coeff_bnds
    REAL, INTENT(OUT), DIMENSION(:) :: b_coeff
    REAL, INTENT(OUT), DIMENSION(:) :: b_coeff_bnds
    REAL, INTENT(OUT) :: p0

    DOUBLE PRECISION, DIMENSION(SIZE(zlev_bnds)) :: vetaf

    INTEGER :: i

    p0 = 101325.
    IF (NPRINTLEV.GT.0) WRITE(6,*) 'p0=',p0

200 FORMAT(18X,F13.10,F25.10)
201 FORMAT(I5,3F15.10)
    OPEN(2,FILE="A_B_full_lev",FORM='formatted')
    do i=1,2
      READ(2,*)
    enddo
    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, a_coeff(i), b_coeff(i), zlevs(i)'
    do i=1,SIZE(zlevs)
      READ(2,200) b_coeff(i), a_coeff(i)
      a_coeff(i)=a_coeff(i)/p0
      zlevs(i)=a_coeff(i)+b_coeff(i)
      IF (NPRINTLEV.GT.0) WRITE(6,201) i, a_coeff(i), b_coeff(i), zlevs(i)
    enddo
    CLOSE(2)

300 FORMAT(31X,F25.10,F25.10)
301 FORMAT(I5,4F15.10)
    OPEN(3,FILE="A_B_half_lev",FORM='formatted')
    do i=1,2
      READ(3,*)
    enddo
    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, a_coeff_bnds(i), b_coeff_bnds(i), zlev_bnds(i), vetaf(i)'
    do i=1,SIZE(zlev_bnds)
      READ(3,300) b_coeff_bnds(i), a_coeff_bnds(i)
      a_coeff_bnds(i)=a_coeff_bnds(i)/p0
      zlev_bnds(i)=a_coeff_bnds(i)+b_coeff_bnds(i)
      if (i.eq.1) then
        vetaf(i)=0.
      else
        vetaf(i)=(zlev_bnds(i)+zlev_bnds(i-1))*0.5
      endif
      IF (NPRINTLEV.GT.0) WRITE(6,301) i, a_coeff_bnds(i), b_coeff_bnds(i), zlev_bnds(i), vetaf(i)
    enddo
    CLOSE(3)

    RETURN
  END SUBROUTINE read_coords_vert

   SUBROUTINE read_time(time_mon, time_mon_bnds, time_day, time_day_bnds, time_6h, time_6h_bnds, time1_6h, time_3h, time_3h_bnds, time1_3h)

!  Original version: S. Stefanescu, ECMWF 2.12.2011
!  Modifications: Bugfixes for computation of NTOTDAYS: J. von Hardenberg, ISAC-CNR, 5.12.2011

    USE NCTL

    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_mon
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_mon_bnds
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_day
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_day_bnds
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_6h
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_6h_bnds
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time1_6h
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_3h
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_3h_bnds
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time1_3h

    INTEGER :: iyr, imo, iday, i6h, i3h
    INTEGER :: NTOTDAYS
    INTEGER, DIMENSION(NYEARS) :: NYRDAYS
    INTEGER, DIMENSION(12) :: NMODAYS

    NMODAYS(1)=31
    NMODAYS(3)=31
    NMODAYS(4)=30
    NMODAYS(5)=31
    NMODAYS(6)=30
    NMODAYS(7)=31
    NMODAYS(8)=31
    NMODAYS(9)=30
    NMODAYS(10)=31
    NMODAYS(11)=30
    NMODAYS(12)=31

    NTOTDAYS=0
    DO iyr=NYEAR0,NYEAR
      IF ((MOD(iyr,400).EQ.0).OR.((MOD(iyr,100).NE.0).AND.(MOD(iyr,4).EQ.0))) THEN
        NYRDAYS(iyr-NYEAR0+1)=366
        IF (iyr.EQ.NYEAR) NMODAYS(2)=29
      ELSE
        NYRDAYS(iyr-NYEAR0+1)=365
        IF (iyr.EQ.NYEAR) NMODAYS(2)=28
      ENDIF
      IF (iyr.LT.NYEAR) NTOTDAYS=NTOTDAYS+NYRDAYS(iyr-NYEAR0+1)
    ENDDO

    IF ((NMONTH.EQ.2).AND.(NMODAYS(2).NE.NMDAYS)) THEN
      WRITE(6,*) 'STOP !!! different number of days for February !!!'
      WRITE(6,*) 'NMODAYS(2)=',NMODAYS(2),'NMDAYS=',NMDAYS
      STOP
    ENDIF

    DO imo=1,NMONTH
      IF (imo.LT.NMONTH) NTOTDAYS=NTOTDAYS+NMODAYS(imo)
    ENDDO

    WRITE(6,*)
    WRITE(6,*) 'Number of days passed since',NYEAR0,'until year',NYEAR,'beginning of month',NMONTH,'is NTOTDAYS=',NTOTDAYS
    WRITE(6,*)

401 FORMAT(3F20.5)
402 FORMAT(I5,3F15.5)
403 FORMAT(I5,4F15.5)

    time_mon = REAL(NTOTDAYS) + REAL(NMODAYS(NMONTH))/2
    time_mon_bnds(1,1)= REAL(NTOTDAYS)
    time_mon_bnds(2,1)= REAL(NTOTDAYS) + REAL(NMODAYS(NMONTH))

    WRITE(6,*) '           time_mon  time_mon_bnds(1,1)  time_mon_bnds(2,1)'
    WRITE(6,401) time_mon, time_mon_bnds(1,1), time_mon_bnds(2,1)
    WRITE(6,*)

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'iday, time_day(iday), time_day_bnds(1,iday), time_day_bnds(2,iday)'
!    DO iday=1,NMODAYS(NMONTH)
    DO iday=1,SIZE(time_day)
     time_day(iday) = REAL(NTOTDAYS) + REAL(iday) - 0.5
     time_day_bnds(1,iday) = REAL(NTOTDAYS) + REAL(iday) - 1.
     time_day_bnds(2,iday) = REAL(NTOTDAYS) + REAL(iday)
     IF (NPRINTLEV.GT.0) WRITE(6,402) iday, time_day(iday), time_day_bnds(1,iday), time_day_bnds(2,iday)
    ENDDO

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i6h, time1_6h(i6h), time_6h(i6h), time_6h_bnds(1,i6h), time_6h_bnds(2,i6h)'
!    DO i6h=1,NMODAYS(NMONTH)*4
    DO i6h=1,SIZE(time_6h)
     time_6h(i6h) = REAL(NTOTDAYS) + i6h*0.25 - 0.125
     time_6h_bnds(1,i6h) = REAL(NTOTDAYS) + (i6h-1)*0.25
     time_6h_bnds(2,i6h) = REAL(NTOTDAYS) + i6h*0.25
     time1_6h(i6h) = REAL(NTOTDAYS) + i6h*0.25
     IF (NPRINTLEV.GT.0) WRITE(6,403) i6h, time1_6h(i6h), time_6h(i6h), time_6h_bnds(1,i6h), time_6h_bnds(2,i6h)
    ENDDO

    IF (NPRINTLEV.GT.0) WRITE(6,*) 'i3h, time1_3h(i3h), time_3h(i3h), time_3h_bnds(1,i3h), time_3h_bnds(2,i3h)'
!    DO i3h=1,NMODAYS(NMONTH)*8
    DO i3h=1,SIZE(time_3h)
      time_3h(i3h) = REAL(NTOTDAYS) + i3h*0.125 - 0.0625
      time_3h_bnds(1,i3h) = REAL(NTOTDAYS) + (i3h-1)*0.125
      time_3h_bnds(2,i3h) = REAL(NTOTDAYS) + i3h*0.125
      time1_3h(i3h) = REAL(NTOTDAYS) + i3h*0.125
      IF (NPRINTLEV.GT.0) WRITE(6,403) i3h, time1_3h(i3h), time_3h(i3h), time_3h_bnds(1,i3h), time_3h_bnds(2,i3h)
    ENDDO

    RETURN
  END SUBROUTINE read_time

  SUBROUTINE handle_err(status, routine_name)
    IMPLICIT NONE
    integer, intent(in) :: status
    character(len=*), intent(in) :: routine_name

    WRITE(*,*) "status=", status
    CALL ABORT(routine_name)
  END SUBROUTINE handle_err



!!   SUBROUTINE read_2d_input_files_mon(field)
!! 
!!     USE IGRIB
!!     USE RDATA
!! 
!!     IMPLICIT NONE
!! 
!!     REAL, INTENT(OUT), DIMENSION(:,:) :: field
!! 
!!     INTEGER :: i, j
!! 
!!     DO j=1,SIZE(field,2)
!!       DO i=1,SIZE(field,1)
!!         field(i,j) = RMMEAN((j-1)*SIZE(field,1)+i)
!!       ENDDO
!!     ENDDO
!! 
!!   END SUBROUTINE read_2d_input_files_mon
!! 
!!   SUBROUTINE read_2d_input_files_day(field)
!! 
!!     USE NCTL
!!     USE IGRIB
!!     USE RDATA
!! 
!!     IMPLICIT NONE
!! 
!!     REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
!! 
!!     INTEGER :: i, j, iday
!! 
!!     field(:,:,:)=0.
!! 
!!     DO iday=1,NMDAYS
!!       DO j=1,SIZE(field,2)
!!         DO i=1,SIZE(field,1)
!!           field(i,j,iday) = RDAY(iday,(j-1)*SIZE(field,1)+i)
!!         ENDDO
!!       ENDDO
!!     ENDDO
!! 
!!   END SUBROUTINE read_2d_input_files_day
!! 
!!   SUBROUTINE read_2d_input_files_6h(field)
!! 
!!     USE NCTL
!!     USE IGRIB
!!     USE RGRIB
!! 
!!     IMPLICIT NONE
!! 
!!     REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
!! 
!!     INTEGER :: i, j, ih6
!! 
!!     field(:,:,:)=0.
!! 
!!     DO ih6=1,NMDAYS*4
!!       DO j=1,SIZE(field,2)
!!         DO i=1,SIZE(field,1)
!!           field(i,j,ih6) = RSEC4(ih6,(j-1)*SIZE(field,1)+i)
!!         ENDDO
!!       ENDDO
!!     ENDDO
!! 
!!   END SUBROUTINE read_2d_input_files_6h
!! 
!!   SUBROUTINE read_2d_input_files_3h(field)
!! 
!!     USE NCTL
!!     USE IGRIB
!!     USE RGRIB
!! 
!!     IMPLICIT NONE
!! 
!!     REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
!! 
!!     INTEGER :: i, j, ih3
!! 
!!     field(:,:,:)=0.
!! 
!!     DO ih3=1,NMDAYS*8
!!       DO j=1,SIZE(field,2)
!!         DO i=1,SIZE(field,1)
!!           field(i,j,ih3) = RSEC4(ih3,(j-1)*SIZE(field,1)+i)
!!         ENDDO
!!       ENDDO
!!     ENDDO
!! 
!!   END SUBROUTINE read_2d_input_files_3h

END MODULE CMOR_TAMIP_ROUTINES
