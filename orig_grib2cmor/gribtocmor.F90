!**********************************
!**********************************
!***                            ***
!***  GRIBTOCMOR                ***
!***  Version 1.1 -  5.12.2011  ***
!***                            ***
!**********************************
!**********************************

MODULE IGRIB
  INTEGER, PARAMETER :: JPFMAX=1000
  INTEGER, PARAMETER :: JPTMAX=22000
  INTEGER NSEC0(2)
  INTEGER NSEC1(1024)
  INTEGER NSEC2(1024)
  INTEGER NSEC3(2)
  INTEGER NSEC4(512)
  INTEGER NFIELDS
  INTEGER NSTEPS(0:JPFMAX)
  INTEGER NTSEC1(JPTMAX,5:18)
  INTEGER NPUNIT
  INTEGER NWUNIT(5)
  INTEGER NFSEC1(JPFMAX,1:9)
  INTEGER NS1ADD(10:48)
  INTEGER ITOTP
  INTEGER ITOTW
  INTEGER NACCFLAG(JPFMAX)
  INTEGER NINTERVAL(JPFMAX)
END MODULE IGRIB

MODULE RGRIB
  REAL*8, dimension(512)              :: RSEC2
  REAL*8, dimension(2)                :: RSEC3
  REAL*8, dimension(:,:), allocatable :: RSEC4
  REAL*8                              :: RMISS
END MODULE RGRIB

MODULE RDATA
  REAL*8, dimension(:), allocatable :: RMMEAN
  REAL*8, dimension(:), allocatable :: RMMAX
  REAL*8, dimension(:), allocatable :: RMMIN
  REAL*8, dimension(:), allocatable :: RMSD
  REAL*8, dimension(:,:), allocatable :: RDAY
END MODULE RDATA

MODULE NCTL
  INTEGER :: NYEAR0, NYEAR, NYEARS, NMONTH, NMDAYS, NMININT, NPRINTLEV
  LOGICAL :: LWGRIB
  CHARACTER(256) :: suffix_mon_sfc, suffix_mon_plev, suffix_mon_mlev
  CHARACTER(256) :: suffix_day_sfc, suffix_day_plev
  CHARACTER(256) :: suffix_3h_sfc, suffix_6h_plev, suffix_6h_mlev
END MODULE NCTL

MODULE NDATASET
  CHARACTER(256) :: inpath, outpath, experiment_id, institution, source, &
                  & calendar, contact, history, comment, references, model_id, &
                  & forcing, institute_id, parent_experiment_id, parent_experiment_rip
  INTEGER :: realization
  DOUBLE PRECISION :: branch_time
END MODULE NDATASET

MODULE local_subrs

  USE cmor_users_functions
  PRIVATE
  PUBLIC read_nam, read_coords, read_coords_vert, read_time, read_2d_input_files_mon, read_2d_input_files_day, read_2d_input_files_6h, read_2d_input_files_3h
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

  SUBROUTINE read_2d_input_files_mon(field)

    USE IGRIB
    USE RDATA

    IMPLICIT NONE

    REAL, INTENT(OUT), DIMENSION(:,:) :: field

    INTEGER :: i, j

    DO j=1,SIZE(field,2)
      DO i=1,SIZE(field,1)
        field(i,j) = RMMEAN((j-1)*SIZE(field,1)+i)
      ENDDO
    ENDDO

  END SUBROUTINE read_2d_input_files_mon
  
  SUBROUTINE read_2d_input_files_day(field)

    USE NCTL
    USE IGRIB
    USE RDATA

    IMPLICIT NONE

    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field

    INTEGER :: i, j, iday

    field(:,:,:)=0.

    DO iday=1,NMDAYS
      DO j=1,SIZE(field,2)
        DO i=1,SIZE(field,1)
          field(i,j,iday) = RDAY(iday,(j-1)*SIZE(field,1)+i)
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE read_2d_input_files_day

  SUBROUTINE read_2d_input_files_6h(field)

    USE NCTL
    USE IGRIB
    USE RGRIB

    IMPLICIT NONE

    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field

    INTEGER :: i, j, ih6

    field(:,:,:)=0.

    DO ih6=1,NMDAYS*4
      DO j=1,SIZE(field,2)
        DO i=1,SIZE(field,1)
          field(i,j,ih6) = RSEC4(ih6,(j-1)*SIZE(field,1)+i)
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE read_2d_input_files_6h

  SUBROUTINE read_2d_input_files_3h(field)

    USE NCTL
    USE IGRIB
    USE RGRIB

    IMPLICIT NONE

    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field

    INTEGER :: i, j, ih3

    field(:,:,:)=0.

    DO ih3=1,NMDAYS*8
      DO j=1,SIZE(field,2)
        DO i=1,SIZE(field,1)
          field(i,j,ih3) = RSEC4(ih3,(j-1)*SIZE(field,1)+i)
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE read_2d_input_files_3h

END MODULE local_subrs

  PROGRAM GRIBTOCMOR
!
!****  GRIBTOCMOR - Converts IFS grib output to CMOR2 netCDF output as requested for CMIP5
!
!      Original version: S. Stefanescu, ECMWF 2.12.2011
!
!      Original subroutines GETALL, MMEAN and WRITEALL: T. Stockdale, ECMWF 29.10.2007
!      Modifications: Work for EC-EARTH monthly files: S. Stefanescu, ECMWF 30.06.2008
!      Modifications: New modules and conversions for CMOR2 variables: S. Stefanescu, ECMWF 2.12.2011
!
!     INPUT
!     -----
!       Will read the grib file fcdata.
!
!       Works with atmosphere data for one month. Data must all be on the same grid and
!       of the same basic type (grid-point fields on regular Gaussian grid N80).
!       Differences in parameter, frequency and level are the only ones allowed.
!
!       The program also computes monthly and daily means and monthly maximum, minimum
!       and standard deviation.
!
!       Namelist NAMCTL controls the lenght of month (in days), the minimum interval
!       for the input data and the optional grib output.
!       Namelist NAMDATASET controls the global atributes for CMOR2 output.
!
!     OUTPUT
!     ------
!        CMIP5 output: netCDF CMOR2 files
!
!        If the optional grib output is selected, will create files: fcdmean -> daily mean 
!                                                                    fcmmean -> monthly mean
!                                                                    fcmmax  -> monthly max
!                                                                    fcmmin  -> monthly min
!                                                                    fcmsd   -> monthly sd
!
!        These will contain a set of grib files ready to be archived into MARS.
!
! ***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!***
! - 152 on model level 1 must be present before the model level variables (to be able to define the vertical coordinate)
! - pressure level fields must be ordered from 1000 to 20 hPa (in the input file)
! - model level fields must be ordered from 1 to 62 (in the input file)
! - in the future add a flag for variables that do not need daily mean computation (to save computing time)
! ***!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!***
!
!--------------------------------------------------------------------------------------------

  USE NCTL
  USE NDATASET
  USE IGRIB
  USE RGRIB

! include module that contains the user-accessible cmor functions.
  USE cmor_users_functions
  USE local_subrs

  IMPLICIT NONE

  ! Dimension parameters:
  ! ---------------------------------
  INTEGER, PARAMETER :: ndays = 31    ! maximum numbers of days in a month
  INTEGER, PARAMETER :: lon = 320     ! number of longitude grid cells
  INTEGER, PARAMETER :: lat = 160     ! number of latitude grid cells
  INTEGER, PARAMETER :: levs = 16     ! number of standard pressure levels output (Amon)
  INTEGER, PARAMETER :: lev8 = 7      ! number of standard pressure levels output (day)
  INTEGER, PARAMETER :: lev3 = 3      ! number of standard pressure levels output (6hrPlev)
  INTEGER, PARAMETER :: lev = 62      ! number of full model levels
  INTEGER, PARAMETER :: n2d_mon = 22  ! number of 2d fields to be output
  INTEGER, PARAMETER :: n2d_day = 14  ! number of 2d fields to be output
  INTEGER, PARAMETER :: n2d_3h = 13   ! number of 2d fields to be output
  INTEGER, PARAMETER :: n3d_monpl = 5 ! number of 3d pl fields to be output
  INTEGER, PARAMETER :: n3d_daypl = 5 ! number of 3d pl fields to be output
  INTEGER, PARAMETER :: n3d_6hpl = 3  ! number of 3d pl fields to be output
  INTEGER, PARAMETER :: n3d_monml = 3 ! number of 3d ml fields to be output
  INTEGER, PARAMETER :: n3d_6hml = 4  ! number of 3d ml fields to be output
  ! ----------------------------------

  ! My parameter number for 2d fields
  INTEGER, DIMENSION(n2d_mon) :: parin2d_mon=(/139, 137, 143, 228, 144, 146, 147, &
  & 151, 152, 164, 165, 166, 167, 169, 175, 179, 182, 201, 202, 205, 207, 212/)
  INTEGER, DIMENSION(n2d_day) :: parin2d_day=(/143, 228, 144, 146, 147, &
  & 151, 164, 167, 169, 175, 179, 201, 202, 207/)
  INTEGER, DIMENSION(n2d_3h) :: parin2d_3h=(/143, 228, 144, 146, 147, &
  & 151, 152, 164, 165, 166, 167, 169, 175/)

  ! My variable names for 2d fields
  CHARACTER (LEN=10), DIMENSION(n2d_mon) :: varin2d_mon=(/'STL1      ', 'TCWV      ', &
& 'CP        ', 'TP=LSP+CP ', 'SF        ', 'SSHF      ', 'SLHF      ', 'MSL       ', &
& 'LNSP      ', 'TCC       ', '10U       ', '10V       ', '2T        ', 'SSRD      ', &
& 'STRD      ', 'TTR       ', 'E         ', 'MX2T      ', 'MN2T      ', 'RO        ', &
& '10SI      ', 'TISR      '/)
  CHARACTER (LEN=10), DIMENSION(n2d_day) :: varin2d_day=(/'CP        ', &
& 'TP=LSP+CP ', 'SF        ', 'SSHF      ', 'SLHF      ', 'MSL       ', 'TCC       ', &
& '2T        ', 'SSRD      ', 'STRD      ', 'TTR       ', 'MX2T      ', 'MN2T      ', &
& '10SI      '/)
  CHARACTER (LEN=10), DIMENSION(n2d_3h)  :: varin2d_3h=(/ 'CP        ', 'TP=LSP+CP ', &
& 'SF        ', 'SSHF      ', 'SLHF      ', 'MSL       ', 'LNSP      ', 'TCC       ', &
& '10U       ', '10V       ', '2T        ', 'SSRD      ', 'STRD      '/)

  ! Units appropriate to my data
  CHARACTER (LEN=10), DIMENSION(n2d_mon) :: units2d_mon=(/'K         ', 'kg m-2    ', &
& 'kg m-2 s-1', 'kg m-2 s-1', 'kg m-2 s-1', 'W m-2     ', 'W m-2     ', 'Pa        ', &
& 'Pa        ', '%         ', 'm s-1     ', 'm s-1     ', 'K         ', 'W m-2     ', &
& 'W m-2     ', 'W m-2     ', 'kg m-2 s-1', 'K         ', 'K         ', 'kg m-2 s-1', &
& 'm s-1     ', 'W m-2     '/)
  CHARACTER (LEN=10), DIMENSION(n2d_day) :: units2d_day=(/'kg m-2 s-1', &
& 'kg m-2 s-1', 'kg m-2 s-1', 'W m-2     ', 'W m-2     ', 'Pa        ', '%         ', &
& 'K         ', 'W m-2     ', 'W m-2     ', 'W m-2     ', 'K         ', 'K         ', &
& 'm s-1     '/)
  CHARACTER (LEN=10), DIMENSION(n2d_3h)  :: units2d_3h=(/ 'kg m-2 s-1', 'kg m-2 s-1', &
& 'kg m-2 s-1', 'W m-2     ', 'W m-2     ', 'Pa        ', 'Pa        ', '%         ', &
& 'm s-1     ', 'm s-1     ', 'K         ', 'W m-2     ', 'W m-2     '/)
 
  ! Corresponding positive definition
  CHARACTER (LEN=4), DIMENSION(n2d_mon) :: positive2d_mon=(/'    ', '    ', &
& '    ', '    ', '    ', 'up  ', 'up  ', '    ', &
& '    ', '    ', '    ', '    ', '    ', 'down', &
& 'down', 'up  ', '    ', '    ', '    ', '    ', &
& '    ', 'down'/)
  CHARACTER (LEN=4), DIMENSION(n2d_day) :: positive2d_day=(/'    ', &
& '    ', '    ', 'up  ', 'up  ', '    ', '    ', &
& '    ', 'down', 'down', 'up  ', '    ', '    ', &
& '    '/)
  CHARACTER (LEN=4), DIMENSION(n2d_3h)  :: positive2d_3h=(/ '    ', '    ', &
& '    ', 'up  ', 'up  ', '    ', '    ', '    ', &
& '    ', '    ', '    ', 'down', 'down'/)

  ! Corresponding Table entry (variable name)
  CHARACTER (LEN=7), DIMENSION(n2d_mon) :: entry2d_mon=(/'ts     ', 'prw    ', &
& 'prc    ', 'pr     ', 'prsn   ', 'hfss   ', 'hfls   ', 'psl    ', &
& 'ps     ', 'clt    ', 'uas    ', 'vas    ', 'tas    ', 'rsds   ', &
& 'rlds   ', 'rlut   ', 'evspsbl', 'tasmax ', 'tasmin ', 'mrros  ', &
& 'sfcWind', 'rsdt   '/)
!!!!!!!!!!!!! mon add mrso, clwvi, huss, hurs, rsus, rlus, rsut -> n2d_mon=22+7=29
  CHARACTER (LEN=7), DIMENSION(n2d_day) :: entry2d_day=(/'prc    ', &
& 'pr     ', 'prsn   ', 'hfss   ', 'hfls   ', 'psl    ', 'clt    ', &
& 'tas    ', 'rsds   ', 'rlds   ', 'rlut   ', 'tasmax ', 'tasmin ', &
& 'sfcWind'/)
!!!!!!!!!!!!! day add huss, hurs, rsus, rlus -> n2d_day=14+4=18
  CHARACTER (LEN=7), DIMENSION(n2d_3h)  :: entry2d_3h=(/ 'prc    ', 'pr     ', &
& 'prsn   ', 'hfss   ', 'hfls   ', 'psl    ', 'ps     ', 'clt    ', &
& 'uas    ', 'vas    ', 'tas    ', 'rsds   ', 'rlds   '/)
!!!!!!!!!!!!! 3h add huss, rsus, rlus -> n2d_3h=13+3=16

  ! My parameter number for 3d pressure level fields
  INTEGER, DIMENSION(n3d_monpl) :: parin3d_monpl=(/129, 130, 131, 132, 133/)
  INTEGER, DIMENSION(n3d_daypl) :: parin3d_daypl=(/129, 130, 131, 132, 133/)
  INTEGER, DIMENSION(n3d_6hpl) :: parin3d_6hpl=(/130, 131, 132/)
  
  ! My variable names for 3d pressure level fields
  CHARACTER (LEN=4), DIMENSION(n3d_monpl) :: varin3d_monpl=(/'Z   ', 'T   ', 'U   ', 'V   ', 'Q   '/)
  CHARACTER (LEN=4), DIMENSION(n3d_daypl) :: varin3d_daypl=(/'Z   ', 'T   ', 'U   ', 'V   ', 'Q   '/)
  CHARACTER (LEN=4), DIMENSION(n3d_6hpl)  :: varin3d_6hpl=(/'T   ', 'U   ', 'V   '/)

  ! Units appropriate to my data
  CHARACTER (LEN=5), DIMENSION(n3d_monpl) :: units3d_monpl=(/'m    ', 'K    ', 'm s-1', 'm s-1', '1    '/)
  CHARACTER (LEN=5), DIMENSION(n3d_daypl) :: units3d_daypl=(/'m    ', 'K    ', 'm s-1', 'm s-1', '1    '/)
  CHARACTER (LEN=5), DIMENSION(n3d_6hpl) :: units3d_6hpl=(/'K    ', 'm s-1', 'm s-1'/)

  ! Corresponding Table entry (variable name)
  CHARACTER (LEN=3), DIMENSION(n3d_monpl) :: entry3d_monpl=(/'zg ', 'ta ', 'ua ', 'va ', 'hus'/) !!!!!!!!!!!!add rel hum (hur)
  CHARACTER (LEN=3), DIMENSION(n3d_daypl) :: entry3d_daypl=(/'zg ', 'ta ', 'ua ', 'va ', 'hus'/) !!!!!!!!!!!!add rel hum (hur)
  CHARACTER (LEN=3), DIMENSION(n3d_6hpl) :: entry3d_6hpl=(/'ta ', 'ua ', 'va '/)

  ! My parameter number for 3d model level fields
  INTEGER, DIMENSION(n3d_monml) :: parin3d_monml=(/246, 247, 248/)
  INTEGER, DIMENSION(n3d_6hml) :: parin3d_6hml=(/130, 131, 132, 133/)

  ! My variable names for 3d model level fields
  CHARACTER (LEN=4), DIMENSION(n3d_monml) :: varin3d_monml=(/'CLWC', 'CIWC', 'CC  '/)
  CHARACTER (LEN=4), DIMENSION(n3d_6hml)  :: varin3d_6hml=(/'T   ', 'U   ', 'V   ', 'Q   '/)

  ! Units appropriate to my data
  CHARACTER (LEN=5), DIMENSION(n3d_monml) :: units3d_monml=(/'1    ', '1    ',  '%    '/)
  CHARACTER (LEN=5), DIMENSION(n3d_6hml) :: units3d_6hml=(/'K    ', 'm s-1', 'm s-1', '1    '/)

  ! Corresponding Table entry (variable name)
  CHARACTER (LEN=3), DIMENSION(n3d_monml) :: entry3d_monml=(/'clw', 'cli', 'cl '/)
  CHARACTER (LEN=3), DIMENSION(n3d_6hml) :: entry3d_6hml=(/'ta ', 'ua ', 'va ', 'hus'/)

  CHARACTER (LEN=256) :: suffix
  CHARACTER (LEN=256), DIMENSION(n2d_mon) :: fn2d_mon
  CHARACTER (LEN=256), DIMENSION(n2d_day) :: fn2d_day
  CHARACTER (LEN=256), DIMENSION(n2d_3h) :: fn2d_3h
  CHARACTER (LEN=256), DIMENSION(n3d_monpl) :: fn3d_monpl
  CHARACTER (LEN=256), DIMENSION(n3d_daypl) :: fn3d_daypl
  CHARACTER (LEN=256), DIMENSION(n3d_6hpl) :: fn3d_6hpl
  CHARACTER (LEN=256), DIMENSION(n3d_monml) :: fn3d_monml
  CHARACTER (LEN=256), DIMENSION(n3d_6hml) :: fn3d_6hml
  CHARACTER (LEN=256) :: file2d_mon_in, file2d_mon_out
  CHARACTER (LEN=256) :: file2d_day_in, file2d_day_out
  CHARACTER (LEN=256) :: file2d_3h_in, file2d_3h_out
  CHARACTER (LEN=256) :: file3d_monpl_in, file3d_monpl_out
  CHARACTER (LEN=256) :: file3d_daypl_in, file3d_daypl_out
  CHARACTER (LEN=256) :: file3d_6hpl_in, file3d_6hpl_out
  CHARACTER (LEN=256) :: file3d_monml_in, file3d_monml_out
  CHARACTER (LEN=256) :: file3d_6hml_in, file3d_6hml_out
  CHARACTER (LEN=15) :: rip
  CHARACTER (LEN=4) :: cyear0
  CHARACTER (LEN=256) :: cname

!  Uninitialized variables used in communicating with CMOR:
!  ---------------------------------------------------------

  INTEGER :: error_flag, error_flag_mon, error_flag_6h, error_flag_bmon, error_flag_b6h, error_flag_amon, error_flag_a6h
  INTEGER :: zfactor_mon_id, zfactor_6h_id
  INTEGER, DIMENSION(n2d_mon) :: var2d_mon_ids
  INTEGER, DIMENSION(n2d_day) :: var2d_day_ids
  INTEGER, DIMENSION(n2d_3h) :: var2d_3h_ids
  INTEGER, DIMENSION(n3d_monpl) :: var3d_monpl_ids
  INTEGER, DIMENSION(n3d_daypl) :: var3d_daypl_ids
  INTEGER, DIMENSION(n3d_6hpl) :: var3d_6hpl_ids
  INTEGER, DIMENSION(n3d_monml) :: var3d_monml_ids
  INTEGER, DIMENSION(n3d_6hml) :: var3d_6hml_ids
  REAL, DIMENSION(lon,lat) :: data2d_mon
  REAL, DIMENSION(lon,lat) :: data2d_mon_ps
  REAL, DIMENSION(lon,lat,ndays) :: data2d_day
  REAL, DIMENSION(lon,lat,ndays*8) :: data2d_3h
  REAL, DIMENSION(lon,lat,ndays*4) :: data2d_6h_psl
  REAL, DIMENSION(lon,lat,ndays*4) :: data2d_6h
  REAL, DIMENSION(lon,lat,ndays*4) :: data2d_6h_ps
  REAL, DIMENSION(lon,lat,levs) :: data3d_monpl
  REAL, DIMENSION(lon,lat,lev8,ndays) :: data3d_daypl
  REAL, DIMENSION(lon,lat,lev3,ndays*4) :: data3d_6hpl
  REAL, DIMENSION(lon,lat,lev) :: data3d_monml
  REAL, DIMENSION(lon,lat,lev,ndays*4) :: data3d_6hml
  DOUBLE PRECISION, DIMENSION(lat) :: alats
  DOUBLE PRECISION, DIMENSION(lon) :: alons
  DOUBLE PRECISION, DIMENSION(levs) :: plevs
  DOUBLE PRECISION, DIMENSION(lev8) :: plev8
  DOUBLE PRECISION, DIMENSION(lev3) :: plev3
  DOUBLE PRECISION, DIMENSION(1) :: time_mon
  DOUBLE PRECISION, DIMENSION(2,1) :: time_mon_bnds
  DOUBLE PRECISION, DIMENSION(ndays) :: time_day
  DOUBLE PRECISION, DIMENSION(2,ndays) :: time_day_bnds
  DOUBLE PRECISION, DIMENSION(ndays*4) :: time_6h
  DOUBLE PRECISION, DIMENSION(2,ndays*4) :: time_6h_bnds
  DOUBLE PRECISION, DIMENSION(ndays*4) :: time1_6h
  DOUBLE PRECISION, DIMENSION(ndays*8) :: time_3h
  DOUBLE PRECISION, DIMENSION(2,ndays*8) :: time_3h_bnds
  DOUBLE PRECISION, DIMENSION(ndays*8) :: time1_3h
  DOUBLE PRECISION, DIMENSION(2,lat) :: bnds_lat
  DOUBLE PRECISION, DIMENSION(2,lon) :: bnds_lon
  DOUBLE PRECISION, DIMENSION(lev) :: zlevs
  DOUBLE PRECISION, DIMENSION(lev+1) :: zlev_bnds
  REAL, DIMENSION(lev) :: a_coeff
  REAL, DIMENSION(lev) :: b_coeff
  REAL, DIMENSION(lev+1) :: a_coeff_bnds
  REAL, DIMENSION(lev+1) :: b_coeff_bnds
  REAL :: p0
  INTEGER :: ilon_mon, ilon_lmon, ilon_day, ilon_6hpl, ilon_6hml, ilon_3h, ilat_mon, ilat_lmon, ilat_day, ilat_6hpl, ilat_6hml, ilat_3h
  INTEGER :: ipres_mon, ipres_day, ipres_6h, ilev_mon, ilev_6h
  INTEGER :: itim_mon, itim1_mon, itim1_lmon, itim2_mon, itim2_lmon, itim1_day, itim2_day, itim1_6hpl, itim1_6hml, itim_3h, itim1_3h

  !  Other variables:
  !  ---------------------

  INTEGER :: i, j, it, m, ii, jj
  INTEGER :: JF

!-----------------------------------------------------------------------

! Initializations

  NFIELDS=0
  RMISS=-999E20

! NACCFLAG   : 1  if input data are accumulated instead of instantaneous
! NINTERVAL  : output frequency or accumulation period (in hours)
  NACCFLAG=0
  NINTERVAL=24

! ITOTP is set by GETALL if TP can be created by combining LSP/CP
! ITOTW is set by GETALL if wind speed can be created by combining 10U/10V
  ITOTP=0
  ITOTW=0

  call read_nam

  WRITE(rip,'(I10)') realization
  rip='r'//TRIM(ADJUSTL(rip))//'i1p1'
  WRITE(6,*) 'rip=',rip

  WRITE(cyear0,'(I4)') NYEAR0
  WRITE(6,*) 'cyear0=',cyear0

  CALL GETALL(0)

  call read_coords(alats, alons, plevs, plev8, plev3, bnds_lat, bnds_lon)
  call read_coords_vert(zlevs, a_coeff, b_coeff, zlev_bnds, a_coeff_bnds, b_coeff_bnds, p0)
  call read_time(time_mon, time_mon_bnds, time_day, time_day_bnds, time_6h, time_6h_bnds, time1_6h, time_3h, time_3h_bnds, time1_3h)

  ! Specify path where tables can be found and indicate that existing
  !    netCDF files should not be overwritten.

  error_flag = cmor_setup(inpath=inpath, netcdf_file_action=CMOR_APPEND)

  WRITE(6,*) 'error_flag=',error_flag

  error_flag = cmor_dataset(                       &
 &      outpath=outpath,                           &
 &      experiment_id=experiment_id,               &
 &      institution=institution,                   &
 &      source=source,                             &
 &      calendar=calendar,                         &
 &      realization=realization,                   &
 &      contact=contact,                           &
 &      history=history,                           &
 &      comment=comment,                           &
 &      references=references,                     &
 &      model_id=model_id,                         &
 &      forcing=forcing,                           &
 &      institute_id=institute_id,                 &
 &      parent_experiment_id=parent_experiment_id, &
 &      branch_time=branch_time,                   &
 &      parent_experiment_rip=parent_experiment_rip)

  WRITE(6,*) 'error_flag=',error_flag

  !  Define all axes that will be needed

  ilon_mon = cmor_axis(              &
       table='Tables/CMIP5_Amon',    &
       table_entry='longitude',      &
       units='degrees_east',         &
       length=lon,                   &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)

  WRITE(6,*) 'ilon_mon=',ilon_mon

  ilon_lmon = cmor_axis(             &
       table='Tables/CMIP5_Lmon',    &
       table_entry='longitude',      &
       units='degrees_east',         &
       length=lon,                   &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)

  WRITE(6,*) 'ilon_lmon=',ilon_lmon

  ilon_day = cmor_axis(              &
       table='Tables/CMIP5_day',     &
       table_entry='longitude',      &
       units='degrees_east',         &
       length=lon,                   &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)

  WRITE(6,*) 'ilon_day=',ilon_day

  ilon_6hpl = cmor_axis(             &
       table='Tables/CMIP5_6hrPlev', &
       table_entry='longitude',      &
       units='degrees_east',         &
       length=lon,                   &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)

  WRITE(6,*) 'ilon_6hpl=',ilon_6hpl

  ilon_6hml = cmor_axis(             &
       table='Tables/CMIP5_6hrLev',  &
       table_entry='longitude',      &
       units='degrees_east',         &
       length=lon,                   &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)

  WRITE(6,*) 'ilon_6hml=',ilon_6hml
 
  ilon_3h = cmor_axis(               &
       table='Tables/CMIP5_3hr',     &
       table_entry='longitude',      &
       units='degrees_east',         &
       length=lon,                   &
       coord_vals=alons,             &
       cell_bounds=bnds_lon)

  WRITE(6,*) 'ilon_3h=',ilon_3h

  ilat_mon = cmor_axis(              &
       table='Tables/CMIP5_Amon',    &
       table_entry='latitude',       &
       units='degrees_north',        &
       length=lat,                   &
       coord_vals=alats,             &
       cell_bounds=bnds_lat)

  WRITE(6,*) 'ilat_mon=',ilat_mon

  ilat_lmon = cmor_axis(             &
       table='Tables/CMIP5_Lmon',    &
       table_entry='latitude',       &
       units='degrees_north',        &
       length=lat,                   &
       coord_vals=alats,             &
       cell_bounds=bnds_lat)

  WRITE(6,*) 'ilat_lmon=',ilat_lmon

  ilat_day = cmor_axis(              &
       table='Tables/CMIP5_day',     &
       table_entry='latitude',       &
       units='degrees_north',        &
       length=lat,                   &
       coord_vals=alats,             &
       cell_bounds=bnds_lat)

  WRITE(6,*) 'ilat_day=',ilat_day

  ilat_6hpl = cmor_axis(             &
       table='Tables/CMIP5_6hrPlev', &
       table_entry='latitude',       &
       units='degrees_north',        &
       length=lat,                   &
       coord_vals=alats,             &
       cell_bounds=bnds_lat)

  WRITE(6,*) 'ilat_6hpl=',ilat_6hpl

  ilat_6hml = cmor_axis(             &
       table='Tables/CMIP5_6hrLev',  &
       table_entry='latitude',       &
       units='degrees_north',        &
       length=lat,                   &
       coord_vals=alats,             &
       cell_bounds=bnds_lat)

  WRITE(6,*) 'ilat_6hml=',ilat_6hml

  ilat_3h = cmor_axis(               &
       table='Tables/CMIP5_3hr',     &
       table_entry='latitude',       &
       units='degrees_north',        &
       length=lat,                   &
       coord_vals=alats,             &
       cell_bounds=bnds_lat)

  WRITE(6,*) 'ilat_3h=',ilat_3h

  ipres_mon = cmor_axis(              &
 &      table='Tables/CMIP5_Amon',    &
 &      table_entry='plevs',          &
 &      units='Pa',                   &
 &      length=levs,                  &
 &      coord_vals=plevs)

  WRITE(6,*) 'ipres_mon=',ipres_mon

  ipres_day = cmor_axis(              &
 &      table='Tables/CMIP5_day',     &
 &      table_entry='plev8',          &
 &      units='Pa',                   &
 &      length=lev8,                  &
 &      coord_vals=plev8)

  WRITE(6,*) 'ipres_day=',ipres_day

  ipres_6h = cmor_axis(               &
 &      table='Tables/CMIP5_6hrPlev', &
 &      table_entry='plev3',          &
 &      units='Pa',                   &
 &      length=lev3,                  &
 &      coord_vals=plev3)

  WRITE(6,*) 'ipres_6h=',ipres_6h

  !   note that the time axis is defined next, but the time coordinate
  !   values and bounds will be passed to cmor through function
  !   cmor_write (later, below).

  itim_mon = cmor_axis(                      &
 &      table='Tables/CMIP5_Amon',           &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=1,                            &
 &      interval='1 month')

  WRITE(6,*) 'itim_mon=',itim_mon

  itim1_mon = cmor_axis(                     &
 &      table='Tables/CMIP5_Amon',           &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=1,                            &
 &      interval='3 hours')

  WRITE(6,*) 'itim1_mon=',itim1_mon

  itim1_lmon = cmor_axis(                    &
 &      table='Tables/CMIP5_Lmon',           &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=1,                            &
 &      interval='3 hours')

  WRITE(6,*) 'itim1_lmon=',itim1_lmon

  itim2_mon = cmor_axis(                     &
 &      table='Tables/CMIP5_Amon',           &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=1,                            & 
 &      interval='6 hours')

  WRITE(6,*) 'itim2_mon=',itim2_mon

  itim2_lmon = cmor_axis(                    &
 &      table='Tables/CMIP5_Lmon',           &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=1,                            &
 &      interval='6 hours')

  WRITE(6,*) 'itim2_lmon=',itim2_lmon

  itim1_day = cmor_axis(                     &
 &      table='Tables/CMIP5_day',            &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=NMDAYS,                       &
 &      interval='3 hours')

  WRITE(6,*) 'itim1_day=',itim1_day

  itim2_day = cmor_axis(                     &
 &      table='Tables/CMIP5_day',            &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=NMDAYS,                       &
 &      interval='6 hours')

  WRITE(6,*) 'itim2_day=',itim2_day

  itim1_6hpl = cmor_axis(                    &
 &      table='Tables/CMIP5_6hrPlev',        &
 &      table_entry='time1',                 &
 &      units='days since '//cyear0//'-1-1', &
 &      length=NMDAYS*4,                     &
 &      interval='6 hours')

  WRITE(6,*) 'itim1_6hpl=',itim1_6hpl

  itim1_6hml = cmor_axis(                    &
 &      table='Tables/CMIP5_6hrLev',         &
 &      table_entry='time1',                 &
 &      units='days since '//cyear0//'-1-1', &
 &      length=NMDAYS*4,                     &
 &      interval='6 hours')

  WRITE(6,*) 'itim1_6hml=',itim1_6hml

  itim_3h = cmor_axis(                       &
 &      table='Tables/CMIP5_3hr',            &
 &      table_entry='time',                  &
 &      units='days since '//cyear0//'-1-1', &
 &      length=NMDAYS*8,                     &
 &      interval='3 hours')

  WRITE(6,*) 'itim_3h=',itim_3h

  itim1_3h = cmor_axis(                      &
 &      table='Tables/CMIP5_3hr',            &
 &      table_entry='time1',                 &
 &      units='days since '//cyear0//'-1-1', &
 &      length=NMDAYS*8,                     &
 &      interval='3 hours')

  WRITE(6,*) 'itim1_3h=',itim1_3h

  !  define model eta levels (although these must be provided, they will
  !    actually be replaced by a+b before writing the netCDF file)

  ilev_mon = cmor_axis(                     &
       table='Tables/CMIP5_Amon',           &
       table_entry='standard_hybrid_sigma', &
       units='1',                           &
       length=lev,                          &
       coord_vals=zlevs,                    &
       cell_bounds=zlev_bnds)

  WRITE(6,*) 'ilev_mon=',ilev_mon

  !   define z-factors needed to transform from model level to pressure

  error_flag_mon = cmor_zfactor(           &
       zaxis_id=ilev_mon,                  &
       zfactor_name='p0',                  &
       units='Pa',                         &
       zfactor_values = p0)

  WRITE(6,*) 'error_flag_mon=',error_flag_mon

  error_flag_bmon = cmor_zfactor(           &
       zaxis_id=ilev_mon,                   &
       zfactor_name='b',                    &
       axis_ids= (/ ilev_mon /),            &
       zfactor_values = b_coeff,            &
       zfactor_bounds = b_coeff_bnds  )

  WRITE(6,*) 'error_flag_bmon=',error_flag_bmon

  error_flag_amon = cmor_zfactor(           &
       zaxis_id=ilev_mon,                   &
       zfactor_name='a',                    &
       axis_ids= (/ ilev_mon /),            &
       zfactor_values = a_coeff,            &
       zfactor_bounds = a_coeff_bnds )

  WRITE(6,*) 'error_flag_amon=',error_flag_amon

  zfactor_mon_id = cmor_zfactor(                     &
       zaxis_id=ilev_mon,                            &
       zfactor_name='ps',                            &
       axis_ids=(/ ilon_mon, ilat_mon, itim2_mon /), &
       units='Pa' )

  WRITE(6,*) 'zfactor_mon_id=',zfactor_mon_id

  ilev_6h = cmor_axis(                      &
       table='Tables/CMIP5_6hrLev',         &
       table_entry='standard_hybrid_sigma', &
       units='1',                           &
       length=lev,                          &
       coord_vals=zlevs,                    &
       cell_bounds=zlev_bnds)

  WRITE(6,*) 'ilev_6h=',ilev_6h

  !   define z-factors needed to transform from model level to pressure

  error_flag_6h = cmor_zfactor(            &
       zaxis_id=ilev_6h,                   &
       zfactor_name='p0',                  &
       units='Pa',                         &
       zfactor_values = p0)

  WRITE(6,*) 'error_flag_6h=',error_flag_6h

  error_flag_b6h = cmor_zfactor(            &
       zaxis_id=ilev_6h,                    &
       zfactor_name='b',                 &
       axis_ids= (/ ilev_6h /),             &
       zfactor_values = b_coeff,            &
       zfactor_bounds = b_coeff_bnds  )

  WRITE(6,*) 'error_flag_b6h=',error_flag_b6h

  error_flag_a6h = cmor_zfactor(            &
       zaxis_id=ilev_6h,                    &
       zfactor_name='a',                    &
       axis_ids= (/ ilev_6h /),             &
       zfactor_values = a_coeff,            &
       zfactor_bounds = a_coeff_bnds )

  WRITE(6,*) 'error_flag_a6h=',error_flag_a6h

  zfactor_6h_id = cmor_zfactor(                         &
       zaxis_id=ilev_6h,                                &
       zfactor_name='ps',                               &
       axis_ids=(/ ilon_6hml, ilat_6hml, itim1_6hml /), &
       units='Pa' )

  WRITE(6,*) 'zfactor_6h_id=',zfactor_6h_id


  WRITE(6,*)
  WRITE(6,*) 'Completed everything up to cmor variable definition and writing output fields'
  WRITE(6,*) 

500 FORMAT(256A)

  IF (TRIM(ADJUSTL(suffix_mon_sfc)).ne."") OPEN(11,FILE="file2d_mon_in",FORM='formatted')
  OPEN(21,FILE="file2d_mon_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_mon_plev)).ne."") OPEN(12,FILE="file3d_monpl_in",FORM='formatted')
  OPEN(22,FILE="file3d_monpl_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_mon_mlev)).ne."") OPEN(13,FILE="file3d_monml_in",FORM='formatted')
  OPEN(23,FILE="file3d_monml_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_day_sfc)).ne."") OPEN(14,FILE="file2d_day_in",FORM='formatted')
  OPEN(24,FILE="file2d_day_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_day_plev)).ne."") OPEN(15,FILE="file3d_daypl_in",FORM='formatted')
  OPEN(25,FILE="file3d_daypl_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_3h_sfc)).ne."") OPEN(16,FILE="file2d_3h_in",FORM='formatted')
  OPEN(26,FILE="file2d_3h_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_6h_plev)).ne."") OPEN(17,FILE="file3d_6hpl_in",FORM='formatted')
  OPEN(27,FILE="file3d_6hpl_out",FORM='formatted')
  IF (TRIM(ADJUSTL(suffix_6h_mlev)).ne."") OPEN(18,FILE="file3d_6hml_in",FORM='formatted')
  OPEN(28,FILE="file3d_6hml_out",FORM='formatted')

      DO JF=1,NFIELDS

         CALL GETALL(JF)

         CALL MMEAN(JF)

!  Monthly data (table CMIP5_Amon or CMIP5_Lmon)

    DO m=1,n2d_mon

      ! Cycle through the 2-d fields, retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (only one time sample for monthly data)

      IF ((((NSEC1(7).EQ.1).OR.(NSEC1(7).EQ.112)).AND.(NSEC1(6).EQ.parin2d_mon(m))) &
 &      .OR.((NSEC1(7).EQ.100).AND.(NSEC1(8).EQ.1000).AND.(NSEC1(6).EQ.152).AND.(parin2d_mon(m).EQ.152))) THEN
 
        WRITE(6,*)
        WRITE(6,*) 'JF, m, NSEC1(6), NSEC1(7), NSEC1(8), parin2d_mon(m)'
        WRITE(6,*) JF, m, NSEC1(6), NSEC1(7), NSEC1(8), parin2d_mon(m)

        if (TRIM(ADJUSTL(suffix_mon_sfc)).eq."") then
          suffix=""
        else
          READ(11,500) fn2d_mon(m)
          suffix=fn2d_mon(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/mon/atmos/'//TRIM(ADJUSTL(entry2d_mon(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry2d_mon(m)))//'_Amon_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_mon_sfc))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

        call read_2d_input_files_mon(data2d_mon)

        IF (NPRINTLEV.GT.1) THEN
          do jj=1,lat
            do ii=1,lon
              WRITE(6,*) 'jj=', jj, 'ii=', ii, 'data2d_mon(ii,jj)=', data2d_mon(ii,jj)
            enddo
          enddo
        ENDIF
 
        IF (NINTERVAL(JF).EQ.3) THEN
          IF (parin2d_mon(m).EQ.205) THEN
            var2d_mon_ids(m) = cmor_variable(                     &
                 table='Tables/CMIP5_Lmon',                       &
                 table_entry=entry2d_mon(m),                      &
                 units=units2d_mon(m),                            &
                 axis_ids=(/ ilon_lmon, ilat_lmon, itim1_lmon /), &
                 missing_value=1.0e28,                            &
                 positive=positive2d_mon(m),                      &
                 original_name=varin2d_mon(m))
          ELSE
            var2d_mon_ids(m) = cmor_variable(                  &
                 table='Tables/CMIP5_Amon',                    &
                 table_entry=entry2d_mon(m),                   &
                 units=units2d_mon(m),                         &
                 axis_ids=(/ ilon_mon, ilat_mon, itim1_mon /), &
                 missing_value=1.0e28,                         &
                 positive=positive2d_mon(m),                   &
                 original_name=varin2d_mon(m))
          ENDIF
        ELSEIF (NINTERVAL(JF).EQ.6) THEN
          IF (parin2d_mon(m).EQ.205) THEN
            var2d_mon_ids(m) = cmor_variable(                    &
                 table='Tables/CMIP5_Lmon',                      &
                 table_entry=entry2d_mon(m),                     &
                 units=units2d_mon(m),                           &
                 axis_ids=(/ ilon_lmon, ilat_lmon, itim2_lmon /), &
                 missing_value=1.0e28,                           &
                 positive=positive2d_mon(m),                     &
                 original_name=varin2d_mon(m))
          ELSE
            var2d_mon_ids(m) = cmor_variable(                  &
                 table='Tables/CMIP5_Amon',                    &
                 table_entry=entry2d_mon(m),                   &
                 units=units2d_mon(m),                         &
                 axis_ids=(/ ilon_mon, ilat_mon, itim2_mon /), &
                 missing_value=1.0e28,                         &
                 positive=positive2d_mon(m),                   &
                 original_name=varin2d_mon(m))            
          ENDIF
        ELSE
          IF (parin2d_mon(m).EQ.212) THEN
            var2d_mon_ids(m) = cmor_variable(                  &
                 table='Tables/CMIP5_Amon',                    &
                 table_entry=entry2d_mon(m),                   &
                 units=units2d_mon(m),                         &
                 axis_ids=(/ ilon_mon, ilat_mon, itim_mon /),  &
                 missing_value=1.0e28,                         &
                 positive=positive2d_mon(m),                   &
                 original_name=varin2d_mon(m))
          ELSE
            WRITE(6,*) '!!! ninterval different from 3 or 6'
            STOP
          ENDIF
        ENDIF

        WRITE(6,*) 'var2d_mon_ids(m)=',var2d_mon_ids(m)

        error_flag = cmor_write(                    &
             var_id        = var2d_mon_ids(m),      &
             data          = data2d_mon,            &
             file_suffix   = TRIM(ADJUSTL(suffix)), &
             ntimes_passed = 1,                     &
             time_vals     = time_mon,              &
             time_bnds     = time_mon_bnds  )

        WRITE(6,*) 'cmor_write error_flag=',error_flag

        IF (error_flag < 0) THEN
          WRITE(6,*) ' Error encountered writing CMIP5_Amon or CMIP5_Lmon Table ' &
               // 'field ', entry2d_mon(m), ', which I call ', varin2d_mon(m)
          WRITE(6,*) ' Was processing time sample: ', time_mon
        ENDIF
  
        error_flag = cmor_close(var2d_mon_ids(m),cname)
        fn2d_mon(m)=cname
        WRITE(6,*) 'fn2d_mon(m)=',fn2d_mon(m)
        WRITE(21,500) fn2d_mon(m)
 
        WRITE(6,*) 'cmor_close error_flag=',error_flag

      ENDIF 
    ENDDO

    DO m=1,n3d_monpl

      ! Cycle through the 3-d fields (pressure levels), retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (only one time sample for monthly data)

      IF ((NSEC1(7).EQ.100).AND.(NSEC1(6).EQ.parin3d_monpl(m))) THEN

        DO i=1,levs
          IF (NSEC1(8).EQ.NINT(plevs(i)/100)) THEN
            WRITE(6,*)
            WRITE(6,*) 'JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_monpl(m), NINT(plevs(i)/100)'
            WRITE(6,*) JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_monpl(m), NINT(plevs(i)/100)
            call read_2d_input_files_mon(data2d_mon)
            data3d_monpl(:,:,i)=data2d_mon(:,:)
          
          IF (i.EQ.levs) THEN

            IF (NPRINTLEV.GT.1) THEN
              do jj=1,lat
                do ii=1,lon
                  WRITE(6,*) 'i=', i, 'jj=', jj, 'ii=', ii, 'data3d_monpl(ii,jj,i)=', data3d_monpl(ii,jj,i)
                enddo
              enddo
            ENDIF

        if (TRIM(ADJUSTL(suffix_mon_plev)).eq."") then
          suffix=""
        else
          READ(12,500) fn3d_monpl(m)
          suffix=fn3d_monpl(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/mon/atmos/'//TRIM(ADJUSTL(entry3d_monpl(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry3d_monpl(m)))//'_Amon_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_mon_plev))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

            IF (NINTERVAL(JF).EQ.3) THEN
              var3d_monpl_ids(m) = cmor_variable(                           &
                   table='Tables/CMIP5_Amon',                               &
                   table_entry=entry3d_monpl(m),                            &
                   units=units3d_monpl(m),                                  &
                   axis_ids=(/ ilon_mon, ilat_mon, ipres_mon, itim1_mon /), &
                   missing_value=1.0e28,                                    &
                   original_name=varin3d_monpl(m))
            ELSEIF (NINTERVAL(JF).EQ.6) THEN
              var3d_monpl_ids(m) = cmor_variable(                           &
                   table='Tables/CMIP5_Amon',                               &
                   table_entry=entry3d_monpl(m),                            &
                   units=units3d_monpl(m),                                  &
                   axis_ids=(/ ilon_mon, ilat_mon, ipres_mon, itim2_mon /), &
                   missing_value=1.0e28,                                    &
                   original_name=varin3d_monpl(m))
            ELSE
              WRITE(6,*) '!!! ninterval different from 3 or 6'
              STOP
            ENDIF

            WRITE(6,*) 'var3d_monpl_ids(m)=',var3d_monpl_ids(m)

            error_flag = cmor_write(                    &
                 var_id        = var3d_monpl_ids(m),    &
                 data          = data3d_monpl,          &
                 file_suffix   = TRIM(ADJUSTL(suffix)), &
                 ntimes_passed = 1,                     &
                 time_vals     = time_mon,              &
                 time_bnds     = time_mon_bnds )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_Amon Table ' &
                    // 'field ', entry3d_monpl(m), ', which I call ', varin3d_monpl(m)
               WRITE(6,*) ' Was processing time sample: ', time_mon
            ENDIF

            error_flag = cmor_close(var3d_monpl_ids(m),cname)
            fn3d_monpl(m)=cname
            WRITE(6,*) 'fn3d_monpl(m)=',fn3d_monpl(m)
            WRITE(22,500) fn3d_monpl(m)
   
            WRITE(6,*) 'cmor_close error_flag=',error_flag

          ENDIF
  
          ENDIF

        ENDDO
      ENDIF
    ENDDO

    ! Save the surface pressure in data2d_mon_ps (from 6-hourly data of grib parameter 152 on model level 1)
    ! to be stored with the 3d model level fields

    IF (((NSEC1(7).EQ.109).AND.(NSEC1(8).EQ.1)).AND.(NSEC1(6).EQ.152)) THEN
      WRITE(6,*)
      WRITE(6,*) 'JF, NSEC1(6), NSEC1(7), NSEC1(8)'
      WRITE(6,*) JF, NSEC1(6), NSEC1(7), NSEC1(8)
      call read_2d_input_files_mon(data2d_mon_ps)
    ENDIF

    DO m=1,n3d_monml

      ! Cycle through the 3-d fields (model levels), retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (only one time sample for monthly data)

      IF ((NSEC1(7).EQ.109).AND.(NSEC1(6).EQ.parin3d_monml(m))) THEN

        DO i=1,lev
          IF (NSEC1(8).EQ.i) THEN
            WRITE(6,*)
            WRITE(6,*) 'JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_monml(m)'
            WRITE(6,*) JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_monml(m)
            call read_2d_input_files_mon(data2d_mon)
            data3d_monml(:,:,i)=data2d_mon(:,:)

          IF (i.EQ.lev) THEN

            IF (NPRINTLEV.GT.1) THEN
              do jj=1,lat
                do ii=1,lon
                  WRITE(6,*) 'i=', i, 'jj=', jj, 'ii=', ii, 'data3d_monml(ii,jj,i)=', data3d_monml(ii,jj,i)
                enddo
              enddo
            ENDIF

        if (TRIM(ADJUSTL(suffix_mon_mlev)).eq."") then
          suffix=""
        else
          READ(13,500) fn3d_monml(m)
          suffix=fn3d_monml(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/mon/atmos/'//TRIM(ADJUSTL(entry3d_monml(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry3d_monml(m)))//'_Amon_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_mon_mlev))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

            IF (NINTERVAL(JF).EQ.3) THEN
              var3d_monml_ids(m) = cmor_variable(                          &
                   table='Tables/CMIP5_Amon',                              &
                   table_entry=entry3d_monml(m),                           &
                   units=units3d_monml(m),                                 &
                   axis_ids=(/ ilon_mon, ilat_mon, ilev_mon, itim1_mon /), &
                   missing_value=1.0e28,                                   &
                   original_name=varin3d_monml(m))
            ELSEIF (NINTERVAL(JF).EQ.6) THEN
              var3d_monml_ids(m) = cmor_variable(                          &
                   table='Tables/CMIP5_Amon',                              &
                   table_entry=entry3d_monml(m),                           &
                   units=units3d_monml(m),                                 &
                   axis_ids=(/ ilon_mon, ilat_mon, ilev_mon, itim2_mon /), &
                   missing_value=1.0e28,                                   &
                   original_name=varin3d_monml(m))
            ELSE
              WRITE(6,*) '!!! ninterval different from 3 or 6'
              STOP
            ENDIF

            WRITE(6,*) 'var3d_monml_ids(m)=',var3d_monml_ids(m)

            error_flag = cmor_write(                    &
                 var_id        = var3d_monml_ids(m),    &
                 data          = data3d_monml,          &
                 file_suffix   = TRIM(ADJUSTL(suffix)), &
                 ntimes_passed = 1,                     &
                 time_vals     = time_mon,              &
                 time_bnds     = time_mon_bnds )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_Amon Table ' &
                    // 'field ', entry3d_monml(m), ', which I call ', varin3d_monml(m)
               WRITE(6,*) ' Was processing time sample: ', time_mon
            ENDIF
 
            error_flag = cmor_write(                &
                 var_id        = zfactor_mon_id,    &
                 data          = data2d_mon_ps,     &
                 ntimes_passed = 1,                 &
                 time_vals     = time_mon,          &
                 time_bnds     = time_mon_bnds,     &
                 store_with    = var3d_monml_ids(m) )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_Amon Table ' &
                    // 'field ', 'ps', ', which I call ', 'LNSP'
               WRITE(6,*) ' Was processing time sample: ', time_mon
            ENDIF

            error_flag = cmor_close(var3d_monml_ids(m),cname)
            fn3d_monml(m)=cname
            WRITE(6,*) 'fn3d_monml(m)=',fn3d_monml(m)
            WRITE(23,500) fn3d_monml(m)

            WRITE(6,*) 'cmor_close error_flag=',error_flag

          ENDIF

          ENDIF

        ENDDO
      ENDIF
    ENDDO

!  End Monthly data (table CMIP5_Amon or CMIP5_Lmon)

!  Daily data (table CMIP5_day)

    DO m=1,n2d_day

      ! Cycle through the 2-d fields, retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (NMDAYS time samples for daily data)

      IF (((NSEC1(7).EQ.1).OR.(NSEC1(7).EQ.112)).AND.(NSEC1(6).EQ.parin2d_day(m))) THEN

        WRITE(6,*)
        WRITE(6,*) 'JF, m, NSEC1(6), NSEC1(7), NSEC1(8), parin2d_day(m)'
        WRITE(6,*) JF, m, NSEC1(6), NSEC1(7), NSEC1(8), parin2d_day(m)

        if (TRIM(ADJUSTL(suffix_day_sfc)).eq."") then
          suffix=""
        else
          READ(14,500) fn2d_day(m)
          suffix=fn2d_day(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/day/atmos/'//TRIM(ADJUSTL(entry2d_day(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry2d_day(m)))//'_day_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_day_sfc))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

        call read_2d_input_files_day(data2d_day)

        IF (NINTERVAL(JF).EQ.3) THEN
          var2d_day_ids(m) = cmor_variable(                  &
               table='Tables/CMIP5_day',                     &
               table_entry=entry2d_day(m),                   &
               units=units2d_day(m),                         &
               axis_ids=(/ ilon_day, ilat_day, itim1_day /), &
               missing_value=1.0e28,                         &
               positive=positive2d_day(m),                   &
               original_name=varin2d_day(m))
        ELSEIF (NINTERVAL(JF).EQ.6) THEN
          var2d_day_ids(m) = cmor_variable(                  &
               table='Tables/CMIP5_day',                     &
               table_entry=entry2d_day(m),                   &
               units=units2d_day(m),                         &
               axis_ids=(/ ilon_day, ilat_day, itim2_day /), &
               missing_value=1.0e28,                         &
               positive=positive2d_day(m),                   &
               original_name=varin2d_day(m))
        ELSE
          WRITE(6,*) '!!! ninterval different from 3 or 6'
          STOP
        ENDIF

        WRITE(6,*) 'var2d_day_ids(m)=',var2d_day_ids(m)

        error_flag = cmor_write(                         &
             var_id        = var2d_day_ids(m),           &
             data          = data2d_day(:,:,1:NMDAYS),   &
             file_suffix   = TRIM(ADJUSTL(suffix)),      &
             ntimes_passed = NMDAYS,                     &
             time_vals     = time_day(1:NMDAYS),         &
             time_bnds     = time_day_bnds(:,1:NMDAYS)  )

        WRITE(6,*) 'cmor_write error_flag=',error_flag

        IF (error_flag < 0) THEN
          WRITE(6,*) ' Error encountered writing CMIP5_day Table ' &
               // 'field ', entry2d_day(m), ', which I call ', varin2d_day(m)
          WRITE(6,*) ' Was processing time sample: ', time_day(1:NMDAYS)
        ENDIF

        error_flag = cmor_close(var2d_day_ids(m),cname)
        fn2d_day(m)=cname
        WRITE(6,*) 'fn2d_day(m)=',fn2d_day(m)
        WRITE(24,500) fn2d_day(m)

        WRITE(6,*) 'cmor_close error_flag=',error_flag

      ENDIF
    ENDDO

    DO m=1,n3d_daypl

      ! Cycle through the 3-d fields (pressure levels), retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (NMDAYS time samples for daily data)

      IF ((NSEC1(7).EQ.100).AND.(NSEC1(6).EQ.parin3d_daypl(m))) THEN

        DO i=1,lev8
          IF (NSEC1(8).EQ.NINT(plev8(i)/100)) THEN
            WRITE(6,*)
            WRITE(6,*) 'JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_daypl(m), NINT(plev8(i)/100)'
            WRITE(6,*) JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_daypl(m), NINT(plev8(i)/100)
            call read_2d_input_files_day(data2d_day)
            data3d_daypl(:,:,i,:)=data2d_day(:,:,:)

          IF (i.EQ.lev8) THEN

            IF (NPRINTLEV.GT.1) THEN
              do jj=1,lat
                do ii=1,lon
                  WRITE(6,*) 'i=', i, 'jj=', jj, 'ii=', ii, 'data3d_daypl(ii,jj,i,1)=', data3d_daypl(ii,jj,i,1)
                enddo
              enddo
            ENDIF

        if (TRIM(ADJUSTL(suffix_day_plev)).eq."") then
          suffix=""
        else
          READ(15,500) fn3d_daypl(m)
          suffix=fn3d_daypl(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/day/atmos/'//TRIM(ADJUSTL(entry3d_daypl(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry3d_daypl(m)))//'_day_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_day_plev))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

            IF (NINTERVAL(JF).EQ.3) THEN
              var3d_daypl_ids(m) = cmor_variable(                           &
                   table='Tables/CMIP5_day',                                &
                   table_entry=entry3d_daypl(m),                            &
                   units=units3d_daypl(m),                                  &
                   axis_ids=(/ ilon_day, ilat_day, ipres_day, itim1_day /), &
                   missing_value=1.0e28,                                    &
                   original_name=varin3d_daypl(m))
            ELSEIF (NINTERVAL(JF).EQ.6) THEN
              var3d_daypl_ids(m) = cmor_variable(                           &
                   table='Tables/CMIP5_day',                                &
                   table_entry=entry3d_daypl(m),                            &
                   units=units3d_daypl(m),                                  &
                   axis_ids=(/ ilon_day, ilat_day, ipres_day, itim2_day /), &
                   missing_value=1.0e28,                                    &
                   original_name=varin3d_daypl(m))
            ELSE
              WRITE(6,*) '!!! ninterval different from 3 or 6'
              STOP
            ENDIF

            WRITE(6,*) 'var3d_daypl_ids(m)=',var3d_daypl_ids(m)

            error_flag = cmor_write(                           &
                 var_id        = var3d_daypl_ids(m),           &
                 data          = data3d_daypl(:,:,:,1:NMDAYS), &
                 file_suffix   = TRIM(ADJUSTL(suffix)),        &
                 ntimes_passed = NMDAYS,                       &
                 time_vals     = time_day(1:NMDAYS),           &
                 time_bnds     = time_day_bnds(:,1:NMDAYS)  )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_day Table ' &
                    // 'field ', entry3d_daypl(m), ', which I call ', varin3d_daypl(m)
               WRITE(6,*) ' Was processing time sample: ', time_day(1:NMDAYS)
            ENDIF

            error_flag = cmor_close(var3d_daypl_ids(m),cname)
            fn3d_daypl(m)=cname
            WRITE(6,*) 'fn3d_daypl(m)=',fn3d_daypl(m)
            WRITE(25,500) fn3d_daypl(m)

            WRITE(6,*) 'cmor_close error_flag=',error_flag

          ENDIF

          ENDIF

        ENDDO
      ENDIF

    ENDDO

!  End Daily data (table CMIP5_day)

!  6-hourly pressure level data (table CMIP5_6hrPlev)

    DO m=1,n3d_6hpl

      ! Cycle through the 3-d fields (pressure levels), retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (NMDAYS*4 time samples for 6-hourly data)

      IF ((NSEC1(7).EQ.100).AND.(NSEC1(6).EQ.parin3d_6hpl(m))) THEN

        DO i=1,lev3
          IF (NSEC1(8).EQ.NINT(plev3(i)/100)) THEN
            WRITE(6,*)
            WRITE(6,*) 'JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_6hpl(m), NINT(plev3(i)/100)'
            WRITE(6,*) JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_6hpl(m), NINT(plev3(i)/100)
            call read_2d_input_files_6h(data2d_6h)
            data3d_6hpl(:,:,i,:)=data2d_6h(:,:,:)

          IF (i.EQ.lev3) THEN

            IF (NPRINTLEV.GT.1) THEN
              do jj=1,lat
                do ii=1,lon
                  WRITE(6,*) 'i=', i, 'jj=', jj, 'ii=', ii, 'data3d_6hpl(ii,jj,i,1)=', data3d_6hpl(ii,jj,i,1)
                enddo
              enddo
            ENDIF

        if (TRIM(ADJUSTL(suffix_6h_plev)).eq."") then
          suffix=""
        else
          READ(17,500) fn3d_6hpl(m)
          suffix=fn3d_6hpl(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/6hr/atmos/'//TRIM(ADJUSTL(entry3d_6hpl(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry3d_6hpl(m)))//'_6hrPlev_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_6h_plev))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

            var3d_6hpl_ids(m) = cmor_variable(                              &
                 table='Tables/CMIP5_6hrPlev',                              &
                 table_entry=entry3d_6hpl(m),                               &
                 units=units3d_6hpl(m),                                     &
                 axis_ids=(/ ilon_6hpl, ilat_6hpl, ipres_6h, itim1_6hpl /), &
                 missing_value=1.0e28,                                      &
                 original_name=varin3d_6hpl(m))

            WRITE(6,*) 'var3d_6hpl_ids(m)=',var3d_6hpl_ids(m)

            error_flag = cmor_write(                            &
                 var_id        = var3d_6hpl_ids(m),             &
                 data          = data3d_6hpl(:,:,:,1:NMDAYS*4), &
                 file_suffix   = TRIM(ADJUSTL(suffix)),         &
                 ntimes_passed = NMDAYS*4,                      &
                 time_vals     = time1_6h(1:NMDAYS*4)  )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_6hrPlev Table ' &
                    // 'field ', entry3d_6hpl(m), ', which I call ', varin3d_6hpl(m)
               WRITE(6,*) ' Was processing time sample: ', time1_6h(1:NMDAYS*4)
            ENDIF

            error_flag = cmor_close(var3d_6hpl_ids(m),cname)
            fn3d_6hpl(m)=cname
            WRITE(6,*) 'fn3d_6hpl(m)=',fn3d_6hpl(m)
            WRITE(27,500) fn3d_6hpl(m)

            WRITE(6,*) 'cmor_close error_flag=',error_flag

          ENDIF

          ENDIF

        ENDDO
      ENDIF

    ENDDO

!  End 6-hourly pressure level data (table CMIP5_6hrPlev)

!  6-hourly model level data (table CMIP5_6hrLev)

    ! Save the surface pressure in data2d_6h_ps (from 6-hourly data of grib parameter 152 on model level 1)
    ! to be stored with the 3d model level fields

    IF (((NSEC1(7).EQ.109).AND.(NSEC1(8).EQ.1)).AND.(NSEC1(6).EQ.152)) THEN
      WRITE(6,*)
      WRITE(6,*) 'JF, NSEC1(6), NSEC1(7), NSEC1(8)'
      WRITE(6,*) JF, NSEC1(6), NSEC1(7), NSEC1(8)
      call read_2d_input_files_6h(data2d_6h_ps)
    ENDIF

    DO m=1,n3d_6hml

      ! Cycle through the 3-d fields (model levels), retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (NMDAYS*4 time samples for 6-hourly data)

      IF ((NSEC1(7).EQ.109).AND.(NSEC1(6).EQ.parin3d_6hml(m))) THEN

        DO i=1,lev
          IF (NSEC1(8).EQ.i) THEN
            WRITE(6,*)
            WRITE(6,*) 'JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_6hml(m)'
            WRITE(6,*) JF, m, i, NSEC1(6), NSEC1(7), NSEC1(8), parin3d_6hml(m)
            call read_2d_input_files_6h(data2d_6h)
            data3d_6hml(:,:,i,:)=data2d_6h(:,:,:)

          IF (i.EQ.lev) THEN

            IF (NPRINTLEV.GT.1) THEN
              do jj=1,lat
                do ii=1,lon
                  WRITE(6,*) 'i=', i, 'jj=', jj, 'ii=', ii, 'data3d_6hml(ii,jj,i,1)=', data3d_6hml(ii,jj,i,1)
                enddo
              enddo
            ENDIF

        if (TRIM(ADJUSTL(suffix_6h_mlev)).eq."") then
          suffix=""
        else
          READ(18,500) fn3d_6hml(m)
          suffix=fn3d_6hml(m)
!          suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &               '/6hr/atmos/'//TRIM(ADJUSTL(entry3d_6hml(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry3d_6hml(m)))//'_6hrLev_'//TRIM(ADJUSTL(model_id))//'_'// &
! &               TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_6h_mlev))//'.nc'
        endif
        WRITE(6,*) 'suffix=',suffix

            var3d_6hml_ids(m) = cmor_variable(                              &
                 table='Tables/CMIP5_6hrLev',                               &
                 table_entry=entry3d_6hml(m),                               &
                 units=units3d_6hml(m),                                     &
                 axis_ids=(/ ilon_6hml, ilat_6hml, ilev_6h, itim1_6hml /),  &
                 missing_value=1.0e28,                                      &
                 original_name=varin3d_6hml(m))

            WRITE(6,*) 'var3d_6hml_ids(m)=',var3d_6hml_ids(m)

            error_flag = cmor_write(                            &
                 var_id        = var3d_6hml_ids(m),             &
                 data          = data3d_6hml(:,:,:,1:NMDAYS*4), &
                 file_suffix   = TRIM(ADJUSTL(suffix)),         &
                 ntimes_passed = NMDAYS*4,                      &
                 time_vals     = time1_6h(1:NMDAYS*4)  )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_6hrLev Table ' &
                    // 'field ', entry3d_6hml(m), ', which I call ', varin3d_6hml(m)
               WRITE(6,*) ' Was processing time sample: ', time1_6h(1:NMDAYS*4)
            ENDIF
             
            error_flag = cmor_write(                   &
                 var_id        = zfactor_6h_id,        &
                 data          = data2d_6h_ps,         &
                 ntimes_passed = NMDAYS*4,             &
                 time_vals     = time1_6h(1:NMDAYS*4), &
                 store_with    = var3d_6hml_ids(m) )

            WRITE(6,*) 'cmor_write error_flag=',error_flag

            IF (error_flag < 0) THEN
               WRITE(6,*) ' Error encountered writing CMIP5_6hrLev Table ' &
                    // 'field ', 'ps', ', which I call ', 'LNSP'
               WRITE(6,*) ' Was processing time sample: ', time1_6h(1:NMDAYS*4)
            ENDIF

            error_flag = cmor_close(var3d_6hml_ids(m),cname)
            fn3d_6hml(m)=cname
            WRITE(6,*) 'fn3d_6hml(m)=',fn3d_6hml(m)
            WRITE(28,500) fn3d_6hml(m)

            WRITE(6,*) 'cmor_close error_flag=',error_flag

          ENDIF

          ENDIF

        ENDDO
      ENDIF

    ENDDO

!  End 6-hourly model level data (table CMIP5_6hrLev)

!  3-hourly data (table CMIP5_3hr); ! and 6-hourly data for psl (table CMIP5_6hrPlev)

    DO m=1,n2d_3h

      ! Cycle through the 2-d fields, retrieve the requested variable, define the cmor variable
      ! and append each variable to the appropriate netCDF file (NMDAYS*8 time samples for 3-hourly data)

      IF ((((NSEC1(7).EQ.1).OR.(NSEC1(7).EQ.112)).AND.(NSEC1(6).EQ.parin2d_3h(m))) &
 &      .OR.((NSEC1(7).EQ.100).AND.(NSEC1(8).EQ.1000).AND.(NSEC1(6).EQ.152).AND.(parin2d_3h(m).EQ.152))) THEN

        WRITE(6,*)
        WRITE(6,*) 'JF, m, NSEC1(6), NSEC1(7), NSEC1(8), parin2d_3h(m)'
        WRITE(6,*) JF, m, NSEC1(6), NSEC1(7), NSEC1(8), parin2d_3h(m)

        IF (parin2d_3h(m).EQ.151) THEN
          if (TRIM(ADJUSTL(suffix_3h_sfc)).eq."") then
            suffix=""
          else
            READ(16,500) fn2d_3h(m)
            suffix=fn2d_3h(m)
!            suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &                 '/6hr/atmos/'//TRIM(ADJUSTL(entry2d_3h(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry2d_3h(m)))//'_6hrPlev_'//TRIM(ADJUSTL(model_id))//'_'// &
! &                 TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_3h_sfc))//'.nc'
          endif
        ELSE
          if (TRIM(ADJUSTL(suffix_3h_sfc)).eq."") then
            suffix=""
          else
            READ(16,500) fn2d_3h(m)
            suffix=fn2d_3h(m)
!            suffix=TRIM(ADJUSTL(outpath))//'/CMIP5/output/'//TRIM(ADJUSTL(institute_id))//'/'//TRIM(ADJUSTL(model_id))//'/'//TRIM(ADJUSTL(experiment_id))// &
! &                 '/3hr/atmos/'//TRIM(ADJUSTL(entry2d_3h(m)))//'/'//TRIM(ADJUSTL(rip))//'/'//TRIM(ADJUSTL(entry2d_3h(m)))//'_3hr_'//TRIM(ADJUSTL(model_id))//'_'// &
! &                 TRIM(ADJUSTL(experiment_id))//'_'//TRIM(ADJUSTL(rip))//'_'//TRIM(ADJUSTL(suffix_3h_sfc))//'.nc'
          endif
        ENDIF
        WRITE(6,*) 'suffix=',suffix

        call read_2d_input_files_3h(data2d_3h)

        ! psl is saved every 6 hours in table CMIP5_6hrPlev
        IF ((NSEC1(7).EQ.1).AND.(NSEC1(6).EQ.151).AND.(parin2d_3h(m).EQ.151)) THEN
          DO i=1,ndays*4
            j=i*2
            data2d_6h_psl(:,:,i)=data2d_3h(:,:,j)
          ENDDO
        ENDIF

        IF (parin2d_3h(m).EQ.151) THEN
          var2d_3h_ids(m) = cmor_variable(                      &
               table='Tables/CMIP5_6hrPlev',                    &
               table_entry=entry2d_3h(m),                       &
               units=units2d_3h(m),                             &
               axis_ids=(/ ilon_6hpl, ilat_6hpl, itim1_6hpl /), &
               missing_value=1.0e28,                            &
               positive=positive2d_3h(m),                       &
               original_name=varin2d_3h(m))
        ELSE
!          IF ((NACCFLAG(JF).EQ.1).OR.(parin2d_3h(m).EQ.164)) THEN
          IF (NACCFLAG(JF).EQ.1) THEN
            var2d_3h_ids(m) = cmor_variable(               &
                 table='Tables/CMIP5_3hr',                 &
                 table_entry=entry2d_3h(m),                &
                 units=units2d_3h(m),                      &
                 axis_ids=(/ ilon_3h, ilat_3h, itim_3h /), &
                 missing_value=1.0e28,                     &
                 positive=positive2d_3h(m),                &
                 original_name=varin2d_3h(m))
          ELSE
            var2d_3h_ids(m) = cmor_variable(                &
                 table='Tables/CMIP5_3hr',                  &
                 table_entry=entry2d_3h(m),                 &
                 units=units2d_3h(m),                       &
                 axis_ids=(/ ilon_3h, ilat_3h, itim1_3h /), &
                 missing_value=1.0e28,                      &
                 positive=positive2d_3h(m),                 &
                 original_name=varin2d_3h(m))
          ENDIF
        ENDIF

        WRITE(6,*) 'var2d_3h_ids(m)=',var2d_3h_ids(m)

        IF (parin2d_3h(m).EQ.151) THEN
          error_flag = cmor_write(                            &
               var_id        = var2d_3h_ids(m),               &
               data          = data2d_6h_psl(:,:,1:NMDAYS*4), &
               file_suffix   = TRIM(ADJUSTL(suffix)),         &
               ntimes_passed = NMDAYS*4,                      &
               time_vals     = time1_6h(1:NMDAYS*4) )
        ELSE
!          IF ((NACCFLAG(JF).EQ.1).OR.(parin2d_3h(m).EQ.164)) THEN
          IF (NACCFLAG(JF).EQ.1) THEN
            error_flag = cmor_write(                         &
                 var_id        = var2d_3h_ids(m),            &
                 data          = data2d_3h(:,:,1:NMDAYS*8),  &
                 file_suffix   = TRIM(ADJUSTL(suffix)),      &
                 ntimes_passed = NMDAYS*8,                   &
                 time_vals     = time_3h(1:NMDAYS*8),        &
                 time_bnds     = time_3h_bnds(:,1:NMDAYS*8) )
          ELSE
            error_flag = cmor_write(                        &
                 var_id        = var2d_3h_ids(m),           &
                 data          = data2d_3h(:,:,1:NMDAYS*8), &
                 file_suffix   = TRIM(ADJUSTL(suffix)),     &
                 ntimes_passed = NMDAYS*8,                  &
                 time_vals     = time1_3h(1:NMDAYS*8) )
          ENDIF
        ENDIF

        WRITE(6,*) 'cmor_write error_flag=',error_flag

        IF (parin2d_3h(m).EQ.151) THEN
          IF (error_flag < 0) THEN
            WRITE(6,*) ' Error encountered writing CMIP5_6hrPlev Table ' &
                 // 'field ', entry2d_3h(m), ', which I call ', varin2d_3h(m)
            WRITE(6,*) ' Was processing time sample: ', time1_6h(1:NMDAYS*4)
          ENDIF
        ELSE
!          IF ((NACCFLAG(JF).EQ.1).OR.(parin2d_3h(m).EQ.164)) THEN
          IF (NACCFLAG(JF).EQ.1) THEN
            IF (error_flag < 0) THEN
              WRITE(6,*) ' Error encountered writing CMIP5_3hr Table ' &
                   // 'field ', entry2d_3h(m), ', which I call ', varin2d_3h(m)
              WRITE(6,*) ' Was processing time sample: ', time_3h(1:NMDAYS*8)
            ENDIF 
          ELSE
            IF (error_flag < 0) THEN
              WRITE(6,*) ' Error encountered writing CMIP5_3hr Table ' &
                   // 'field ', entry2d_3h(m), ', which I call ', varin2d_3h(m)
              WRITE(6,*) ' Was processing time sample: ', time1_3h(1:NMDAYS*8)
            ENDIF
          ENDIF
        ENDIF

        error_flag = cmor_close(var2d_3h_ids(m),cname)
        fn2d_3h(m)=cname
        WRITE(6,*) 'fn2d_3h(m)=',fn2d_3h(m)
        WRITE(26,500) fn2d_3h(m)

        WRITE(6,*) 'cmor_close error_flag=',error_flag

      ENDIF
    ENDDO

!  End 3-hourly data (table CMIP5_3hr)

  IF(JF.EQ.NFIELDS) THEN

    !   Close all files opened by CMOR (if any is still not closed).

    error_flag = cmor_close()

    WRITE(6,*)
    WRITE(6,*) '***************************'
    WRITE(6,*)
    WRITE(6,*) 'cmor executed to completion'
    WRITE(6,*)
    WRITE(6,*) '***************************'

  ENDIF

  IF (LWGRIB) THEN
    CALL WRITEALL(JF)
  ELSE
    CALL DEALLOC(JF)
  ENDIF

  ENDDO ! End JF loop

  if (TRIM(ADJUSTL(suffix_mon_sfc)).ne."") CLOSE(11)
  CLOSE(21)
  if (TRIM(ADJUSTL(suffix_mon_plev)).ne."") CLOSE(12)
  CLOSE(22)
  if (TRIM(ADJUSTL(suffix_mon_mlev)).ne."") CLOSE(13)
  CLOSE(23)
  if (TRIM(ADJUSTL(suffix_day_sfc)).ne."") CLOSE(14)
  CLOSE(24)
  if (TRIM(ADJUSTL(suffix_day_plev)).ne."") CLOSE(15)
  CLOSE(25)
  if (TRIM(ADJUSTL(suffix_3h_sfc)).ne."") CLOSE(16)
  CLOSE(26)
  if (TRIM(ADJUSTL(suffix_6h_plev)).ne."") CLOSE(17)
  CLOSE(27)
  if (TRIM(ADJUSTL(suffix_6h_mlev)).ne."") CLOSE(18)
  CLOSE(28)

  STOP
  END


      SUBROUTINE GETALL(JF)
!
!****  GETALL  - retrieves all GRIB fields from specified data file
!
!      Original version:               T. Stockdale    ECMWF  05.04.94
!      Modified: arbitrary fields      T. Stockdale    ECMWF  22.06.00
!
!     METHOD
!     ------
!     GETALL retrieves all the 2-d data fields, during several passes
!     through the subroutine.
!
!     It should first be called with JF=0, when it will read through
!     the data file, determine the total number of fields to be read,
!     and allocate the arrays.
!
!     If both convective and large scale precip are present, GETALL will
!     also create a total precip field (code 228). Similarly, GETALL
!     will create a 10m wind speed (207) from the u and v components.
!
!     Subsequent calls will read in all timesteps of a given field.
!
!     i/o efficiency could be improved by building a table of locations
!     on the first pass through the file, and using this in subsequent
!     reads. If i/o is a real problem, then this should be considered.
!
!-----------------------------------------------------------------------

      USE NCTL
      USE IGRIB
      USE RGRIB

      IMPLICIT NONE

      INTEGER NFTOTP
      INTEGER NFTOTW
 
      INTEGER, PARAMETER :: KLENG=2000000
      INTEGER, PARAMETER :: KLEND=1400000

      INTEGER KGRIB(KLENG)

      INTEGER KSEC0(2)
      INTEGER KSEC1(1024)
      INTEGER KSEC2(1024)
      INTEGER KSEC3(2)
      INTEGER KSEC4(512)
    
      INTEGER :: JJ, JF, JT, J, JFF, JTW, JTP, JR
      INTEGER :: IOUTLEN, IRET, KRET, KWORD
      INTEGER :: IMATCH, IPREV, IBOTHR, IBOTHW
      INTEGER :: NMAXSTEPS, NTOT, NSTEPSP, NSTEPSW

      REAL*8 PSEC2(512)
      REAL*8 PSEC3(2)
      REAL*8 PSEC4(KLEND)

      CHARACTER*14 CLFILE

      LOGICAL LLTOTP,LLTOTW

      IF((JF.GT.NFIELDS).AND.(NFIELDS.NE.0)) THEN
         WRITE(6,*) 'No more fields to retrieve'
         CALL PBCLOSE(NPUNIT,KRET)
         RETURN
      ENDIF


      CLFILE="fcdata"

      IF(JF.EQ.0) THEN
         CALL PBOPEN(NPUNIT,CLFILE,'r',IRET)
         IF(IRET.NE.0) THEN
            WRITE(6,*) 'Error opening file'
            CALL ABORT()
         ENDIF
         WRITE(6,*)
         WRITE(6,*) 'Opened  ',clfile
      ELSE
         CALL PBSEEK(NPUNIT,0,0,IRET)
         IF(IRET.NE.0) THEN
            WRITE(6,*) 'Error rewinding file'
            CALL ABORT()
         ENDIF
         WRITE(6,*)
         WRITE(6,*) 'Rewound ',clfile
      ENDIF

!
!  Note: We read grib data one at a time, and assign them to arrays
!   depending on the parameter (KSEC1(6)).  The exact order of the data
!   does not matter, as long as for each field, the data are in
!   increasing order of model step.
!

      NTOT=0
      PSEC3(2)=RMISS
! KSEC3 apparently not set by GRIBEX if section 3 does not exist, so need these values here
      KSEC3(1)=0
      KSEC3(2)=-999

      NSTEPS=0
      NSTEPSP=0
      NSTEPSW=0

      IBOTHR=0
      IBOTHW=0

      DO JR=1,JPTMAX*JPFMAX

         CALL PBGRIB(NPUNIT,KGRIB,KLENG*4,IOUTLEN,IRET)

         IF(IRET.NE.0) THEN
            IF(JF.GT.0) THEN
              WRITE(6,*) 'Field nr. ',JF,' is ',NFSEC1(JF,6),', read: ',&
     &                 NSTEPS(JF),' steps'
              WRITE(6,*)  'No more data to read in'
            ELSE
              WRITE(6,*)  'First pass through data complete'
            ENDIF
            GOTO 200
         ELSE
            NTOT=NTOT+1
         ENDIF

         CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &         PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'I',KRET)
         IF(KRET.GT.0) THEN
            WRITE(6,*) 'Error decoding grib header'
            WRITE(6,*) 'KRET is ',KRET
            CALL ABORT()
         ENDIF

!  On first pass, build up table of which fields present in file
!  and allocate memory

         IF(JF.EQ.0) THEN
            IF(NFIELDS.EQ.0) THEN
               IPREV=0
               DO J=1,9
                  NFSEC1(1,J)=KSEC1(J)
               ENDDO
               NFIELDS=1
               WRITE(6,*) 'Field ',NFIELDS,' is ',NFSEC1(NFIELDS,6)
               IF(KSEC1(1).NE.128) THEN
                  WRITE(6,*) ' code table is ',NFSEC1(NFIELDS,1)
               ENDIF
               IF(KSEC1(7).EQ.100) THEN
                  WRITE(6,*) ' pl level is ',NFSEC1(NFIELDS,8)
               ELSEIF(KSEC1(7).EQ.113) THEN
                  WRITE(6,*) ' pt level is ',NFSEC1(NFIELDS,8)
               ELSEIF(KSEC1(7).EQ.117) THEN
                  WRITE(6,*) ' pv level is ',NFSEC1(NFIELDS,8)
               ELSEIF(KSEC1(7).EQ.109) THEN
                  WRITE(6,*) ' ml level is ',NFSEC1(NFIELDS,8)
               ENDIF
            ELSE
               IPREV=0
               DO JFF=1,NFIELDS
                  IMATCH=1
                  DO J=1,2
                     IF(KSEC1(J).NE.NFSEC1(JFF,J)) IMATCH=0
                  ENDDO
                  DO J=6,9
                     IF(KSEC1(J).NE.NFSEC1(JFF,J)) IMATCH=0
                  ENDDO
                  IF(IMATCH.EQ.1) IPREV=1
               ENDDO
               IF(IPREV.EQ.0) THEN
                  NFIELDS=NFIELDS+1
                  IF(NFIELDS.GT.JPFMAX) THEN
                     WRITE(6,*) 'JPFMAX too small'
                     CALL ABORT
                  ENDIF
                  DO J=1,9
                     NFSEC1(NFIELDS,J)=KSEC1(J)
                  ENDDO
                  WRITE(6,*) 'Field ',NFIELDS,' is ',NFSEC1(NFIELDS,6)
                  IF(KSEC1(1).NE.128) THEN
                     WRITE(6,*) ' code table is ',NFSEC1(NFIELDS,1)
                  ENDIF
                  IF(KSEC1(7).EQ.100) THEN
                     WRITE(6,*) ' pl level is ',NFSEC1(NFIELDS,8)
                  ELSEIF(KSEC1(7).EQ.113) THEN
                     WRITE(6,*) ' pt level is ',NFSEC1(NFIELDS,8)
                  ELSEIF(KSEC1(7).EQ.117) THEN
                     WRITE(6,*) ' pv level is ',NFSEC1(NFIELDS,8)
                  ELSEIF(KSEC1(7).EQ.109) THEN
                     WRITE(6,*) ' ml level is ',NFSEC1(NFIELDS,8)
                  ENDIF
               ENDIF
            ENDIF
!  Check if now is the time to add total precip (228) or 
!  10 m wind speed (207) to the list
            IF((IPREV.EQ.0).AND.(KSEC1(1).EQ.128)) THEN
               IF(KSEC1(6).EQ.142) THEN
                 DO JFF=1,NFIELDS-1
                   IF(NFSEC1(JFF,6).EQ.143) IBOTHR=1
                 ENDDO
               ENDIF
               IF(KSEC1(6).EQ.143) THEN
                 DO JFF=1,NFIELDS-1
                   IF(NFSEC1(JFF,6).EQ.142) IBOTHR=1
                 ENDDO
               ENDIF
               IF(KSEC1(6).EQ.165) THEN
                 DO JFF=1,NFIELDS-1
                   IF(NFSEC1(JFF,6).EQ.166) IBOTHW=1
                 ENDDO
               ENDIF
               IF(KSEC1(6).EQ.166) THEN
                 DO JFF=1,NFIELDS-1
                   IF(NFSEC1(JFF,6).EQ.165) IBOTHW=1
                 ENDDO
               ENDIF
               DO JFF=1,NFIELDS-1
                 IF(NFSEC1(JFF,6).EQ.228) IBOTHR=0
               ENDDO
               DO JFF=1,NFIELDS-1
                 IF(NFSEC1(JFF,6).EQ.207) IBOTHW=0
               ENDDO
               IF(IBOTHR.EQ.1) THEN
                  WRITE(6,*) 'Large scale and convective precip present'
                  WRITE(6,*) 'Adding total precip (228) to list'
                  NFIELDS=NFIELDS+1
                  ITOTP=1
                  NFTOTP=NFIELDS
                  IF(NFIELDS.GT.JPFMAX) THEN
                     WRITE(6,*) 'JPFMAX too small'
                     CALL ABORT
                  ENDIF
                  NFSEC1(NFIELDS,6)=228
                  DO J=7,9
                     NFSEC1(NFIELDS,J)=KSEC1(J)
                  ENDDO
                  WRITE(6,*) 'Field ',NFIELDS,' is ',NFSEC1(NFIELDS,6)
               ENDIF
               IF(IBOTHW.EQ.1) THEN
                  WRITE(6,*) '10 metre u and v both present'
                  WRITE(6,*) 'Adding 10 m wind speed (207) to list'
                  NFIELDS=NFIELDS+1
                  ITOTW=1
                  NFTOTW=NFIELDS
                  IF(NFIELDS.GT.JPFMAX) THEN
                     WRITE(6,*) 'JPFMAX too small'
                     CALL ABORT
                  ENDIF
                  NFSEC1(NFIELDS,6)=207
                  DO J=7,9
                     NFSEC1(NFIELDS,J)=KSEC1(J)
                  ENDDO
                  WRITE(6,*) 'Field ',NFIELDS,' is ',NFSEC1(NFIELDS,6)
               ENDIF
            ENDIF
         ENDIF


         IF(JF.EQ.0) THEN
!  Pass through the decoding for the first field to obtain 
!   array size and NSTEPS
           IMATCH=1
           LLTOTP=.FALSE.
           LLTOTW=.FALSE.
           DO J=1,2
             IF(KSEC1(J).NE.NFSEC1(1,J)) IMATCH=0
           ENDDO
           DO J=6,9
             IF(KSEC1(J).NE.NFSEC1(1,J)) IMATCH=0
           ENDDO
         ENDIF

         IF(JF.GT.0) THEN
!  If we want this field on this pass, then decode fully
           IMATCH=1
           DO J=1,2
             IF(KSEC1(J).NE.NFSEC1(JF,J)) IMATCH=0
           ENDDO
           DO J=6,9
             IF(KSEC1(J).NE.NFSEC1(JF,J)) IMATCH=0
           ENDDO
! Special treatment for total precip / wind speed, in the case
! that ITOTP=1 / ITOTW=1. (If ITOTP / ITOTW is zero, then
! total precip / wind speed is just an ordinary field).
           LLTOTP=.FALSE.
           IF((KSEC1(1).EQ.128).AND.(NFSEC1(JF,6).EQ.228).AND.&
     &        (ITOTP.EQ.1)) THEN
              IF((KSEC1(6).EQ.142).OR.(KSEC1(6).EQ.143)) IMATCH=1
              IF(KSEC1(6).EQ.228) IMATCH=0
              LLTOTP=.TRUE.
           ENDIF
           LLTOTW=.FALSE.
           IF((KSEC1(1).EQ.128).AND.(NFSEC1(JF,6).EQ.207).AND.&
     &         (ITOTW.EQ.1)) THEN
              IF((KSEC1(6).EQ.165).OR.(KSEC1(6).EQ.166)) IMATCH=1
              IF(KSEC1(6).EQ.207) IMATCH=0
              LLTOTW=.TRUE.
           ENDIF
         ENDIF

         IF(IMATCH.EQ.1) THEN

            CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &         PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'D',KRET)
            IF(KRET.GT.0) THEN
               WRITE(6,*) 'Error decoding grib'
               WRITE(6,*) 'KRET is ',KRET
               CALL ABORT()
            ENDIF
            
            IF(.NOT.(LLTOTP.OR.LLTOTW)) THEN
               NSTEPS(JF)=NSTEPS(JF)+1
            ELSE
               IF(KSEC1(6).EQ.142) NSTEPS(JF)=NSTEPS(JF)+1
               IF(KSEC1(6).EQ.143) NSTEPSP=NSTEPSP+1
               IF(KSEC1(6).EQ.165) NSTEPS(JF)=NSTEPS(JF)+1
               IF(KSEC1(6).EQ.166) NSTEPSW=NSTEPSW+1
            ENDIF

            JT=NSTEPS(JF)
            JTP=NSTEPSP
            JTW=NSTEPSW


            IF(JT.EQ.1) THEN
               DO JJ=1,2
                  NSEC0(JJ)=KSEC0(JJ)
                  NSEC3(JJ)=KSEC3(JJ)
                  RSEC3(JJ)=PSEC3(JJ)
               ENDDO
               DO JJ=1,512
                  NSEC4(JJ)=KSEC4(JJ)
                  RSEC2(JJ)=PSEC2(JJ)
               ENDDO
               DO JJ=1,1024
                  NSEC1(JJ)=KSEC1(JJ)
!                  IF(JF.EQ.1) THEN
!      WRITE(6,*) 'JJ, KSEC1(JJ), NSEC1(JJ) = ',JJ,KSEC1(JJ),NSEC1(JJ)
!                  ENDIF
                  NSEC2(JJ)=KSEC2(JJ)
               ENDDO
               IF(LLTOTP) THEN
                  NSEC1(6)=228
               ENDIF
               IF(LLTOTW) THEN
                  NSEC1(6)=207
               ENDIF

               IF(JF.GT.0) THEN
                 IF ((KSEC1(6).EQ.44).OR.(KSEC1(6).EQ.45).OR.&
     &              (KSEC1(6).EQ.50).OR.(KSEC1(6).EQ.57).OR.& 
     &              (KSEC1(6).EQ.58).OR.(KSEC1(6).EQ.142).OR.&
     &              (KSEC1(6).EQ.143).OR.(KSEC1(6).EQ.144).OR.&
     &              (KSEC1(6).EQ.145).OR.(KSEC1(6).EQ.146).OR.&
     &              (KSEC1(6).EQ.147).OR.(KSEC1(6).EQ.169).OR.&
     &              (KSEC1(6).EQ.175).OR.(KSEC1(6).EQ.176).OR.&
     &              (KSEC1(6).EQ.177).OR.(KSEC1(6).EQ.178).OR.&
     &              (KSEC1(6).EQ.179).OR.(KSEC1(6).EQ.180).OR.&
     &              (KSEC1(6).EQ.181).OR.(KSEC1(6).EQ.182).OR.&
     &              (KSEC1(6).EQ.189).OR.(KSEC1(6).EQ.195).OR.&
     &              (KSEC1(6).EQ.196).OR.(KSEC1(6).EQ.197).OR.&
     &              (KSEC1(6).EQ.205).OR.(KSEC1(6).EQ.208).OR.&
     &              (KSEC1(6).EQ.209).OR.(KSEC1(6).EQ.210).OR.&
     &              (KSEC1(6).EQ.211).OR.(KSEC1(6).EQ.228).OR.&
     &              (KSEC1(6).EQ.239).OR.(KSEC1(6).EQ.240)) THEN
                   WRITE(6,*) 'Accumulated field'
                   NACCFLAG(JF)=1
                 ENDIF
               ENDIF
            ENDIF

! Check a bit more of the grib header, then copy data

            DO JJ=2,4
               IF(NSEC1(JJ).NE.KSEC1(JJ)) THEN
                  WRITE(6,*) 'Inconsistent grib headers in KSEC1'
                  WRITE(6,*)  NSEC1(JJ),KSEC1(JJ)
                  WRITE(6,*) 'JF,JT,JJ: ',JF,JT,JJ
               ENDIF
            ENDDO
            NTSEC1(JT,5)=KSEC1(5)
            DO JJ=6,9
               IF(NSEC1(JJ).NE.KSEC1(JJ)) THEN
                  IF((JJ.EQ.6).AND.(LLTOTP.OR.LLTOTW)) THEN
!                   WRITE(6,*) 'Using ',KSEC1(6),' to build ',NSEC1(6)
!                   WRITE(6,*) 'jt,jt2 ',jt,jt2
                  ELSE
                    WRITE(6,*) 'Inconsistent grib headers in KSEC1'
                    WRITE(6,*)  NSEC1(JJ),KSEC1(JJ)
                    WRITE(6,*) 'JF,JT,JJ: ',JF,JT,JJ
                    CALL ABORT()
                  ENDIF
               ENDIF
            ENDDO
            DO JJ=10,18
               NTSEC1(JT,JJ)=KSEC1(JJ)
            ENDDO
            DO JJ=19,36
               IF(NSEC1(JJ).NE.KSEC1(JJ)) THEN
                  WRITE(6,*) 'Inconsistent grib headers in KSEC1'
                  WRITE(6,*)  NSEC1(JJ),KSEC1(JJ)
                  WRITE(6,*) 'JF,JT,JJ: ',JF,JT,JJ
               ENDIF
            ENDDO

            IF(JF.GT.0) THEN
            IF(.NOT.(LLTOTP.OR.LLTOTW)) THEN
               DO JJ=1,NSEC4(1)
                  RSEC4(JT,JJ)=PSEC4(JJ)
               ENDDO
            ELSE
              IF(LLTOTP) THEN
                IF(((KSEC1(6).EQ.142).AND.(NSTEPS(JF).GT.NSTEPSP)).OR.&
     &          ((KSEC1(6).EQ.143).AND.(NSTEPSP.GT.NSTEPS(JF))) ) THEN
                  DO JJ=1,NSEC4(1)
                     RSEC4(JT,JJ)=PSEC4(JJ)
                  ENDDO
                ELSE
                    DO JJ=1,NSEC4(1)
                       RSEC4(JTP,JJ)=RSEC4(JTP,JJ)+PSEC4(JJ)
                    ENDDO
                ENDIF
              ELSEIF(LLTOTW) THEN
                IF(((KSEC1(6).EQ.165).AND.(NSTEPS(JF).GT.NSTEPSW)).OR.&
     &          ((KSEC1(6).EQ.166).AND.(NSTEPSW.GT.NSTEPS(JF))) ) THEN
                  DO JJ=1,NSEC4(1)
                     RSEC4(JT,JJ)=PSEC4(JJ)*PSEC4(JJ)
                  ENDDO
                ELSE
                  DO JJ=1,NSEC4(1)
                     RSEC4(JTW,JJ)=&
     &                    SQRT(RSEC4(JTW,JJ)+PSEC4(JJ)*PSEC4(JJ))
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
            ENDIF

!  End of IMATCH=1 block
         ENDIF

!  End of loop to read fields
      ENDDO

!  Exit loop: no more data to read in
  200    CONTINUE

      IF(JF.EQ.0) THEN

         IF(ITOTP.EQ.1) NTOT=NTOT+NSTEPS(NFTOTP)
         IF(ITOTW.EQ.1) NTOT=NTOT+NSTEPS(NFTOTW)

         WRITE(6,*)
         WRITE(6,*) 'Number of fields  : ',NFIELDS
         WRITE(6,*) 'Total number read : ',NTOT
         WRITE(6,*) 'Size of each 2d field  : ',NSEC4(1)

         IF(NFIELDS.EQ.0) THEN
            WRITE(6,*) 'No data'
            CALL ABORT()
         ENDIF

         NMAXSTEPS=NMDAYS*24/NMININT
         WRITE(6,*) 'NMAXSTEPS = ',NMAXSTEPS

!  Allocate memory

         ALLOCATE (RSEC4(NMAXSTEPS,NSEC4(1)))
         WRITE(6,*) 'Allocated RSEC4: ',&
     &              (NMAXSTEPS)*NSEC4(1)*4,' bytes'
         WRITE(6,*)

      ENDIF

      RETURN
      END


      SUBROUTINE MMEAN(JF)
!
!   This code is *NOT* completely general. It assumes:
!    - the interval for each field is constant during the whole month
!    - the forecasts all start at 0Z.
!

      USE NCTL, ONLY: NMDAYS, LWGRIB
      USE IGRIB
      USE RGRIB
      USE RDATA
 
      IMPLICIT NONE

      INTEGER IMBEG,IMEND

      INTEGER :: JF, JT, JJ, IOUTF, JTDAY, IMISS
      INTEGER :: IYYYYMM, IM, IY, IVD, IVH, IVDFIRST, IVHFIRST
      REAL*8 :: ZANO, ZSS, ZD, ZFACT

      REAL*8 :: RZERO

      RZERO=0.0
!      WRITE(6,*) 'RZERO=',RZERO

      WRITE(6,*) 'MMEAN for field ',NSEC1(6)


!  First pass: create daily values, and set arrays defining months
!  Careful if averaging interval is not 24h for accumulated fields

      NINTERVAL(JF)=NMDAYS*24/NSTEPS(JF)
      WRITE(6,*) 'NINTERVAL = ',NINTERVAL(JF)

      IF((NINTERVAL(JF).GT.24).AND.(NSEC1(6).NE.212)) THEN
        WRITE(6,*) 'NINTERVAL cannot be greater than 24'
        CALL ABORT
      ENDIF

      DO JT=1,NSTEPS(JF)

        IF (NSEC1(6).EQ.212) THEN

          IMBEG=1
          IMEND=1      

        ELSE 

          IVD=NTSEC1(JT,12)
          IVH=NTSEC1(JT,13) 
 
          IF(NTSEC1(JT,18).EQ.0) THEN
            IF((IVD.EQ.1).AND.(IVH.EQ.0)) IMEND=JT
          ELSEIF(NTSEC1(JT,18).EQ.2) THEN
            IF((IVD.EQ.NMDAYS).AND.(IVH.EQ.(24-NINTERVAL(JF)))) IMEND=JT
          ELSE
            WRITE(6,*) 'Time range indicator not recognized'
            WRITE(6,*) 'KSEC1(18) = ',NTSEC1(JT,18)
            CALL ABORT
          ENDIF
 
          IVDFIRST=1
          IVHFIRST=NINTERVAL(JF)
          IF(NTSEC1(JT,18).EQ.0) THEN
            IF(NINTERVAL(JF).EQ.24) THEN 
              IVDFIRST=2
              IVHFIRST=0
            ENDIF 
          ELSEIF(NTSEC1(JT,18).EQ.2) THEN
              IVDFIRST=1
              IVHFIRST=0
          ELSE
              WRITE(6,*) 'Time range indicator not recognized'
              WRITE(6,*) 'KSEC1(18) = ',NTSEC1(JT,18)
              CALL ABORT 
          ENDIF
         
          IF((IVD.EQ.IVDFIRST).AND.(IVH.EQ.IVHFIRST)) THEN
             IMBEG=JT
             IMEND=0
          ENDIF

        ENDIF

      ENDDO

      WRITE(6,*) 'IMBEG, IMEND: ',IMBEG,IMEND

! Check whether the month is complete; if not, we will ignore it

      IF(IMEND.EQ.0) THEN
         WRITE(6,*) 'Insufficient data to calculate monthly means'
         CALL ABORT
      ENDIF

! Convert geopotential [m**2 s**-2] to geopotential height [m] (for pressure levels)

      IF((NSEC1(7).EQ.100).AND.(NSEC1(6).EQ.129)) THEN

        ZFACT=1.0/9.80665

        DO JT=1,NSTEPS(JF)

          IF(NTSEC1(JT,5).EQ.192) THEN
            DO JJ=1,NSEC4(1)
              IF (RSEC4(JT,JJ).NE.RMISS)&
     &          RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
            ENDDO
          ELSE
            DO JJ=1,NSEC4(1)
              RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
            ENDDO
          ENDIF

        ENDDO

      ENDIF  

! Convert lnsp [adim.] to sp [Pa] (for pressure level 1000 and model level 1)

      IF((NSEC1(6).EQ.152).AND.(((NSEC1(7).EQ.100).AND.(NSEC1(8).EQ.1000)).OR.((NSEC1(7).EQ.109).AND.(NSEC1(8).EQ.1)))) THEN

        DO JT=1,NSTEPS(JF)

          IF(NTSEC1(JT,5).EQ.192) THEN
            DO JJ=1,NSEC4(1)
              IF (RSEC4(JT,JJ).NE.RMISS)&
     &          RSEC4(JT,JJ)=EXP(RSEC4(JT,JJ))
            ENDDO
          ELSE
            DO JJ=1,NSEC4(1)
              RSEC4(JT,JJ)=EXP(RSEC4(JT,JJ))
            ENDDO
          ENDIF

        ENDDO

      ENDIF

! Convert cloud cover [0-1] to cloud cover [%]

      IF(((NSEC1(7).EQ.109).AND.(NSEC1(6).EQ.248)).OR.((NSEC1(7).EQ.1).AND.(NSEC1(6).EQ.164))) THEN

        ZFACT=100.

        DO JT=1,NSTEPS(JF)

          IF(NTSEC1(JT,5).EQ.192) THEN
            DO JJ=1,NSEC4(1)
              IF (RSEC4(JT,JJ).NE.RMISS)&
     &          RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
            ENDDO
          ELSE
            DO JJ=1,NSEC4(1)
              RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
            ENDDO
          ENDIF

        ENDDO

      ENDIF

!  Convert some fluxes (146, 147 and 179) to be positive upwards

   IF((NSEC1(7).EQ.1).AND.((NSEC1(6).EQ.146).OR.(NSEC1(6).EQ.147).OR.(NSEC1(6).EQ.179))) THEN
     
     ZFACT=-1.
     
     DO JT=1,NSTEPS(JF)

        IF(NTSEC1(JT,5).EQ.192) THEN
          DO JJ=1,NSEC4(1)
            IF (RSEC4(JT,JJ).NE.RMISS)&
     &        RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
          ENDDO
        ELSE
          DO JJ=1,NSEC4(1)
            RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
          ENDDO
        ENDIF

     ENDDO
 
   ENDIF

!  Convert to non-accumulated MKS units. This is good for most fields
!  (eg fluxes receive values in W/m2), but for rainfall we get m/s,
!  not the most commonly used unit.

      IF(NACCFLAG(JF).EQ.1) THEN

        DO JT=1,NSTEPS(JF)

           ZFACT=1.0/(NINTERVAL(JF)*3600.0)

           IF(NTSEC1(JT,5).EQ.192) THEN
             DO JJ=1,NSEC4(1)
                IF (RSEC4(JT,JJ).NE.RMISS)&
     &             RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
             ENDDO
           ELSE
             DO JJ=1,NSEC4(1)
                RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
             ENDDO
           ENDIF
        ENDDO

      ENDIF

! Convert non-accumulated precipitations, evaporation and runoff from [m/s] to [kg m-2 s-1]

      IF((NSEC1(7).EQ.1).AND.((NSEC1(6).EQ.142).OR.(NSEC1(6).EQ.143).OR.(NSEC1(6).EQ.144).OR.(NSEC1(6).EQ.228).OR.(NSEC1(6).EQ.182).OR.(NSEC1(6).EQ.205))) THEN

        ZFACT=1000.

        DO JT=1,NSTEPS(JF)

          IF(NTSEC1(JT,5).EQ.192) THEN
            DO JJ=1,NSEC4(1)
              IF (RSEC4(JT,JJ).NE.RMISS)&
     &          RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
            ENDDO
          ELSE
            DO JJ=1,NSEC4(1)
              RSEC4(JT,JJ)=RSEC4(JT,JJ)*ZFACT
            ENDDO
          ENDIF

        ENDDO

      ENDIF

! Only positive precipitation (grib has some very small negative values)

      IF((NSEC1(7).EQ.1).AND.((NSEC1(6).EQ.142).OR.(NSEC1(6).EQ.143).OR.(NSEC1(6).EQ.144).OR.(NSEC1(6).EQ.228))) THEN
        DO JT=1,NSTEPS(JF)
          DO JJ=1,NSEC4(1)
            IF(RSEC4(JT,JJ).LT.0.) RSEC4(JT,JJ)=0.
          ENDDO
        ENDDO
      ENDIF

      WRITE(6,*) 'NSEC4(1)=',NSEC4(1)

      IF(JF.EQ.1) THEN
        ALLOCATE (RMMEAN(NSEC4(1)))
        ALLOCATE (RMMAX(NSEC4(1)))
        ALLOCATE (RMMIN(NSEC4(1)))
        ALLOCATE (RMSD(NSEC4(1)))
        ALLOCATE (RDAY(NMDAYS,NSEC4(1)))
        WRITE(6,*) 'Allocated RDATA: ',NSEC4(1)*16,' bytes'
      ENDIF


!  Calculate daily mx2t, mn2t and 10m wind gust
!  Calculate daily means
!  Calculate monthly statistics
!    Use two passes to reduce effect of rounding error on s.d.
!    Be very careful with SQRT: errors here interact very
!    strangely with GRIBEX when IEEE arithimetic is being used.
!    If these are spectral fields, calculate only mean.


      IOUTF=2
      IF(NSEC4(3).EQ.0) THEN
         IOUTF=5
         IF((NSEC2(1).GE.50).AND.(NSEC2(1).LE.89)) IOUTF=2
      ENDIF

      DO JJ=1,NSEC4(1)
        DO JTDAY=1,NMDAYS
          IF ((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.49)) THEN
            RDAY(JTDAY,JJ)=-9.9E15
          ELSEIF(NSEC1(6).EQ.202) THEN
            RDAY(JTDAY,JJ)=9.9E15
          ELSE
            RDAY(JTDAY,JJ)=0.0
          ENDIF
        ENDDO
        RMMEAN(JJ)=0.0
      ENDDO

      JTDAY=1
      IMISS=0
      IF((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.202)&
     &                    .OR.(NSEC1(6).EQ.49)) THEN
        ZD=REAL(1.0,8)/REAL(NMDAYS,8)
      ELSE
        ZD=REAL(1.0,8)/REAL(IMEND-IMBEG+1,8)
      ENDIF

      IF (NSEC1(6).EQ.212) THEN
       
        DO JJ=1,NSEC4(1)
          RMMEAN(JJ)=RSEC4(1,JJ)
        ENDDO

      ELSE

        DO JT=IMBEG,IMEND
          DO JJ=1,NSEC4(1)       
            IF(RSEC4(JT,JJ).EQ.RMISS) IMISS=1
            IF ((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.49)) THEN
              RDAY(JTDAY,JJ)=MAX(RDAY(JTDAY,JJ),RSEC4(JT,JJ))
            ELSEIF(NSEC1(6).EQ.202) THEN
              RDAY(JTDAY,JJ)=MIN(RDAY(JTDAY,JJ),RSEC4(JT,JJ))
            ELSE
              RDAY(JTDAY,JJ)=RDAY(JTDAY,JJ)+RSEC4(JT,JJ)
              RMMEAN(JJ)=RMMEAN(JJ)+RSEC4(JT,JJ)
            ENDIF   
          ENDDO
          IF(MOD(JT,(24/NINTERVAL(JF))).EQ.0) THEN
            DO JJ=1,NSEC4(1)
              IF((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.202)&
     &                            .OR.(NSEC1(6).EQ.49)) THEN
                RMMEAN(JJ)=RMMEAN(JJ)+RDAY(JTDAY,JJ)
              ELSE
                RDAY(JTDAY,JJ)=RDAY(JTDAY,JJ)/(24/NINTERVAL(JF))
              ENDIF
              IF(IMISS.EQ.1) RDAY(JTDAY,JJ)=RMISS
            ENDDO
            JTDAY=JTDAY+1
          ENDIF
        ENDDO
        DO JJ=1,NSEC4(1)
          RMMEAN(JJ)=RMMEAN(JJ)*ZD
          IF(IMISS.EQ.1) RMMEAN(JJ)=RMISS
        ENDDO

      ENDIF

      IF(LWGRIB.AND.(IOUTF.EQ.5)) THEN
        DO JJ=1,NSEC4(1)
          ZSS=0.0
          RMMAX(JJ) =-9.9E15
          RMMIN(JJ) = 9.9E15
          IF((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.202)&
     &                        .OR.(NSEC1(6).EQ.49)) THEN
            DO JTDAY=1,NMDAYS
              ZANO=RDAY(JTDAY,JJ)-RMMEAN(JJ)
              ZSS=ZSS+ZANO*ZANO
              IF(RDAY(JTDAY,JJ).GT.RMMAX(JJ))&
     &                  RMMAX(JJ)=RDAY(JTDAY,JJ)
              IF(RDAY(JTDAY,JJ).LT.RMMIN(JJ))&
     &                  RMMIN(JJ)=RDAY(JTDAY,JJ)
            ENDDO  
          ELSE 
            DO JT=IMBEG,IMEND
              ZANO=RSEC4(JT,JJ)-RMMEAN(JJ)
              ZSS=ZSS+ZANO*ZANO
              IF(RSEC4(JT,JJ).GT.RMMAX(JJ))&
     &                  RMMAX(JJ)=RSEC4(JT,JJ)
              IF(RSEC4(JT,JJ).LT.RMMIN(JJ))&
     &                  RMMIN(JJ)=RSEC4(JT,JJ)
            ENDDO 
          ENDIF
          RMSD(JJ)=SQRT(MAX(ZSS*ZD,RZERO))
          IF(RMMEAN(JJ).EQ.RMISS) THEN
            RMMAX(JJ)=RMISS
            RMMIN(JJ)=RMISS
            RMSD(JJ)=RMISS
          ENDIF
        ENDDO
      ENDIF

!  Set grib headers


      JT=IMBEG
      IY=NTSEC1(JT,10) + (NSEC1(21)-1)*100
      IM=NTSEC1(JT,11)
      IYYYYMM=IY*100+IM
      WRITE(6,*) 'grib headers set for ',IYYYYMM
      NS1ADD(10)=NTSEC1(JT,10)
      NS1ADD(11)=NTSEC1(JT,11)
      NS1ADD(12)=1
      NS1ADD(13)=0
      NS1ADD(14)=0
      NS1ADD(15)=NTSEC1(JT,15)
      NS1ADD(16)=NMDAYS*24
      NS1ADD(17)=0
      NS1ADD(18)=10

      NS1ADD(37)=16
      NS1ADD(38)=NSEC1(38)
      NS1ADD(39)=80
!      IF(NSEC1(40).EQ.1090) THEN
!        NS1ADD(40)=1091
!      ELSEIF(NSEC1(40).EQ.1082) THEN
!        NS1ADD(40)=1092
!      ELSEIF(NSEC1(40).EQ.1220) THEN
!        NS1ADD(40)=1221
!      ELSEIF(NSEC1(40).EQ.1222) THEN
!        NS1ADD(40)=1223
!      ELSEIF(NSEC1(40).EQ.1230) THEN
!        NS1ADD(40)=1231
!      ELSEIF(NSEC1(40).EQ.1232) THEN
!        NS1ADD(40)=1233
!      ELSE
!        WRITE(6,*) 'STREAM of original data not recognized'
!        WRITE(6,*) 'NSEC1(40) is ',NSEC1(40)
!        CALL ABORT
!      ENDIF
      NS1ADD(40)=1221
      NS1ADD(41)=NSEC1(41)
      NS1ADD(42)=NSEC1(42)
      NS1ADD(43)=NSEC1(43)
      NS1ADD(44)=NSEC1(44)
      NS1ADD(45)=NSEC1(45)
      NS1ADD(46)=IYYYYMM
      IF((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.202)&
     &                    .OR.(NSEC1(6).EQ.49)) THEN
        NS1ADD(47)=24
      ELSE
        NS1ADD(47)=NINTERVAL(JF)
      ENDIF
!   "Subperiod from which monthly means derived (eg 6-hour, 24-hour)"
!  For monthly means of accumulated values, this could be thought undefined.
!  We take it to mean "the frequency of output from which the monthly
!  statistics were derived", in which case a value of 24 is appropriate
!  in the present case. Alternative might be to set to zero, but this would
!  make the archive more complicated.

      NS1ADD(48)=1
! KSEC1(48) was added to make fdb and mars archiving easier

      RETURN
      END


      SUBROUTINE WRITEALL(JF)

!  Write daily mx2t, mn2t, 10m wind gust or daily mean data to a file
!  Write monthly mean data to a file, ready for direct archiving to MARS

!  We do not write max,min,sd for spectral fields, since these have not
!  been calculated.

      USE NCTL, ONLY: NMDAYS
      USE IGRIB
      USE RGRIB
      USE RDATA

      IMPLICIT NONE

      INTEGER, PARAMETER :: KLENG=2000000
      INTEGER, PARAMETER :: KLEND=1400000
      INTEGER KGRIB(KLENG)

      INTEGER KSEC0(2)
      INTEGER KSEC1(1024)
      INTEGER KSEC2(1024)
      INTEGER KSEC3(2)
      INTEGER KSEC4(512)

      INTEGER :: JJ, JF, JTDAY, IMISS
      INTEGER :: KRET, KWORD, IRET
      INTEGER :: IOUTF, JTYPE

      REAL*8 PSEC2(512)
      REAL*8 PSEC3(2)
      REAL*8 PSEC4(KLEND)

      CHARACTER*20 CLFILE(5)


      CLFILE(1)="fcdmean"
      CLFILE(2)="fcmmean"
      CLFILE(3)="fcmmax"
      CLFILE(4)="fcmmin"
      CLFILE(5)="fcmsd"

      CALL GRSVCK(0)


      IOUTF=2
      IF(NSEC4(3).EQ.0) THEN
         IOUTF=5
         IF((NSEC2(1).GE.50).AND.(NSEC2(1).LE.89)) IOUTF=2
      ENDIF

      IF(JF.EQ.1) THEN
         WRITE(6,*) 'KSEC2(1), KSEC4(3): ',NSEC2(1),NSEC4(3)
         WRITE(6,*) 'Number of output files: ',IOUTF
         DO JTYPE=1,IOUTF
            CALL PBOPEN(NWUNIT(JTYPE),CLFILE(JTYPE),'w',IRET)
            IF(IRET.NE.0) THEN
               WRITE(6,*) 'Error opening output file'
               CALL ABORT
            ENDIF
         ENDDO
      ENDIF

      DO JJ=1,2
         KSEC0(JJ)=NSEC0(JJ)
         KSEC3(JJ)=NSEC3(JJ)
         PSEC3(JJ)=RSEC3(JJ)
      ENDDO
      DO JJ=1,512
         KSEC4(JJ)=NSEC4(JJ)
         PSEC2(JJ)=RSEC2(JJ)
      ENDDO
      DO JJ=1,1024
         KSEC2(JJ)=NSEC2(JJ)
      ENDDO

!  Daily mean and daily mx2t, mn2t and 10m wind gust
 
      DO JJ=1,45
        KSEC1(JJ)=NSEC1(JJ)
      ENDDO
      DO JJ=46,1024
        KSEC1(JJ)=0
      ENDDO
      KSEC1(13)=12
      IF ((NSEC1(6).EQ.201).OR.(NSEC1(6).EQ.49)) THEN
        KSEC1(39)=81
      ELSEIF(NSEC1(6).EQ.202) THEN
        KSEC1(39)=82
      ELSE
        KSEC1(39)=80
      ENDIF

      DO JTDAY=1,NMDAYS
        KSEC1(12)=JTDAY
        IMISS=0
        DO JJ=1,NSEC4(1)
          PSEC4(JJ)=RDAY(JTDAY,JJ)
          IF(PSEC4(JJ).EQ.RMISS) IMISS=1
        ENDDO
        IF(IMISS.EQ.0) THEN
          KSEC1(5)=128
        ELSE
          KSEC1(5)=192
        ENDIF
        IF(NACCFLAG(JF).EQ.1) THEN
          IF(NSEC1(1).EQ.128) THEN
            KSEC1(1)=172
          ELSE
            WRITE(6,*) 'Code table not found for rate of accumulation'
            WRITE(6,*) 'Input code table: ',NSEC1(1)
            WRITE(6,*) 'Input parameter : ',NSEC1(6)
            CALL ABORT
          ENDIF
        ELSE
          KSEC1(1)=NSEC1(1)
        ENDIF

        CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &        PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'C',KRET)
        IF(KRET.NE.0) THEN
           WRITE(6,*) 'Problem with GRIB coding'
           CALL ABORT
        ENDIF
        CALL PBWRITE(NWUNIT(1),KGRIB,KWORD*4,KRET)
        IF(KRET.LT.0) THEN
           WRITE(6,*) 'Problem writing to file - cannot continue'
           CALL ABORT
        ENDIF
        WRITE(6,*) 'Daily mean of',KSEC1(6),'written for day',JTDAY,':',&
     &               KWORD,' words'
      ENDDO


!  End daily mean and daily mx2t, mn2t and 10m wind gust

      DO JJ=1,36
        KSEC1(JJ)=NSEC1(JJ)
      ENDDO
      DO JJ=10,18
        KSEC1(JJ)=NS1ADD(JJ)
      ENDDO
      DO JJ=37,48
        KSEC1(JJ)=NS1ADD(JJ)
      ENDDO
      DO JJ=49,1024
        KSEC1(JJ)=0
      ENDDO

!  Monthly mean

      KSEC1(39)=80

      IMISS=0
      DO JJ=1,NSEC4(1)
         PSEC4(JJ)=RMMEAN(JJ)
         IF(PSEC4(JJ).EQ.RMISS) IMISS=1
      ENDDO
      IF(IMISS.EQ.0) THEN
        KSEC1(5)=128
      ELSE
        KSEC1(5)=192
      ENDIF
      IF(NACCFLAG(JF).EQ.1) THEN
        IF(NSEC1(1).EQ.128) THEN
          KSEC1(1)=172
        ELSE
          WRITE(6,*) 'Code table not found for rate of accumulation'
          WRITE(6,*) 'Input code table: ',NSEC1(1)
          WRITE(6,*) 'Input parameter : ',NSEC1(6)
          CALL ABORT
        ENDIF
      ELSE
        KSEC1(1)=NSEC1(1)
      ENDIF

      CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &      PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'C',KRET)
      IF(KRET.NE.0) THEN
         WRITE(6,*) 'Problem with GRIB coding'
         CALL ABORT
      ENDIF
      CALL PBWRITE(NWUNIT(2),KGRIB,KWORD*4,KRET)
      IF(KRET.LT.0) THEN
         WRITE(6,*) 'Problem writing to file - cannot continue'
         CALL ABORT
      ENDIF
      WRITE(6,*) 'Monthly mean of ',KSEC1(6),' written: ',&
     &             KWORD,' words'

      IF(IOUTF.GT.2) THEN

!  Monthly maximum

      KSEC1(39)=81

      DO JJ=1,NSEC4(1)
         PSEC4(JJ)=RMMAX(JJ)
      ENDDO
      CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &      PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'C',KRET)
      IF(KRET.NE.0) THEN
         WRITE(6,*) 'Problem with GRIB coding'
         CALL ABORT
      ENDIF
      CALL PBWRITE(NWUNIT(3),KGRIB,KWORD*4,KRET)
      IF(KRET.LT.0) THEN
         WRITE(6,*) 'Problem writing to file - cannot continue'
         CALL ABORT
      ENDIF
      WRITE(6,*) 'Monthly max of ',KSEC1(6),' written: ',&
     &             KWORD,' words'

!  Monthly minimum

      KSEC1(39)=82

      DO JJ=1,NSEC4(1)
         PSEC4(JJ)=RMMIN(JJ)
      ENDDO
      CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &      PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'C',KRET)
      IF(KRET.NE.0) THEN
         WRITE(6,*) 'Problem with GRIB coding'
         CALL ABORT
      ENDIF
      CALL PBWRITE(NWUNIT(4),KGRIB,KWORD*4,KRET)
      IF(KRET.LT.0) THEN
         WRITE(6,*) 'Problem writing to file - cannot continue'
         CALL ABORT
      ENDIF
      WRITE(6,*) 'Monthly min of ',KSEC1(6),' written: ',&
     &             KWORD,' words'

!  Monthly sd

      KSEC1(39)=83

      DO JJ=1,NSEC4(1)
         PSEC4(JJ)=RMSD(JJ)
      ENDDO

      CALL GRIBEX(KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,&
     &      PSEC4(1),KLEND,KGRIB,KLENG,KWORD,'C',KRET)
      IF(KRET.NE.0) THEN
         WRITE(6,*) 'Problem with GRIB coding'
         CALL ABORT
      ENDIF
      CALL PBWRITE(NWUNIT(5),KGRIB,KWORD*4,KRET)
      IF(KRET.LT.0) THEN
         WRITE(6,*) 'Problem writing to file - cannot continue'
         CALL ABORT
      ENDIF
      WRITE(6,*) 'Monthly sd of ',KSEC1(6),' written: ',&
     &             KWORD,' words'

      ENDIF

!  Check if this is last pass; if so, then exit cleanly

      IF(JF.EQ.NFIELDS) THEN
        DO JTYPE=1,IOUTF
           CALL PBCLOSE(NWUNIT(JTYPE),KRET)
        ENDDO
        DEALLOCATE(RSEC4)
        WRITE(6,*) 'Deallocated RSEC4'
        DEALLOCATE(RMMEAN)
        DEALLOCATE(RMMAX)
        DEALLOCATE(RMMIN)
        DEALLOCATE(RMSD)
        DEALLOCATE(RDAY)
        WRITE(6,*) 'Deallocated RDATA'
        WRITE(6,*) 'Last field completed'
        STOP
      ENDIF

      RETURN
      END


      SUBROUTINE DEALLOC(JF)

!  Deallocate and close grib file

      USE IGRIB
      USE RGRIB
      USE RDATA

      IMPLICIT NONE

      INTEGER :: JF

      CALL GRSVCK(0)


!  Check if this is last pass; if so, then exit cleanly

      IF(JF.EQ.NFIELDS) THEN
        DEALLOCATE(RSEC4)
        WRITE(6,*) 'Deallocated RSEC4'
        DEALLOCATE(RMMEAN)
        DEALLOCATE(RMMAX)
        DEALLOCATE(RMMIN)
        DEALLOCATE(RMSD)
        DEALLOCATE(RDAY)
        WRITE(6,*) 'Deallocated RDATA'
        WRITE(6,*) 'Last field completed'
        STOP
      ENDIF

      RETURN
      END

