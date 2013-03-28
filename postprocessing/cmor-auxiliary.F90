MODULE NCMORIO
     CHARACTER(256) :: curr_file ,    inpath,       outpath, &
                       model_varname, cmor_varname, model_units
END MODULE NCMORIO

MODULE NATTRIBUTES
     CHARACTER(256) :: calendar,               comment,        contact,                 &
                       experiment_id,          forcing,        initialization_method,   &
                       institute_id,           institution,    model_id,                &
                       modeling_realm,         original_name,  parent_experiment_id,    &
                       parent_experiment_rip,  positive,       references,              &
                       source,                 table_id
     INTEGER :: realization
     DOUBLE PRECISION :: branch_time
END MODULE NATTRIBUTES

MODULE CMOR_TAMIP_ROUTINES

  USE cmor_users_functions
  USE netcdf
    PRIVATE
      PUBLIC read_nml, read_coords, read_coords_vert, read_1d_coord, read_time, &
      read_2d_input_files_mon, read_2d_input_files_day, read_2d_input_files_6h, &
      read_2d_input_files_3h, handle_err
      CONTAINS
  SUBROUTINE read_nml
    USE NCMORIO
    USE NATTRIBUTES

    IMPLICIT NONE

    INTEGER :: NULNAM

#include "namattributes.h"
#include "namcmorio.h"

    curr_file             = "N/A"
    inpath                = "N/A"
    outpath               = "N/A"

    calendar              = "N/A"
    comment               = "N/A"
    contact               = "N/A"
    experiment_id         = "N/A"
    forcing               = "N/A"
    initialization_method = "N/A"
    institute_id          = "N/A"
    institution           = "N/A"
    model_id              = "N/A"
    modeling_realm        = "N/A"
    parent_experiment_id  = "N/A"
    parent_experiment_rip = "N/A"
    references            = "N/A"
    source                = "N/A"
    table_id              = "N/A"
    realization           = 1
    branch_time           = 0.0

    NULNAM=4

    OPEN(NULNAM, FILE="cmor.nml", DELIM="QUOTE")
    READ(NULNAM, NAMCMORIO)
    CLOSE(NULNAM)

    OPEN(NULNAM, FILE="cmor.nml", DELIM="QUOTE")
    READ(NULNAM, NAMATTRIBUTES)
    CLOSE(NULNAM)

     WRITE(6,*) 'inpath=', trim(inpath)
     WRITE(6,*) 'outpath=', trim(outpath)

    RETURN
  END SUBROUTINE read_nml

  SUBROUTINE read_1d_coord(ncid, rhVarId, coord_name, coord_array, coord_array_bounds)

    USE netcdf

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncid, rhVarId
    CHARACTER(len=*), INTENT(IN) :: coord_name
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:), ALLOCATABLE :: coord_array
    DOUBLE PRECISION, INTENT(OUT), DIMENSION(:, :), ALLOCATABLE :: coord_array_bounds

    INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: dimIDs
    INTEGER :: ndims, numLength, id, i_bounds, status, i
    CHARACTER(LEN=128) :: dim_name

    status = nf90_inquire_variable(ncid, rhVarId, dimids = dimIDs, ndims=ndims)
    if(status /= nf90_NoErr) call handle_err(status, "NF90_INQUIRE_VARIABLE")
    do id=1, ndims
        status = nf90_inquire_dimension(ncid, dimIDs(id), name=dim_name, len = numLength)
        if(status /= nf90_NoErr) call handle_err(status, "NF90_INQUIRE_DIMENSION")
        if (dim_name .eq. coord_name) then
            allocate(coord_array(numLength))
            coord_array = 0.0

            status = nf90_get_var(ncid, id, coord_array)
            if(status /= nf90_NoErr) call handle_err(status, "NF90_GET_VAR")

            allocate(coord_array_bounds(2, numLength))
            coord_array_bounds = 0.0

            if (dim_name .eq. "lat") then
                coord_array_bounds(1, 1) = 90
                coord_array_bounds(2, 1) = coord_array(1) + (coord_array(2)-coord_array(1))/2
                do i_bounds=2, numLength-1
                    coord_array_bounds(1, i_bounds) = coord_array(i_bounds) - (coord_array(i_bounds)-coord_array(i_bounds-1))/2
                    coord_array_bounds(2, i_bounds) = coord_array(i_bounds) + (coord_array(i_bounds+1)-coord_array(i_bounds))/2
                end do
                coord_array_bounds(1, numLength) = coord_array(numLength) - (coord_array(numLength)-coord_array(numLength-1))/2
                coord_array_bounds(2, numLength) = -90
            end if

            if (dim_name .eq. "lon") then
                do i_bounds=1, numLength
                    coord_array_bounds(1, i_bounds) = coord_array(i_bounds) - 180./numLength
                    coord_array_bounds(2, i_bounds) = coord_array(i_bounds) + 180./numLength
                end do
            end if

            if (dim_name .eq. "time") then
                !! Explicit fix to the IFS_LASTOUT = .FALSE. setting,
                !! remove once TAMIP-runs are rerun with IFS_LASTOUT = .TRUE.
                coord_array(numLength) = coord_array(numLength - 1) + 3
                !! End of explicit fix

                coord_array_bounds(1, 1) = coord_array(1) - (coord_array(2)-coord_array(1))/2
                coord_array_bounds(2, 1) = coord_array(1) + (coord_array(2)-coord_array(1))/2
                do i_bounds=2, numLength-1
                    coord_array_bounds(1, i_bounds) = coord_array(i_bounds) - (coord_array(i_bounds)-coord_array(i_bounds-1))/2
                    coord_array_bounds(2, i_bounds) = coord_array(i_bounds) + (coord_array(i_bounds+1)-coord_array(i_bounds))/2
                end do
                coord_array_bounds(1, numLength) = coord_array(numLength) - (coord_array(numLength)-coord_array(numLength-1))/2
                coord_array_bounds(2, numLength) = coord_array(numLength) + (coord_array(numLength)-coord_array(numLength-1))/2
            end if
        end if
    end do
   END SUBROUTINE read_1d_coord

   SUBROUTINE handle_err(status, routine_name)
     IMPLICIT NONE
     integer, intent(in) :: status
     character(len=*), intent(in) :: routine_name

     WRITE(*,'(A,I3)') "status=", status
     WRITE(*,*) trim(nf90_strerror(status))
     CALL ABORT(routine_name)
   END SUBROUTINE handle_err
!!
!!
!!     IMPLICIT NONE
!!
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alats
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: alons
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plevs
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plev8
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: plev3
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lat
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: bnds_lon
!!     DOUBLE PRECISION, DIMENSION(SIZE(alats)) :: lats
!!
!!     INTEGER :: i
!!
!! 101 FORMAT(I5,3F10.5)
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, alons(i), bnds_lon(1,i), bnds_lon(2,i)'
!!     do i=1,SIZE(alons)
!!        alons(i)=(i-1)*360./SIZE(alons)
!!        bnds_lon(1,i) = alons(i) - 180./SIZE(alons)
!!        bnds_lon(2,i) = alons(i) + 180./SIZE(alons)
!!        IF (NPRINTLEV.GT.0) WRITE(6,101) i, alons(i), bnds_lon(1,i), bnds_lon(2,i)
!!     enddo
!!
!! 100 FORMAT(50X,F9.5)
!!     OPEN(1,FILE="N80Gaussian",FORM='formatted')
!!     do i=1,4
!!       READ(1,*)
!!     enddo
!!     do i=1,SIZE(alats)
!!       READ(1,100) lats(i)
!! ! reorder from S to N
!! !      alats(SIZE(alats)-i+1)=lats(i)
!! ! reordering not needed, done authomatically by CMOR
!!       alats(i)=lats(i)
!!     enddo
!!     CLOSE(1)
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, alats(i), bnds_lat(1,i), bnds_lat(2,i)'
!!     do i=1,SIZE(alats)
!!       if (i.eq.1) then
!! !        bnds_lat(1,i)=-90. ! when reordering from S to N
!!         bnds_lat(1,i)=90.
!!       else
!!         bnds_lat(1,i)=alats(i)-(alats(i)-alats(i-1))/2
!!       endif
!!       if (i.eq.SIZE(alats)) then
!! !        bnds_lat(2,i)=90. ! when reordering from S to N
!!         bnds_lat(2,i)=-90.
!!       else
!!         bnds_lat(2,i)=alats(i)+(alats(i+1)-alats(i))/2
!!       endif
!!       IF (NPRINTLEV.GT.0) WRITE(6,101) i, alats(i), bnds_lat(1,i), bnds_lat(2,i)
!!     enddo
!!
!! 102 FORMAT(I5,F20.5)
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, plevs(i)'
!!     plevs = (/100000., 92500., 85000., 70000.,&
!!      60000., 50000., 40000., 30000., 25000., 20000.,&
!!      15000., 10000., 7000., 5000., 3000., 2000. /)
!!     IF (NPRINTLEV.GT.0) THEN
!!       do i = 1, SIZE(plevs)
!!         WRITE(6,102) i, plevs(i)
!!       enddo
!!     ENDIF
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, plev8(i)'
!!     plev8 = (/100000., 85000., 70000.,&
!!      50000., 25000., 10000., 5000. /)
!!     IF (NPRINTLEV.GT.0) THEN
!!       do i = 1, SIZE(plev8)
!!         WRITE(6,102) i, plev8(i)
!!       enddo
!!     ENDIF
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, plev3(i)'
!!     plev3 = (/85000., 50000., 25000. /)
!!     IF (NPRINTLEV.GT.0) THEN
!!       do i = 1, SIZE(plev3)
!!         WRITE(6,102) i, plev3(i)
!!       enddo
!!     ENDIF
!!
!!     RETURN
!!   END SUBROUTINE read_coords
!!
!!   SUBROUTINE read_coords_vert(zlevs, a_coeff, b_coeff, zlev_bnds, a_coeff_bnds, b_coeff_bnds, p0)
!!
!!
!!     IMPLICIT NONE
!!
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: zlevs
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: zlev_bnds
!!     REAL, INTENT(OUT), DIMENSION(:) :: a_coeff
!!     REAL, INTENT(OUT), DIMENSION(:) :: a_coeff_bnds
!!     REAL, INTENT(OUT), DIMENSION(:) :: b_coeff
!!     REAL, INTENT(OUT), DIMENSION(:) :: b_coeff_bnds
!!     REAL, INTENT(OUT) :: p0
!!
!!     DOUBLE PRECISION, DIMENSION(SIZE(zlev_bnds)) :: vetaf
!!
!!     INTEGER :: i
!!
!!     p0 = 101325.
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'p0=',p0
!!
!! 200 FORMAT(18X,F13.10,F25.10)
!! 201 FORMAT(I5,3F15.10)
!!     OPEN(2,FILE="A_B_full_lev",FORM='formatted')
!!     do i=1,2
!!       READ(2,*)
!!     enddo
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, a_coeff(i), b_coeff(i), zlevs(i)'
!!     do i=1,SIZE(zlevs)
!!       READ(2,200) b_coeff(i), a_coeff(i)
!!       a_coeff(i)=a_coeff(i)/p0
!!       zlevs(i)=a_coeff(i)+b_coeff(i)
!!       IF (NPRINTLEV.GT.0) WRITE(6,201) i, a_coeff(i), b_coeff(i), zlevs(i)
!!     enddo
!!     CLOSE(2)
!!
!! 300 FORMAT(31X,F25.10,F25.10)
!! 301 FORMAT(I5,4F15.10)
!!     OPEN(3,FILE="A_B_half_lev",FORM='formatted')
!!     do i=1,2
!!       READ(3,*)
!!     enddo
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i, a_coeff_bnds(i), b_coeff_bnds(i), zlev_bnds(i), vetaf(i)'
!!     do i=1,SIZE(zlev_bnds)
!!       READ(3,300) b_coeff_bnds(i), a_coeff_bnds(i)
!!       a_coeff_bnds(i)=a_coeff_bnds(i)/p0
!!       zlev_bnds(i)=a_coeff_bnds(i)+b_coeff_bnds(i)
!!       if (i.eq.1) then
!!         vetaf(i)=0.
!!       else
!!         vetaf(i)=(zlev_bnds(i)+zlev_bnds(i-1))*0.5
!!       endif
!!       IF (NPRINTLEV.GT.0) WRITE(6,301) i, a_coeff_bnds(i), b_coeff_bnds(i), zlev_bnds(i), vetaf(i)
!!     enddo
!!     CLOSE(3)
!!
!!     RETURN
!!   END SUBROUTINE read_coords_vert
!!
!!    SUBROUTINE read_time(time_mon, time_mon_bnds, time_day, time_day_bnds, time_6h, time_6h_bnds, time1_6h, time_3h, time_3h_bnds, time1_3h)
!!
!! !  Original version: S. Stefanescu, ECMWF 2.12.2011
!! !  Modifications: Bugfixes for computation of NTOTDAYS: J. von Hardenberg, ISAC-CNR, 5.12.2011
!!
!!
!!     IMPLICIT NONE
!!
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_mon
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_mon_bnds
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_day
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_day_bnds
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_6h
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_6h_bnds
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time1_6h
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time_3h
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:,:) :: time_3h_bnds
!!     DOUBLE PRECISION, INTENT(OUT), DIMENSION(:) :: time1_3h
!!
!!     INTEGER :: iyr, imo, iday, i6h, i3h
!!     INTEGER :: NTOTDAYS
!!     INTEGER, DIMENSION(NYEARS) :: NYRDAYS
!!     INTEGER, DIMENSION(12) :: NMODAYS
!!
!!     NMODAYS(1)=31
!!     NMODAYS(3)=31
!!     NMODAYS(4)=30
!!     NMODAYS(5)=31
!!     NMODAYS(6)=30
!!     NMODAYS(7)=31
!!     NMODAYS(8)=31
!!     NMODAYS(9)=30
!!     NMODAYS(10)=31
!!     NMODAYS(11)=30
!!     NMODAYS(12)=31
!!
!!     NTOTDAYS=0
!!     DO iyr=NYEAR0,NYEAR
!!       IF ((MOD(iyr,400).EQ.0).OR.((MOD(iyr,100).NE.0).AND.(MOD(iyr,4).EQ.0))) THEN
!!         NYRDAYS(iyr-NYEAR0+1)=366
!!         IF (iyr.EQ.NYEAR) NMODAYS(2)=29
!!       ELSE
!!         NYRDAYS(iyr-NYEAR0+1)=365
!!         IF (iyr.EQ.NYEAR) NMODAYS(2)=28
!!       ENDIF
!!       IF (iyr.LT.NYEAR) NTOTDAYS=NTOTDAYS+NYRDAYS(iyr-NYEAR0+1)
!!     ENDDO
!!
!!     IF ((NMONTH.EQ.2).AND.(NMODAYS(2).NE.NMDAYS)) THEN
!!       WRITE(6,*) 'STOP !!! different number of days for February !!!'
!!       WRITE(6,*) 'NMODAYS(2)=',NMODAYS(2),'NMDAYS=',NMDAYS
!!       STOP
!!     ENDIF
!!
!!     DO imo=1,NMONTH
!!       IF (imo.LT.NMONTH) NTOTDAYS=NTOTDAYS+NMODAYS(imo)
!!     ENDDO
!!
!!     WRITE(6,*)
!!     WRITE(6,*) 'Number of days passed since',NYEAR0,'until year',NYEAR,'beginning of month',NMONTH,'is NTOTDAYS=',NTOTDAYS
!!     WRITE(6,*)
!!
!! 401 FORMAT(3F20.5)
!! 402 FORMAT(I5,3F15.5)
!! 403 FORMAT(I5,4F15.5)
!!
!!     time_mon = REAL(NTOTDAYS) + REAL(NMODAYS(NMONTH))/2
!!     time_mon_bnds(1,1)= REAL(NTOTDAYS)
!!     time_mon_bnds(2,1)= REAL(NTOTDAYS) + REAL(NMODAYS(NMONTH))
!!
!!     WRITE(6,*) '           time_mon  time_mon_bnds(1,1)  time_mon_bnds(2,1)'
!!     WRITE(6,401) time_mon, time_mon_bnds(1,1), time_mon_bnds(2,1)
!!     WRITE(6,*)
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'iday, time_day(iday), time_day_bnds(1,iday), time_day_bnds(2,iday)'
!! !    DO iday=1,NMODAYS(NMONTH)
!!     DO iday=1,SIZE(time_day)
!!      time_day(iday) = REAL(NTOTDAYS) + REAL(iday) - 0.5
!!      time_day_bnds(1,iday) = REAL(NTOTDAYS) + REAL(iday) - 1.
!!      time_day_bnds(2,iday) = REAL(NTOTDAYS) + REAL(iday)
!!      IF (NPRINTLEV.GT.0) WRITE(6,402) iday, time_day(iday), time_day_bnds(1,iday), time_day_bnds(2,iday)
!!     ENDDO
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i6h, time1_6h(i6h), time_6h(i6h), time_6h_bnds(1,i6h), time_6h_bnds(2,i6h)'
!! !    DO i6h=1,NMODAYS(NMONTH)*4
!!     DO i6h=1,SIZE(time_6h)
!!      time_6h(i6h) = REAL(NTOTDAYS) + i6h*0.25 - 0.125
!!      time_6h_bnds(1,i6h) = REAL(NTOTDAYS) + (i6h-1)*0.25
!!      time_6h_bnds(2,i6h) = REAL(NTOTDAYS) + i6h*0.25
!!      time1_6h(i6h) = REAL(NTOTDAYS) + i6h*0.25
!!      IF (NPRINTLEV.GT.0) WRITE(6,403) i6h, time1_6h(i6h), time_6h(i6h), time_6h_bnds(1,i6h), time_6h_bnds(2,i6h)
!!     ENDDO
!!
!!     IF (NPRINTLEV.GT.0) WRITE(6,*) 'i3h, time1_3h(i3h), time_3h(i3h), time_3h_bnds(1,i3h), time_3h_bnds(2,i3h)'
!! !    DO i3h=1,NMODAYS(NMONTH)*8
!!     DO i3h=1,SIZE(time_3h)
!!       time_3h(i3h) = REAL(NTOTDAYS) + i3h*0.125 - 0.0625
!!       time_3h_bnds(1,i3h) = REAL(NTOTDAYS) + (i3h-1)*0.125
!!       time_3h_bnds(2,i3h) = REAL(NTOTDAYS) + i3h*0.125
!!       time1_3h(i3h) = REAL(NTOTDAYS) + i3h*0.125
!!       IF (NPRINTLEV.GT.0) WRITE(6,403) i3h, time1_3h(i3h), time_3h(i3h), time_3h_bnds(1,i3h), time_3h_bnds(2,i3h)
!!     ENDDO
!!
!!     RETURN
!!   END SUBROUTINE read_time
!!









!!
!!
!!
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
