!###############################################################################
! checkinp.f90
! Author: Matthew Janiga (matthew.janiga@gmail.com)
! Last Updated: Mar. 25, 2011
!
! Description:  Function calls individual subroutines which check the gridfile,
! auxfile, and trajefile for correct formatting and consitency.                                                                           
!
!###############################################################################

!############## TO DO LIST ###########################
! Nothing at this time.

!###########################################################################
! checkinp                                                              ####
! Contains subroutines which perform checks on the input netCDF files   ####
!###########################################################################

MODULE mod_checkinp
  USE mod_fileio
  USE mod_grid
  USE netcdf
  USE mod_traj
  IMPLICIT NONE

CONTAINS

  !###########################################################################
  ! checkinp()                                                            ####
  ! Calls individual subroutines which perform checks on the individual   ####
  ! netCDF files.                                                         ####
  !###########################################################################

  SUBROUTINE checkinp()
    IMPLICIT NONE

    ! Check the gridfile
    CALL check_gridfile()

    ! Check auxiliary file if there are auxiliary variables
    IF( naux >= 1) THEN
       CALL check_auxfile()
    END IF

    ! Check the trajfile
    CALL check_trajfile()

  END SUBROUTINE checkinp

  !###########################################################################
  ! check()                                                               ####
  ! Checks if error is returned on netCDF call.                           ####
  !###########################################################################

  SUBROUTINE check(status)
    INTEGER, INTENT (in) :: status
    
    IF(status /= nf90_noerr) THEN
      PRINT *, TRIM(nf90_strerror(status))
      STOP
    END IF
  END SUBROUTINE check

  !###########################################################################
  ! check_all()                                                           ####
  ! Checks if error is returned on netCDF call.                           ####
  !###########################################################################

  SUBROUTINE check_all(status)
    INTEGER, INTENT (in) :: status
    
    IF(status /= 0) THEN
      PRINT *, "Not enough space to allocate array."
      STOP
    END IF
  END SUBROUTINE check_all

  !###########################################################################
  ! check_trajfile()                                                      #### 
  ! Examines the trajfile looking for: number of trajectories, getting    ####
  ! the initial conditions, and initializing trajectory arrays.           ####
  !###########################################################################

  SUBROUTINE check_trajfile()
    IMPLICIT NONE

    ! ID numbers

    INTEGER :: ncid, error
    INTEGER :: lev_dimid, lon_dimid, lat_dimid, time_dimid, traj_dimid
    INTEGER :: lev_varid, lon_varid, lat_varid, time_varid, traj_varid

    INTEGER, PARAMETER :: NDIMS = 1

    ! The start and count arrays will tell the netCDF library where to
    ! read our data.

    INTEGER :: start(NDIMS), count(NDIMS)

    ! For loops
    INTEGER :: i

    ! Open the file. 
    CALL check( nf90_open( trajfile, nf90_nowrite, ncid) )

    ! Get the variable ids of the coordinate variable traj.
    CALL check( nf90_inq_varid( ncid, TRAJ_NAME, traj_varid) )

    ! Get the variable ids of the inital point arrays.
    CALL check( nf90_inq_varid( ncid, TIME_NAME, time_varid) )
    CALL check( nf90_inq_varid( ncid, LEV_NAME, lev_varid) )
    CALL check( nf90_inq_varid( ncid, LAT_NAME, lat_varid) )
    CALL check( nf90_inq_varid( ncid, LON_NAME, lon_varid) )

    ! Get the dimension ids of the coordinate variables.
    CALL check( nf90_inq_dimid( ncid, TRAJ_NAME, traj_dimid) )

    ! Get the dimensions of the coordinate variable traj.
    CALL check( nf90_inquire_dimension( ncid, traj_dimid, len = ntraj) )

    PRINT *, 'Num trajectories: ', ntraj

    ! Allocate space for initial condition arrays.
    ALLOCATE(i_time(ntraj))
    ALLOCATE(i_lev(ntraj))
    ALLOCATE(i_lat(ntraj))
    ALLOCATE(i_lon(ntraj))

    ! Allocate space for last_traj
    ALLOCATE(last_valid(ntraj))

    ! Allocate space for trajectory on/off switches
    ALLOCATE(t_on(ntraj))

    ! Read in initial condition arrays
    CALL check( nf90_get_var(ncid, time_varid, i_time) )
    CALL check( nf90_get_var(ncid, lev_varid, i_lev) )
    CALL check( nf90_get_var(ncid, lat_varid, i_lat) )
    CALL check( nf90_get_var(ncid, lon_varid, i_lon) )

    ! Close the file
    CALL check( nf90_close( ncid) )

 !   PRINT *, "Initial times follow...."
 !   DO i=1,ntraj
 !      PRINT *, i, i_time(i)
 !   END DO

 !   PRINT *, "Initial pressures follow...."
 !   DO i=1,ntraj
 !      PRINT *, i, i_lev(i)
 !   END DO

 !   PRINT *, "Initial lats follow...."
 !   DO i=1,ntraj
 !      PRINT *, i, i_lat(i)
 !   END DO

 !   PRINT *, "Initial lons follow...."
 !   DO i=1,ntraj
 !      PRINT *, i, i_lon(i)
 !   END DO


    ! Allocate space for trajectories.
    ! Because these arrays can be quite large we need to check if there is enough memory 
    ALLOCATE(t_time(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_lev(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_lat(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_lon(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_u(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_v(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_w(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_sp(ntraj,nstep), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF
    ALLOCATE(t_aux(ntraj,nstep,naux), STAT=error)
    IF (error /= 0) THEN 
       PRINT *, 'FATAL: Not enough memory to allocate array.'
       STOP
    END IF

    PRINT *, "Memory allocated for trajectories successfully."

    !########### Check if initial positions are in domain ##########
    ! Trajectories with initial points in domain are turned on and 
    ! those that are not are turned off.

    ! Turn on all trajectories
    t_on = .TRUE.

    PRINT *, "Switches on...."
 !   DO i=1,ntraj
 !      PRINT *, i, t_on(i)
!    END DO

    ! Check time
    DO i=1,ntraj
       IF( i_time(i) < gtime(1) .OR.  i_time(i) > gtime(gntime) )THEN
          t_on(i) = .FALSE.
          last_valid(i) = mv
          PRINT *, 'Trajectory out of bounds (time): ',i
       END IF
    END DO

    ! Check lev
    DO i=1,ntraj
       IF( i_lev(i) > glev(1) .OR.  i_lev(i) < glev(gnlev) )THEN
          t_on(i) = .FALSE.
          last_valid(i) = mv
          PRINT *, 'Trajectory out of bounds (lev): ',i
       END IF
    END DO

    ! Check lat
    DO i=1,ntraj
       IF( i_lat(i) < glat(1) .OR.  i_lat(i) > glat(gnlat) )THEN
          t_on(i) = .FALSE.
          last_valid(i) = mv
          PRINT *, 'Trajectory out of bounds (lat): ',i
       END IF
    END DO

    ! Check lon
    DO i=1,ntraj
       IF( i_lon(i) < glon(1) .OR.  i_lon(i) > glon(gnlon) )THEN
          t_on(i) = .FALSE.
          last_valid(i) = mv
          PRINT *, 'Trajectory out of bounds (lon): ',i
       END IF
    END DO

!    DO i=1,ntraj
!       PRINT *, i, t_on(i)
!    END DO
!    PRINT *, "Switches checked...."

  END SUBROUTINE check_trajfile

  !###########################################################################
  ! check_auxfile()                                                       #### 
  ! Examines the auxiliary file and does quick checks for consitency      ####
  ! with the gridfile.                                                    ####
  !###########################################################################

  SUBROUTINE check_auxfile()
    IMPLICIT NONE

    ! ID numbers

    INTEGER :: ncid
    INTEGER :: lev_dimid, lon_dimid, lat_dimid, time_dimid
    INTEGER :: lev_varid, lon_varid, lat_varid, time_varid

    INTEGER, PARAMETER :: NDIMS = 4

    ! The start and count arrays will tell the netCDF library where to
    ! read our data.

    INTEGER :: start(NDIMS), COUNT(NDIMS)

    ! For loops
    INTEGER :: i

    ! Open the file. 
    CALL check( nf90_open( auxfile, nf90_nowrite, ncid) )

    ! Get the variable ids of the coordinate variables.
    CALL check( nf90_inq_varid( ncid, TIME_NAME, time_varid) )
    CALL check( nf90_inq_varid( ncid, LEV_NAME, lev_varid) )
    CALL check( nf90_inq_varid( ncid, LAT_NAME, lat_varid) )
    CALL check( nf90_inq_varid( ncid, LON_NAME, lon_varid) )

    ! Get the variable ids of the auxiliary variables.
    DO i=1,naux
       CALL check( nf90_inq_varid( ncid, aux_name(i), aux_varid(i)) )
       PRINT *, aux_name(i)
       PRINT *, aux_varid(i)
    END DO

    ! Close the file
    CALL check( nf90_close( ncid) )

  END SUBROUTINE check_auxfile

  !###########################################################################
  ! check_gridfile()                                                      ####
  ! Stores the dimensions of the gridfile and performs checks to see if   ####
  ! the data is correctly formated.                                       ####
  !###########################################################################

  SUBROUTINE check_gridfile()
    IMPLICIT NONE

    ! ID numbers

    INTEGER :: ncid
    INTEGER :: lev_dimid, lon_dimid, lat_dimid, time_dimid
    INTEGER :: lev_varid, lon_varid, lat_varid, time_varid

    ! Number of dimensions
    INTEGER, PARAMETER :: NDIMS = 4

    ! The start and count arrays will tell the netCDF library where to
    ! read our data.

    INTEGER :: start(NDIMS), COUNT(NDIMS)

    ! For loops 

    INTEGER :: i

    ! Horizontal spacing

    REAL :: dlat, dlon

    ! Open the file. 
    CALL check( nf90_open(gridfile, nf90_nowrite, ncid) )
 
    ! Get the variable ids of the coordinate variables.
    CALL check( nf90_inq_varid( ncid, TIME_NAME, time_varid) )
    CALL check( nf90_inq_varid( ncid, LEV_NAME, lev_varid) )
    CALL check( nf90_inq_varid( ncid, LAT_NAME, lat_varid) )
    CALL check( nf90_inq_varid( ncid, LON_NAME, lon_varid) )

    PRINT *, "TIME variable ID: ", time_varid
    PRINT *, "LEV variable ID: ", lev_varid
    PRINT *, "LAT variable ID: ", lat_varid
    PRINT *, "LON variable ID: ", lon_varid

    ! Get the variable ids of the 4D variables.
    CALL check( nf90_inq_varid( ncid, U_NAME, u_varid) )
    CALL check( nf90_inq_varid( ncid, V_NAME, v_varid) )
    CALL check( nf90_inq_varid( ncid, W_NAME, w_varid) )
    CALL check( nf90_inq_varid( ncid, SP_NAME, sp_varid) )

    PRINT *, "U variable ID: ", u_varid
    PRINT *, "V variable ID: ", v_varid
    PRINT *, "W variable ID: ", w_varid
    PRINT *, "SP variable ID: ", sp_varid

    ! Get the dimension ids of the coordinate variables.
    CALL check( nf90_inq_dimid( ncid, TIME_NAME, time_dimid) )
    CALL check( nf90_inq_dimid( ncid, LEV_NAME, lev_dimid) )
    CALL check( nf90_inq_dimid( ncid, LAT_NAME, lat_dimid) )
    CALL check( nf90_inq_dimid( ncid, LON_NAME, lon_dimid) )

    PRINT *, "TIME ID: ", time_dimid
    PRINT *, "LEV ID: ", lev_dimid
    PRINT *, "LAT ID: ", lat_dimid
    PRINT *, "LON ID: ", lon_dimid

    ! Get the dimensions of the coordinate variables.
    CALL check( nf90_inquire_dimension( ncid, time_dimid, len = gntime) )
    CALL check( nf90_inquire_dimension( ncid, lev_dimid, len = gnlev) )
    CALL check( nf90_inquire_dimension( ncid, lat_dimid, len = gnlat) )
    CALL check( nf90_inquire_dimension( ncid, lon_dimid, len = gnlon) )

    PRINT *, "GRID NTIMES: ", gntime
    PRINT *, "GRID NLEVS: ", gnlev
    PRINT *, "GRID NLATS: ", gnlat
    PRINT *, "GRID NLONS: ", gnlon

    ! Allocate sizes to stored grid dimensions
    ALLOCATE(gtime(gntime))
    ALLOCATE(glev(gnlev))
    ALLOCATE(glat(gnlat))
    ALLOCATE(glon(gnlon))

    ! Perform basic checks on dimensions
    IF (gntime <= 3 .OR. gnlev <= 3 .OR. gnlat <= 5 .OR. gnlon <= 5) THEN
       PRINT *, "FATAL ERROR: Dimensions do not meet minimum sizes."
       STOP
    END IF

    ! Read in dimensions.
    call check( nf90_get_var(ncid, time_varid, gtime) )
    call check( nf90_get_var(ncid, lev_varid, glev) )
    call check( nf90_get_var(ncid, lat_varid, glat) )
    call check( nf90_get_var(ncid, lon_varid, glon) )

    ! Close the file. This frees up any internal netCDF resources           
    ! associated with the file.                                         
    CALL check( nf90_close(ncid) )

    ! Test print of dimensions
!    PRINT *, "Time array follows...."
!    DO i=1,gntime
!       PRINT *, i, gtime(i)
!    END DO

!    PRINT *, "Lev array follows...."
!    DO i=1,gnlev
!       PRINT *, i, glev(i)
!    END DO

!    PRINT *, "Lat array follows...."
!    DO i=1,gnlat
!       PRINT *, i, glat(i)
!    END DO

!    PRINT *, "Lon array follows...."
!    DO i=1,gnlon
!       PRINT *, i, glon(i)
!    END DO

    ! Check if time is continuously increasing and evenly spaced
    dtime = gtime(2)-gtime(1)
    DO i=1,gntime-1
       IF (gtime(i) >= gtime(i+1)) THEN
         PRINT *, "FATAL ERROR: Time is not continously increasing"
         STOP
       END IF
       IF ( (gtime(i+1)-gtime(i)) /= dtime) THEN
         PRINT *, "FATAL ERROR: Time is not evenly spaced"
         STOP
       END IF
    END DO

    ! Check if lev is continuously decreasing
    DO i=1,gnlev-1
       IF (glev(i) <= glev(i+1)) THEN
         PRINT *, "FATAL ERROR: Lev is not continously increasing"
         STOP
       END IF
    END DO

    ! Check if lat and lon are continuously increasing and evenly spaced with equal resolution
    dlat = glat(2)-glat(1)
    DO i=1,gnlat-1
       IF (glat(i) >= glat(i+1)) THEN
         PRINT *, "FATAL ERROR: Lat is not continously increasing"
         STOP
       END IF
       IF ( (glat(i+1)-glat(i)) /= dlat) THEN
         PRINT *, "CAUTION: Lat is not evenly spaced"
         !STOP
       END IF
    END DO

    dlon = glon(2)-glon(1)
    DO i=1,gnlon-1
       IF (glon(i) >= glon(i+1)) THEN
         PRINT *, "FATAL ERROR: Lon is not continously increasing"
         STOP
       END IF
       IF ( (glon(i+1)-glon(i)) /= dlon) THEN
         PRINT *, "CAUTION: Lon is not evenly spaced"
        ! STOP
       END IF
    END DO

    IF (dlon /= dlat) THEN
       PRINT *, "CAUTION: The resolution of lat and lon are not the same"
      ! STOP
    ELSE
       dhor = dlat
    END IF

    PRINT *, "All gridfile checks have been sucessfully completed"

  END SUBROUTINE check_gridfile

END MODULE mod_checkinp
