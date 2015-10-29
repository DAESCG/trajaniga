!###############################################################################
! global_mod.f90
! Author: Matthew Janiga (matthew.janiga@gmail.com)
! Last Updated: Mar. 25, 2011
!
! Description:  Global variables available to the individual stages of the
! program.
!                                                                              
!###############################################################################

!###########################################################################
! mod_fileio                                                            ####
! Variables which contain the names of the netCDF files used by simtraj ####
!###########################################################################

MODULE mod_fileio
  IMPLICIT NONE
  SAVE

  INTEGER :: fi_nml = 3             ! NAMELIST file index
  CHARACTER(LEN = 200) :: paramfile  ! Name of NAMELIST parameter file
  CHARACTER(LEN = 200) :: gridfile   ! Contains grids for computing kinematic
  CHARACTER(LEN = 200) :: auxfile    ! Contains grids for computing auxiliary
  CHARACTER(LEN = 200) :: trajfile   ! Contains the initial coordinates of the
  CHARACTER(LEN = 200) :: outfile    ! Output file for trajectories.

  INTEGER :: u_varid, v_varid, w_varid, sp_varid     ! NetCDF ID of grid variables
  INTEGER, ALLOCATABLE, DIMENSION(:) :: aux_varid    ! NetCDF ID of auxiliary variables
  INTEGER :: gncid                                   ! NetCDF ID for grid file
  INTEGER :: ancid                                   ! NetCDF ID for auxiliary file

END MODULE mod_fileio

!###########################################################################
! mod_grid                                                              ####
! Grid information                                                      ####
!###########################################################################

MODULE mod_grid
  IMPLICIT NONE
  SAVE

  !#### Names of variables stored in netCDF files #####
  CHARACTER (len = *), PARAMETER :: U_NAME = "u"
  CHARACTER (len = *), PARAMETER :: V_NAME = "v"
  CHARACTER (len = *), PARAMETER :: W_NAME = "w"
  CHARACTER (len = *), PARAMETER :: SP_NAME = "sp"
  CHARACTER (len = *), PARAMETER :: LEV_NAME = "lev"
  CHARACTER (len = *), PARAMETER :: LAT_NAME = "lat"
  CHARACTER (len = *), PARAMETER :: LON_NAME = "lon"
  CHARACTER (len = *), PARAMETER :: TIME_NAME = "time"
  CHARACTER (len = *), PARAMETER :: LAT_UNITS = "degrees_north"
  CHARACTER (len = *), PARAMETER :: LON_UNITS = "degrees_east"
  CHARACTER (len = *), PARAMETER :: TRAJ_NAME = "traj"
  CHARACTER(LEN = 80), ALLOCATABLE, DIMENSION(:) :: aux_name  

  !#### Grid domain information #####

  INTEGER :: gntime, gnlev, gnlat, gnlon
  REAL, ALLOCATABLE, DIMENSION(:) :: gtime
  REAL, ALLOCATABLE, DIMENSION(:) :: glev
  REAL, ALLOCATABLE, DIMENSION(:) :: glat
  REAL, ALLOCATABLE, DIMENSION(:) :: glon
  REAL :: dtime, dhor

  !#### Auxiliary domain information #####
  INTEGER :: naux                                   ! Number auxiliary variables
  INTEGER, ALLOCATABLE, DIMENSION(:) :: aux_dim     ! 2D (xyt) or 3D (xyzt)

END MODULE mod_grid

!###########################################################################
! mod_traj                                                              ####
! Stores the trajectories in all their glory!                           ####
!###########################################################################

MODULE mod_traj
  IMPLICIT NONE
  SAVE

  !#### Constants #####
  REAL, PARAMETER :: mv = 99999.9     ! Missing value for trajectories
  INTEGER, PARAMETER :: mvi = -999    ! Missing value for elements

  !#### Number of trajectories #####
  INTEGER :: ntraj  

  !#### Values for time stepping ########
  INTEGER :: time_step  ! Time step (s)
  INTEGER :: nstep      ! Number of time steps
  INTEGER :: c_step     ! Current time step

  !#### Inital Values ####
  REAL, ALLOCATABLE, DIMENSION(:) :: i_time
  REAL, ALLOCATABLE, DIMENSION(:) :: i_lev
  REAL, ALLOCATABLE, DIMENSION(:) :: i_lat
  REAL, ALLOCATABLE, DIMENSION(:) :: i_lon

  !#### Values Over Time #####

  ! Stored (traj, step)
  
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: t_on    ! On/Off switch
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_time   ! Time (s relative to t0)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_lev    ! Pressure (Pa)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_lat    ! Latitude (degrees north)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_lon    ! Longitude (degrees east)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_u      ! Zonal Wind (m/s)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_v      ! Meridional Wind (m/s)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_w      ! Vertical Velocity (Pa/s)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: t_sp     ! Surface Pressure (Pa)

  ! Stored (traj, step, var#)

  REAL, ALLOCATABLE, DIMENSION(:,:,:) :: t_aux  ! Auxiliary Variables

  !#### Values for Trajectory #####

  INTEGER, ALLOCATABLE, DIMENSION(:) :: last_valid  ! End of trajectory

END MODULE mod_traj

