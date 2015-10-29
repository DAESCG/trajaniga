!###############################################################################
!  simtraj.f90
!  Author: Matthew Janiga (matthew.janiga@gmail.com)
!  Last Updated: Mar. 25, 2011
!
!  Description: This is the program main it calls all the different subprograms.
!###############################################################################

PROGRAM simtraj
  USE mod_namelist     ! Responsible for reading in program settings from NAMELIST.
  USE mod_checkinp     ! Responsible for input file checks and dimension queries.
  USE mod_calctraj     ! Responsible for trajectory calculation.
  USE mod_writetraj    ! Responsible for writing trajectories to file.
  IMPLICIT NONE
 
  !########################################################################
  ! 1.) Get information from NAMELIST file and store in global modules. ###
  !########################################################################

  CALL read_namelist()    
  PRINT *, '1.) Values in NAMELIST file read succesfully.'

  !###################################################################################
  ! 2.) Query the gridfile, auxfile, and trajfile for internal consitentcy,       ####
  ! correct formating, and to determine settings for the trajectory calculations. ####
  !###################################################################################

  CALL checkinp()    
  PRINT *, '2.) Input netCDF files have passed internal consitency and formatting checks.'
  PRINT *, '    Grid spacing and domain sizes have been stored in memory.'

  !###################################################################################
  ! 3.) Perform the trajectory calculations
  !###################################################################################

  CALL calctraj()    
  PRINT *, '3.) Trajectory calculation completed.'

  !###################################################################################
  ! 4.) Write the trajectories to netCDF file.
  !###################################################################################

  CALL writetraj()    
  PRINT *, '4.) Trajectory calculations outputed to netCDF file.'

END PROGRAM simtraj
