!###############################################################################
! read_namelist.f90
! Author: Matthew Janiga (matthew.janiga@gmail.com)
! Last Updated: Mar. 25, 2011
!
! Description:  Function reads in program settings from a NAMELIST file. These are 
! then stored in global modules for future use.
!                                                                              
!###############################################################################

MODULE mod_namelist
  USE mod_fileio
  USE mod_traj
  USE mod_grid
  IMPLICIT NONE

CONTAINS

!###########################################################################
! read_namelist()                                                       ####
! Reads values from the namelist file to determine program settings     ####
!###########################################################################

  SUBROUTINE read_namelist()
    IMPLICIT NONE

    !#####################
    ! Local variables ####
    !#####################

    LOGICAL :: paramfile_exist        ! Existence of the netCDF files.
    LOGICAL :: gridfile_exist        
    LOGICAL :: auxfile_exist         
    LOGICAL :: trajfile_exist        

    INTEGER, DIMENSION(50) :: aux_dim_t                ! Temporary arrays for aux stuff
    CHARACTER(LEN = 80), DIMENSION(50) :: aux_name_t   ! it goes to the allocatable arrays.

    INTEGER :: i

    !##############################################
    ! Use NAMELIST to grab program settings. ######
    !##############################################

    NAMELIST /PARAMS/ gridfile,auxfile,trajfile,outfile,naux,aux_dim_t,aux_name_t,nstep,time_step

    !############################################################################
    ! Get the argument 'paramfile.nml' which specifies the location of the ######
    ! NAMELIST file. The NAMELIST file contains program settings. Multiple ######
    ! NAMELIST files are not supported. Lastly, open and read paramfile    ######
    !############################################################################

    IF (iargc() == 1) THEN
       ! Retrive argument paramfile
       CALL getarg(1,paramfile)

       ! Inquire if paramfile exists
       INQUIRE(FILE=paramfile,EXIST=paramfile_exist)
       IF (paramfile_exist) THEN
          PRINT *, 'FILE PRESENT: ',paramfile
       ELSE
          PRINT *, 'FATAL ERROR'
          PRINT *, 'FILE NOT PRESENT: ',paramfile
          STOP
       END IF
    ELSE
       PRINT *, 'FATAL ERROR:      multiple arguments'
       PRINT *, 'CORRECT USAGE:    simtraj namelist.nml'
       STOP
    END IF

    ! Grab the variable values from paramfile.nml.

    OPEN(fi_nml,FILE=paramfile)
    READ(fi_nml,NML=PARAMS)

    !####################################################################
    ! Allocate sizes to the aux variables using the temporary ones ######
    !####################################################################

    ! Temporary arrays are used since NAMELIST can't pass values to
    ! an allocatable array. So aux_dim and aux_name are handled as so.

    IF( naux > 1 )THEN
       ALLOCATE(aux_dim(naux))
       ALLOCATE(aux_name(naux))
       ALLOCATE(aux_varid(naux))

       aux_dim(1:naux) = aux_dim_t(1:naux)
       aux_name(1:naux) = aux_name_t(1:naux)

       PRINT *, "Num auxiliary vars: ", naux

       DO i=1,naux
          PRINT *, 'Var: ',aux_name(i)
          PRINT *, 'Dimension: ',aux_dim(i)
       END DO
    END IF

    ! Inquire if gridfile exists
    INQUIRE(FILE=gridfile,EXIST=gridfile_exist)
    IF (gridfile_exist) THEN
       PRINT *, 'FILE PRESENT: ',gridfile
    ELSE
       PRINT *, 'FATAL ERROR'
       PRINT *, 'FILE NOT PRESENT: ',gridfile
       STOP
    END IF

    ! Inquire if auxfile exists
    INQUIRE(FILE=auxfile,EXIST=auxfile_exist)
    IF (auxfile_exist) THEN
       PRINT *, 'FILE PRESENT: ',auxfile
    ELSE
       PRINT *, 'FATAL ERROR'
       PRINT *, 'FILE NOT PRESENT: ',auxfile
       STOP
    END IF

    ! Inquire if trajfile exists
    INQUIRE(FILE=trajfile,EXIST=trajfile_exist)
    IF (trajfile_exist) THEN
       PRINT *, 'FILE PRESENT: ',trajfile
    ELSE
       PRINT *, 'FATAL ERROR'
       PRINT *, 'FILE NOT PRESENT: ',trajfile
       STOP
    END IF

  END SUBROUTINE read_namelist

END MODULE mod_namelist
