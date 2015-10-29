!###############################################################################
!  writetraj.f90
!  Author: Matthew Janiga (matthew.janiga@gmail.com)
!  Last Updated: Apr. 6, 2011
!
!  Description: Writes the trajectories to file.
!###############################################################################

MODULE mod_writetraj
  USE mod_fileio
  USE netcdf
  USE mod_traj
  USE mod_checkinp
  IMPLICIT NONE

CONTAINS

  !###########################################################################
  ! writetraj()                                                           ####
  ! Writes the trajectories to a netCDF file.                             ####
  !###########################################################################

  SUBROUTINE writetraj()
    IMPLICIT NONE

    ! ID numbers

    INTEGER :: ncid, error, tr_dimid, step_dimid, tr_varid, step_varid
    INTEGER :: nvar

    INTEGER, DIMENSION(naux+9) :: varid
    INTEGER, DIMENSION(2) :: dimids

    INTEGER :: i

    CHARACTER (len = *), PARAMETER :: STEP_NAME = "time_step"
    CHARACTER (len = *), PARAMETER :: TRAJ_NAME = "trajectory"

    CHARACTER (len = *), PARAMETER :: LONG_NAME = "long_name"
    CHARACTER (len = *), PARAMETER :: TRAJ_LONG_NAME = "trajectory"
    CHARACTER (len = *), PARAMETER :: STEP_LONG_NAME = "time_step"
    CHARACTER (len = *), PARAMETER :: FILL_VALUE = "_FillValue"

    INTEGER, DIMENSION(nstep) :: nc_steps
    INTEGER, DIMENSION(ntraj) :: nc_trajs

    INTEGER, PARAMETER :: deflev = 1


    ! Number of variables to write
    nvar = naux+9

    ! Create descriptor arrays
    DO i=1,nstep
       nc_steps(i) = i
    END DO
    DO i=1,ntraj
       nc_trajs(i) = i
    END DO

    ! Create the file. 
    CALL check( nf90_create( path = outfile, cmode = NF90_HDF5, ncid = ncid) )

    PRINT *, "Created"

    ! Specify variable IDs
    DO i=1,nvar
       varid(i) = i
    END DO

    ! Define dimensions
    CALL check( nf90_def_dim(ncid, TRAJ_NAME, ntraj, tr_dimid) )
    CALL check( nf90_def_dim(ncid, STEP_NAME, nstep, step_dimid) )

    ! Dimension names
    CALL check( nf90_def_var(ncid, STEP_NAME, NF90_INT, step_dimid, step_varid) )
    CALL check( nf90_def_var(ncid, TRAJ_NAME, NF90_INT, tr_dimid, tr_varid) )

    ! Dimension attributes
    CALL check( nf90_put_att(ncid, tr_varid, LONG_NAME, TRAJ_LONG_NAME) )
    CALL check( nf90_put_att(ncid, step_varid, LONG_NAME, STEP_LONG_NAME) )

    PRINT *, "Dimensions defined"

    ! Dimensions of variables along trajectory
    dimids =  (/ tr_dimid, step_dimid /)

    ! Define variables along trajectory (base variables)
    CALL check( nf90_def_var(ncid, "time", NF90_FLOAT, dimids, varid(1)) )
    CALL check( nf90_def_var(ncid, "lev", NF90_FLOAT, dimids, varid(2)) )
    CALL check( nf90_def_var(ncid, "lat", NF90_FLOAT, dimids, varid(3)) )
    CALL check( nf90_def_var(ncid, "lon", NF90_FLOAT, dimids, varid(4)) )
    CALL check( nf90_def_var(ncid, "u", NF90_FLOAT, dimids, varid(5)) )
    CALL check( nf90_def_var(ncid, "v", NF90_FLOAT, dimids, varid(6)) )
    CALL check( nf90_def_var(ncid, "w", NF90_FLOAT, dimids, varid(7)) )
    CALL check( nf90_def_var(ncid, "sp", NF90_FLOAT, dimids, varid(8)) )

    ! Define variables along trajectory (auxiliary variables)
    IF (naux >= 1) THEN
       DO i=1,naux
          CALL check( nf90_def_var(ncid, aux_name(i), NF90_FLOAT, dimids, varid(8+i)) )
       END DO
    END IF
    CALL check( nf90_def_var(ncid, "last_valid", NF90_INT, tr_dimid, varid(8+naux+1)) )


    PRINT *, "Variables defined"

    ! Store attributes
    DO i=1,nvar-1
       CALL check( nf90_put_att(ncid, varid(i), FILL_VALUE, mv) )
    END DO
    CALL check( nf90_put_att(ncid, varid(8+naux+1), FILL_VALUE, mvi) )

    PRINT *, "Attributes defined"

    ! Deflate variables (requres cmode = HDF5)
    DO i=1,nvar
       CALL check( nf90_def_var_deflate(ncid, varid(i), deflate = 1, shuffle = 0, deflate_level =  deflev ) )
    END DO

    PRINT *, "Variables deflated"

    ! End of variables definitions
    CALL check( nf90_enddef(ncid) )

    ! Write dimension descriptor arrays
    CALL check( nf90_put_var(ncid, tr_varid, nc_trajs) )
    CALL check( nf90_put_var(ncid, step_varid, nc_steps) )

    ! Write base variables
    CALL check( nf90_put_var(ncid, varid(1), t_time) )
    CALL check( nf90_put_var(ncid, varid(2), t_lev) )
    CALL check( nf90_put_var(ncid, varid(3), t_lat) )
    CALL check( nf90_put_var(ncid, varid(4), t_lon) )
    CALL check( nf90_put_var(ncid, varid(5), t_u) )
    CALL check( nf90_put_var(ncid, varid(6), t_v) )
    CALL check( nf90_put_var(ncid, varid(7), t_w) )
    CALL check( nf90_put_var(ncid, varid(8), t_sp) )

    ! Write auxiliary varibles
    IF (naux >= 1) THEN
       DO i=1,naux
          CALL check( nf90_put_var(ncid, varid(8+i), t_aux(:,:,i)) )
       END DO
    END IF
    CALL check( nf90_put_var(ncid, varid(8+naux+1), last_valid) )

    PRINT *, "Variables written"

    ! Close the file
    CALL check( nf90_close( ncid) )

  END SUBROUTINE writetraj

END MODULE mod_writetraj
