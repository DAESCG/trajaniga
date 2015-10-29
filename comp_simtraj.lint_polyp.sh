#!/bin/tcsh -f
###############################################################################
# sim_trajectory 1.0 
# Matthew Janiga (matthew.janiga@gmail.com)
# Last Updated: Mar. 20, 2011
#
# Description: sim_trajectory is a program used for the calculation of kinematic
# trajectories it is partly based on the traj3d and flextra trajectory programs.
#
###############################################################################
# Compilation Notes:
#
# $INCFLAGS should be set to the directory of your netcdf.mod file.
#
# To determine the appropriate libraries to link to on your system use type...
# "nc-config --libs"
# 
# This program makes use of netCDF 4 libraries to see if you have these type..
# "nc-config --has-nc4"
#
# "nc-config -help" can provide more information.
#
###############################################################################

# Compiler Flag
set FF = "ifort"

# Library and Include Flags
set LIBFLAGS = "-L/unidata/intel/lib -lnetcdf -lnetcdff -L/unidata/intel/lib -lhdf5_hl -lhdf5"
set INCFLAGS = "-I/unidata/intel/include"

# Optimization and Debugging Flags
# -fdefault-real-8 sets the default real size to a 64-bit double
set FFLAGS = ""
set FFLAGS = ($FFLAGS $INCFLAGS)
set FFLAGS = ($FFLAGS "-fPIC -c -g -O3 -fbounds-check -fimplicit-none -Wextra" )

# Compile Objects
$FF $FFLAGS global_mod.f90 read_namelist.f90 checkinp.f90 calctraj.wpet.lint_polyp.f90 writetraj.f90 simtraj.f90

# Link to Libraries and Output Executable
$FF -o simtraj_lint_polyp *.o $LIBFLAGS

# Clean Up Objects
 rm -f *.o 

# Clean Up Modules
rm -f *.mod

