SHELL = /bin/bash

GF95 = -O1 -ffree-form -fno-second-underscore -m64 -fPIC -I.
INTL = -fPIC -O3 -free -assume byterecl -convert big_endian -static-intel 
LD = -Wl,-rpath,/unidata/intel/lib
#FC=gfortran $(GF95)
FF = ifort
FC = $(FF) $(INTL) -c


debug = -g -traceback -check all -fp-stack-check

#NCFLAGS = -limf -g -O2 -I/unidata/64/include -L/unidata/64/lib -lnetcdff -lnetcdf -lnetcdf #`nf-config --fflags --flibs`
NCLIBS = -L/unidata/intel/lib -lhdf5_hl -lhdf5 -lnetcdf -lnetcdff 
INCFLAGS = -I/unidata/intel/include

CODE = global_mod.f90\
    read_namelist.f90\
    checkinp.f90     \
    writetraj.f90    \
    calctraj.wpet.lint_polyp.f90\
	simtraj.f90

#$(LIB) : $(CODE)
#    for member in $?; do make member MODULE=$$member;done

#member : $(MODULE)
#	$(FC) $(NCLIBS) $(INCFLAGS) $? $(?F:.f90=.o)

simtraj:
#	$(FC) $(NCLIBS) $(INCFLAGS) $(CODE) -o simtraj_lint_polyp 
#	$(FC) global_mod.f90
#	$(FC) read_namelist.f90
#	$(FC) $(NCLIBS) $(INCFLAGS) checkinp.f90
#	$(FC) $(NCLIBS) $(INCFLAGS) writetraj.f90
#	$(FC) $(NCLIBS) $(INCFLAGS) calctraj.wpet.lint_polyp.f90
#	$(FC) $(NCLIBS) $(INCFLAGS) simtraj.f90
	$(FC) $(NCLIBS) $(INCFLAGS) $(CODE)
	$(FF) $(LD) -o simtraj_lint_polyp *.o $(NCLIBS) 
	rm -f *.o *.mod

clean:
	rm -f *.mod *.0 simtraj_lint_polyp


#
#
#        nc2arl: nc2arl.f
#            $(FC) $(NCFLAGS) $@.f $(LIB) -o $@
#            #   ranlib $(LIB)
#   rm -f $(?F:.f=.o)
#
#   clean :
#       rm -rf nc2arl
#       ~

#
# "nc-config -help" can provide more information.
#
###############################################################################
