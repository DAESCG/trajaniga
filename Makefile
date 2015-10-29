SHELL = /bin/bash

GF95 = -O1 -ffree-form -fno-second-underscore -m64 -fPIC -I.
INTL = -O3 -free -L -lgfortran -assume byterecl -convert big_endian

#FC=gfortran $(GF95)
FC = ifort $(INTL)


debug = -g -traceback -check all -fp-stack-check

#NCFLAGS = -limf -g -O2 -I/unidata/64/include -L/unidata/64/lib -lnetcdff -lnetcdf -lnetcdf #`nf-config --fflags --flibs`
NCLIBS = -L/unidata/intel/lib -lnetcdf -lnetcdff -L/unidata/intel/lib -lhdf5_hl -lhdf5
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
	$(FC) $(NCLIBS) $(INCFLAGS) $(CODE) -o simtraj_lint_polyp 
	rm -f *.mod

clean:
	rm -f *.mod simtraj_lint_polyp


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
