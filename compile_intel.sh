ifort -c varibles.f90 -O3  -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o varibles.o
ifort -c config_utils.f90 -O3  -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o config_utils.o
ifort -c dateutil.f90 -O3  -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o dateutil.o
ifort -c write_rec.f90 -O3  -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o write_rec.o
ifort -c parse_args.f90 -O3  -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o parse_args.o
ifort -c grapes23ddat.f90 -O3  -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o grapes23ddat.o
ifort  varibles.o config_utils.o dateutil.o write_rec.o parse_args.o grapes23ddat.o  -O3 -qopenmp -g -traceback  -L/g1/app/mathlib/eccodes/2.17.0/intel/lib/ -leccodes -leccodes_f90 -I/g1/app/mathlib/eccodes/2.17.0/intel/include/ -o grapes23ddat


#uncomment to run
#./grapes23ddat -i /g1/COMMONDATA/OPER/NWPC/GRAPES_MESO_3KM/Prod-grib/2019101706/ORIG/rmf.hgra.2019101706003.grb2  -o 3d1.dat  -d 2019101706003  -p /g1/COMMONDATA/OPER/NWPC/GRAPES_MESO_3KM/Prod-grib/2019101706/ORIG/rmf.hgra.2019101706002.grb2   -c v1.cfg