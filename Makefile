FC=gfortran
#export LD_LIBRARY_PATH=/root/app/eccodes_install/lib/:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH=/usr/lib:/home/app/eccodes_install/lib:/lib:/lib64:/usr/local/lib:/use/local/lib64:$LD_LIBRARY_PATH
#do not use O3
LMPI=-L/usr/lib/mpich -lmpi
LECCODES=-L/home/app/eccodes_install/lib -leccodes -leccodes_f90
INCLUDE=-I/usr/include/mpich/ -I/home/app/eccodes_install/include  
#if use intel compiler, change the -fopenmp to -qopenmp
FLAGS=-O0 -fopenmp   $(LECCODES) $(INCLUDE)

SHELL=/bin/bash
.phony:process_args
#process_args:
IDX=001
PIDX=000
#$(shell echo $(IDX))#echo $(PIDX)
DATA=2019101706
INPUTDATA=-i /home/g2data/rmf.hgra.$(DATA)$(IDX).grb2
OUTPUTDATA=-o /home/output3d/3d1.dat
CFGFILE=-c v1.cfg
CDATA=-d $(DATA)$(IDX)
PDATA=-p /home/g2data/rmf.hgra.$(DATA)$(PIDX).grb2
#OUTPUTHEAD=--k
#FIRSTFILE=--f
RUN_ARGS= $(INPUTDATA)  $(OUTPUTDATA)  $(CDATA)  $(PDATA) $(OUTPUTHEAD) $(FIRSTFILE) $(CFGFILE)

#objects := $(patsubst %.f90,%.o, $(wildcard *.f90) )
objects :=varibles.o config_utils.o dateutil.o write_rec.o parse_args.o grapes23ddat.o 

#all:grapes23ddat 

grapes23ddat:$(objects)
	$(FC)  $(objects) ${FLAGS} -o grapes23ddat

#	@make run

%.o:%.f90
	$(FC) -c $< ${FLAGS} -o $@
%.mod:%.o
	$(FC) ${FLAGS}  $<  $@

.phony:run
run:
	./grapes23ddat $(RUN_ARGS)

.phony:clean
clean:
	rm *.o *.mod grapes23ddat

.phony:cleanall
cleanall:
	rm *.o *.mod grapes23ddat *.txt *.log *.dat

