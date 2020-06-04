module variables

    type string
  
      character(len=500), allocatable :: str
  
    end type string

    character(len=16) temp_name
    integer len_cmd, omp_rank1
    character, dimension(42):: process_date


    integer argc
    character*256 full_out_path
    character*32 out_name
    character*200 out_path_postfix
    !character(len=*), parameter :: inDir = '/gpfs/home/stu6/rho/thy_related/fortran_version/GRAPES_3K'
    !character(len=*), parameter :: outFile = '/gpfs/home/stu6/rho/thy_related/fortran_version'
    character(len=*), parameter :: inDir = '/home/g2data'
    character(len=*), parameter :: outFile = '/home/output3d'
    character(len=*), parameter :: filePrefix = 'rmf.hgra'
    character(len=*), parameter :: fileSuffix = 'grb2'
    character(len=42)  startDate 
    !integer, parameter :: nfiles = 36
    ! real, parameter :: latMinCP = 35.40  
    ! real, parameter :: latMaxCP = 42.53  
    ! real, parameter :: lonMinCP = 112.6  
    ! real, parameter :: lonMaxCP = 122.1  
    ! real :: latMinCP   
    ! real :: latMaxCP 
    ! real :: lonMinCP 
    ! real :: lonMaxCP  
    integer, dimension(19)::levsIncl
    integer ,allocatable :: gids(:)
    ! type(string) :: filenames(nfiles)
    ! type(string) :: filePaths(nfiles)
    !integer i,j,k,t
    integer len_fprefix, len_startDate,len_fileSuffix
    integer ::  status,mcount
    integer ::  ifile, gid,gidPRMSL, Ni,Nj
    integer ::  nlats, nlons
    integer :: iLatMinGRIB,iLatMaxGRIB,iLonMinGRIB,iLonMaxGRIB
    integer :: temp1darr(1)
    integer :: NX, NY, NZ

    integer mpi_rank,mpi_size,ierror,mpi_work, para_io, mul_io
    INTEGER :: request,recv_b, io3dpos, sk_ret
    integer :: iorec9

    character(len=256) :: cmd_cat

    real, dimension(:), allocatable :: lats,lons


    integer, allocatable :: levels(:),varParCat(:),varParNum(:),prodDefinTemNum(:)

    character(len=256), allocatable :: varNames(:)

    integer ::                &
    !!!!!!!!!!!!!!! 2D !!!!!!!!!!!!!!!
                gidPRES(800), &
                gidRAIN(800), &
                gidRADSW(800), &
                gidRADLW(800), &
                gidT2(800), &
                gidQ2(800), &
                gidU10(800), &
                gidV10(800), &
                gidSC(800), &
                gidSST(800), &
   !!!!!!!!!!!!!!! 3D !!!!!!!!!!!
                gidHGT(800), &
                gidTMP(800), &
                gidU(800), &
                gidV (800),&
                gidW(800), &
                gidRH(800), &
                gidVAPMR(800), &
                gidCLDMR(800), &
                gidRAINMR(800), &
                gidICEMR(800), &
                gidSNOWMR(800), &
                gidGRPMR(800)
                

    real, allocatable :: yyy

    integer, parameter:: iolog=44,ioheader = 48

    character(256)::io3dname
    character(len=*), parameter ::ioheadername="header.dat"

    !integer io3d
    character(len=*) ,parameter ::fmt1 = '3D.DAT            2.1             GRAPES 3km Model Product'
    character(len=*) ,parameter ::fmt2 = '0     '
    character(len=*) ,parameter ::fmt3 = '(6I3)'
    !character(len=*) ,parameter ::fmt4 = '(A4,F9.4,2F10.4,F7.2,2F10.3,F8.3,2I4,I3)'
    character(len=*) ,parameter ::fmt4 = '(a3,1x,f9.4,f10.4,2f7.2,2f10.3,f8.3,2i4,i3)'
    character(len=*) ,parameter ::fmt5 = '(23I3)'
    character(len=*) ,parameter ::fmt6 = '(I4,3I2.2,I5,3I4)'
    character(len=*) ,parameter ::fmt7 = '(6I4,2F10.4,2F9.4)'
    character(len=*) ,parameter ::fmt7_1 = '(F6.3)'
    character(len=*) ,parameter ::fmt8 = '(2I4,F9.4,F10.4,I5,I3,F9.4,F10.4,I5)'
    ! 4+12+8+8+8+8+24+8+24 =104
    character(len=*) ,parameter ::fmt9 = '(I4,3I2.2,2I3,F7.1,F5.2,I2,3F8.1,F8.2,3F8.1)'
    !i10,2i3,f7.1,f5.2,i2,3f8.1,f8.2,3f8.1
    ! 4+4+8+4+8+
    character(len=*) ,parameter ::fmt9_1 = '(I4,I6,F6.1,I4,F5.1,F6.2,I3,F5.2,5F6.3)'
    integer*4 :: io3d = 45
    integer*4 :: iocfg = 50

    real(kind=8), dimension(:,:), allocatable :: precedeRAINgrd





    character(len=256) argv
    !the BASE grib2 file's name with absolute path
    character(len=256) input_g2bfile

    !the grib2 file's name which will be processed, with absolute path
    character(len=256) input_g2file

    !the grib2 file's name which will be processed, which is precede 'input_g2file',
    ! with absolute path
    character(len=256) input_pg2file

    !the 3d file's name with absolute path
    character(len=256) output_3dfile
    !the config file's name with absolute path
    character(len=256) cfg_file

    !!!!!!!!!!!!!!!!!!!!!!!!!!

    logical lheader, lfirst_file
    character(len=42) :: precede_date

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! logical to check if the var exist in grib2 file
    ! true if exist, vice verse
    logical :: l2dPRES
    logical :: l2dRAIN
    logical :: l2dRADSW
    logical :: l2dRADLW
    logical :: l2dT2
    logical :: l2dQ2
    logical :: l2dU10
    logical :: l2dV10
    logical :: l2dSC
    logical :: l2dSST

    logical :: l3dHTG
    logical :: l3dTMP
    logical :: l3dV
    logical :: l3dU
    logical :: l3dW
    logical :: l3dRH
    logical :: l3dVAPMR
    logical :: l3dCLDMR
    logical :: l3dRAINMR
    logical :: l3dICEMR
    logical :: l3dSNOWMR
    logical :: l3dGRPMR



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real :: latMinCP
    real :: latMaxCP  
    real :: lonMinCP  
    real :: lonMaxCP 

    integer, parameter :: maxvar = 100
    !nfiles including the start-forecast file
    integer num2d,num3d,numlevel, nfiles

    character(len=10), dimension(maxvar) :: shortName2d,shortName3d
    character(len=10), dimension(maxvar) :: grapesname2d,grapesname3d
    integer, dimension(maxvar)           :: level2d 
    !3d level
    integer, dimension(maxvar)           :: plevel
    integer, dimension(maxvar)           :: paracat2d,paracat3d
    integer, dimension(maxvar)           :: paranum2d,paranum3d
    integer, dimension(maxvar)           :: prodnum2d,prodnum3d
    real, dimension(maxvar)              :: cnv2d,cnv3d
    real, dimension(maxvar)              :: default2d,default3d

    !namelist/cfg/num2d

    namelist/cfg/num2d,shortName2d,paracat2d,&
            paranum2d,prodnum2d,cnv2d,default2d,grapesname2d,level2d,num3d,&
            shortName3d,paracat3d,paranum3d,prodnum3d,cnv3d,default3d,&
            grapesname3d,numlevel,plevel,latMinCP,latMaxCP,&
            lonMinCP,lonMaxCP,nfiles

    
    integer, dimension(:),allocatable :: max_squence

  
  end module variables


