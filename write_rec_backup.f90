module write_rec
    !use params
    !use varaibles
    use variables
    use dateutil
    use eccodes
    use omp_lib
    implicit none
    include 'mpif.h'
    
    

    
    contains
    

    subroutine writeRec1
        write(io3d,'(A)') fmt1
    end subroutine

    
    subroutine writeRec2
        write(io3d,'(A)') fmt2  
    end subroutine
    
    subroutine writeRec3
        integer, parameter::IOUTW = 1, &
                            IOUTQ = 1, &
                            IOUTC = 1, &
                            IOUTI = 1, &
                            IOUTG = 1, &
                            IOSRF = 0 
        
        write(io3d,fmt3) IOUTW,IOUTQ,IOUTC,IOUTI,IOUTG,IOSRF   
    end subroutine
    
    subroutine writeRec4
        !integer NX,NY,NZ
        real RLATC,RLONC,TRUELAT1,TRUELAT2,X1DMN,Y1DMN,DXY
        character(len=3) MAPTXT
        MAPTXT = 'LLC' 
        RLATC = (lats(iLatMinGRIB)+lats(iLatMaxGRIB))/2.  
        RLONC = (lons(iLonMinGRIB)+lons(iLonMaxGRIB))/2.
        TRUELAT1 = lats(iLatMinGRIB)
        TRUELAT2 = lats(iLatMinGRIB+1)
        X1DMN = 0.0  
        Y1DMN = 0.0 
        DXY = 0.0
        !'(A4,F9.4,2F10.4,F7.2,2F10.3,F8.3,'+',2I4,I3,)'
        write(io3d,fmt4) MAPTXT,RLATC,RLONC,TRUELAT1,TRUELAT2,X1DMN,Y1DMN,DXY,NX,NY,NZ
    
    end subroutine
    
    subroutine writeRec5
        write(io3d,fmt5) 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    end subroutine
    
    subroutine writeRec6
        integer BYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,IBYRM
        character(len=4) MAPTXT
        character(:), allocatable:: date
        date = startDate

        read(date(1:4),*) IBYRM
        read(date(5:6),*) IBMOM
        read(date(7:8),*) IBDYM
        read(date(9:10),*) IBHRM
        
        NHRSMM5 = nfiles-1

        !'(I4,3I2.2,I5,3I4)'
        write(io3d,fmt6) IBYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,NX,NY,NZ
        !write(*,*) IBYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,NX,NY,NZ
    end subroutine
    
    subroutine writeRec7
        integer NX1,NY1,NX2,NY2,NZ1,NZ2,s,ns
        real RXMIN,RXMAX,RYMIN,RYMAX
        real SIGMA(19)
        NX1 = 1  
        NY1 = 1  
        NX2 = NX 
        NY2 = NY  
        NZ1 = 1 
        NZ2 = NZ 
        RXMIN = lons(iLonMinGRIB)  
        RXMAX = lons(iLonMaxGRIB)
        RYMIN = lats(iLatMinGRIB) 
        RYMAX = lats(iLatMaxGRIB) 
        write(io3d,fmt7) NX1,NY1,NX2,NY2,NZ1,NZ2,RXMIN,RXMAX,RYMIN,RYMAX
        !write(*,*) '7', NX1,NY1,NX2,NY2,NZ1,NZ2,RXMIN,RXMAX,RYMIN,RYMAX
        SIGMA = levsIncl/1013.25
        ns=size(SIGMA)
        do s = 1, ns
            write(io3d,fmt7_1) SIGMA(s)
        enddo    
    end subroutine
    
    subroutine writeRec8
        integer IINDEX,JINDEX,IELEVDOT,ILAND,IELEVCRS,i,j
        real XLATDOT,XLONGDOT,XLATCRS,XLONGCRS
        IELEVDOT = 0 
        ILAND = -9
        XLATCRS = -999 
        XLONGCRS = -999 
        IELEVCRS = -999
        !'(2I4,F9.4,F10.4,I5,I3,F9.4,F10.4,I5)'
        do j = 1,NY
            JINDEX = j
            XLATDOT = lats(iLatMinGRIB-j)
            do i = 1,NX
                IINDEX = i
                XLONGDOT = lons(iLonMinGRIB+i)
                write(io3d,fmt8) IINDEX,JINDEX,XLATDOT,XLONGDOT,IELEVDOT,ILAND,XLATCRS,XLONGCRS,IELEVCRS
            enddo
        enddo
    end subroutine

    ! subroutine writeRec9
        
    !     integer t,ijuldy,iyr,ijul,ihr,nhrinc,imo,iday,status
    !     integer k,i,j,i1,j1,k1
    !     integer BYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,IBYRM
    !     integer :: numPoints

    !     integer omp_rank,omp_rank1
    !     real tm1,tm2,tm3,tm4

        
        

    !     !'(I4,3I2.2,2I3,F7.1,F5.2,I2,3F8.1,F8.2,3F8.1)'
    !     !MYR,MMO,MDAY,MHR,IX,JX,PRES,RAIN,SC,RADSW,RADLW,T2,Q2,WD10,WS10,SST)

    !     integer :: MYR,MMO,MDAY,MHR,IX,JX,SC
    !     real(kind=8) :: PRES,RAIN,RADSW,RADLW,T2,Q2,WD10,WS10,SST

    !     !PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
    !     !'(I4,I6,F6.1,I4,F5.1,F6.2,I3,F5.2,5F6.3)'

    !     integer :: PRES2,Z,WD,RH
    !     real(kind=8) :: TEMPK,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR,VAPMR,WS,W

    !     !real(kind=8), parameter :: PI = 3.1415926535897932384626433
    !     real(kind=8), parameter :: PI = 3.141592653589793115997963468544185161590576171875
    !     real(kind=8), parameter :: D2PI = 360.0

    !     real(kind=8), dimension(:), allocatable       :: values1d 
    !     !real(kind=8), dimension(:,:), allocatable       :: PRESgrd 
    !     real(kind=8), dimension(:,:), allocatable :: PRESgrd
    !     real(kind=8), dimension(:,:), allocatable :: RAINgrd
    !     real(kind=8), dimension(:,:), allocatable :: RADSWgrd
    !     real(kind=8), dimension(:,:), allocatable :: RADLWgrd
    !     real(kind=8), dimension(:,:), allocatable :: T2grd
    !     real(kind=8), dimension(:,:), allocatable :: Q2grd
    !     real(kind=8), dimension(:,:), allocatable :: U10grd
    !     real(kind=8), dimension(:,:), allocatable :: V10grd
    !     real(kind=8), dimension(:,:), allocatable :: WS10grd
    !     real(kind=8), dimension(:,:), allocatable :: WD10grd

    !     real(kind=8), dimension(:,:), allocatable :: values2d
    !     real(kind=8), dimension(:,:,:), allocatable :: HGTgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: TMPgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: Ugrd
    !     real(kind=8), dimension(:,:,:), allocatable :: Vgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: Wgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: RHgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: VAPMRgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: CLDMRgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: RAINMRgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: ICEMRgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: SNOWMRgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: GRPMRgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: WSgrd
    !     real(kind=8), dimension(:,:,:), allocatable :: WDgrd

    !     ! real(kind=8), dimension(:), allocatable :: T_1D_HGTgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_TMPgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_Ugrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_Vgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_Wgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_RHgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_VAPMRgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_CLDMRgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_RAINMRgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_ICEMRgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_SNOWMRgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_GRPMRgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_WSgrd
    !     ! real(kind=8), dimension(:), allocatable :: T_1D_WDgrd

    !     integer, dimension(:,:),allocatable:: MYR_ARR
    !     integer, dimension(:,:),allocatable:: MMO_ARR
    !     integer, dimension(:,:),allocatable:: MDAY_ARR
    !     integer, dimension(:,:),allocatable:: MHR_ARR
    !     integer, dimension(:,:),allocatable:: IX_ARR
    !     integer, dimension(:,:),allocatable:: JX_ARR
    !     real(kind=8), dimension(:,:),allocatable:: PRES_ARR
    !     real(kind=8), dimension(:,:),allocatable:: RAIN_ARR
    !     integer, dimension(:,:),allocatable:: SC_ARR
    !     real(kind=8), dimension(:,:),allocatable:: RADSW_ARR
    !     real(kind=8), dimension(:,:),allocatable:: RADLW_ARR
    !     real(kind=8), dimension(:,:),allocatable:: T2_ARR
    !     real(kind=8), dimension(:,:),allocatable:: Q2_ARR
    !     real(kind=8), dimension(:,:),allocatable:: WD10_ARR
    !     real(kind=8), dimension(:,:),allocatable:: WS10_ARR
    !     real(kind=8), dimension(:,:),allocatable:: SST_ARR

    !     integer, dimension(:,:,:), allocatable :: PRES2_ARR
    !     integer, dimension(:,:,:), allocatable :: Z_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: TEMPK_ARR
    !     integer, dimension(:,:,:), allocatable :: WD_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: WS_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: W_ARR
    !     integer, dimension(:,:,:), allocatable :: RH_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: VAPMR_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: CLDMR_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: RAINMR_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: ICEMR_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: SNOWMR_ARR
    !     real(kind=8), dimension(:,:,:), allocatable :: GRPMR_ARR
        
    
    !     character(len=42), allocatable:: date
    !     integer, dimension(:), allocatable :: gids1
    !     integer ::arr_shape(2), arr_order(2)

    !     !call MPI_Init(ierror)
    !     call MPI_Comm_rank(MPI_COMM_WORLD,mpi_rank,ierror)
    !     call MPI_Comm_size(MPI_COMM_WORLD,mpi_size,ierror)

    !     date = startDate
    !     arr_shape = (/Nj,Ni/)
    !     arr_order = (/2,1/)
    !     SC = 0
    !     SST = 0
    !     para_io = 0
    !     iorec9 = 47
    !     mul_io = 1
    !     if(mul_io==1) then
    !         if(mpi_rank<10) then
    !             write(iorec9name(6:6),'(I1)') mpi_size
    !             write(iorec9name(12:12),'(I1)') mpi_rank
    !             print *, iorec9name
    !         else
    !             stop 'MPI SIZE .GT. 10 NOT YET HANDLE !'
    !         endif
    !         open(unit=iorec9, file=iorec9name)
    !     endif

    !     allocate(PRESgrd(Nj,Ni))
    !     allocate(RAINgrd(Nj,Ni))
    !     allocate(RADSWgrd(Nj,Ni))
    !     allocate(RADLWgrd(Nj,Ni))
    !     allocate(T2grd(Nj,Ni))
    !     allocate(Q2grd(Nj,Ni))
    !     allocate(U10grd(Nj,Ni))
    !     allocate(V10grd(Nj,Ni))
    !     allocate(WS10grd(Nj,Ni))
    !     allocate(WD10grd(Nj,Ni))

    !     allocate(HGTgrd(Nj,Ni,NZ))
    !     allocate(TMPgrd(Nj,Ni,NZ))
    !     allocate(Ugrd(Nj,Ni,NZ))
    !     allocate(Vgrd(Nj,Ni,NZ))
    !     allocate(Wgrd(Nj,Ni,NZ))
    !     allocate(RHgrd(Nj,Ni,NZ))
    !     allocate(VAPMRgrd(Nj,Ni,NZ))
    !     allocate(CLDMRgrd(Nj,Ni,NZ))
    !     allocate(RAINMRgrd(Nj,Ni,NZ))
    !     allocate(ICEMRgrd(Nj,Ni,NZ))
    !     allocate(SNOWMRgrd(Nj,Ni,NZ))
    !     allocate(GRPMRgrd(Nj,Ni,NZ))
    !     allocate(WSgrd(Nj,Ni,NZ))
    !     allocate(WDgrd(Nj,Ni,NZ))

    !     ! allocate(T_1D_HGTgrd(Nj*Ni))
    !     ! allocate(T_1D_TMPgrd(Nj*Ni))
    !     ! allocate(T_1D_Ugrd(Nj*Ni))
    !     ! allocate(T_1D_Vgrd(Nj*Ni))
    !     ! allocate(T_1D_Wgrd(Nj*Ni))
    !     ! allocate(T_1D_RHgrd(Nj*Ni))
    !     ! allocate(T_1D_VAPMRgrd(Nj*Ni))
    !     ! allocate(T_1D_CLDMRgrd(Nj*Ni))
    !     ! allocate(T_1D_RAINMRgrd(Nj*Ni))
    !     ! allocate(T_1D_ICEMRgrd(Nj*Ni))
    !     ! allocate(T_1D_SNOWMRgrd(Nj*Ni))
    !     ! allocate(T_1D_GRPMRgrd(Nj*Ni))
    !     ! allocate(T_1D_WSgrd(Nj*Ni))
    !     ! allocate(T_1D_WDgrd(Nj*Ni))

        
    !     ! allocate(MYR_ARR(Nj,Ni))
    !     ! allocate(MMO_ARR(Nj,Ni))
    !     ! allocate(MDAY_ARR(Nj,Ni))
    !     ! allocate(MHR_ARR(Nj,Ni))
    !     ! allocate(IX_ARR(Nj,Ni))
    !     ! allocate(JX_ARR(Nj,Ni))
    !     ! allocate(PRES_ARR(Nj,Ni))
    !     ! allocate(RAIN_ARR(Nj,Ni))
    !     ! allocate(SC_ARR(Nj,Ni))
    !     ! allocate(RADSW_ARR(Nj,Ni))
    !     ! allocate(RADLW_ARR(Nj,Ni))
    !     ! allocate(T2_ARR(Nj,Ni))
    !     ! allocate(Q2_ARR(Nj,Ni))
    !     ! allocate(WD10_ARR(Nj,Ni))
    !     ! allocate(WS10_ARR(Nj,Ni))
    !     ! allocate(SST_ARR(Nj,Ni))

    !     ! allocate(PRES2_ARR(Nj,Ni,NZ))
    !     ! allocate(Z_ARR(Nj,Ni,NZ))
    !     ! allocate(TEMPK_ARR(Nj,Ni,NZ))
    !     ! allocate(WD_ARR(Nj,Ni,NZ))
    !     ! allocate(WS_ARR(Nj,Ni,NZ))
    !     ! allocate(W_ARR(Nj,Ni,NZ))
    !     ! allocate(RH_ARR(Nj,Ni,NZ))
    !     ! allocate(VAPMR_ARR(Nj,Ni,NZ))
    !     ! allocate(CLDMR_ARR(Nj,Ni,NZ))
    !     ! allocate(RAINMR_ARR(Nj,Ni,NZ))
    !     ! allocate(ICEMR_ARR(Nj,Ni,NZ))
    !     ! allocate(SNOWMR_ARR(Nj,Ni,NZ))
    !     ! allocate(GRPMR_ARR(Nj,Ni,NZ))

        
        
        
        
        
    !     !2019, 10, 17, 0
    !     ! iyr = 2019
    !     ! imo = 10
    !     ! iday = 17
    !     ! ihr = 23
        
    !     ! call julday(iyr,imo,iday,ijuldy)
    !     ! write(*,*) 'ijuldy: ',ijuldy
    !     ! call incr(iyr,ijuldy,ihr,3)
    !     ! write(*,*) 'iyr,ijul,ihr: ',iyr,ijuldy,ihr

    !     ! call grday(iyr,ijuldy,imo,iday)
    !     ! write(*,*) 'iyr,ijul,imo,iday: ',iyr,ijuldy,imo,iday,ihr
    !     !print *, 'debug1'
    !     read(date(1:4),*) IBYRM
    !     read(date(5:6),*) IBMOM
    !     read(date(7:8),*) IBDYM
    !     !read(date(9:10),*) IBHRM
    !     !print *, 'debug2'
    !     IBHRM = 0
    !     iyr = IBYRM
    !     imo = IBMOM
    !     iday = IBDYM
    !     ihr = IBHRM

    !     !print *, 'debug3'
    !     allocate(values1d(Ni*Nj))
    !     !return
    !     !call omp_set_nested(.true.)
    !     !call omp_set_dynamic(.true.)
    !     if(mpi_rank.gt.1 .and. mod(mpi_size,2) .ne. 0) stop 'MPI SIZE MUST BE EVEN'
    !     mpi_work = nfiles/mpi_size
    !     print *, mpi_rank,1+mpi_work*mpi_rank, mpi_work*(mpi_rank+1)
    !     do t = 1+mpi_work*mpi_rank, mpi_work*(mpi_rank+1)
        
    !         write(*,*) "Processing file ",trim(filenames(t)%str)
    !         call julday(IBYRM,IBMOM,IBDYM,ijuldy)
    !         call incr(IBYRM,ijuldy,IBHRM,t-1)
    !         call grday(IBYRM,ijuldy,IBMOM,IBDYM)

    !         MYR = IBYRM
    !         MMO = IBMOM
    !         MDAY = IBDYM
    !         MHR = IBHRM
    !         !write(*,*) MYR,MMO,MDAY,MHR
               
    !         !write(*,*) iyr,ijuldy,imo,iday

    !         call grib_open_file(ifile,filePaths(t)%str,'r',status)
    !         call codes_grib_multi_support_on(status)
    !         call grib_count_in_file(ifile,mcount)
    !         allocate(gids1(mcount))

    !         ! $omp parallel do shared(gids1,mcount) private(status)&
    !         ! $omp firstprivate(ifile)
    !         !tm1=omp_get_wtime()
    !         do i = 1, mcount 
    !             call codes_grib_new_from_file(ifile,gids1(i),status)	
    !         enddo
    !         !tm2=omp_get_wtime()
    !         !print *, tm2-tm1
    !         ! $omp end parallel do

    !         call grib_close_file(ifile,status)

    !         !print *, 'gidPRES(1)',gidPRES(1)

    !         !call codes_get_size(gidPRES(1),'values',numPoints)   
    !         !allocate(PRESgrd(Nj,Ni))
    !         tm1=omp_get_wtime()
    !         !$omp parallel private(omp_rank,values1d) num_threads(8) 
    !         omp_rank = omp_get_thread_num()
    !         !print *, 'my_id: ',omp_rank
    !         if(omp_rank==0) then
    !         call codes_get(gidPRES(1),'values',values1d)   
    !         PRESgrd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==1) then
    !         call codes_get(gidRAIN(1),'values',values1d)
    !         RAINgrd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==2) then
    !         call codes_get(gidRADSW(1),'values',values1d)
    !         RADSWgrd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==3) then
    !         call codes_get(gidRADLW(1),'values',values1d)
    !         RADLWgrd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==4) then
    !         call codes_get(gidT2(1),'values',values1d)
    !         T2grd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==5) then
    !         call codes_get(gidQ2(1),'values',values1d)
    !         Q2grd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==6) then
    !         call codes_get(gidU10(1),'values',values1d)
    !             U10grd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
    !         if(omp_rank==7) then
    !         call codes_get(gidV10(1),'values',values1d)
    !         V10grd = RESHAPE(values1d,arr_shape, order=arr_order)
    !         endif
  
        
    !         !$omp end parallel
            
    !         tm2=omp_get_wtime()

    !         print *, 'omp1', tm2-tm1

    !         WS10grd = U10grd**2+V10grd**2
    !         WD10grd = atan2(V10grd,U10grd)
    !         WD10grd = WD10grd * (180.0/PI)
    !         WD10grd = WD10grd + 180.0
    !         WD10grd = -WD10grd
    !         WD10grd = WD10grd + 90.0
    !         WD10grd = dmod(WD10grd,D2PI)+360



    !         !print *, 'omp2 begin'
    !         tm1 = omp_get_wtime()
    !         !$omp parallel private(k,values1d,tm1,tm2,omp_rank) &
    !         !$omp shared(arr_order,arr_shape) num_threads(19)
            
 
    !         omp_rank = omp_get_thread_num()
    !         !print *, omp_rank+1
    !         k=omp_rank+1
    !         ! $omp do 
    !         !do k =1 , NZ
    !             ! $omp parallel private(values1d,tm1,tm2,omp_rank1) &
    !             ! $omp shared(arr_order,arr_shape)&
    !             ! $omp num_threads(4)
                
    !             !omp_get_num_threads(3)
    !             ! $omp sections 
            
            
    !             omp_rank1 = omp_get_thread_num()
    !             !print *, 'omp_rank1',omp_rank1,'omp_rank',omp_rank
                
    !             !if(omp_rank1==0)then
    !                 tm1 = omp_get_wtime()
    !                 !print *, "gidHGT(k)",gidHGT(k)
                    
    !                 !private(k,values1d,tm1,tm2,omp_rank) shared(arr_order,arr_shape), firstprivate(gidHGT)
    !                 !print *, "gidHGT(k)",gidHGT(k)
    !                 ! $omp section
    !                 call codes_get(gidHGT(k),'values',values1d)
    !                 HGTgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 !private(k,values1d,tm1,tm2,omp_rank) shared(arr_order,arr_shape), firstprivate(gidTMP)
    !                 ! $omp section
    !                 call codes_get(gidTMP(k),'values',values1d)
    !                 TMPgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 ! $omp section
    !                 call codes_get(gidU(k),'values',values1d)
    !                 Ugrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
    !             !endif   
                     

    !             !if(omp_rank1==1)then 
    !                 !private(k,values1d,tm1,tm2,omp_rank) shared(arr_order,arr_shape), firstprivate(gidV)
    !                 ! $omp section
    !                 call codes_get(gidV(k),'values',values1d)
    !                 Vgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 !private(k,values1d,tm1,tm2,omp_rank) shared(arr_order,arr_shape), firstprivate(gidW)
    !                 ! $omp section
    !                 call codes_get(gidW(k),'values',values1d)
    !                 Wgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                    
    !                 !private(k,values1d,tm1,tm2,omp_rank) shared(arr_order,arr_shape), firstprivate(gidRH)
    !                 ! $omp section
    !                 call codes_get(gidRH(k),'values',values1d)
    !                 RHgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    

    !                 ! tm1 = omp_get_wtime()
    !                 ! print *, "RHgrd",-tm2+tm1
    !                 tm2=omp_get_wtime()
    !                 !print *,"omp_t1", tm2-tm1

    !             !endif
                

                
    !             !if(omp_rank1==2)then
    !                 tm1 = omp_get_wtime()
                    
                    
    !                 call codes_get(gidVAPMR(k),'values',values1d)
    !                 VAPMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 call codes_get(gidCLDMR(k),'values',values1d)
    !                 CLDMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 call codes_get(gidRAINMR(k),'values',values1d)
    !                 RAINMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
    !             !endif
                    
    !             !if(omp_rank1==3)then 
    !                 call codes_get(gidICEMR(k),'values',values1d)
    !                 ICEMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 call codes_get(gidSNOWMR(k),'values',values1d)
    !                 SNOWMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 call codes_get(gidGRPMR(k),'values',values1d)
    !                 GRPMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                    
    !                 ! tm1 = omp_get_wtime()
    !                 ! print *, "GRPMRgrd",-tm2+tm1
    !                 tm2=omp_get_wtime()
    !                 !print *,"omp_t2", tm2-tm1

            
            
    !         !$omp end parallel
    !         tm2 = omp_get_wtime()
    !         print*, 'rank:',omp_rank, 'time: ', tm2-tm1
    !         !WSgrd = sqrt(Ugrd**2+Vgrd**2)


    !         !print *, 'debug5',NY,NX,NZ
    !         !$omp parallel private(omp_rank1) num_threads(48) 
    !         !$omp do  
    !         do j1 = iLatMinGRIB-NY,iLatMinGRIB-1
    !             do i1 = iLonMinGRIB+1, iLonMinGRIB+NX
    !                 do k1 = 1,NZ                       
    !                     WDgrd(j1,i1,k1) = atan2(Vgrd(j1,i1,k1),Ugrd(j1,i1,k1))
    !                     WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1)*(180/PI)
    !                     WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1) + 180.0                       
    !                     WDgrd(j1,i1,k1) = -WDgrd(j1,i1,k1)
    !                     WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1)+90.0
    !                     WDgrd(j1,i1,k1) = dmod(WDgrd(j1,i1,k1),D2PI)+360
    !                     WSgrd(j1,i1,k1) = sqrt(Ugrd(j1,i1,k1)**2+Vgrd(j1,i1,k1)**2)
    !                 enddo
    !             enddo
    !         enddo
    !         !$omp end do
    !         !$omp end parallel 


    !         tm1=omp_get_wtime()


    !         do j = 1, NY
    !             JX = j
    !             do i = 1, NX
    !                 IX = i
    !                 PRES = PRESgrd(iLatMinGRIB-j,iLonMinGRIB+i) * 0.01 
    !                 !sea level pressure (hPa)
    !                 RAIN = RAINgrd(iLatMinGRIB-j,iLonMinGRIB+i) * 0.01 
    !                 !total rainfall
    !                 RADSW = RADSWgrd(iLatMinGRIB-j,iLonMinGRIB+i) 
    !                 !Short wave radiation at the surface (W/m**2)
    !                 RADLW = RADLWgrd(iLatMinGRIB-j,iLonMinGRIB+i) 
    !                 !long wave radiation at the top (W/m**2)
    !                 T2 = T2grd(iLatMinGRIB-j,iLonMinGRIB+i) 
    !                 !Air temperature at 2 m (K)
    !                 Q2 = Q2grd(iLatMinGRIB-j,iLonMinGRIB+i) *1000 
    !                 !Specific humidity at 2 m (g/kg)
    !                 WD10 = WD10grd(iLatMinGRIB-j,iLonMinGRIB+i) 
    !                 !Wind direction of 10-m wind (m/s)
    !                 WS10 = WS10grd(iLatMinGRIB-j,iLonMinGRIB+i) 
    !                 !Wind speed of

    !                 if(mul_io==1) then 
    !                     write(iorec9,fmt9) MYR,MMO,MDAY,MHR,IX,JX,PRES,RAIN,SC,RADSW,RADLW,T2,Q2,WD10,WS10,SST
    !                 endif

    !                 do k = 1, NZ
    !                     PRES2 = levsIncl(k)  
    !                     ! Pressure (mb)
    !                     Z = int(HGTgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
    !                     !Elevation (m above sea level)
    !                     TEMPK=int(TMPgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
    !                     !Temperature (Kelvin)
    !                     WD=int(WDgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
    !                     !Wind direction (degrees)
    !                     WS=WSgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) 
    !                     !Wind speed (m/s)
    !                     W=Wgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) 
    !                     !Vertical velocity (m/s)
    !                     RH=int(RHgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
    !                     !Relative humidity (%)
    !                     VAPMR=VAPMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) *1000 
    !                     !Vapor mixing ratio (g/kg)
    !                     CLDMR=CLDMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * 1000 
    !                     !Cloud mixing ratio (g/kg)
    !                     RAINMR=RAINMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * 1000 
    !                     !Rain mixing ratio (g/kg)
    !                     ICEMR=ICEMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * 1000
    !                     !Ice mixing ratio (g/kg)
    !                     SNOWMR=SNOWMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) *1000 
    !                     !Snow mixing ratio (g/kg)
    !                     GRPMR=GRPMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) *1000 
    !                     !Graupel mixing ratio (g/kg)

    !                     if(mul_io==1) then 
    !                         write(iorec9,fmt9_1)PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
    !                     endif
    !                     !write(*,*) PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
    !                 enddo
    !             enddo
    !         enddo

        
    !         do i1 = 1, mcount
    !             call grib_release(i1)
    !         enddo

   
    !         deallocate(gids1)
    !         !deallocate(values)
    !         !deallocate(PRESgrd)
            
    !     !deallocate(values1d)
    !     enddo

    !     deallocate(PRESgrd)
    !     deallocate(RAINgrd)
    !     deallocate(RADSWgrd)
    !     deallocate(RADLWgrd)
    !     deallocate(T2grd)
    !     deallocate(Q2grd)
    !     deallocate(U10grd)
    !     deallocate(V10grd)
    !     deallocate(WS10grd)
    !     deallocate(WD10grd)

    !     deallocate(HGTgrd)
    !     deallocate(TMPgrd)
    !     deallocate(Ugrd)
    !     deallocate(Vgrd)
    !     deallocate(Wgrd)
    !     deallocate(RHgrd)
    !     deallocate(VAPMRgrd)
    !     deallocate(CLDMRgrd)
    !     deallocate(RAINMRgrd)
    !     deallocate(ICEMRgrd)
    !     deallocate(SNOWMRgrd)
    !     deallocate(GRPMRgrd)
    !     deallocate(WSgrd)
    !     deallocate(WDgrd)
        

    !     call MPI_Finalize(ierror)

    !     !write(io3d,fmt9) 
    ! end subroutine

    ! single file processing
    subroutine writeRec10
        
        integer t,ijuldy,iyr,ijul,ihr,nhrinc,imo,iday,status
        integer k,i,j,i1,j1,k1
        integer BYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,IBYRM
        integer :: numPoints

        integer omp_rank,omp_rank1
        real tm1,tm2,tm3,tm4

        
        

        !'(I4,3I2.2,2I3,F7.1,F5.2,I2,3F8.1,F8.2,3F8.1)'
        !MYR,MMO,MDAY,MHR,IX,JX,PRES,RAIN,SC,RADSW,RADLW,T2,Q2,WD10,WS10,SST)

        integer :: MYR,MMO,MDAY,MHR,IX,JX,SC
        real(kind=8) :: PRES,RAIN,RADSW,RADLW,T2,Q2,WD10,WS10,SST

        !PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
        !'(I4,I6,F6.1,I4,F5.1,F6.2,I3,F5.2,5F6.3)'

        integer :: PRES2,Z,WD,RH
        real(kind=8) :: TEMPK,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR,VAPMR,WS,W

        !real(kind=8), parameter :: PI = 3.1415926535897932384626433
        real(kind=8), parameter :: PI = 3.141592653589793115997963468544185161590576171875
        real(kind=8), parameter :: D2PI = 360.0

        real(kind=8), dimension(:), allocatable       :: values1d 
        !real(kind=8), dimension(:,:), allocatable       :: PRESgrd 
        real(kind=8), dimension(:,:), allocatable :: PRESgrd
        real(kind=8), dimension(:,:), allocatable :: RAINgrd
        real(kind=8), dimension(:,:), allocatable :: RADSWgrd
        real(kind=8), dimension(:,:), allocatable :: RADLWgrd
        real(kind=8), dimension(:,:), allocatable :: T2grd
        real(kind=8), dimension(:,:), allocatable :: Q2grd
        real(kind=8), dimension(:,:), allocatable :: U10grd
        real(kind=8), dimension(:,:), allocatable :: V10grd
        real(kind=8), dimension(:,:), allocatable :: WS10grd
        real(kind=8), dimension(:,:), allocatable :: WD10grd

        real(kind=8), dimension(:,:), allocatable :: values2d
        real(kind=8), dimension(:,:,:), allocatable :: HGTgrd
        real(kind=8), dimension(:,:,:), allocatable :: TMPgrd
        real(kind=8), dimension(:,:,:), allocatable :: Ugrd
        real(kind=8), dimension(:,:,:), allocatable :: Vgrd
        real(kind=8), dimension(:,:,:), allocatable :: Wgrd
        real(kind=8), dimension(:,:,:), allocatable :: RHgrd
        real(kind=8), dimension(:,:,:), allocatable :: VAPMRgrd
        real(kind=8), dimension(:,:,:), allocatable :: CLDMRgrd
        real(kind=8), dimension(:,:,:), allocatable :: RAINMRgrd
        real(kind=8), dimension(:,:,:), allocatable :: ICEMRgrd
        real(kind=8), dimension(:,:,:), allocatable :: SNOWMRgrd
        real(kind=8), dimension(:,:,:), allocatable :: GRPMRgrd
        real(kind=8), dimension(:,:,:), allocatable :: WSgrd
        real(kind=8), dimension(:,:,:), allocatable :: WDgrd

        
    
        character(len=42), allocatable:: date
        integer, dimension(:), allocatable :: gids1
        integer ::arr_shape(2), arr_order(2)



        date = startDate
        arr_shape = (/Nj,Ni/)
        arr_order = (/2,1/)
        SC = 0
        SST = 0
        para_io = 0
        iorec9 = 47
        mul_io = 1
        if(mul_io==1) then
            if(mpi_rank<10) then
                write(iorec9name(6:6),'(I1)') mpi_size
                write(iorec9name(12:12),'(I1)') mpi_rank
                print *, iorec9name
            else
                stop 'MPI SIZE .GT. 10 NOT YET HANDLE !'
            endif
            open(unit=iorec9, file=iorec9name)
        endif

        allocate(precedeRAINgrd(Nj,Ni))
        

        allocate(PRESgrd(Nj,Ni))
        allocate(RAINgrd(Nj,Ni))
        allocate(RADSWgrd(Nj,Ni))
        allocate(RADLWgrd(Nj,Ni))
        allocate(T2grd(Nj,Ni))
        allocate(Q2grd(Nj,Ni))
        allocate(U10grd(Nj,Ni))
        allocate(V10grd(Nj,Ni))
        allocate(WS10grd(Nj,Ni))
        allocate(WD10grd(Nj,Ni))

        allocate(HGTgrd(Nj,Ni,NZ))
        allocate(TMPgrd(Nj,Ni,NZ))
        allocate(Ugrd(Nj,Ni,NZ))
        allocate(Vgrd(Nj,Ni,NZ))
        allocate(Wgrd(Nj,Ni,NZ))
        allocate(RHgrd(Nj,Ni,NZ))
        allocate(VAPMRgrd(Nj,Ni,NZ))
        allocate(CLDMRgrd(Nj,Ni,NZ))
        allocate(RAINMRgrd(Nj,Ni,NZ))
        allocate(ICEMRgrd(Nj,Ni,NZ))
        allocate(SNOWMRgrd(Nj,Ni,NZ))
        allocate(GRPMRgrd(Nj,Ni,NZ))
        allocate(WSgrd(Nj,Ni,NZ))
        allocate(WDgrd(Nj,Ni,NZ))

        date = startDate
        read(date(1:4),*) IBYRM
        read(date(5:6),*) IBMOM
        read(date(7:8),*) IBDYM
        read(date(12:13),*) IBHRM
        !print *, 'debug2'
        ! IBHRM = 0
        ! iyr = IBYRM
        ! imo = IBMOM
        ! iday = IBDYM
        ! ihr = IBHRM

        !print *, 'debug3'
        allocate(values1d(Ni*Nj))
        !return
        !call omp_set_nested(.true.)
        !call omp_set_dynamic(.true.)

        
        write(*,*) "Processing file ",trim(input_g2file)
        ! call julday(IBYRM,IBMOM,IBDYM,ijuldy)
        ! call incr(IBYRM,ijuldy,IBHRM,t-1)
        ! call grday(IBYRM,ijuldy,IBMOM,IBDYM)
        ! date = startDate

        ! read(date(1:4),*) IBYRM
        ! read(date(5:6),*) IBMOM
        ! read(date(7:8),*) IBDYM

        MYR = IBYRM
        MMO = IBMOM
        MDAY = IBDYM
        MHR = IBHRM
        !write(*,*) MYR,MMO,MDAY,MHR
            
        !write(*,*) iyr,ijuldy,imo,iday
        call get_rrain

        call grib_open_file(ifile,trim(input_g2file),'r',status)
        call codes_grib_multi_support_on(status)
        call grib_count_in_file(ifile,mcount)
        allocate(gids1(mcount))

        ! $omp parallel do shared(gids1,mcount) private(status)&
        ! $omp firstprivate(ifile)
        !tm1=omp_get_wtime()
        do i = 1, mcount 
            call codes_grib_new_from_file(ifile,gids1(i),status)	
        enddo
        !tm2=omp_get_wtime()
        !print *, tm2-tm1
        ! $omp end parallel do

        call grib_close_file(ifile,status)

        

        !print *, 'gidPRES(1)',gidPRES(1)

        !call codes_get_size(gidPRES(1),'values',numPoints)   
        !allocate(PRESgrd(Nj,Ni))
        !print *, 'lfirst_file',lfirst_file
        !PRESgrd = 
        tm1=omp_get_wtime()
        !$omp parallel private(omp_rank,values1d) num_threads(8) 
        omp_rank = omp_get_thread_num()
        !print *, 'my_id: ',omp_rank
        if(omp_rank==0) then
            if(l2dPRES) then
                call codes_get(gidPRES(1),'values',values1d)   
                PRESgrd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif

        endif

        if(omp_rank==1) then
        call codes_get(gidRAIN(1),'values',values1d)
        RAINgrd = RESHAPE(values1d,arr_shape, order=arr_order)
        if(.not. lfirst_file) then
            print *, sum(RAINgrd),sum(precedeRAINgrd)
            RAINgrd = RAINgrd - precedeRAINgrd
            print *, sum(RAINgrd),sum(precedeRAINgrd)
        endif
        endif

        if(omp_rank==2) then
        call codes_get(gidRADSW(1),'values',values1d)
        RADSWgrd = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(omp_rank==3) then
        call codes_get(gidRADLW(1),'values',values1d)
        RADLWgrd = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(omp_rank==4) then
        call codes_get(gidT2(1),'values',values1d)
        T2grd = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(omp_rank==5) then
        call codes_get(gidQ2(1),'values',values1d)
        Q2grd = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(omp_rank==6) then
        call codes_get(gidU10(1),'values',values1d)
            U10grd = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(omp_rank==7) then
        call codes_get(gidV10(1),'values',values1d)
        V10grd = RESHAPE(values1d,arr_shape, order=arr_order)
        endif
        !$omp end parallel
        print *, 'vn: ',sum(V10grd),sum(U10grd)
        tm2=omp_get_wtime()

        print *, 'omp1', tm2-tm1

        ! WS10grd = U10grd**2+V10grd**2
        ! WD10grd = atan2(V10grd,U10grd)
        ! WD10grd = WD10grd * (180.0/PI)
        ! WD10grd = WD10grd + 180.0
        ! WD10grd = -WD10grd
        ! WD10grd = WD10grd + 90.0
        ! WD10grd = dmod(WD10grd,D2PI)+360

        ! $omp parallel do
        print *, iLatMinGRIB-NY,iLatMinGRIB-1,iLonMinGRIB+1, iLonMinGRIB+NX
        print *, NY,NX, Nj,Ni
        do j1 = iLatMinGRIB-NY,iLatMinGRIB-1
            do i1 = iLonMinGRIB+1, iLonMinGRIB+NX
                WS10grd(j1,i1) = U10grd(j1,i1)**2+V10grd(j1,i1)**2
                WD10grd(j1,i1) = atan2(V10grd(j1,i1),U10grd(j1,i1))
                WD10grd(j1,i1) = WD10grd(j1,i1) * (180.0/PI)
                WD10grd(j1,i1) = WD10grd(j1,i1) + 180.0
                WD10grd(j1,i1) = -WD10grd(j1,i1)
                WD10grd(j1,i1) = WD10grd(j1,i1) + 90.0
                WD10grd(j1,i1) = dmod(WD10grd(j1,i1),D2PI)
                if(WD10grd(j1,i1)<0) then
                    WD10grd(j1,i1) = WD10grd(j1,i1)+360
                endif
            enddo
        enddo
        ! $omp end parallel do

        print *, "WS10grd:",sum(WS10grd), sum(WD10grd)

        print *, 'omp2 begin'
        tm1 = omp_get_wtime()
        !$omp parallel private(k,values1d,tm1,tm2,omp_rank) &
        !$omp shared(arr_order,arr_shape) num_threads(19)
        

        omp_rank = omp_get_thread_num()
        !print *, omp_rank+1
        k=omp_rank+1
        ! $omp do 
        !do k =1 , NZ
            ! $omp parallel private(values1d,tm1,tm2,omp_rank1) &
            ! $omp shared(arr_order,arr_shape)&
            ! $omp num_threads(3)
            
            !omp_get_num_threads(3)
            ! $omp sections 
            !omp_rank1 = omp_get_thread()
        
            !omp_rank1 = omp_get_thread_num()

                call codes_get(gidHGT(k),'values',values1d)
                HGTgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                call codes_get(gidTMP(k),'values',values1d)
                TMPgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                call codes_get(gidU(k),'values',values1d)
                Ugrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                call codes_get(gidV(k),'values',values1d)
                Vgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                
                call codes_get(gidW(k),'values',values1d)
                Wgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                call codes_get(gidRH(k),'values',values1d)
                RHgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                call codes_get(gidVAPMR(k),'values',values1d)
                VAPMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                
                call codes_get(gidCLDMR(k),'values',values1d)
                CLDMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                
                call codes_get(gidRAINMR(k),'values',values1d)
                RAINMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)

                call codes_get(gidICEMR(k),'values',values1d)
                ICEMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                
                call codes_get(gidSNOWMR(k),'values',values1d)
                SNOWMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                
                call codes_get(gidGRPMR(k),'values',values1d)
                GRPMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
                

        
        
        !$omp end parallel
        tm2 = omp_get_wtime()
        print*, 'rank:',omp_rank, 'time: ', tm2-tm1
        !WSgrd = sqrt(Ugrd**2+Vgrd**2)


        !print *, 'debug5',NY,NX,NZ
        !$omp parallel private(omp_rank1) num_threads(48) 
        !$omp do  
        do j1 = iLatMinGRIB-NY,iLatMinGRIB-1
            do i1 = iLonMinGRIB+1, iLonMinGRIB+NX
                do k1 = 1,NZ                       
                    WDgrd(j1,i1,k1) = atan2(Vgrd(j1,i1,k1),Ugrd(j1,i1,k1))
                    WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1)*(180/PI)
                    WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1) + 180.0                       
                    WDgrd(j1,i1,k1) = -WDgrd(j1,i1,k1)
                    WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1)+90.0
                    WDgrd(j1,i1,k1) = dmod(WDgrd(j1,i1,k1),D2PI)
                    if(WDgrd(j1,i1,k1)<0) then
                        WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1) + 360
                    endif
                    WSgrd(j1,i1,k1) = sqrt(Ugrd(j1,i1,k1)**2+Vgrd(j1,i1,k1)**2)
                enddo
            enddo           
        enddo
        !$omp end do
        !$omp end parallel 


        tm1=omp_get_wtime()


        do j = 1, NY
            JX = j
            do i = 1, NX
                IX = i
                PRES = PRESgrd(iLatMinGRIB-j,iLonMinGRIB+i) * 0.01 
                !sea level pressure (hPa)
                RAIN = RAINgrd(iLatMinGRIB-j,iLonMinGRIB+i) * 0.01 
                !total rainfall
                RADSW = RADSWgrd(iLatMinGRIB-j,iLonMinGRIB+i) 
                !Short wave radiation at the surface (W/m**2)
                RADLW = RADLWgrd(iLatMinGRIB-j,iLonMinGRIB+i) 
                !long wave radiation at the top (W/m**2)
                T2 = T2grd(iLatMinGRIB-j,iLonMinGRIB+i) 
                !Air temperature at 2 m (K)
                Q2 = Q2grd(iLatMinGRIB-j,iLonMinGRIB+i) *1000 
                !Specific humidity at 2 m (g/kg)
                WD10 = WD10grd(iLatMinGRIB-j,iLonMinGRIB+i) 
                !Wind direction of 10-m wind (m/s)
                WS10 = WS10grd(iLatMinGRIB-j,iLonMinGRIB+i) 
                !Wind speed of

                if(mul_io==1) then 
                    write(io3d,fmt9) MYR,MMO,MDAY,MHR,IX,JX,PRES,RAIN,SC,RADSW,RADLW,T2,Q2,WD10,WS10,SST
                endif

                do k = 1, NZ
                    PRES2 = levsIncl(k)  
                    ! Pressure (mb)
                    Z = int(HGTgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
                    !Elevation (m above sea level)
                    TEMPK=int(TMPgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
                    !Temperature (Kelvin)
                    WD=int(WDgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
                    !Wind direction (degrees)
                    WS=WSgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) 
                    !Wind speed (m/s)
                    W=Wgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) 
                    !Vertical velocity (m/s)
                    RH=int(RHgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) 
                    !Relative humidity (%)
                    VAPMR=VAPMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) *1000 
                    !Vapor mixing ratio (g/kg)
                    CLDMR=CLDMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * 1000 
                    !Cloud mixing ratio (g/kg)
                    RAINMR=RAINMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * 1000 
                    !Rain mixing ratio (g/kg)
                    ICEMR=ICEMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * 1000
                    !Ice mixing ratio (g/kg)
                    SNOWMR=SNOWMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) *1000 
                    !Snow mixing ratio (g/kg)
                    GRPMR=GRPMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) *1000 
                    !Graupel mixing ratio (g/kg)

                    if(mul_io==1) then 
                        write(io3d,fmt9_1)PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
                    endif
                    !write(*,*) PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
                enddo
            enddo
        enddo

    
        do i1 = 1, mcount
            call grib_release(i1)
        enddo


        deallocate(gids1)
        !deallocate(values)
        !deallocate(PRESgrd)
        
    !deallocate(values1d)
        

        deallocate(PRESgrd)
        deallocate(RAINgrd)
        deallocate(RADSWgrd)
        deallocate(RADLWgrd)
        deallocate(T2grd)
        deallocate(Q2grd)
        deallocate(U10grd)
        deallocate(V10grd)
        deallocate(WS10grd)
        deallocate(WD10grd)

        deallocate(HGTgrd)
        deallocate(TMPgrd)
        deallocate(Ugrd)
        deallocate(Vgrd)
        deallocate(Wgrd)
        deallocate(RHgrd)
        deallocate(VAPMRgrd)
        deallocate(CLDMRgrd)
        deallocate(RAINMRgrd)
        deallocate(ICEMRgrd)
        deallocate(SNOWMRgrd)
        deallocate(GRPMRgrd)
        deallocate(WSgrd)
        deallocate(WDgrd)
        

        !call MPI_Finalize(ierror)

        !write(io3d,fmt9) 
    end subroutine

    subroutine get_rrain()
        !use variables
        use eccodes
        integer ifile,status,mcount,i1
        real(kind=8), dimension(:),allocatable:: values1d
        integer , dimension(:),allocatable:: gids1
        !print *, input_pg2file
        call grib_open_file(ifile,trim(input_pg2file),'r',status)
        call codes_grib_multi_support_on(status)
        call grib_count_in_file(ifile,mcount)
        allocate(gids1(mcount))
        allocate(values1d(Ni*Nj))

        ! $omp parallel do shared(gids1,mcount) private(status)&
        ! $omp firstprivate(ifile)
        !tm1=omp_get_wtime()
        do i = 1, mcount 
            call codes_grib_new_from_file(ifile,gids1(i),status)	
        enddo
        !tm2=omp_get_wtime()
        !print *, tm2-tm1
        ! $omp end parallel do

        call grib_close_file(ifile,status)

        call codes_get(gidRAIN(1),'values',values1d)
        precedeRAINgrd = RESHAPE(values1d,(/Nj,Ni/), order=(/2,1/))
        print *, sum(precedeRAINgrd)
        do i1 = 1, mcount
            call grib_release(i1)
        enddo
    end subroutine
    


end module write_rec
