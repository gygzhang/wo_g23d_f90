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
        
        NHRSMM5 = nfiles
        write(io3d,fmt6) IBYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,NX,NY,NZ
    end subroutine
    
    subroutine writeRec7
        integer NX1,NY1,NX2,NY2,NZ1,NZ2,s,ns
        real RXMIN,RXMAX,RYMIN,RYMAX
        real SIGMA(numlevel)
        
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
        SIGMA = plevel(1:numlevel)/1013.25
        ns = numlevel
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

    
    ! single file processing
    subroutine writeRec9
        
        integer t,ijuldy,iyr,ijul,ihr,nhrinc,imo,iday,status
        integer k,i,j,i1,j1,k1
        integer BYRM,IBMOM,IBDYM,IBHRM,NHRSMM5,IBYRM
        integer :: numPoints

        integer omp_rank,omp_rank1
        real tm1,tm2,tm3,tm4


        integer :: MYR,MMO,MDAY,MHR,IX,JX,SC
        real(kind=8) :: PRES,RAIN,RADSW,RADLW,T2,Q2,WD10,WS10,SST

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
        real(kind=8), dimension(:,:), allocatable :: SCgrd
        real(kind=8), dimension(:,:), allocatable :: SSTgrd

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
        ! if(mul_io==1) then
        !     if(mpi_rank<10) then
        !         write(iorec9name(6:6),'(I1)') mpi_size
        !         write(iorec9name(12:12),'(I1)') mpi_rank
        !         print *, iorec9name
        !     else
        !         stop 'MPI SIZE .GT. 10 NOT YET HANDLE !'
        !     endif
        !     open(unit=iorec9, file=iorec9name)
        ! endif

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
        allocate(SCgrd(Nj,Ni))
        allocate(SSTgrd(Nj,Ni))

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
        read(date(13:14),*) IBHRM
        !print *, date,date(9:11),date(13:14)
        !print *, 'debug2'
        ! IBHRM = 0
        ! iyr = IBYRM
        ! imo = IBMOM
        ! iday = IBDYM
        ! ihr = IBHRM

        !print *, 'debug3'
        allocate(values1d(Ni*Nj))

        
        write(*,*) "Processing file ",trim(input_g2file)

        MYR = IBYRM
        MMO = IBMOM
        MDAY = IBDYM
        MHR = IBHRM
        !write(*,*) MYR,MMO,MDAY,MHR
            
        !write(*,*) iyr,ijuldy,imo,iday
        !print *, 'debug1'
        if(l2dRAIN .and. .not.lheader .and..not.lfirst_file) then
            call get_rrain
        endif
        !print *, 'xxxxxx'
        call grib_open_file(ifile,trim(input_g2file),'r',status)
        call codes_grib_multi_support_on(status)
        call grib_count_in_file(ifile,mcount)
        allocate(gids1(mcount))
        !print *, 'debug1'
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


        PRES = default2d(1)
        RAIN = default2d(2)
        RADSW = default2d(3)
        RADLW = default2d(4)
        T2 = default2d(5)
        Q2 = default2d(6)
        WD10 = default2d(7)
        WS10 = default2d(8)
        SC = default2d(9)
        SST = default2d(10)

        Z = default3d(1)
        TEMPK = default3d(2)
        WD = default3d(3)
        WS = default3d(4)
        W = default3d(5)
        RH = default3d(6)
        VAPMR = default3d(7)
        CLDMR = default3d(8)
        RAINMR = default3d(9)
        ICEMR = default3d(10)
        SNOWMR = default3d(11)
        GRPMR = default3d(12)
        


        !$omp parallel private(omp_rank,values1d) num_threads(num2d) 
        omp_rank = omp_get_thread_num()
        !print *, 'my_id: ',omp_rank
        if(omp_rank==0) then
            if(l2dPRES) then
                call codes_get(gidPRES(1),'values',values1d)   
                PRESgrd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif

        endif

        if(omp_rank==1) then
            if(l2dRAIN) then
                call codes_get(gidRAIN(1),'values',values1d)
                RAINgrd = RESHAPE(values1d,arr_shape, order=arr_order)
                if(.not. lfirst_file) then
                    !print *, sum(RAINgrd),sum(precedeRAINgrd)
                    RAINgrd = RAINgrd - precedeRAINgrd
                    !print *, sum(RAINgrd),sum(precedeRAINgrd)
                endif
            endif
        endif

        if(omp_rank==2) then
            if(l2dRADSW) then
                call codes_get(gidRADSW(1),'values',values1d)
                RADSWgrd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif
        
        if(omp_rank==3) then
            if(l2dRADLW) then
                call codes_get(gidRADLW(1),'values',values1d)
                RADLWgrd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif
        
        if(omp_rank==4) then
            if(l2dT2) then
                call codes_get(gidT2(1),'values',values1d)
                T2grd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif
        
        if(omp_rank==5) then
            if(l2dQ2) then
                call codes_get(gidQ2(1),'values',values1d)
                Q2grd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif
        
        if(omp_rank==6) then
            if(l2dU10) then
                call codes_get(gidU10(1),'values',values1d)
                U10grd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif
        
        if(omp_rank==7) then
            if(l2dV10) then
                call codes_get(gidV10(1),'values',values1d)
                V10grd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif

        if(omp_rank==8) then
            if(l2dSC) then
                call codes_get(gidSC(1),'values',values1d)
                SCgrd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif

        if(omp_rank==9) then
            if(l2dSST) then
                call codes_get(gidSST(1),'values',values1d)
                SSTgrd = RESHAPE(values1d,arr_shape, order=arr_order)
            endif
        endif

        !$omp end parallel


        ! WS10grd = U10grd**2+V10grd**2
        ! WD10grd = atan2(V10grd,U10grd)
        ! WD10grd = WD10grd * (180.0/PI)
        ! WD10grd = WD10grd + 180.0
        ! WD10grd = -WD10grd
        ! WD10grd = WD10grd + 90.0
        ! WD10grd = dmod(WD10grd,D2PI)+360
        
        if(l2dU10 .and. l2dV10) then
            !$omp parallel do shared(WS10grd,U10grd,V10grd,WD10grd) collapse(1)
            do j1 = iLatMinGRIB-NY,iLatMinGRIB-1
                do i1 = iLonMinGRIB+1, iLonMinGRIB+NX
                    WS10grd(j1,i1) = sqrt(U10grd(j1,i1)**2+V10grd(j1,i1)**2)
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
            !$omp end parallel do
        endif

        !$omp parallel private(k,values1d,tm1,tm2,omp_rank) &
        !$omp shared(arr_order,arr_shape) num_threads(19)
        omp_rank = omp_get_thread_num()
        k=omp_rank+1
        !do k =1 , NZ
        if(l3dHTG) then
            call codes_get(gidHGT(k),'values',values1d)
            HGTgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dTMP) then
            call codes_get(gidTMP(k),'values',values1d)
            TMPgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dU) then
            call codes_get(gidU(k),'values',values1d)
            Ugrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dV) then
            call codes_get(gidV(k),'values',values1d)
            Vgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dW) then
            call codes_get(gidW(k),'values',values1d)
            Wgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dRH) then
            call codes_get(gidRH(k),'values',values1d)
            RHgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dVAPMR) then
            call codes_get(gidVAPMR(k),'values',values1d)
            VAPMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dCLDMR) then
            call codes_get(gidCLDMR(k),'values',values1d)
            CLDMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dRAINMR) then
            call codes_get(gidRAINMR(k),'values',values1d)
            RAINMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dICEMR) then
            call codes_get(gidICEMR(k),'values',values1d)
            ICEMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dSNOWMR) then
            call codes_get(gidSNOWMR(k),'values',values1d)
            SNOWMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif

        if(l3dGRPMR) then
            call codes_get(gidGRPMR(k),'values',values1d)
            GRPMRgrd(:,:,k) = RESHAPE(values1d,arr_shape, order=arr_order)
        endif
                

        
        
        !$omp end parallel
        tm2 = omp_get_wtime()


        if(l3dU .and. l3dV) then
            !$omp parallel private(omp_rank1) 
            !$omp do  collapse(2)
            do j1 = iLatMinGRIB-NY,iLatMinGRIB-1
                do i1 = iLonMinGRIB+1, iLonMinGRIB+NX
                    do k1 = 1,NZ                       
                        WDgrd(j1,i1,k1) = atan2(Vgrd(j1,i1,k1),Ugrd(j1,i1,k1))
                        WDgrd(j1,i1,k1) = WDgrd(j1,i1,k1)*(180.0/PI)
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
        endif


        tm1=omp_get_wtime()


        do j = 1, NY
            JX = j
            do i = 1, NX
                IX = i
                if(l2dPRES) then
                    !sea level pressure (hPa)
                    PRES = PRESgrd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(1)
                endif

                if(l2dRAIN) then
                    !total rainfall
                    RAIN = RAINgrd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(2) 
                endif

                if(l2dRADSW) then
                    !Short wave radiation at the surface (W/m**2)
                    RADSW = RADSWgrd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(3)
                endif

                if(l2dRADLW) then
                    !long wave radiation at the top (W/m**2)
                    RADLW = RADLWgrd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(4) 
                endif

                if(l2dT2) then
                    !Air temperature at 2 m (K)
                    T2 = T2grd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(5) 
                endif

                if(l2dQ2) then
                    !Specific humidity at 2 m (g/kg)
                    Q2 = Q2grd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(6) 
                endif

                if(l2dU10 .and. l2dV10) then
                    !Wind direction of 10-m wind (m/s)
                    WD10 = WD10grd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(7) 
                endif

                if(l2dU10 .and. l2dV10) then
                    !Wind speed of
                    WS10 = WS10grd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(8) 
                endif

                if(l2dSC) then
                    !Wind direction of 10-m wind (m/s)
                    SC = SCgrd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(9) 
                endif

                if(l2dSST) then
                    !Wind speed of
                    SST = WS10grd(iLatMinGRIB-j,iLonMinGRIB+i) * cnv2d(10) 
                endif
                

                 
                write(io3d,fmt9) MYR,MMO,MDAY,MHR,IX,JX,PRES,RAIN,SC,RADSW,RADLW,T2,Q2,WD10,WS10,SST
                

                do k = 1, NZ
                    if(.true.) then
                        ! Pressure (mb)
                        PRES2 = plevel(k) * cnv3d(1) 
                    endif
                    
                    if(l3dHTG) then
                        !Elevation (m above sea level)
                        Z = int(HGTgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) * cnv3d(1) 
                    endif
                    
                    if(l3dTMP) then
                        !Temperature (Kelvin)
                        TEMPK=int(TMPgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) * cnv3d(2) 
                    endif
                    
                    if(l3dU .and. l3dV) then
                        !Wind direction (degrees)
                        WD=int(WDgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) * cnv3d(3) 
                    endif
                    
                    if(l3dU .and. l3dV) then
                        !Wind speed (m/s)
                        WS=WSgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(4) 
                    endif
                    
                    if(l3dW) then
                        !Vertical velocity (m/s)
                        W=Wgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(5) 
                    endif
                    
                    if(l3dRH) then
                        !Relative humidity (%)
                        RH=int(RHgrd(iLatMinGRIB-j,iLonMinGRIB+i,k)) * cnv3d(6) 
                    endif
                    
                    if(l3dVAPMR) then
                        !Vapor mixing ratio (g/kg)
                        VAPMR=VAPMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(7) 
                    endif
                    
                    if(l3dCLDMR) then
                        !Cloud mixing ratio (g/kg)
                        CLDMR=CLDMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(8) 
                    endif
                    
                    if(l3dRAINMR) then
                        !Rain mixing ratio (g/kg)
                        RAINMR=RAINMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(9) 
                    endif
                    
                    if(l3dICEMR) then
                        !Ice mixing ratio (g/kg)
                        ICEMR=ICEMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(10)
                    endif
                    
                    if(l3dSNOWMR) then
                        !Snow mixing ratio (g/kg)
                        SNOWMR=SNOWMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(11) 
                    endif
                    
                    if(l3dGRPMR) then
                        !Graupel mixing ratio (g/kg)
                        GRPMR=GRPMRgrd(iLatMinGRIB-j,iLonMinGRIB+i,k) * cnv3d(12) 
                    endif 
                    write(io3d,fmt9_1)PRES2,Z,TEMPK,WD,WS,W,RH,VAPMR,CLDMR,RAINMR,ICEMR,SNOWMR,GRPMR
                    
                enddo
            enddo
        enddo

    
        do i1 = 1, mcount
            call grib_release(i1)
        enddo


        deallocate(gids1)
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
        deallocate(SCgrd)
        deallocate(SSTgrd)

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
        
    end subroutine

    subroutine get_rrain()
        !use variables
        use eccodes
        integer ifile,status,mcount,i1,i
        real(kind=8), dimension(:),allocatable:: values1d
        integer , dimension(:),allocatable:: gids1

        call grib_open_file(ifile,trim(input_pg2file),'r',status)
        call codes_grib_multi_support_on(status)
        call grib_count_in_file(ifile,mcount)
        
        allocate(gids1(mcount))
        
        allocate(values1d(Ni*Nj))

        do i = 1, mcount 
            call codes_grib_new_from_file(ifile,gids1(i),status)	
        enddo

        call grib_close_file(ifile,status)

        call codes_get(gidRAIN(1),'values',values1d)
        precedeRAINgrd = RESHAPE(values1d,(/Nj,Ni/), order=(/2,1/))

        do i1 = 1, mcount
            call grib_release(i1)
        enddo
        deallocate(gids1)
        deallocate(values1d)
        
    end subroutine
    


end module write_rec
