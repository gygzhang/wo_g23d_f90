

 !!!!!!!!
!   export LD_LIBRARY_PATH=/gpfs/home/stu6/app/eccodes_install/lib:/gpfs/home/stu6/app/openjpeg_install/lib
!   2 #export LD_LIBRARY_PATH=/gpfs/home/stu6/app/openjpeg_install/lib
!   3 gfortran -I/gpfs/home/stu6/app/eccodes_install/include  -L/gpfs/home/stu6/app/eccodes_install/lib -leccodes -leccodes_    f90 -L/gpfs/home/stu6/app/openjpeg_install/lib -lopenjp2 -lopenjp2   apidump_cpoy.f90 -o test1
!   4 ./test1 

program grapes23ddat
    !use params
    use variables
    use eccodes
    use grib_api
    use write_rec
    use parse_args
    use config_utils

    implicit none

    integer i,j ,k,idx
    
    ! integer mpi_rank,mpi_size,ierror,mpi_work, para_io, mul_io
    !     INTEGER :: request,recv_b, io3dpos, sk_ret

    
    

    levsIncl = (/1000,975,950,925,900,850,800,750,700,650,600,550,500,450,400,350,300,200,100/)
    gidPRES = -1
    gidRAIN = -1
    gidRADSW = -1
    gidRADLW = -1
    gidT2 = -1
    gidQ2 = -1
    gidU10 = -1
    gidV10 = -1
    gidSC = -1
    gidSST = -1
    gidHGT = -1
    gidTMP = -1
    gidU = -1
    gidV  = -1
    gidW = -1
    gidRH = -1
    gidVAPMR = -1
    gidCLDMR = -1
    gidRAINMR = -1
    gidICEMR = -1
    gidSNOWMR = -1
    gidGRPMR =-1

    call get_args()
    io3dname = output_3dfile
    call read_cfg

    allocate(max_squence(numlevel))
    !! get the idx of levels by value from high to low
    !! 1000 500 800 700 will return 1,4,2,3
    call get_squence_max2min
    
    open(unit=io3d,file=io3dname,status='unknown')
    
    len_fprefix = len(filePrefix)
    len_startDate = len(startDate)
    len_fileSuffix = len(fileSuffix)

    
    call grib_open_file(ifile,input_g2file,'r',status)
    call codes_grib_multi_support_on(status)
    call grib_count_in_file(ifile,mcount)
    allocate(gids(mcount))
    do i = 1, mcount
	    call codes_grib_new_from_file(ifile,gids(i),status)	
    enddo

    call grib_close_file(ifile,status)

    allocate(levels(mcount))
    allocate(varParCat(mcount))
    allocate(varNames(mcount))
    allocate(varParNum(mcount))
    allocate(prodDefinTemNum(mcount))

    do i = 1, mcount
        gid = gids(i)
        call codes_get(gid,'shortName',varNames(i),status)
        call codes_get(gid,'level',levels(i),status)
        call codes_get(gid,'parameterCategory',varParCat(i),status)
        call codes_get(gid,'parameterNumber',varParNum(i),status)
        call codes_get(gid,'productDefinitionTemplateNumber',prodDefinTemNum(i),status)
    enddo

    do j =1, size(varNames)
        if(trim(varNames(j))=='prmsl') then
            gidPRMSL = j
        endif
    enddo

    ! assume all variables exist
    l2dPRES = .true.
    l2dRAIN = .true.
    l2dRADSW = .true.
    l2dRADLW = .true.
    l2dT2 = .true.
    l2dQ2 = .true.
    l2dU10 = .true.
    l2dV10 = .true.

    l3dHTG = .true.
    l3dTMP = .true.
    l3dU = .true.
    l3dV = .true.
    l3dW = .true.
    l3dRH = .true.
    l3dVAPMR = .true.
    l3dCLDMR = .true.
    l3dRAINMR = .true.
    l3dICEMR = .true.
    l3dSNOWMR = .true.
    l3dGRPMR = .true.
    l2dSC = .true.
    l2dSST = .true.

    do i = 1, mcount
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 2D !!!!!!!!!!!!!!!!!!!!!!!
        if(varParCat(i) == paracat2d(1) .and. varParNum(i) == paranum2d(1) .and. levels(i) == level2d(1)) then
            gidPRES(count(gidPRES/=-1) + 1) = i
        endif
        
        if (varParCat(i) == paracat2d(2) .and. varParNum(i) == paranum2d(2) .and. levels(i) == level2d(2)) then
            gidRAIN(count(gidRAIN/=-1) + 1) = i
        endif
        
        if (varParCat(i) == paracat2d(3) .and. varParNum(i) == paranum2d(3) .and. levels(i) == level2d(3)) then
            gidRADSW(count(gidRADSW/=-1) + 1) = i
        endif
        
        if (varParCat(i) == paracat2d(4) .and. varParNum(i) == paranum2d(4) .and. levels(i) == level2d(4)) then
            gidRADLW(count(gidRADLW/=-1) + 1) = i
        endif
        
        if (varParCat(i) == paracat2d(5) .and. varParNum(i) == paranum2d(5) .and. levels(i) == level2d(5)) then
            gidT2(count(gidT2/=-1) + 1) = i
        endif
        
        if (varParCat(i) == paracat2d(6) .and. varParNum(i) == paranum2d(6) .and. levels(i) == level2d(6)) then
            gidQ2(count(gidQ2/=-1) + 1) = i
        endif
        
        if (varNames(i) == shortName2d(7) .and. varParCat(i) == paracat2d(7) .and. varParNum(i) == paranum2d(7) &
            .and. prodDefinTemNum(i) == prodnum2d(7) .and. levels(i) == level2d(7)) then
            gidU10(count(gidU10/=-1) + 1) = i
        endif
        
        if (varNames(i) == shortName2d(8) .and. varParCat(i) == paracat2d(8) .and. varParNum(i) == paranum2d(8) &
            .and. prodDefinTemNum(i) == prodnum2d(8) .and. levels(i) == level2d(8)) then
            gidV10(count(gidV10/=-1) + 1) = i
        endif

        if (varNames(i) == shortName2d(9) .and. varParCat(i) == paracat2d(9) .and. varParNum(i) == paranum2d(9) &
            .and. prodDefinTemNum(i) == prodnum2d(9) .and. levels(i) == level2d(9)) then
            gidSC(count(gidSC/=-1) + 1) = i
        endif
        
        if (varNames(i) == shortName2d(10) .and. varParCat(i) == paracat2d(10) .and. varParNum(i) == paranum2d(10) &
            .and. prodDefinTemNum(i) == prodnum2d(10) .and. levels(i) == level2d(10)) then
            gidSST(count(gidSST/=-1) + 1) = i
        endif

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 3D !!!!!!!!!!!!!!!!!!!!!!!

        if (varParCat(i) == paracat3d(1) .and. varParNum(i) == paranum3d(1).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidHGT(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(2) .and. varParNum(i) == paranum3d(2).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidTMP(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(3) .and. varParNum(i) == paranum3d(3).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidU(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(4) .and. varParNum(i) == paranum3d(4).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidV(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(5) .and. varParNum(i) == paranum3d(5).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidW(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(6) .and. varParNum(i) == paranum3d(6).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidRH(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(7) .and. varParNum(i) == paranum3d(7).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidVAPMR(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(8) .and. varParNum(i) == paranum3d(8) .and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidCLDMR(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(9) .and. varParNum(i) == paranum3d(9) .and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidRAINMR(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(10) .and. varParNum(i) == paranum3d(10) .and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidICEMR(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(11) .and. varParNum(i) == paranum3d(11) .and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidSNOWMR(idx) = i
        endif
        
        if (varParCat(i) == paracat3d(12) .and. varParNum(i) == paranum3d(12).and. any(plevel(1:numlevel)==levels(i))) then
            call get_idx_by_value(levels(i),idx)
            gidGRPMR(idx) = i     
        endif

    enddo

    !if not got any element in array, missing variable occur

    !!!!!!!!!!!!!! 2D  !!!!!!!!!!!!!!!!!!!!!!
    if(count(gidPRES/=-1)==0) then 
        l2dPRES = .false.
    endif
     
    if(count(gidRAIN/=-1)==0) then 
        l2dRAIN = .false.
    endif
     
    if(count(gidRADSW/=-1)==0) then 
        l2dRADSW = .false.
    endif
     
    if(count(gidRADLW/=-1)==0) then 
        l2dRADLW = .false.
    endif
     
    if(count(gidT2/=-1)==0) then 
        l2dT2 = .false.
    endif
     
    if(count(gidQ2/=-1)==0) then 
        l2dQ2 = .false.
    endif
     
    if(count(gidU10/=-1)==0) then 
        l2dU10 = .false.
    endif
     
    if(count(gidV10/=-1)==0) then 
        l2dV10 = .false.
    endif

    if(count(gidSC/=-1)==0) then 
        l2dSC = .false.
    endif
     
    if(count(gidSST /=-1)==0) then 
        l2dSST = .false.
    endif

    !!!!!!!!!!!!!! 3D !!!!!!!!!!!!!!!!!
     
    if(count(gidHGT/=-1)==0) then 
        l3dHTG = .false.
    endif
     
    if(count(gidTMP/=-1)==0) then 
        l3dTMP = .false.
    endif
     
    if(count(gidU/=-1)==0) then 
        l3dU = .false.
    endif
     
    if(count(gidV /=-1)==0) then 
        l3dV = .false.
    endif
     
    if(count(gidW/=-1)==0) then 
        l3dW = .false.
    endif
     
    if(count(gidRH/=-1)==0) then 
        l3dRH = .false.
    endif
     
    if(count(gidVAPMR/=-1)==0) then 
        l3dVAPMR = .false.
    endif
     
    if(count(gidCLDMR/=-1)==0) then 
        l3dCLDMR = .false.
    endif
     
    if(count(gidRAINMR/=-1)==0) then 
        l3dRAINMR = .false.
    endif
     
    if(count(gidICEMR/=-1)==0) then 
        l3dICEMR = .false.
    endif
     
    if(count(gidSNOWMR/=-1)==0) then 
        l3dSNOWMR = .false.
    endif
     
    if(count(gidGRPMR /=-1)==0) then 
        l3dGRPMR = .false.
    endif

    
    
    
    call grib_get_size(gidPRMSL,'distinctLatitudes',nlats)

    call grib_get_size(gidPRMSL,'distinctLongitudes',nlons)
    

    allocate(lats(nlats))
    allocate(lons(nlons))

    
    call grib_get(gidPRMSL,'distinctLatitudes',lats)

    call grib_get(gidPRMSL,'distinctLongitudes',lons)

    call grib_get(gidPRMSL,'Ni',Ni)

    call grib_get(gidPRMSL,'Nj',Nj)


    do i = nlats, 1,-1
        if(lats(i) .ge. latMinCP) then
            iLatMinGRIB = i + 1
            exit
        endif
    enddo

    do i = nlats,1, -1
        if(lats(i) .gt. latMaxCP) then
            iLatMaxGRIB= i
            exit
        endif
    enddo

    do i = 1, nlons-1
        if(lons(i+1).ge. lonMinCP) then
            iLonMinGRIB=i
            exit
        endif
    enddo

    do i = 1, nlons -1
        if(lons(i+1).gt. lonMaxCP) then
            iLonMaxGRIB = i +1
            exit
        endif
    enddo

    NX=iLonMaxGRIB-iLonMinGRIB+1 
    NY=iLatMinGRIB-iLatMaxGRIB+1 
    
    NZ = numlevel

    
    
    do i = 1, mcount
        call grib_release(i)
    enddo

    if(lheader .eqv. .true.) then
        call writeRec1
        call writeRec2
        call writeRec3
        call writeRec4
        call writeRec5
        call writeRec6
        call writeRec7
        call writeRec8
    endif
    
    call writeRec9

    deallocate(max_squence)
    
end program grapes23ddat

subroutine get_squence_max2min
    use variables
    implicit none
    integer i,j,cnt
    do i = 1, numlevel
        cnt = 1
        do j = 1, numlevel
            if(plevel(i)<plevel(j)) then
                cnt = cnt + 1
            endif
        enddo
        max_squence(i) = cnt
    enddo
end subroutine 

subroutine get_idx_by_value(level,idx)
    use variables
    integer, intent(in):: level
    integer, intent(out):: idx
    integer i
    idx = -999
    do i = 1, numlevel
        if(plevel(i)==level) then
            idx = i 
            exit
        endif
    enddo


end subroutine






