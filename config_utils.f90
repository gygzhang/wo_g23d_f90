!	Name: Guiying Zhang
!	Organization: School of Computer Science, Chengdu University of Information Technology
!	Contact: gygzhang@foxmail.com

module config_utils
    use variables
    implicit none

    contains

    subroutine make_cfg()
        integer i
        open(unit=iocfg,file=cfg_file)
        
        write(iocfg,'(a)') '&cfg'  

        !!!!!!!!!!!!!!!!!!!!!!! 2d !!!!!!!!!!!!!!!!!!!!!!!!!!
        write(iocfg,'(a)') 'num2d = 10'
        write(iocfg,'(a)') "shortName2d = 'prmsl','unknown','ssr','str','2t','q','10u','10v','sc','sst'"
        write(iocfg,'(a)') 'paracat2d = 3,1,4,5,0,1,2,2,-9,-9'
        write(iocfg,'(a)') 'paranum2d = 1,8,9,5,0,0,2,3,-9,-9'
        write(iocfg,'(a)') 'prodnum2d = -1,-1,-1,-1,-1,-1,0,0,-9,-9'
        write(iocfg,'(a)') 'cnv2d = 0.01,0.01,1.,1.,1.,1000.,1.,1.,-9,-9'
        ! total 10, the last two is for SC and SST
        write(iocfg,'(a)') 'default2d = -9,-9,-9,-9,-9,-9,-9,-9,0,0'

        write(iocfg,'(a)') "grapesname2d = 'PRES','RAIN','RADSW','RADLW','T2',&
                                           'Q2','U10','V10','SC','SST'"
        write(iocfg,'(a)') 'level2d = 0,0,0,0,2,2,10,10,-9,-9'                                   
        
        
        !!!!!!!!!!!!!!!!!!!!! 3d !!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(iocfg,'(a)') 'num3d = 12'
        write(iocfg,'(a)') "shortName3d = 'gh','t','u','v',&
                            'wz','r',' q','clwmr','rwmr','icmr','snmr','grle'"    
        write(iocfg,'(a)') 'paracat3d = 3,0,2,2,2,1,1,1,1,1,1,1'
        write(iocfg,'(a)') 'paranum3d = 5,0,2,3,9,1,0,22,24,23,25,32'
        write(iocfg,'(a)') 'prodnum3d = -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1'
        write(iocfg,'(a)') 'cnv3d = 1.,1.,1.,1.,1.,1.,1000.,1000.,&
                                    1000.,1000.,1000.,1000.'
        write(iocfg,'(a)') 'default3d = -9,-9,-9,-9,-9,-9,-9,-9,&
                                        -9,-9,-9,-9'
        write(iocfg,'(a)') "grapesname3d = 'HTG','TMP','U','V','W','RH',&
            'VAPMR','CLDMR','RAINMR','ICEMR','SNOWMR','GRPMR'"
        write(iocfg,'(a)') 'numlevel = 19'        
        write(iocfg,'(a)') 'plevel = 1000,975,950,925,900,850,800,750,700,&
                                     650,600,550,500,450,400,350,300,200,100'

        !!!!!!! latitude lontitude !!!!!!!!!!!!!!!!!!
        write(iocfg,'(a)') 'latMinCP = 35.40'
        write(iocfg,'(a)') 'latMaxCP = 42.53'
        write(iocfg,'(a)') 'lonMinCP = 112.6'
        write(iocfg,'(a)') 'lonMaxCP = 122.1'
        write(iocfg,'(a)') 'nfiles = 36'

        write(iocfg,'(a)') '/'
        
        close(iocfg)
        
        
        
    end subroutine make_cfg

    subroutine read_cfg()
        logical :: fexist        

        inquire(file=trim(cfg_file), exist=fexist)
        if(.not. fexist) then
            cfg_file = 'grapes23ddat.cfg'
            write(*,*) 'Generating a new configuration: ',trim(cfg_file)
            call make_cfg
        else
            write(*,*) 'Using an existing configuration: ', trim(cfg_file)
        endif

        open(iocfg,file=trim(cfg_file))
        read(iocfg,NML=cfg)
        close(iocfg)
    end subroutine
end module
