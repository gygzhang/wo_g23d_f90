
module parse_args
    !implicit none
    !use params
    use variables

    
    
    !save
contains

    subroutine get_args()
        
        integer i,j,k
        !contains
        argc=iargc()
        if(argc .eq. 0) then
            write(*,*) './grapes23ddat:  missing option'
            write(*,*) 'option usage:'
            write(*,*) '    -i /where/file/in/filename.grib2     [input grib2 file]'
            write(*,*) '    -o /where/file/in/out3d.dat          [output 3d file]'
            write(*,*) '    -c /where/file/in/filename.cfg       [config file]'
            write(*,*) '    -d YYYYMMDDHHTTT                     [start date]'
            write(*,*) '        This option specify the date of the grib2 file '
            write(*,*) '        specified by -i option(which you want to process)'
            write(*,*) '    -p /where/file/in/filename.grib2     [file precede input file]'
            write(*,*) '        This option use to specify the file just precede one '
            write(*,*) '        forecast-time the file specified by -i. If -p is omitted,'
            write(*,*) '         it means the file -i specified is the first-time file '
            write(*,*) '    -b /where/file/in/filename.grib2     [input base grib2 file]'
            write(*,*) '        This option use to specify the name of base grib2 file '
            write(*,*) '        which contains header information'
            write(*,*) '    --k                                   [output header logic var]'
            write(*,*) '        This option use to specify the condition on if output '
            write(*,*) '        header, if -k used, then output header, start-forecastpoint file only'
            write(*,*) '    --f                                   [start-forecast point file]'
            write(*,*) '        This option use to specify the condition on if the file  '
            write(*,*) '        specified is start-forecast point file, if --f used, then output header,'
            write(*,*) '         start-forecast point file only    '
            write(*,*) '    -h/--help                             [help]'
            write(*,*) '        show this pages '
            write(*,*)
            write(*,*) 'usage case:'

            write(*,*) 'process the file which is not the start-forecast point file (only write data)'
            write(*,*) ' > ./grapes23ddat -i /home/g2data/rmf.hgra.2019101706001.grb2 -o /home/output3d/3d1.dat&
                          -d 2019101706001  -p /home/g2data/rmf.hgra.2019101706000.grb2   -c grapes23ddat.cfg'
            
            write(*,*)
            write(*,*) 'process the file which is the start-forecast point file (write header and data)'
            
            write(*,*) ' > ./grapes23ddat -i /home/g2data/rmf.hgra.2019101706000.grb2  -o /home/output3d/3d1.dat&
                          -d 2019101706000 --k --f -c grapes23ddat.cfg'
            ! write(*,*) ' -e YYYYMMDDHH                    [end date]'
            ! write(*,*) '     If -e option is omitted, means you just want to process'
            ! write(*,*) '     only the file with start date'
           
            write(*,*) ''
            stop
        endif
        i=0
        lheader = .false.
        lfirst_file = .false.
        do while(i<=argc)
            call getarg(i,argv)
            i = i + 1
            !print *, i,argv
            select case(argv)
            case('-i','-I')
                call getarg(i,argv)
                read(argv,'(a)') input_g2file
            case('-o','-O')
                call getarg(i,argv)
                read(argv,'(a)') output_3dfile
            case('-c','-C')
                call getarg(i,argv)
                read(argv,'(a)') cfg_file
            case('-d','-D')
                call getarg(i,argv)
                read(argv,'(a)') startDate
            case('-b','-B')
                call getarg(i,argv)
                read(argv,'(a)') input_g2bfile
            case('-p','-P')
                call getarg(i,argv)
                read(argv,'(a)') input_pg2file
            case('--k','--K')           
                lheader = .true.
            case('--f','--F')           
                lfirst_file = .true.

            case('--help','-h')           
                write(*,*) './grapes23ddat help:'
                write(*,*) '    -i /where/file/in/filename.grib2     [input grib2 file]'
                write(*,*) '    -o /where/file/in/out3d.dat          [output 3d file]'
                write(*,*) '    -c /where/file/in/filename.cfg       [config file]'
                write(*,*) '    -d YYYYMMDDHHTTT                     [start date]'
                write(*,*) '        This option specify the date of the grib2 file '
                write(*,*) '        specified by -i option(which you want to process)'
                write(*,*) '    -p /where/file/in/filename.grib2     [file precede input file]'
                write(*,*) '        This option use to specify the file just precede one '
                write(*,*) '        forecast-time the file specified by -i. If -p is omitted,'
                write(*,*) '         it means the file -i specified is the first-time file '
                write(*,*) '    -b /where/file/in/filename.grib2     [input base grib2 file]'
                write(*,*) '        This option use to specify the name of base grib2 file '
                write(*,*) '        which contains header information'
                write(*,*) '    --k                                   [output header logic var]'
                write(*,*) '        This option use to specify the condition on if output '
                write(*,*) '        header, if -k used, then output header, start-forecastpoint file only'
                write(*,*) '    --f                                   [start-forecast point file]'
                write(*,*) '        This option use to specify the condition on if the file  '
                write(*,*) '        specified is start-forecast point file, if --f used, then output header,'
                write(*,*) '         start-forecast point file only    '
                write(*,*) '    -h/--help                             [help]'
                write(*,*) '        show this pages '
            
                write(*,*)
                write(*,*) 'usage case:'

                write(*,*) 'process the file which is not the start-forecast point file (only write data)'
                write(*,*) ' > ./grapes23ddat -i /home/g2data/rmf.hgra.2019101706001.grb2 -o /home/output3d/3d1.dat&
                            -d 2019101706001  -p /home/g2data/rmf.hgra.2019101706000.grb2   -c grapes23ddat.cfg'
                
                write(*,*)
                write(*,*) 'process the file which is the start-forecast point file (write header and data)'
                
                write(*,*) ' > ./grapes23ddat -i /home/g2data/rmf.hgra.2019101706000.grb2  -o /home/output3d/3d1.dat&
                            -d 2019101706000 --k --f -c grapes23ddat.cfg'
                stop
            end select
            !i = i + 1 
        enddo
        !if(trim(input_pg2file) .eq. trim())

        ! do while(argc .gt.0)
        !     call getarg(argc,argv)
        !     print *, argc,argv
        !     select case(argv(1:2))
        !     case('-i','-I')
        !         read(argv(4:),'(a)') input_g2file
        !     case('-o','-O')
        !         read(argv(4:),'(a)') output_3dfile
        !     case('-c','-C')
        !         read(argv(4:),'(a)') cfg_file
        !     case('-d','-D')
        !         read(argv(4:),'(a)') startDate
        !     case('-b','-B')
        !         read(argv(4:),'(a)') input_g2bfile
        !     end select
        !     argc = argc -1
        ! enddo
        ! print *, len(trim(input_g2file)), len(input_g2file)
        ! print *, len(trim(output_3dfile)), len(output_3dfile)
        ! print *, len(trim(cfg_file)), len(cfg_file)
        ! print *, len(trim(startDate)), len(startDate)
        ! print *, len(trim(input_g2bfile)), len(input_g2bfile)


    end subroutine


end module parse_args