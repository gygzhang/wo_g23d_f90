module dateutil
    implicit none

    contains

    subroutine inc_datetime()
        ! IBYRM = int(date[0:4])  
        ! IBMOM = int(date[4:6])  
        ! IBDYM = int(date[6:8]) 
        ! IBHRM = int(date[8:10])

    end 

    subroutine grday(iyr,ijul,imo,iday)
        !----------------------------------------------------------------------
        !
        ! --- CALUTILS   Version: 7.0.0    Level: 000602                  GRDAY
        !                J. Scire, SRC
        !
        ! --- PURPOSE:  Compute the Gregorian date (month, day) from the
        !               Julian day
        !
        ! --- UPDATE
        ! ---               000602  (DGS): YYYY format for year
        !
        ! --- INPUTS:
        !            * - integer      - Unit number for list file output
        !           IYR - integer      - Year
        !          IJUL - integer      - Julian day
        !
        ! --- OUTPUT:
        !           IMO - integer      - Month
        !          IDAY - integer      - Day
        !
        ! --- GRDAY called by:  host subroutines
        ! --- GRDAY calls:      none
        !----------------------------------------------------------------------
            integer iyr,ijul,imo,iday
            integer ileap, nleft,ierr,i,ninc
            integer ksec,nhrinc
        !
              integer kday(12,2)
              data kday/31,59,90,120,151,181,212,243,273,304,334,365,&
                        31,60,91,121,152,182,213,244,274,305,335,366/
        !
        !
              ileap=1
              if(mod(iyr,4).eq.0)ileap=2
              if(ijul.lt.1.or.ijul.gt.kday(12,ileap))go to 11
        !
              do 10 i=1,12
              if(ijul.gt.kday(i,ileap))go to 10
              imo=i
              iday=ijul
              if(imo.ne.1)iday=ijul-kday(imo-1,ileap)
              return
        10    continue
        !
        11    continue
              write(*,12)iyr,ijul
        12    format(//2x,'ERROR in SUBR. GRDAY -- invalid Julian day '//2x,&
               'iyr = ',i5,3x,'ijul = ',i5)
              write(*,*)
              stop 'Halted in GRDAY -- see list file.'
              end

    subroutine julday(iyr,imo,iday,ijuldy)
        !----------------------------------------------------------------------
!
! --- CALUTILS   Version: 7.0.0    Level: 000602                 JULDAY
! ---            J. Scire, SRC
!
! --- PURPOSE:  Compute the Julian day number from the Gregorian
!               date (month, day)
!
! --- UPDATE
! ---               000602  (DGS): YYYY format for year
!
! --- INPUTS:
!            IO - integer      - Unit number for list file output
!           IYR - integer      - Year
!           IMO - integer      - Month
!          IDAY - integer      - Day
!
! --- OUTPUT:
!          IJUL - integer      - Julian day
!
! --- JULDAY called by:  host subroutines
! --- JULDAY calls:      none
!----------------------------------------------------------------------
        integer iyr,imo,iday,ijuldy
        integer ileap, nleft,ierr,i,ninc
        integer ksec,nhrinc
              integer kday(12)
              data kday/0,31,59,90,120,151,181,212,243,273,304,334/
        !
        ! --- Check for valid input data
              ierr=0
        ! --- Check for valid month
              if(imo.lt.1.or.imo.gt.12)ierr=1
        ! --- Check for valid day in 30-day months
              if(imo.eq.4.or.imo.eq.6.or.imo.eq.9.or.imo.eq.11)then
                 if(iday.gt.30)ierr=1
              else if(imo.eq.2)then
                 if(mod(iyr,4).eq.0)then
        ! ---       February in a leap year
                    if(iday.gt.29)ierr=1
                 else
        ! ---       February in a non-leap year
                    if(iday.gt.28)ierr=1
                 endif
              else
        ! ---    Check for valid day in 31-day months
                 if(iday.gt.31)ierr=1
              endif
        !
              if(ierr.eq.1)then
                 write(*,*)
                 write(*,*)'ERROR in SUBR. JULDAY'
                 write(*,*)'Invalid date - IYR = ',iyr,' IMO = ',&
                  imo,' IDAY = ',iday
                 write(*,*)
                 stop 'Halted in JULDAY -- see list file.'
              endif
        !
        ! --- Compute the Julian day
              ijuldy=kday(imo)+iday
              if(imo.le.2)return
              if(mod(iyr,4).EQ.0)ijuldy=ijuldy+1
        !
              return
              end

    subroutine incrs(iyr,ijul,ihr,isec,nsec)
        integer iyr,ijul,ihr,isec,nsec
        integer ileap, nleft,ierr,i,ninc
        integer ksec,nhrinc
        if(nsec.GE.0) then
            ! ---    Increment seconds
                     isec=isec+nsec
                     if(isec.GE.3600) then
                        nhrinc=isec/3600
                        isec=MOD(isec,3600)
                        call INCR(iyr,ijul,ihr,nhrinc)
                     endif
                  else
            ! ---   Decrement seconds
                     isec=isec+nsec
                     if(isec.LT.0) then
            ! ---       Earlier hour
                        ksec=-isec
                        if(ksec.GE.3600) then
            ! ---          Back up at least 1 hour
                           nhrinc=ksec/3600
                           ksec=MOD(ksec,3600)
                           nhrinc=-nhrinc
                           call INCR(iyr,ijul,ihr,nhrinc)
                        endif
                        isec=-ksec
                        if(isec.LT.0) then
            ! ---          Back up 1 more hour
                           nhrinc=-1
                           isec=3600+isec
                           call INCR(iyr,ijul,ihr,nhrinc)
                        endif
                     endif
                  endif
    end

    subroutine incr(iyr,ijul,ihr,nhrinc)
        !----------------------------------------------------------------------
        !
        ! --- CALUTILS   Version: 7.0.0    Level: 000602                   INCR
        !                J. Scire, SRC
        !
        ! --- PURPOSE:  Increment the time and date by "NHRINC" hours
        !
        ! --- UPDATE
        ! ---               000602  (DGS): add message to "stop"
        ! ---               980304  (DGS): Allow for a negative "increment" of
        !                                  up to 24 hours
        ! ---               980304  (DGS): Allow for arbitrarily large nhrinc
        !
        ! --- INPUTS:
        !       *     - integer - Unit number for list file output
        !       IYR    - integer - Current year
        !       IJUL   - integer - Current Julian day
        !       IHR    - integer - Current hour (00-23)
        !       NHRINC - integer - Time increment (hours)
        !
        !               NOTE: "NHRINC" must >= -24
        !                      Hour is between 00-23
        !
        ! --- OUTPUT:
        !       IYR    - integer - Updated year
        !       IJUL   - integer - Updated Julian day
        !       IHR    - integer - Updated hour (00-23)
        !
        ! --- INCR called by: host subroutines
        ! --- INCR calls:     none
        !----------------------------------------------------------------------
        !
        ! --- Check nhrinc
              integer iyr,ijul,ihr
              integer ileap, nleft,ierr,i,ninc
              integer ksec,nhrinc
              write(*,*) 'nhrinc',nhrinc
        ! --- Save increment remaining (needed if nhrinc > 8760)
              nleft=nhrinc
        !
        ! --- Process change in hour
              if(nhrinc.gt.0)then
        !
        10       ninc=MIN0(nleft,8760)
                 nleft=nleft-ninc
        !
        ! ---    Increment time
                 ihr=ihr+ninc
                 if(ihr.le.23)return
        !
        ! ---    Increment day
                 ijul=ijul+ihr/24
                 ihr=mod(ihr,24)
        !
        ! ---    ILEAP = 0 (non-leap year) or 1 (leap year)
                 if(mod(iyr,4).eq.0)then
                    ileap=1
                 else
                    ileap=0
                 endif
        !
                 if(ijul.gt.365+ileap) then
        ! ---       Update year
                    iyr=iyr+1
                    ijul=ijul-(365+ileap)
                 endif
        !
        ! ---    Repeat if more hours need to be added
                 if(nleft.GT.0) goto 10
        !
              elseif(nhrinc.lt.0)then
        ! ---    Decrement time
                 ihr=ihr+nhrinc
                 if(ihr.lt.0)then
                    ihr=ihr+24
                    ijul=ijul-1
                    if(ijul.lt.1)then
                       iyr=iyr-1
                       if(mod(iyr,4).eq.0)then
                          ijul=366
                       else
                          ijul=365
                       endif
                    endif
                 endif
              endif
        !
              return
              end
end module