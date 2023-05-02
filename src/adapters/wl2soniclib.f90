! wl2soniclib.f90 - Adapter from WindLogger to SonicLib form
!
! -------------------------------------------------------------------------------------------------
!
! Copyright 2023 Patrizia Favaron
!
! Permission is herebiy granted, free of charge, to any person obtaining a copy of this software and associated
! documentation files (the "Software"), to deal in the Software without restriction, including without limitation
! the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
! and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all copies or substantial portions
! of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
! TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
! OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
! DEALINGS IN THE SOFTWARE.
!
! -------------------------------------------------------------------------------------------------
!
program wl2soniclib
    
    use datetime
    
    implicit none
    
    ! Locals
    integer             :: iRetCode
    character(len=256)  :: sInPath
    character(len=256)  :: sInFile
    character(len=256)  :: sOutPath
    character(len=256)  :: sOutFile
    character(len=128)  :: sBuffer
    type(Time)          :: tTimeFrom
    integer             :: iTimeFrom
    integer             :: iTimeTo
    type(Time)          :: tTime
    integer             :: iTime
    integer             :: iNumDays
    logical             :: lIsFile
    integer             :: iLUN_in
    integer             :: iLUN_out
    integer             :: iMinuteIn
    integer             :: iSecondIn
    integer             :: iSecond
    integer             :: iU, iV, iW, iT
    real                :: rU, rV, rW, rT
    
    ! Get parameters
    if(command_argument_count() /= 4) then
        print *, "wl2sonic - Adapter from WindLogger to SonicLib form"
        print *
        print *, "Usage:"
        print *
        print *, "  ./wl2sonic <In_Path> <Date_Time_From> <Num_Days> <Out_Path>"
        print *
        print *, "Copyright 2023 by Patrizia Favaron"
        print *
        print *, "This is open-source software, covered by the MIT license"
        print *
        stop
    end if
    call get_command_argument(1, sInPath)
    call get_command_argument(2, sBuffer)
    tTimeFrom = fromDayString(sBuffer)
    if(.not. isValid(tTimeFrom)) then
        print *, "Invalid number of days"
        stop
    end if
    iTimeFrom = toEpoch(tTimeFrom)
    call get_command_argument(3, sBuffer)
    read(sBuffer, *, iostat=iRetCode) iNumDays
    if(iRetCode /= 0) then
        print *, "Invalid number of days"
        stop
    end if
    call get_command_argument(4, sOutPath)
    
    ! Main loop: iterate over hours in desired time interval
    iTimeTo = iTimeFrom + 24*3600 * iNumDays - 1    ! Trick: the "-1" to prevent use of last time, which belongs to the next day
    do iTime = iTimeFrom, iTimeTo, 3600
        
        ! Form input file name, and check its corresponding file really exists
        tTime = fromEpoch(iTime)
        write(sInFile, "(a, '/', 4i2.2, '.DAT')") &
            trim(sInPath), &
            mod(tTime % iYear, 100), tTime % iMonth, tTime % iDay, tTime % iHour
        inquire(file=sInFile, exist=lIsFile)
        if(.not.lIsFile) cycle
        
        ! Inform user
        print *, "Processing file ", trim(sInFile)
        
        ! Generate output file name
        write(sOutFile, "(a, '/', i4.4,2i2.2,'.',i2.2,'.csv')") &
            trim(sOutPath), &
            tTime % iYear, tTime % iMonth, tTime % iDay, tTime % iHour
            
        ! Perform file form conversion
        open(newunit=iLUN_in,  file=sInFile,  status='old',     action='read' )
        open(newunit=iLUN_out, file=sOutFile, status='unknown', action='write')
        write(iLUN_out, "('time.stamp, u, v, w, t')")
        do
            read(iLUN_in, "(a)", iostat=iRetCode) sBuffer
            if(iRetCode /= 0) exit
            if(len_trim(sBuffer) /= 61) cycle
            read(sBuffer, "(14x,i2,1x,i2,2x,4(4x,i6))", iostat=iRetCode) &
                iMinuteIn, iSecondIn, &
                iV, iU, iW, iT
            iSecond = 60*iMinuteIn + iSecondIn
            rU = iU / 100.0
            rV = iV / 100.0
            rW = iW / 100.0
            rT = iT / 100.0
            write(iLUN_out, "(i4,4(',',f8.2))") iSecond, rU, rV, rW, rT
        end do
        close(iLUN_out)
        close(iLUN_in)
        
    end do
    
    ! Leave
    print *, "*** END JOB ***"

end program wl2soniclib
