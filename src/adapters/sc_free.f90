! sc_free - Program for converting from '*.*d' to SonicLib format, with
!           no conversion of analog channel data (this allows greater
!           configuration flexibility.
!
! Written by: Patrizia Favaron
! e-mail:     patti.favaron@gmail.com
!
!------------------------------------------------------------------
! Statement of Licensing Conditions
!------------------------------------------------------------------
!
! With many thanks to the Environmental Physics research group
! within the Physics Department "Aldo Pontremoli" of the
! University of Milan for their help, support, and encouragement.
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation
! files (the "Software"), to deal in the Software without
! restriction, including without limitation the rights to use,
! copy, modify, merge, publish, distribute, sublicense, and/or
! sell copies of the Software, and to permit persons to whom the
! Software is furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
! HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
! FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
! OTHER DEALINGS IN THE SOFTWARE.
!
!------------------------------------------------------------------

    program SonicConvert

        implicit none
        
        ! Local variables
        character(len=256)  :: sInputFile
        character(len=256)  :: sOutputFile
        integer             :: iRetCode
        integer             :: iU, iV, iW, iT
        integer, dimension(4)   :: ivA
        real                :: u, v, w, t, a4, a5, a6, a7
        real                :: rTimeStamp
        character(len=64)   :: sBuffer
        logical             :: lFirst = .true.
        integer             :: iChanH2O
        integer             :: iChanCO2
        real                :: rMultH2O
        real                :: rOffH2O
        real                :: rMultCO2
        real                :: rOffCO2
        
        ! Get input parameters
        if(command_argument_count() /= 8) then
            print *, "sc_free - Program for converting from '*.*d' to SonicLib format, with"
            print *, "          no conversion of analog channel data (this allows greater"
            print *, "          configuration flexibility."
            print *
            print *, "Usage:"
            print *, ""
            print *, "  sc_free <InputFile> <ChnH2O> <ChnCO2> <MultH2O> <OffH2O> <MultCO2> <OffCO2> <OutputFile>"
            print *, ""
            print *, "Example:"
            print *, ""
            print *, "  sc_free 20150304.14d 6 7 0.085451826 100.0 0.00122074 12.0 20150304.14d.csv"
            print *, ""
            print *, "Copyright 2022 by Patrizia Favaron"
            print *, "                  All rights reserved"
            print *
            stop
        end if
        call get_command_argument(1, sInputFile)
        call get_command_argument(2, sBuffer)
        read(sBuffer, *, iostat=iRetCode) iChanH2O
        call get_command_argument(3, sBuffer)
        read(sBuffer, *, iostat=iRetCode) iChanCO2
        call get_command_argument(4, sBuffer)
        read(sBuffer, *, iostat=iRetCode) rMultH2O
        call get_command_argument(5, sBuffer)
        read(sBuffer, *, iostat=iRetCode) rOffH2O
        call get_command_argument(6, sBuffer)
        read(sBuffer, *, iostat=iRetCode) rMultCO2
        call get_command_argument(7, sBuffer)
        read(sBuffer, *, iostat=iRetCode) rOffCO2
        call get_command_argument(8, sOutputFile)
        
        ! Perform input file conversion
        open(10, file=sInputFile, status='old', action='read', iostat=iRetCode)
        if(iRetCode /= 0) then
            print *, "sc:: error: Input file ", trim(sInputFile), " not found or inaccessible."
            stop
        end if
        open(11, file=sOutputFile, status='unknown', action='write')
        write(11, "('time.stamp,u   ,v   ,w   ,t   ,q   ,c')")
        
        do
            read(10, *, iostat=iRetCode) sBuffer, rTimeStamp
            if(iRetCode /= 0) exit
            if(sBuffer(3:3) == 'x') then
                ! Sonic quadruple
                read(sBuffer, "(1x,4(5x,i5))") iU, iV, iW, iT
                lFirst = .false.
            else
                read(sBuffer, "(1x,4(5x,i5))") ivA
                if(.not.lFirst) then
                    write(11, "(1x, f7.2,4(',',f6.2),2(',',f9.4))") &
                        rTimeStamp, &
                        iV/100., &
                        iU/100., &
                        iW/100., &
                        iT/100., &
                        ivA(iChanH2O-3)*rMultH2O + rOffH2O, &
                        ivA(iChanCO2-3)*rMultCO2 + rOffCO2
                end if
            end if
        end do

        ! Release resources and leave
        close(11)
        close(10)
        
    end program SonicConvert
