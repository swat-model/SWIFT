      subroutine channel_read
      
      use channel_module1
      
      implicit none      
      
      character (len=80) :: titldum   !             |title of file
      character (len=80) :: header    !             |header of file
      character (len=16) :: namedum   !             |
      integer :: eof                  !             |end of file
      integer :: imax                 !none         |determine max number for array (imax) and total number in file
      logical :: i_exist              !none         |check to determine if file exists
      integer :: idb                  !             |
      integer :: i                    !none         |counter  

      eof = 0
      imax = 0
     
      open (1,file = "chan.dat",recl=1200)
      if (.not. i_exist) then
        allocate (cha1(0:0))
      else
      do
        open (1,file=in_cha%hyd_sed)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
          do while (eof == 0)
            read (1,*,iostat=eof) titldum
            if (eof < 0) exit
            imax = imax + 1
          end do  
          
        allocate (cha1(0:imax))

        rewind (1)
        read (1,*,iostat=eof) titldum
        if (eof < 0) exit
        read (1,*,iostat=eof) header
        if (eof < 0) exit
        
        do idb = 1, imax
          read (1,*,iostat=eof) cha1(idb)
          if (eof < 0) exit
        end do

        exit
      end do
      end if

      close (1)
      return
      end subroutine channel_read