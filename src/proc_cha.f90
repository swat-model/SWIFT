      subroutine proc_cha
    
      use channel_module1
      use hydrograph_module  !, only : sp_ob
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: icha_db              !none         |number of channel data inputs
      integer :: icha                 !             |channel counter
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      integer :: icha_dbmx
      
      eof = 0
      
      open (107,file = "chan.dat",recl=1200)
      read (107,*) titldum   !title
      read (107,*) icha_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      allocate (cha1(sp_ob%chandeg))
      allocate (cha2(sp_ob%chandeg))
      
      !! read channel data
      do icha = 1, icha_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, cha1(i)%dat
        if (eof < 0) exit
      end do

      close (107)
       
      !! read channel instream delivery data
      open (107,file = "chan_dr.dat",recl=1200)
      read (107,*) titldum   !title
      read (107,*) icha_dbmx
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      allocate (cha_dr(icha_dbmx))
      
      !! read hru data
      do icha = 1, icha_dbmx
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the channel delivery ratio file
        read (107,*,iostat=eof) k, cha_dr(i)
        if (eof < 0) exit
      end do
      
      !! crosswalk channel delivery ratio name with database name
      do icha = 1, sp_ob%chandeg
        do icha_db = 1, icha_dbmx
          if (cha1(icha)%dat%instream_dr == cha_dr(icha_db)%name) then
            cha1(icha)%dr_db = icha_db
            exit
          end if
        end do
      end do
      

      close (107)
      return
      end subroutine proc_cha