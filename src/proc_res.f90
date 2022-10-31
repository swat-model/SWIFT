      subroutine proc_res
     
      use reservoir_module1
      use hydrograph_module !, only : sp_ob
      use input_file_module
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: ires_db              !none         |number of channel data inputs
      integer :: ires                 !             |channel counter
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      
      eof = 0
      
      open (107,file = in_res%res_dat,recl=1200)
      read (107,*) titldum   !title
      read (107,*) ires_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
                  
      allocate (res1(sp_ob%res))
      allocate (res2(sp_ob%res))
      
      !! read hru data
      do ires = 1, ires_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, res1(i)%name, res1(i)%res_db
        if (eof < 0) exit
      end do

      close (107)

	  return
      end subroutine proc_res