      subroutine cli_pmeas
    
      use climate_module1
         
      implicit none

      character (len=80) :: titldum   !             |title of file
      integer :: eof                  !             |end of file
      integer :: iprecip_db           !none         |number of precip gages
      integer :: iaqu                 !             |channel counter
      integer :: i                    !none         |counter
      integer :: k                    !none         |counter
      
      eof = 0
      
      open (107,file = "precip.dat",recl=1200)
      read (107,*) titldum      !title
      read (107,*) iprecip_db
      read (107,*) titldum      !header
      read (107,*) titldum      !units
      
      allocate (precip(iprecip_db))
      
      !! read hru data
      do iaqu = 1, iprecip_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, precip(i)%name, precip(i)%ave_ann_mm
        if (eof < 0) exit
      end do

      close (107)
      
      return
      end subroutine cli_pmeas