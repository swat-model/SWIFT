      subroutine proc_date_time
     
      implicit none
      
      integer :: date_time(8)           !              |
      character*10 b(3)                 !              |
  
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7) 
      write (9003,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7) 
1234  format(/,"  Date of Sim", 2x,i2,"/",i2,"/",i4, " Time",2x,i2,":",i2,":",i2)
      
      call cli_pmeas
      
111   format (1x,a, 5x,"Time",2x,i2,":",i2,":",i2)
      
      return
      
      end subroutine proc_date_time