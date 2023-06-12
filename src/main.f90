      program main

      use hydrograph_module !, only : prog

      implicit none
      
      integer :: date_time(8)           !              |
      character*10 b(3)                 !              |
      character (len=80) :: prog
      
      !prog = " SWIFT Feb 14 2022    MODULAR Rev 2022.1"
      prog = " SWIFT Jan 18 2023    MODULAR Rev 2023.1"

      write (*,1000)
      open (9003,file='simulation.out')
      write (9003,1000)
1000  format(1x,"                  SWIFT               ",/,             &
     &          "             Revision 1.0             ",/,             &
     &          "               PC Version             ",/,             &
     &          "    Program reading . . . executing",/)
      
      !! find critical shear for Peter
      !call peter_tc

      call basin_read_objs
      call readcio_read

      call dr_db_read
      call hyd_connect

      call lsu_read_elements        !defining landscape units by hru

      call cli_pmeas
      call recall_read
      call proc_hru
      call proc_ru
      call proc_cha
      call proc_aqu
      call proc_res
      !! write header to output files
      call proc_open
      
      !! route and add average annual flows
      call command
           
      !! write successful completion to screen and file
      write (*,1001)
      write (9003,1001)
      open (107,file="success.fin")
  
      call DATE_AND_TIME (b(1), b(2), b(3), date_time)
      write (*,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7)
      write (9003,1234) date_time(2), date_time(3), date_time(1), date_time(5), date_time(6), date_time(7)
1234  format(/,"  Date of Sim", 2x,i2,"/",i2,"/",i4, " Time",2x,i2,":",i2,":",i2)
            
      write (107,1001)     
 1001 format (/," Execution successfully completed ")

	  stop      
      end