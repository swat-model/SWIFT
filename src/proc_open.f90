      subroutine proc_open
    
      use channel_module1
      use reservoir_module1
      use hydrograph_module

      implicit none

      !! open output files
      open (1977,file = "res.out",recl=1200)
      open (1978,file = "aqu.out",recl=1200)
      write (1978,*) bsn%name, prog
      write (1978, "(2X(A2, 10X), (A5,26X), (A5,13X), (A,9X), (A,3X),(A,2X),*(A,3X)))") &
          hyd_hdr
      !write(1978, "(2X(A2, 10X), (A5,26X), (A5,13X), (A,9X), (A,3X),(A,2X),*(A,3X)))") &
          !hyd_out_units
      
      open (1979,file = "cha.out",recl=1200)
      write (1979,*) bsn%name, prog
      write(1979, "((A4),1X,(A25),28X,(A22),*(A22))") "  #", "name" ,"area        ", &
          hdr_cha, hdr_cha, hdr_cha, hdr_cha, hdr_cha, hdr_cha
      write(1979, "((A4),1X,(A25),28X,(A22),*(A22))") "   ", " " ,"ha             ", &
          hdr_cha_unt, hdr_cha_unt, hdr_cha_unt, hdr_cha_unt, hdr_cha_unt, hdr_cha_unt
      
      open (1988,file = "cha_bank.out",recl=1200)
      write (1988,*) bsn%name, prog
      write (1988,*) hdr_cha_bnk
      write (1988,*) hdr_cha_bnk_unt
      
      open (1985,file = "cha_bed.out",recl=1200)
      write (1985,*) bsn%name, prog
      write (1985,*) hdr_cha_bed
      write (1985,*) hdr_cha_bed_unt
      
      open (1986,file = "cha_fp.out",recl=1200)
      write (1986,*) bsn%name, prog
      write (1986,*) hdr_cha_fp
      write (1986,*) hdr_cha_fp_unt
      
      open (1987,file = "res_flo.out",recl=1200)
      write (1987,*) bsn%name, prog
      write (1987,*) hdr_res_floout
      write (1987,*) hdr_res_floout_unt
      
      open (1980,file = "hru.out",recl=1200)
      write (1980,*) bsn%name, prog
      !write (1980, "(6X,2(A5),24X,A8,8X,A11,1X,A4,14X,A9,1X,A3,*(A15))") , hru_header
      open (1981,file = "out.out",recl=1200)
      write (1981,*) bsn%name, prog
      open (1982,file = "ru.out",recl=1200)
      write (1982,*) bsn%name, prog
      open (1983,file = "hyd.out",recl=1200)
      write (1983,*) bsn%name, prog
      
      !! write headers in output files

	  return
      
      end subroutine proc_open