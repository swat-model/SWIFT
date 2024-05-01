      subroutine proc_open
    
      use channel_module1
      use reservoir_module1

      implicit none

      !! open output files
      open (1977,file = "res.out",recl=1200)
      open (1978,file = "aqu.out",recl=1200)
      
      open (1979,file = "cha.out",recl=1200)
      write (1979,*) hdr_cha
      write (1979,*) hdr_cha_unt
      
      open (1988,file = "cha_bank.out",recl=1200)
      write (1988,*) hdr_cha_bnk
      write (1988,*) hdr_cha_bnk_unt
      
      open (1985,file = "cha_bed.out",recl=1200)
      write (1985,*) hdr_cha_bed
      write (1985,*) hdr_cha_bed_unt
      
      open (1986,file = "cha_fp.out",recl=1200)
      write (1986,*) hdr_cha_fp
      write (1986,*) hdr_cha_fp_unt
      
      open (1987,file = "res_flo.out",recl=1200)
      write (1987,*) hdr_res_floout
      write (1987,*) hdr_res_floout_unt
      
      open (1980,file = "hru.out",recl=1200)
      open (1981,file = "out.out",recl=1200)
      open (1982,file = "ru.out",recl=1200)
      open (1983,file = "hyd.out",recl=1200)
      
      !! write headers in output files

	  return
      
      end subroutine proc_open