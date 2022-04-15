      subroutine proc_open

      implicit none

      !! open output files
      open (1977,file = "res.out",recl=1200)
      open (1978,file = "aqu.out",recl=1200)
      open (1979,file = "cha.out",recl=1200)
      open (1980,file = "hru.out",recl=1200)
      open (1981,file = "out.out",recl=1200)
      open (1982,file = "ru.out",recl=1200)
      open (1983,file = "hyd.out",recl=1200)
      
      !! write headers in output files

	  return
      
      end subroutine proc_open