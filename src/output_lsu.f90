      subroutine output_lsu
    
      use hru_module1
      use climate_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
      use maximum_data_module
      use calibration_data_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: ielem          !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      !integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      real :: lsu_area
      real :: precip_lsu
      logical :: i_exist        !none       |check to determine if file exists
      character (len=80) :: titldum   !           |title of file

      !! open hru.dat file
      open (107,file = "lsu_ex.out",recl=1200)
      write (107,*) '  LANDSCAPE UNIT OUTPUT  '
      write (107,*) '  HEADERS '  !HEADERS - NUBZ
      
      !! sum lsu data
      do iob = 1, db_mx%lsu_out
        lsu_area = 0.
        precip_lsu = 0.
        do ielem = 1,  lsu_out(iob)%num_tot
          do ipro = 1, 5
            ihru = lsu_out(iob)%num(ielem)
            lsu_area = lsu_area + lsu_elem(ihru)%ru_frac * hru_dat(ihru)%precip
            precip_lsu = precip_lsu + lsu_elem(ihru)%ru_frac * hru_dat(ihru)%precip
            lsu_ex(iob)%hyd(ipro) = lsu_ex(iob)%hyd(ipro) + lsu_elem(ihru)%ru_frac * ob(ihru)%hd(ipro)
          end do
        end do
        
        write (107, *) ' LSU   ', lsu_out(iob)%name, '   AREA HA   ', lsu_area
        write (107, *) ' PRECIPITATION  ', precip_lsu
        write (107, *) ' LSU       ', iob
        write (107, *) ' TOTAL     ', lsu_ex(iob)%hyd(1)
        write (107, *) ' PERCO     ', lsu_ex(iob)%hyd(2)
        write (107, *) ' SURFACE   ', lsu_ex(iob)%hyd(3)
        write (107, *) ' LATERAL   ', lsu_ex(iob)%hyd(4)
        write (107, *) ' TILE      ', lsu_ex(iob)%hyd(5)
      end do
      
      close (107)
      
	  return
      end subroutine output_lsu