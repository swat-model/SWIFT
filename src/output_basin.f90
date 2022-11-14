      subroutine output_basin
    
      use hru_module1
      use climate_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
      use calibration_data_module
    
      use hydrograph_module
      
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      !integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      real :: runoff_mm
      real :: bsn_area
      real :: precip_bsn
      logical :: i_exist        !none       |check to determine if file exists
      character (len=80) :: titldum   !           |title of file

      !! open basin.out file
      open (107,file = "basin_ex.out",recl=1200)
      write (107,*) '  BASIN OUTPUT  '
      write (107,*) '  HEADERS  '  !HEADERS - NUBZ
      
      !! sum hru data
      bsn_area = 0.
      precip_bsn = 0.
      do iob = 1, sp_ob%hru
          ihru = lsu_elem(iob)%obtypno
          bsn_area = bsn_area + lsu_elem(ihru)%bsn_frac !hru_dat(ihru)%area_ha
          precip_bsn = precip_bsn + lsu_elem(ihru)%bsn_frac * hru_dat(ihru)%precip
        do ipro = 1, 5
          bsn_out(ipro) = bsn_out(ipro) + lsu_elem(ihru)%bsn_frac * ob(ihru)%hd(ipro)
        end do
      end do
      
      !! write basin output
      write (107, *) ' BASIN   ', bsn%name, '   AREA HA   ', bsn_area
      write (107, *) ' PRECIPITATION  ', precip_bsn
      write (107, *) ' TOTAL     ', bsn_out(1)
      write (107, *) ' PERCO     ', bsn_out(2)
      write (107, *) ' SURFACE   ', bsn_out(3)
      write (107, *) ' LATERAL   ', bsn_out(4)
      write (107, *) ' TILE      ', bsn_out(5)
      
      close (107)
      
	  return
      end subroutine output_basin