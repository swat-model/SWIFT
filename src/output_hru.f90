      subroutine output_hru
    
      use hru_module1
      use climate_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      !integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      real :: runoff_mm
      logical :: i_exist        !none       |check to determine if file exists
      character (len=80) :: titldum   !           |title of file

      !! open hru.dat file
      open (1984,file="hru_ex.out",recl=1200)
      write (1984,*) titldum
      write (1984,*) titldum  !HEADERS - NUBZ
      
      !! read hru data
      do ihru = 1, sp_ob%hru
        write (1984, *) ' HRU       ', ihru
        call hyd_convert__mass_to_ha (ob(ihru)%hd(1), hru_dat(ihru)%area_ha)
        write (1984, *) ' TOTAL     ', ob(ihru)%hd(1)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(2), hru_dat(ihru)%area_ha)
        write (1984, *) ' PERCO     ', ob(ihru)%hd(2)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(3), hru_dat(ihru)%area_ha)
        write (1984, *) ' SURFACE   ', ob(ihru)%hd(3)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(4), hru_dat(ihru)%area_ha)
        write (1984, *) ' LATERAL   ', ob(ihru)%hd(4)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(5), hru_dat(ihru)%area_ha)
        write (1984, *) ' TILE      ', ob(ihru)%hd(5)
      end do
      
      close (1984)
      
	  return
      end subroutine output_hru