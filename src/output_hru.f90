      subroutine output_hru
    
      use hru_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter

      !! open hru.dat file
      open (1984,file="hru_ex.out",recl=1200)
      write (1984,*) '  HRU LANDSCAPE OUTPUT  '
      write (1984,"(10X,*(A))") hru_ex_header
      write (1984,*) hdr_exco
      write (1984,*) hdr_exco_unt
      
      !! write hru data
      do ihru = 1, sp_ob%hru
          
        call hyd_convert__mass_to_ha (ob(ihru)%hd(1), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(2), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(3), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(4), hru_dat(ihru)%area_ha)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(5), hru_dat(ihru)%area_ha)
        write (1984, "((I,6X),*(F,9X)))") ihru, hru_dat(ihru)%area_ha, ob(ihru)%hd(1), ob(ihru)%hd(2), ob(ihru)%hd(3) &
            , ob(ihru)%hd(4), ob(ihru)%hd(5)

      end do
      

      
      close (1984)
      
	  return
      end subroutine output_hru