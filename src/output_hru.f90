      subroutine output_hru
    
      use hru_module1
      use hydrograph_module  !, only : sp_ob, sp_ob1, ob, hru_ex, hru_dr, hru_bmp
      use input_file_module
    
      implicit none
      
      integer :: ihru           !none       |hru counter

      !! open hru.dat file
      open (1984,file="hru_ex.out",recl=1200)
      write (1984,*) bsn%name, prog
      !write (1984, "(1(11x)1A,1(4x)1A,5(1(6x)1A,1(8x)1A,6(1(6X)1A)))") hru_ex_header
      write (1984,*) hdr_exco
      !write (1984,"(1(1(22X)A,1(9X)),(5(4(1(14X)1A,1(12X)1A),1(8X)1A,1(8X)1A)))") "ha",hdr_exco_unt, hdr_exco_unt, hdr_exco_unt, hdr_exco_unt ,hdr_exco_unt
      
      !! write hru data
      do ihru = 1, sp_ob%hru
        write (1984, *) ' HRU ', ihru, '  AREA_HA ', hru_dat(ihru)%area_ha
        call hyd_convert__mass_to_ha (ob(ihru)%hd(1), hru_dat(ihru)%area_ha)
        write (1984, *) ' TOTAL     ', ob(ihru)%hd(1)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(2), hru_dat(ihru)%area_ha)
        write (1984, *) ' PERCO     ', ob(ihru)%hd(2)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(3), hru_dat(ihru)%area_ha)
        write (1984, *) ' SURFACE   ', ob(ihru)%hd(3)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(4), hru_dat(ihru)%area_ha)
        write (1984, *) ' LATERAL   ', ob(ihru)%hd(4)
        call hyd_convert__mass_to_ha (ob(ihru)%hd(5), hru_dat(ihru)%area_ha)
        !write (1984, "((I,6X),*(F,9X)))") ihru, hru_dat(ihru)%area_ha, ob(ihru)%hd(1), ob(ihru)%hd(2), ob(ihru)%hd(3) &
            !, ob(ihru)%hd(4), ob(ihru)%hd(5)
	write (1984, *) ' TILE      ', ob(ihru)%hd(5)
      end do
      

      
      close (1984)
      
	  return
      end subroutine output_hru