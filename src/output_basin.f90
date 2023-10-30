      subroutine output_basin
    
      use hru_module1
      use channel_module1
      use climate_module1
      use hydrograph_module
      use input_file_module
      use calibration_data_module
    
      use hydrograph_module
      
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: icha           !none       |channel counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      !integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      real :: runoff_mm
      real :: bsn_area
      real :: precip_bsn
      real :: ch_tot_length = 0.
      logical :: i_exist                !none       |check to determine if file exists
      character (len=80) :: titldum     !           |title of file

      !! open basin landscape output file
      open (107,file = "basin_ex.out",recl=1200)
      write (107,*) '  BASIN LANDSCAPE OUTPUT  '
      write(107, "((A4,27X),(A4,9X),(A6,13X),(A1,4X),(A20),(*(A20:1X)))") lsu_header, iob_header
      write(107, "(58X, *(A,8X, 1(A,6X),1(A,8X),1(A,6X),(A,2X),3(A,4X)4X))") &
          hdr_exco, hdr_exco, hdr_exco, hdr_exco, hdr_exco
      write(107, "(58X, *(A,8X, 1(A,6X),1(A,8X),1(A,6X),(A,2X),3(A,4X)4X))") &
          hdr_exco_unt, hdr_exco_unt, hdr_exco_unt, hdr_exco_unt, hdr_exco_unt
      
      !! sum hru data
      bsn_area = 0.
      precip_bsn = 0.
      do iob = 1, sp_ob%hru
        ihru = lsu_elem(iob)%obtypno
        bsn_area = bsn_area + lsu_elem(ihru)%bsn_frac !hru_dat(ihru)%area_ha
        precip_bsn = precip_bsn + lsu_elem(ihru)%bsn_frac * hru_dat(ihru)%precip
        do ipro = 1, 5
          bsn_hru_out(ipro) = bsn_hru_out(ipro) + lsu_elem(ihru)%bsn_frac * ob(ihru)%hd(ipro)
        end do
      end do
      
      !! write basin output
      write(107, "(A,(2(F)6X),(A,2X),(F,3X),*(F,6X))") bsn%name, bsn_area, precip_bsn, " - " ,&
        bsn_hru_out(1), bsn_hru_out(2), bsn_hru_out(3), bsn_hru_out(4), bsn_hru_out(5)
      
      close (107)
      
      !! open basin channel output file
      open (107,file = "basin_ch.out",recl=1200)
      write (107,*) '  BASIN CHANNEL OUTPUT  '
      
      !! sum channel data
      do icha = 1, sp_ob%chandeg
        iob = sp_ob1%chandeg + icha - 1
        bsn_ch_out%bank_ero = bsn_ch_out%bank_ero + cha2(icha)%bank_ero
        bsn_ch_out%bed_ero = bsn_ch_out%bed_ero + cha2(icha)%bed_ero
        bsn_ch_out%fp_dep = bsn_ch_out%fp_dep + cha2(icha)%fp_dep
        bsn_ch_out%bed_dep = bsn_ch_out%bed_dep + cha2(icha)%bed_dep
      end do
      
      !! write basin output
      write (107,"(1(A8),21X,1(A16),19X,*(A24))") 'BASIN','TOTAL_LENGTH', bsn_cha
      write (107,"(1(A8),25X,1(A2), 24X,(A18), 6X,*(A24))") '   ', 'KM', hdr_cha_unt, &
          hdr_cha_unt, hdr_cha_unt, hdr_cha_unt
      write(107, '(3X,1(A16), 1(F20.4), 16X, 32(F24.4))') bsn%name, ch_tot_length, &
          bsn_ch_out%bank_ero, bsn_ch_out%bed_ero, bsn_ch_out%fp_dep, bsn_ch_out%bed_dep
      
      close (107)
      
	  return
      end subroutine output_basin