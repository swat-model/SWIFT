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
      write (107, *) ' BASIN   ', bsn%name, '   AREA HA   ', bsn_area
      write (107,*) hdr_exco
      write (107,*) hdr_exco_unt
      write (107, *) ' PRECIPITATION  ', precip_bsn
      write (107, *) ' TOTAL     ', bsn_hru_out(1)
      write (107, *) ' PERCO     ', bsn_hru_out(2)
      write (107, *) ' SURFACE   ', bsn_hru_out(3)
      write (107, *) ' LATERAL   ', bsn_hru_out(4)
      write (107, *) ' TILE      ', bsn_hru_out(5)
      
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
      write (107, *) ' BASIN   ', bsn%name, '   TOTAL_LENGTH KM ', ch_tot_length
      write (107,*) hdr_cha
      write (107,*) hdr_cha_unt
      write (107, *) ' BANK_ERO  ', bsn_ch_out%bank_ero
      write (107, *) ' BED_ERO   ', bsn_ch_out%bed_ero
      write (107, *) ' FP_DEP    ', bsn_ch_out%fp_dep
      write (107, *) ' BED_DEP   ', bsn_ch_out%bed_dep
      
      close (107)
      
	  return
      end subroutine output_basin