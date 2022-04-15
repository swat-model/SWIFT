      subroutine proc_hru
    
      use hru_module1
      use climate_module1
      use hydrograph_module, only : sp_ob, sp_ob1, ob, hru2, hru_db2
    
      implicit none
      
      integer :: ihru           !none       |hru counter
      integer :: iob            !none       |object counter
      integer :: ihru_db        !none       |number of hru data
      integer :: ipro           !none       |number of process (total runoff,surf,perc,lat,tile)
      integer :: iwst           !none       |precip gage number
      integer :: i, k           !none       |variables used to set data number
      integer :: iostat, eof
      character (len=80) :: titldum   !           |title of file
      
      allocate (hru1(sp_ob%hru))
      allocate (hru2(sp_ob%hru))

      !! read hru.dat
      open (107,file = "hru.dat",recl=1200)
      read (107,*) titldum
      read (107,*) ihru_db
      read (107,*) titldum   !header
      read (107,*) titldum   !units
      
      allocate (hru_db(ihru_db))
      allocate (hru_db2(ihru_db))
      
      !! read hru data
      do ihru = 1, ihru_db
        read (107,*,iostat=eof) i
        if (eof < 0) exit
        backspace (107)
        !! read from the aquifer database file named aquifer.aqu
        read (107,*,iostat=eof) k, hru_db(i)%name, hru_db(i)%lum, hru_db(i)%soil,  &
                                        hru_db(i)%surf_stor, hru_db(i)%rout_over
        if (eof < 0) exit
        
        !! if surface storage (wetland/rice) read storages, if not read hru loadings (runoff)
        if (hru_db(i)%surf_stor == "n") then
          !! read loadings from each process 
          do ipro = 1, 5
            read (107,*,iostat=eof) hru_db2(i)%runoff(ipro)
          end do
        else
          !! read impoundment parameters for wetlands/rice
          read (107,*,iostat=eof) hru_db(i)%wet
        end if
        
        !! if routing over an hru, input required data
        if (hru_db(i)%rout_over == "y") then
          do ipro = 1, 5
            read (107,*,iostat=eof) hru_db2(i)%rout_dr(ipro)
          end do
        end if
        
      end do

      !! set hru data to proper input data
      do iob = 1, sp_ob%hru
        ihru = ob(iob)%props
        hru1(iob) = hru_db(ihru)
        hru1(iob)%obj_no = sp_ob1%hru + ihru - 1
        iwst = ob(hru1(iob)%obj_no)%wst
        hru1(iob)%precip = precip(iwst)%ave_ann_mm
        hru1(iob)%area_ha = ob(hru1(iob)%obj_no)%area_ha
        !! convert to m3, t, kg for routing
        do ipro = 1, 5
          ob(iob)%hd(ipro)%flo = hru_db2(ihru)%runoff(ipro)%flo * hru1(iob)%precip   
          ob(iob)%hd(ipro)%sed = hru_db2(ihru)%runoff(ipro)%sed * hru1(iob)%area_ha
          ob(iob)%hd(ipro)%orgn = hru_db2(ihru)%runoff(ipro)%orgn * hru1(iob)%area_ha
          ob(iob)%hd(ipro)%sedp = hru_db2(ihru)%runoff(ipro)%sedp * hru1(iob)%area_ha
          ob(iob)%hd(ipro)%no3 = hru_db2(ihru)%runoff(ipro)%no3 * hru1(iob)%area_ha
          ob(iob)%hd(ipro)%solp = hru_db2(ihru)%runoff(ipro)%solp * hru1(iob)%area_ha
          ob(iob)%hd(ipro)%nh3 = hru_db2(ihru)%runoff(ipro)%nh3 * hru1(iob)%area_ha
          ob(iob)%hd(ipro)%no2 = hru_db2(ihru)%runoff(ipro)%no2 * hru1(iob)%area_ha
        end do
        
      end do
      
      close (107)
      
	  return
      end subroutine proc_hru