      module channel_module1
    
      implicit none

      type channel_data1
        character(len=25) :: name
        character(len=16) :: order
        real :: chw = 0.        !m          |channel width
        real :: chd = 0.        !m          |channel depth
        real :: chs = 0.        !m/m        |channel slope
        real :: chl = 0.        !km         |channel length
        real :: chn = 0.        !           |channel Manning's n
        real :: wd_yr = 0.      !frac       |baseline widths per year of erosion
        real :: cherod = 0.     !           |channel erodibility
        real :: cov = 0.        !0-1        |channel cover factor
        real :: wd_rto = 0.     !0.5-100    |width depth ratio
        real :: chseq = 0.      !m/m        |equilibrium channel slope
        real :: d50 = 0.        !mm         |channel median sediment size
        real :: ch_clay = 0.    !%          |clay percent of bank and bed
        real :: carbon = 0.     !%          |cabon percent of bank and bed
        real :: ch_bd = 0.      !t/m3       |dry bulk density
        real :: chss = 0.       !           |channel side slope
        real :: bedldcoef = 0.  !           |percent of sediment entering the channel that is bed material
        real :: fps = 0.        !m/m        |flood plain slope
        real :: fpn = 0.        !           |flood plain Manning's n
        real :: n_conc = 0.     !mg/kg      |nitrogen concentration in channel bank
        real :: p_conc = 0.     !mg/kg      |phosphorus concentration in channel bank
        real :: p_bio = 0.      !frac       |fraction of p in bank that is bioavailable
        !! in addition to SWAT channel inputs
        real :: sinu = 1.25     !           |SI <1.05: almost straight; 1.05 ≤ SI <1.25: winding
                                                ! 1.25 ≤ SI <1.50: twisty; 1.50 ≤ SI: meandering
        real :: fp_flo_fr = .1      !frac       |fraction of annual flow that flows through flood plain (0.03-0.15)
        real :: fp_inun_days = 2.   !days       |average annual number of days the flood plain is inundated (9-15)
        real :: q1_qm = 10.         !           |ratio of 1% daily flow to the mean daily flow (2-40)
        real :: rcurv_exp = 1.2     !           |exponent (b) of the sediment rating curve (C=aQ^b)
      end type channel_data1
      
      type channel1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        type (channel_data1) :: dat
      end type channel1
      type (channel1), dimension(:), allocatable :: cha1
 
      type sd_ch_output
        real :: flo_in = 0.             !(m^3/s)       |average daily inflow rate during time step
        real :: aqu_in = 0.             !(m^3/s)       |aveerage daily aquifer inflow rate during timestep
        real :: flo = 0.                !(m^3/s)       |average daily outflow rate during timestep
        real :: peakr = 0.              !(m^3/s)       |average peak runoff rate during timestep
        real :: sed_in = 0.             !(tons)        |sediment in
        real :: sed_out = 0.            !(tons)        |sediment out
        real :: washld = 0.             !(tons)        |wash load (suspended) out
        real :: bedld = 0.              !(tons)        |bed load out
        real :: dep = 0.                !(tons)        |deposition in channel and flood plain
        real :: deg_btm = 0.            !(tons)        |erosion of channel bottom 
        real :: deg_bank = 0.           !(tons)        |erosion of channel bank
        real :: hc_sed = 0.             !(tons)        |erosion from gully head cut
        real :: width = 0.              !m             |channel bank full top width at end of time step
        real :: depth = 0.              !m             |channel bank full depth at end of time step
        real :: slope = 0.              !m/m           |channel slope
        real :: deg_btm_m = 0.          !(m)           !downcutting of channel bottom
        real :: deg_bank_m = 0.         !(m)           |widening of channel banks
        real :: hc_m = 0.               !(m)           |headcut retreat
        real :: flo_in_mm = 0.          !(mm)          |inflow rate total sum for each time step
        real :: aqu_in_mm = 0.          !(mm)          |aquifer inflow rate total sum for each time step
        real :: flo_mm = 0.             !(mm)          |outflow rate total sum for each time step
        real :: sed_stor = 0.           !(tons)        |sed storage at end of timestep 
      end type sd_ch_output
      
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: chsd_a
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_d
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_m
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_y
      type (sd_ch_output), dimension(:), allocatable, save :: schsd_a
      type (sd_ch_output) :: bchsd_d
      type (sd_ch_output) :: bchsd_m
      type (sd_ch_output) :: bchsd_y
      type (sd_ch_output) :: bchsd_a
      type (sd_ch_output) :: chsdz
            
      type sdch_header
          character (len=6) :: day        =  "  jday"
          character (len=6) :: mo         =  "   mon"
          character (len=6) :: day_mo     =  "   day"
          character (len=6) :: yrc        =  "    yr"
          character (len=8) :: isd        =  "   unit "
          character (len=8) :: id         =  " gis_id "           
          character (len=16) :: name      =  " name          "        
          character(len=16) :: flo_in     =  "         flo_in"        ! (m^3/s)
          character(len=16) :: aqu_in     =  "         aqu_in"        ! (m^3/s)
          character(len=16) :: flo        =  "        flo_out"        ! (m^3/s)
          character(len=15) :: peakr      =  "          peakr"        ! (m^3/s)
          character(len=15) :: sed_in     =  "         sed_in"        ! (tons)
          character(len=15) :: sed_out    =  "        sed_out"        ! (tons)
          character(len=15) :: washld     =  "         washld"        ! (tons)
          character(len=15) :: bedld      =  "          bedld"        ! (tons)
          character(len=15) :: dep        =  "            dep"        ! (tons)
          character(len=15) :: deg_btm    =  "        deg_btm"        ! (tons)
          character(len=15) :: deg_bank   =  "       deg_bank"        ! (tons)
          character(len=15) :: hc_sed     =  "         hc_sed"        ! (tons)
          character(len=15) :: width      =  "          width"        ! (m)
          character(len=15) :: depth      =  "          depth"        ! (m)
          character(len=15) :: slope      =  "          slope"        ! (m/m)
          character(len=15) :: deg_btm_m  =  "        deg_btm"        ! (m)
          character(len=15) :: deg_bank_m =  "       deg_bank"        ! (m)
          character(len=15) :: hc_len     =  "         hc_len"        ! (m)
          character(len=16) :: flo_in_mm  =  "      flo_in_mm"        ! (mm)
          character(len=16) :: aqu_in_mm  =  "      aqu_in_mm"        ! (mm)
          character(len=16) :: flo_mm     =  "     flo_out_mm"        ! (mm)
      end type sdch_header
      type (sdch_header) :: sdch_hdr
      
     type sdch_header_units
          character (len=6) :: day        =  "      "
          character (len=6) :: mo         =  "      "
          character (len=6) :: day_mo     =  "      "
          character (len=6) :: yrc        =  "      "
          character (len=8) :: isd        =  "        "
          character (len=8) :: id         =  "        "           
          character (len=16) :: name      =  "              "        
          character(len=16) :: flo_in     =  "          m^3/s"       ! (m^3/s)
          character(len=16) :: aqu_in     =  "          m^3/s"       ! (m^3/s)      
          character(len=16) :: flo        =  "          m^3/s"       ! (m^3/s) 
          character(len=15) :: peakr      =  "          m^3/s"        ! (m^3/s)
          character(len=15) :: sed_in     =  "           tons"        ! (tons)
          character(len=15) :: sed_out    =  "           tons"        ! (tons)
          character(len=15) :: washld     =  "           tons"        ! (tons)
          character(len=15) :: bedld      =  "           tons"        ! (tons)
          character(len=15) :: dep        =  "           tons"        ! (tons)
          character(len=15) :: deg_btm    =  "           tons"        ! (tons)
          character(len=15) :: deg_bank   =  "           tons"        ! (tons)
          character(len=15) :: hc_sed     =  "           tons"        ! (tons)
          character(len=15) :: width      =  "              m"        ! (m)
          character(len=15) :: depth      =  "              m"        ! (m)
          character(len=15) :: slope      =  "            m/m"        ! (m/m)
          character(len=15) :: deg_btm_m  =  "              m"        ! (m)
          character(len=15) :: deg_bank_m =  "              m"        ! (m)
          character(len=15) :: hc_len     =  "              m"        ! (m)
          character(len=16) :: flo_in_mm  =  "             mm"        ! (mm)
          character(len=16) :: aqu_in_mm  =  "             mm"        ! (mm)      
          character(len=16) :: flo_mm     =  "             mm"        ! (mm) 
      end type sdch_header_units
      type (sdch_header_units) :: sdch_hdr_units
     
      interface operator (+)
        module procedure chsd_add
      end interface
      
      interface operator (/)
        module procedure chsd_div
      end interface
              
      interface operator (//)
        module procedure chsd_ave
      end interface
        
      interface operator (*)
        module procedure chsd_mult
      end interface 

      contains
!! routines for swatdeg_hru module

      function chsd_add(cho1,cho2) result (cho3)
      type (sd_ch_output),  intent (in) :: cho1
      type (sd_ch_output),  intent (in) :: cho2
      type (sd_ch_output) :: cho3
       cho3%flo_in = cho1%flo_in + cho2%flo_in
       cho3%aqu_in = cho1%aqu_in + cho2%aqu_in
       cho3%flo = cho1%flo + cho2%flo
       cho3%peakr = cho1%peakr + cho2%peakr
       cho3%sed_in = cho1%sed_in + cho2%sed_in
       cho3%sed_out = cho1%sed_out + cho2%sed_out
       cho3%washld = cho1%washld + cho2%washld
       cho3%bedld = cho1%bedld + cho2%bedld
       cho3%dep = cho1%dep + cho2%dep
       cho3%deg_btm = cho1%deg_btm + cho2%deg_btm
       cho3%deg_bank = cho1%deg_bank + cho2%deg_bank
       cho3%hc_sed = cho1%hc_sed + cho2%hc_sed
       cho3%width = cho1%width + cho2%width
       cho3%depth = cho1%depth + cho2%depth
       cho3%slope = cho1%slope + cho2%slope
       cho3%deg_btm_m = cho1%deg_btm_m + cho2%deg_btm_m
       cho3%deg_bank_m = cho1%deg_bank_m + cho2%deg_bank_m
       cho3%hc_m = cho1%hc_m + cho2%hc_m
       cho3%flo_in_mm = cho1%flo_in_mm + cho2%flo_in_mm
       cho3%aqu_in_mm = cho1%aqu_in_mm + cho2%aqu_in_mm
       cho3%flo_mm = cho1%flo_mm + cho2%flo_mm
       cho3%sed_stor = cho1%sed_stor + cho2%sed_stor
      end function
      
      function chsd_div (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo_in = ch1%flo_in
        ch2%aqu_in = ch1%aqu_in
        ch2%flo = ch1%flo
        ch2%peakr = ch1%peakr
        ch2%sed_in = ch1%sed_in / const
        ch2%sed_out = ch1%sed_out / const
        ch2%washld = ch1%washld / const
        ch2%bedld = ch1%bedld / const
        ch2%dep = ch1%dep / const
        ch2%deg_btm = ch1%deg_btm / const
        ch2%deg_bank = ch1%deg_bank / const
        ch2%hc_sed = ch1%hc_sed / const
        ch2%width = ch1%width
        ch2%depth = ch1%depth
        ch2%slope = ch1%slope
        ch2%deg_btm_m = ch1%deg_btm_m
        ch2%deg_bank_m = ch1%deg_bank_m
        ch2%hc_m = ch1%hc_m
        ch2%flo_in_mm = ch1%flo_in_mm / const
        ch2%aqu_in_mm = ch1%aqu_in_mm / const
        ch2%flo_mm = ch1%flo_mm / const
        ch2%sed_stor = ch1%sed_stor / const
      end function chsd_div
            
      function chsd_ave (ch1,const) result (ch2)
        type (sd_ch_output), intent (in) :: ch1
        real, intent (in) :: const
        type (sd_ch_output) :: ch2
        ch2%flo_in = ch1%flo_in / const
        ch2%aqu_in = ch1%aqu_in / const
        ch2%flo = ch1%flo / const
        ch2%peakr = ch1%peakr / const
        ch2%sed_in = ch1%sed_in
        ch2%sed_out = ch1%sed_out
        ch2%washld = ch1%washld
        ch2%bedld = ch1%bedld
        ch2%dep = ch1%dep
        ch2%deg_btm = ch1%deg_btm
        ch2%deg_bank = ch1%deg_bank
        ch2%hc_sed = ch1%hc_sed
        ch2%width = ch1%width / const
        ch2%depth = ch1%depth / const
        ch2%slope = ch1%slope / const
        ch2%deg_btm_m = ch1%deg_btm_m / const
        ch2%deg_bank_m = ch1%deg_bank_m / const
        ch2%hc_m = ch1%hc_m / const
        ch2%flo_in_mm = ch1%flo_in_mm
        ch2%aqu_in_mm = ch1%aqu_in_mm
        ch2%flo_mm = ch1%flo_mm
        ch2%sed_stor = ch1%sed_stor
      end function chsd_ave
      
      function chsd_mult (const, chn1) result (chn2)
        type (sd_ch_output), intent (in) :: chn1
        real, intent (in) :: const
        type (sd_ch_output) :: chn2
        chn2%flo_in = const * chn1%flo_in
        chn2%aqu_in = const * chn1%aqu_in
        chn2%flo = const * chn1%flo
        chn2%peakr = const * chn1%peakr
        chn2%sed_in = const * chn1%sed_in
        chn2%sed_out = const * chn1%sed_out
        chn2%washld = const * chn1%washld
        chn2%bedld = const * chn1%bedld
        chn2%dep = const * chn1%dep
        chn2%deg_btm = const * chn1%deg_btm
        chn2%deg_bank = const * chn1%deg_bank
        chn2%hc_sed = const * chn1%hc_sed 
        chn2%width = const * chn1%width
        chn2%depth = const * chn1%depth
        chn2%slope = const * chn1%slope
        chn2%deg_btm_m = const * chn1%deg_btm_m
        chn2%deg_bank_m = const * chn1%deg_bank_m
        chn2%hc_m = const * chn1%hc_m
        chn2%flo_in_mm = const * chn1%flo_in_mm
        chn2%aqu_in_mm = const * chn1%aqu_in_mm
        chn2%flo_mm = const * chn1%flo_mm
        chn2%sed_stor = const * chn1%sed_stor
      end function chsd_mult
    
      end module channel_module1