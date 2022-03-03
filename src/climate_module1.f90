      module climate_module1
     
      implicit none

      type exco_del_ratios          !!if delivery ratio for routing, all units in fraction delivered
        real :: flo = 0.            !! precip ratio     |volume of water
        real :: sed = 0.            !! t/ha             |sediment
        real :: orgn = 0.           !! kg N/ha          |organic N
        real :: sedp = 0.           !! kg P/ha          |organic P
        real :: no3 = 0.            !! kg N/ha          |NO3-N
        real :: solp = 0.           !! kg P/ha          |mineral (soluble P)
        real :: nh3 = 0.            !! kg N/ha          |NH3
        real :: no2 = 0.            !! kg N/ha          |NO2
      end type exco_del_ratios

      type precipitation
        character(len=25) :: name = ""
        real :: ave_ann_mm
      end type precipitation
      type (precipitation), dimension(:), allocatable, target :: precip

      end module climate_module1