      module aquifer_module1
    
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

      type aquifer1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        type (exco_del_ratios) :: trap
      end type aquifer1
      type (aquifer1), dimension(:), allocatable, target :: aqu1

      end module aquifer_module1