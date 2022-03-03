      module exco_module1
    
      implicit none

      type exco_del_ratios          !!if delivery ratio for routing, all units in fraction delivered
        real :: flo = 0.            !! ha-m     |volume of water
        real :: sed = 0.            !! t                |sediment
        real :: orgn = 0.           !! kg N             |organic N
        real :: sedp = 0.           !! kg P             |organic P
        real :: no3 = 0.            !! kg N             |NO3-N
        real :: solp = 0.           !! kg P             |mineral (soluble P)
        real :: nh3 = 0.            !! kg N             |NH3
        real :: no2 = 0.            !! kg N             |NO2
      end type exco_del_ratios

      type export_coefficient1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        type (exco_del_ratios) :: load
      end type export_coefficient1
      type (export_coefficient1), dimension(:), allocatable, target :: exco1

      end module exco_module1 