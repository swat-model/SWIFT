      module reservoir_module1
    
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
            
      type reservoir_data
        real :: psa = 0.          !ha            |res surface area when res is filled to princ spillway
        real :: pvol = 0.         !ha-m          |vol of water needed to fill the res to the princ spillway (read in as ha-m
                                  !                and converted to m^3)
        real :: esa = 0.          !ha            |res surface area when res is filled to emerg spillway 
        real :: evol = 0.         !ha-m          |vol of water needed to fill the res to the emerg spillway (read in as ha-m
                                  !                and converted to m^3)
      end type reservoir_data
      
      type reservoir1
        character(len=25) :: name = ""
        integer :: obj_no
        real :: area_ha
        real :: km
        type (reservoir_data) :: res_db
        type (exco_del_ratios) :: trap
      end type reservoir1
      type (reservoir1), dimension(:), allocatable, target :: res1

      end module reservoir_module1