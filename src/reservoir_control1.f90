      subroutine reservoir_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use reservoir_module1
      use reservoir_data_module
      use hydrograph_module !, only : ob
      
      implicit none

      integer :: ires                   !none       |reservoir number
      integer :: ires_db                !none       |reservoir data pointer
      integer,  intent (in) :: iob      !none       |object number
      real :: residence                 !m3/m3      |residence time
      real :: trap_eff                  !frac       |Brune reservoir trap efficiency
      
      ires = ob(iob)%num
      ires_db = ob(iob)%props
      res1(ires)%area_ha = ob(iob)%area_ha
            
      !! calculate Brune trap efficiency (deposition)
      if (ob(iob)%hin%flo > 1.) then
        residence = res1(ires)%res_db%pvol / ob(iob)%hin%flo
        trap_eff = residence / (.012 + 1.02 * residence)
      else
        trap_eff = 0.
      end if
      
      !! flow and constituents leaving reservoir
      ob(iob)%hd(1) = (1. - trap_eff) * ob(iob)%hin
      
      write (1977, *) ires, res1(ires)%name, res1(ires)%area_ha
      write (1977, *) ' INFLOW    ', ob(iob)%hin
      write (1977, *) ' OUTFLOW   ', ob(iob)%hd(1)
      
      return
      end subroutine reservoir_control1