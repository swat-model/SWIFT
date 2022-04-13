      subroutine hru_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use hru_module1
      use hydrograph_module, only : ob
      
      implicit none

      integer :: j                  !none          |same as ihru (hru number)
      integer :: ihru
      integer,  intent (in) :: iob
      
      ihru = ob(iob)%props
      
      !! if no routing or wetland, ob()%hd(:) is set from the inputs
      
      !! if routing over the hru, add the runon/runoff
      if (hru1(ihru)%rout_over == "y") then
          
          ob(iob)%hd(3)%flo = ob(iob)%hd(3)%flo + ob(iob)%hin_sur%flo * hru2(ihru)%rout_dr(3)%flo
          ob(iob)%hd(4)%flo = ob(iob)%hd(4)%flo + ob(iob)%hin_lat%flo * hru2(ihru)%rout_dr(4)%flo
          ob(iob)%hd(5)%flo = ob(iob)%hd(5)%flo + ob(iob)%hin_lat%flo * hru2(ihru)%rout_dr(5)%flo
          ob(iob)%hd(1)%flo = ob(iob)%hd(3)%flo + ob(iob)%hd(4)%flo + ob(iob)%hd(5)%flo
          !! don't adjust percolation
          !! need sed, orgn, sedp, no3, solp, nh3, no2 for each hdyrograph
      end if
   
      !! if wetland, use wetland model
      if (hru1(ihru)%surf_stor == "y") then
        !! call wetland model - similar to reservoir
      end if
      
      return
      end subroutine hru_control1