      subroutine reservoir_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use reservoir_module1
      use hydrograph_module, only : ob
      
      implicit none

      integer :: ires           !none          |reservoir number
      integer :: ires_db        !none          |reservoir data pointer
      integer,  intent (in) :: iob            !none          |object number
      
      ires = ob(iob)%num
      ires_db = ob(iob)%props
            
      !! calculate trap efficiency (deposition) - given inflow hyd, reservoir inputs
      !res1(ires)%trap
      
      return
      end subroutine reservoir_control1