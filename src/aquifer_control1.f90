      subroutine aquifer_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use hydrograph_module
      
      implicit none

      integer,  intent (in) :: iob
      integer :: iaqu           !none          |aquifer number
      integer :: iaqu_db        !none          |aquifer data pointer
      
      iaqu = ob(iob)%num
      iaqu_db = ob(iob)%props
            
      !! calculate trap efficiency
      ob(iob)%hd(1) = ob(iob)%hin ** aqu1(iaqu_db)%trap   ! ** is an intrinsic function to multiply 
      
      return
      end subroutine aquifer_control1