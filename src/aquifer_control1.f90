      subroutine aquifer_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use aquifer_module1
      use hydrograph_module !, only : ob, aqu2
      
      implicit none

      integer,  intent (in) :: iob
      integer :: iaqu           !none          |aquifer number
      integer :: iaqu_db        !none          |aquifer data pointer
      
      iaqu = ob(iob)%num
      iaqu_db = ob(iob)%props
            
      !! calculate trap efficiency
      ob(iob)%hd(1) = ob(iob)%hin ** aqu2(iaqu_db)%trap   ! ** is an intrinsic function to multiply 
         
      write (1978, *) iaqu, aqu1(iaqu)%name, aqu1(iaqu)%area_ha
      write (1978, *) ' INFLOW    ', ob(iob)%hin
      write (1978, *) ' OUTFLOW   ', ob(iob)%hd(1)
      
      !!!! write format changes
      !!!!write (1978,*) bsn%name, prog
      !!!!write (1978, "((I4,10X),(A10,6X),(F20.4,6X),(F20.4,1X),6(F20.4,1X),*(F20.4,1X)))") &
      !    iaqu, aqu1(iaqu)%name, aqu1(iaqu)%area_ha, ob(iob)%hin, ob(iob)%hd(1)
      
      return
      end subroutine aquifer_control1