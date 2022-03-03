      subroutine cha_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use channel_module1
      use hydrograph_module, only : ob
      
      implicit none

      integer,  intent (in) :: iob
      integer :: icha           !none          |channel number
      integer :: icha_db        !none          |channel data pointer
      
      icha = ob(iob)%num
      icha_db = ob(iob)%props
            
      !! calculate erosion and deposition - given inflow hyd, channel inputs, Q1/Qm ratio?
      
      !! calculate bank erosion
      !cha1(icha)%bank_ero
      
      !! calculate downcutting
      !cha1(icha)%downcut
      
      !! calculate depsosition
      !cha1(icha)%dep
        
      !! sediment leaving channel
      !ob(iob)%hd(1) = ob(iob)%hin + cha1(icha)%bank_ero + cha1(icha)%downcut - cha1(icha)%dep 
      
      return
      end subroutine cha_control1