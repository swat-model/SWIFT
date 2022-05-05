      subroutine cha_control1 (iob)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

      use channel_module1
      use hydrograph_module !, only : ob, cha2
      
      implicit none

      integer,  intent (in) :: iob
      integer :: icha                   !none       |channel number
      integer :: icha_db                !none       |channel data pointer
      real :: trap_eff                  !frac       |sediment flood plain trap efficiency
      real :: dep_fr                    !frac       |fraction of sediment entering the flood plain that is deposited
      real :: fp_sed                    !frac       |annual sediment in flood plain flow
      real :: ch_flo                    !frac       |annual flow in channel flow
      real :: rad_curv                  !           |radius of curvature
      real :: bed_te                    !           |shear on channel bed
      real :: bank_te                   !           |shear on channel bank
      real :: t_crit                    !           |radius of curvature factor 
      real :: rc_fac                    !           |shear on channel bed
      real :: bank_kd                   !           |bank erodibility
      real :: cov_fac                   !           |cover factor
      real :: bank_ero                  !           |bank erosion
      real :: bed_ero                  !           |bed erosion
      
      icha = ob(iob)%num
      icha_db = ob(iob)%props
            
      !! calculate flood plain deposition and losses
      !! calculate sediment concntration at bankfull (Q1) using sediment rating curve exp
      fp_sed = (cha1(icha)%dat%q1_qm) ** cha1(icha)%dat%rcurv_exp
      !! fraction overbank sediment deposited - based on fall velocity and days inundated
      trap_eff = 1.2 * exp(-0.424 * cha1(icha)%dat%fp_inun_days)
      !! calculate deposition and outflow
      dep_fr = cha1(icha)%dat%fp_flo_fr * fp_sed * trap_eff
      dep_fr = amax1(0., dep_fr)
      dep_fr = amin1(1., dep_fr)
      cha2(icha)%fp_dep = dep_fr * ob(iob)%hin
      !ob(iob)%hd(1) = ob(iob)%hin - cha2(icha)%fp_dep 
      
      !! calculate in channel depsosition and losses -  input delivery ratio
      ch_flo = 1. - cha1(icha)%dat%fp_flo_fr
      cha2(icha)%ch_rout = 0.05 * ch_flo * ob(iob)%hin   !need to multiply by dr (not 0.5 - need input)
      
      !! calculate bank erosion
      rad_curv = (11.26 * cha1(icha)%dat%chw * cha1(icha)%dat%sinu ** 1.5) /        &
                                       (13. * (cha1(icha)%dat%sinu - 1.) ** 0.5)
      bed_te = 9810. * cha1(icha)%dat%chd * cha1(icha)%dat%chs
      bank_te = 0.75 * bed_te
      t_crit = 0.498 * 10 ** (0.0182 * cha1(icha)%dat%ch_clay)
      rc_fac = 3.62 * (rad_curv * cha1(icha)%dat%chw) ** -0.5234
      bank_kd = (cha1(icha)%dat%chw * cha1(icha)%dat%wd_yr) / (bank_te / t_crit - 1.)
      bank_kd = amax1(0., bank_kd)
      if (cha1(icha)%dat%chd > 2.) then
        cov_fac = 0.001
      else
        cov_fac = cha1(icha)%dat%cov
      end if
      bank_ero = bank_kd * (((bank_te * rc_fac * (1. - cov_fac)) / t_crit) - 1.)
      cha2(icha)%bank_ero%sed = amax1(0., bank_ero)
      bed_ero = bank_kd * ((bed_te / (2.587 * cha1(icha)%dat%d50)) - 1.)
      cha2(icha)%bed_ero%sed = amax1(0., bed_ero)
      !! calculate associated nutrients - ppm
      cha2(icha)%bank_ero%orgn = cha2(icha)%bank_ero%sed * cha1(icha)%dat%n_conc
      cha2(icha)%bank_ero%sedp = (1. - cha1(icha)%dat%p_bio) * cha2(icha)%bank_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bank_ero%no3 = 0.
      cha2(icha)%bank_ero%solp = cha1(icha)%dat%p_bio * cha2(icha)%bank_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bank_ero%nh3 = 0.
      cha2(icha)%bank_ero%no2 = 0.
      
      !! calculate instream losses
      

      !! sediment leaving channel
      ob(iob)%hd(1) = ob(iob)%hin + cha2(icha)%bank_ero + cha2(icha)%bed_ero - cha2(icha)%fp_dep - cha2(icha)%ch_rout
         
      write (1979, *) icha, cha1(icha)%name, cha1(icha)%area_ha
      write (1979, *) ' INFLOW    ', ob(iob)%hin
      write (1979, *) ' OUTFLOW   ', ob(iob)%hd(1)
      write (1979, *) ' BANK_ERO  ', cha2(icha)%bank_ero
      write (1979, *) ' BED_ERO   ', cha2(icha)%bed_ero
      write (1979, *) ' FP_DEP    ', cha2(icha)%fp_dep
      write (1979, *) ' CH_ROUT   ', cha2(icha)%ch_rout
      
      return
      end subroutine cha_control1