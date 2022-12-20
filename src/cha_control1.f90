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
      integer :: idr                    !none       |pointer to instream delivery ratio
      real :: trap_eff                  !frac       |sediment flood plain trap efficiency
      real :: depo_fr                   !frac       |fraction of sediment entering the flood plain that is deposited
      real :: fp_sed_conc               !frac       |annual sediment in flood plain flow
      real :: ch_flo                    !frac       |annual flow in channel flow
      real :: rad_curv                  !           |radius of curvature
      real :: bed_te                    !           |shear on channel bed
      real :: bank_te                   !           |shear on channel bank
      real :: t_crit                    !           |radius of curvature factor 
      real :: rc_fac                    !           |shear on channel bed
      real :: bank_kd                   !           |bank erodibility
      real :: cov_fac                   !           |cover factor
      real :: bank_ero                  !           |bank erosion
      real :: bed_ero                   !           |bed erosion
      real :: e_days                    !days       !days for calculating bank erosion
      real :: arc_len                   !m          |arc length
      real :: ebank_t                   !t          |bank erosion in tons
      real :: ebank_m                   !m          |bank erosion in meters
      real :: d50_m                     !m          |median bed particle size
      real :: str_pow                   !           |stream power
      real :: str_pow_cr                !           |critical stream power to initiate transport
      real :: sedcap_ppm                !ppm        |sediment transport capacity in ppm
      real :: sedcap_t                  !t          |sediment transport capacity in tons
      real :: sedin_t                   !t          |sediment entering reach
      real :: ebtm_t                    !t          |bed erosion in tons
      real :: ebtm_m                    !m          |bed erosion in meters
      real :: dep                       !t          |sediment deposition in channel
      real :: qcms                      !m3/s       |flow rate at bankfull
      real :: hyd_rad                   !m          |hydraulic radius
      real :: xarea                     !m2         |cross sectional area
      real :: b_exp                     !           |exponent on v/vcr
      real :: vel                       !m/s        |flow velocity
      real :: vel_bend                  !m/s        |flow velocity in meander bend
      real :: vel_tot                   !m/s        |mean flow velocity
      real :: vel_crit                  !m/s        |critical flow velocity to initiate bed movement
      real :: cohesion                  !??         |soil cohesion
      real :: veg                       !           |cover factor
      real :: fpdepo_mm                 !mm         |flood plain sediment deposition
      real :: fpwidth_km                !mm         |flood plain width
      real :: w_yr                      !           |widths of bank erosion per year
      real :: d_yr                      !           |depths of downcutting per year
      
      icha = ob(iob)%num
      icha_db = ob(iob)%props
      cha1(icha)%area_ha = ob(iob)%area_ha
      cha1(icha)%name = ob(iob)%name
            
      !! check equations with Peter
      !cha1(icha)%dat%chw = 150.
      !cha1(icha)%dat%chd = 3.
      !cha1(icha)%dat%chs = 0.0009
      !cha1(icha)%dat%ch_clay = 35.
      !cha1(icha)%dat%ch_bd = 1.43
      !cha1(icha)%dat%sinu = 1.4
      
      !! calculate flood plain deposition and losses
      !! calculate sediment concentration at bankfull (Q1) using sediment rating curve exp
      fp_sed_conc = (cha1(icha)%sed%q1_qm) ** cha1(icha)%sed%rcurv_exp
      !! fraction overbank sediment deposited - based on fall velocity and days inundated
      trap_eff = 1.2 * exp(-0.424 * cha1(icha)%sed%fp_inun_days)
      !! calculate deposition and outflow
      depo_fr = cha1(icha)%sed%fp_flo_fr * fp_sed_conc * trap_eff
      depo_fr = amax1(0., depo_fr)
      depo_fr = amin1(1., depo_fr)
      cha2(icha)%fp_dep = depo_fr * ob(iob)%hin
      
      !! calculate in channel depsosition and losses -  input delivery ratio
      ch_flo = 1. - cha1(icha)%sed%fp_flo_fr
      cha2(icha)%ch_rout = cha_dr(icha)%instr_dr ** (ch_flo * ob(iob)%hin)
      
      !! calculate bank erosion
      b_exp = 8.53 / (1. + exp(-29.11 + 1.8 * cha1(icha)%dat%ch_clay)) ** 0.0248
      vel = 1.37 * (cha1(icha)%dat%chs ** 0.31) * (12. * cha1(icha)%dat%chw) ** 0.32
      rad_curv = (12. * cha1(icha)%dat%chw * cha1(icha)%dat%sinu ** 1.5) /        &
                                       (13. * (cha1(icha)%dat%sinu - 0.999) ** 0.5)
      vel_bend = vel * (1.74 - 0.52 * log10 (rad_curv / cha1(icha)%dat%chw))
      vel_tot = 0.33 * vel_bend + 0.66 * vel
      cohesion = (-87.1 + (42.82 * cha1(icha)%dat%ch_clay) - (0.267 * cha1(icha)%dat%ch_clay ** 2.) &
                                     + (0.029 * cha1(icha)%dat%ch_clay ** 3.)) * 2.
      cohesion = amax1 (1000., cohesion)    !min 1000 for sandy soils
      veg = 0.                              !function of cover factor
      vel_crit = log10 (8.8 * cha1(icha)%dat%chd / 0.004) * (0.0004 * ((cha1(icha)%dat%ch_bd *     &
                        1000. - 1000.) * 9.81 * 0.004 + 0.021 * cohesion + veg)) ** 0.5
      e_days = 3.65     !assume 1% of time (q1 flows)
      ebank_m = 0.0024 * e_days * (vel_tot / vel_crit) ** b_exp
          
      !! calc mass of sediment eroded -> t = bankcut (m) * depth (m) * lengthcut (m) * bd (t/m3)
      !! arc length = 0.33 * meander wavelength * sinuosity  -> protected length 
      arc_len = 0.33 *  (12. * cha1(icha)%dat%chw) * cha1(icha)%dat%sinu
      ebank_t = ebank_m * cha1(icha)%dat%chd * arc_len * cha1(icha)%dat%ch_bd
      cha2(icha)%bank_ero%sed = amax1 (0., ebank_t)
      
      !!calculate bed erosion
      d50_m = cha1(icha)%dat%d50 / 1000.
      !! compute flow rate at bankfull - assume rectangular
      xarea = cha1(icha)%dat%chw * cha1(icha)%dat%chd
      hyd_rad = xarea / (cha1(icha)%dat%chw + 2. * cha1(icha)%dat%chd)
      qcms = xarea * hyd_rad ** .6666 * Sqrt(cha1(icha)%dat%chs) / (cha1(icha)%dat%chn + 0.0001)
      str_pow = 9810. * qcms * cha1(icha)%dat%chs / cha1(icha)%dat%chw !stream power
      str_pow_cr = 100. * (16.2 * d50_m) ** 1.5
      if (str_pow - str_pow_cr > 1.e-6) then
        sedcap_ppm = 0.0214 * (str_pow - str_pow_cr) ** 1.5 / d50_m * (qcms /   &
                                                 cha1(icha)%dat%chw) ** -0.833
      else
        sedcap_ppm = 0.
      end if
      sedcap_t = sedcap_ppm * ob(iob)%hin%flo / 1000000.  !t = ppm (g/m3) * 1 t/m3 / 1000000. g/t
      !! concentration of incoming and bank erosion
      sedin_t = ob(iob)%hin%sed + ebank_t - cha2(icha)%fp_dep%sed
      ebtm_t = sedcap_t - sedin_t
      if (ebtm_t > 0.) then
        cha2(icha)%bed_ero%sed = ebtm_t
        !! m = t / (t/m3 * m * km * 1000 m/km)
        ebtm_m = ebtm_t / (1.3 * cha1(icha)%dat%chw * cha1(icha)%dat%chl * 1000.)
      else
        cha2(icha)%bed_dep%sed = -ebtm_t
        cha2(icha)%bed_dep%orgn = (-ebtm_t / sedin_t) * (ob(iob)%hin%orgn - cha2(icha)%fp_dep%orgn)
        cha2(icha)%bed_dep%sedp = (-ebtm_t / sedin_t) * (ob(iob)%hin%sedp - cha2(icha)%fp_dep%sedp)
        ebtm_m = 0.
      end if
      
      !rad_curv = (11.26 * cha1(icha)%dat%chw * cha1(icha)%dat%sinu ** 1.5) /        &
      !                                 (13. * (cha1(icha)%dat%sinu - 1.) ** 0.5)
      !bed_te = 9810. * cha1(icha)%dat%chd * cha1(icha)%dat%chs
      !bank_te = 0.75 * bed_te
      !t_crit = 0.498 * 10 ** (0.0182 * cha1(icha)%dat%ch_clay)
      !rc_fac = 3.62 * (rad_curv * cha1(icha)%dat%chw) ** -0.5234
      !bank_kd = (cha1(icha)%dat%chw * cha1(icha)%sed%wd_yr) / (bank_te / t_crit - 1.)
      !bank_kd = amax1(0., bank_kd)
      !if (cha1(icha)%dat%chd > 2.) then
      !  cov_fac = 0.001
      !else
      !  cov_fac = cha1(icha)%dat%cov
      !end if
      !bank_ero = bank_kd * (((bank_te * rc_fac * (1. - cov_fac)) / t_crit) - 1.)
      !cha2(icha)%bank_ero%sed = amax1(0., bank_ero)
      !bed_ero = bank_kd * ((bed_te / (2.587 * cha1(icha)%dat%d50)) - 1.)
      !cha2(icha)%bed_ero%sed = amax1(0., bed_ero)
      
      !! calculate associated nutrients - ppm
      cha2(icha)%bank_ero%orgn = cha2(icha)%bank_ero%sed * cha1(icha)%dat%n_conc
      cha2(icha)%bank_ero%sedp = (1. - cha1(icha)%dat%p_bio) * cha2(icha)%bank_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bank_ero%no3 = 0.
      cha2(icha)%bank_ero%solp = cha1(icha)%dat%p_bio * cha2(icha)%bank_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bank_ero%nh3 = 0.
      cha2(icha)%bank_ero%no2 = 0.

      !! calculate associated nutrients - ppm
      cha2(icha)%bed_ero%orgn = cha2(icha)%bed_ero%sed * cha1(icha)%dat%n_conc
      cha2(icha)%bed_ero%sedp = (1. - cha1(icha)%dat%p_bio) * cha2(icha)%bed_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bed_ero%no3 = 0.
      cha2(icha)%bed_ero%solp = cha1(icha)%dat%p_bio * cha2(icha)%bed_ero%sed * cha1(icha)%dat%p_conc
      cha2(icha)%bed_ero%nh3 = 0.
      cha2(icha)%bed_ero%no2 = 0.

      !! sediment leaving channel
      ob(iob)%hd(1) = ob(iob)%hin + cha2(icha)%bank_ero + cha2(icha)%bed_ero - cha2(icha)%fp_dep
         
      write (1979, *) icha, cha1(icha)%name, cha1(icha)%area_ha
      write (1979, *) ' INFLOW    ', ob(iob)%hin
      write (1979, *) ' OUTFLOW   ', ob(iob)%hd(1)
      write (1979, *) ' BANK_ERO  ', cha2(icha)%bank_ero
      write (1979, *) ' BED_ERO   ', cha2(icha)%bed_ero
      write (1979, *) ' BED_DEP   ', cha2(icha)%bed_dep
      write (1979, *) ' FP_DEP    ', cha2(icha)%fp_dep
      write (1979, *) ' CH_ROUT   ', cha2(icha)%ch_rout
      
      !! bank parameter output
      w_yr = ebank_m / cha1(icha)%dat%chw
      write (1984, *) icha, cha1(icha)%name, cha1(icha)%area_ha, cha1(icha)%dat%chw, cha1(icha)%dat%chl,    &
          cha1(icha)%dat%ch_bd, cha1(icha)%dat%sinu, vel_tot, vel_crit, cohesion, e_days, rad_curv,         &
          arc_len, ebank_t, ebank_m, w_yr
      
      !! bed parameter output
      d_yr = ebtm_m / cha1(icha)%dat%chd
      write (1985, *) icha, cha1(icha)%name, cha1(icha)%area_ha, cha1(icha)%dat%chw, cha1(icha)%dat%chd,    &
          cha1(icha)%dat%chl, d50_m, qcms, str_pow, str_pow_cr, sedcap_ppm, sedcap_t, sedin_t, ebtm_t,      &
          ebtm_m, d_yr
      
      !! flood plain parameter output
      fpwidth_km = 0.000012 * (100. * cha1(icha)%area_ha) ** 0.982 !Beighley and Gummadi, ESPL
      !! mm = t / (km*km*t/m3) * km/1000.m * km/1000.m * 1000.mm/m = t/(km*km*t/m3) / 1000.
      fpdepo_mm = cha2(icha)%fp_dep%sed / (1000. * fpwidth_km *  cha1(icha)%dat%chl * cha1(icha)%dat%ch_bd)
      write (1986, *) icha, cha1(icha)%name, cha1(icha)%area_ha, cha1(icha)%sed%q1_qm,                      &
          cha1(icha)%sed%rcurv_exp, cha1(icha)%sed%fp_inun_days, cha1(icha)%sed%fp_flo_fr, fp_sed_conc,     &
          trap_eff, depo_fr, fpwidth_km, ob(iob)%hin%sed, cha2(icha)%fp_dep%sed, fpdepo_mm
      
      return
      end subroutine cha_control1