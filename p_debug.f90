!> @file p_debug.f90
!! This file contaions external procedure(s) for testing and debugging. By the
!! separate nature, all the procedures from this file are `external`.
!! @warning They should not be used or called from the normal model code,
!!          nor referred in the main Makefile.
!! @note    By the temporary and ephemeral nature of the debugging code, the
!!          @ref intro_style_rules "coding style" requirements are greatly
!!          relaxed here.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!> Debugging and testing lifecycle, random wakls etc.
!! Has been initially implemented as `DEBUG_03: block`
!! @warning Code formatting style is not necessarily adhered to here.
subroutine life_cycles_DEBUG_TEST

  use COMMONDATA
  use FILE_IO
  use THE_EVOLUTION

  real(SRP) :: visrange, irradiance, step_rwalk, cost_step
  integer :: food_item_selected, conspecif_selected
  integer :: i, iii, ind
  type(FOOD_RESOURCE) :: joined_food_res_tmp

  type(TIMER_CPU) :: stopwatch_walk_wise

  real(SRP), dimension(100) ::  way_passed ! way = distance between walks.

  real(SRP) :: surlig_log_dt, surlig_log_st

  !> PROCNAME is the procedure name for logging and debugging
  character(len=*), parameter :: PROCNAME = "(life_cycles_DEBUG_TEST)"

  integer :: agent_in

  call LOG_CONFIGURE("writeonstdout" , .TRUE.)
  call LOG_MSG( "*** START DEBUG BLOCK ***" )
  call LOG_CONFIGURE("writeonstdout" , IS_SCREEN_OUTPUT)

  !> ### Testing and debugging: description ###
  !> Testing how the procedure for joining several food resources works.
  !! Here it just joins the food resources in the safe and dangerous
  !! habitats. Because the agents are limited to stay in the safe, this just
  !! creates an extra processing overhead.
  call joined_food_res_tmp%join( habitat_safe%food, habitat_dangerous%food, &
                                 reindex=.TRUE., label="TEMPORARY" )

  call joined_food_res_tmp%save_csv(csv_file_name="foods_joined.csv")
  call habitat_safe%food%save_csv(csv_file_name="foods_safe.csv")
  call habitat_dangerous%food%save_csv(csv_file_name="foods_dangerous.csv")

  Global_Rescale_Maximum_Motivation = 6.0_SRP

  DO_INDS: do ind = 1, 20

    if (proto_parents%individual(ind)%is_dead()) exit DO_INDS

    call LOG_CONFIGURE("writeonstdout" , .TRUE.)
    call LOG_MSG( "Individual " // TOSTR(ind) )
    call LOG_CONFIGURE("writeonstdout" , IS_SCREEN_OUTPUT)

    call LOG_DBG(LTAG_INFO // "Body  mass: " // TOSTR(proto_parents%individual(ind)%get_mass()))
    call LOG_DBG(LTAG_INFO // "Birth mass: " // TOSTR(proto_parents%individual(ind)%get_mass_birth()))
    call LOG_DBG(LTAG_INFO // "Body  leng: " // TOSTR(proto_parents%individual(ind)%get_length()))
    call LOG_DBG(LTAG_INFO // "Energy:     " // TOSTR(proto_parents%individual(ind)%get_energy() ))

    call LOG_DBG( LTAG_INFO // "Standard cost of swimming 1 SL  : " //       &
                  TOSTR(proto_parents%individual(ind)%cost_swim_std(1)) )
    call LOG_DBG( LTAG_INFO // "Standard cost of swimming 100 SL: " //       &
                  TOSTR(proto_parents%individual(ind)%cost_swim_std(100)) )
    call LOG_DBG( LTAG_INFO // "Standard cost of swimming LS SL: " //        &
                  TOSTR(proto_parents%individual(ind)%cost_swim_std()) )
    call LOG_DELIMITER(LOG_LEVEL_CHAPTER)

    WALKS_BEH: do i=1, 500 ! LIFESPAN

      if (proto_parents%individual(ind)%is_dead()) then
        call LOG_MSG("*****************")
        call LOG_MSG("AGENT " // TOSTR(proto_parents%individual(ind)%get_id()) // " DEAD" )
        exit WALKS_BEH
      end if

      !Global_Time_Step_Model_Current = i

      !> Initialise stopwatch for timing each walk.
      call stopwatch_walk_wise%start("Walk # " // TOSTR(i))

      if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
      call LOG_DBG("Agent walk no=" // TOSTR(i) // " , agent ID " //    &
                  TOSTR(proto_parents%individual(ind)%get_id()) //      &
                  " (# " // TOSTR(ind) // "), name:"                    &
                  // trim(proto_parents%individual(ind)%individ_label())&
                  // ", is male: " // TOSTR(proto_parents%individual(ind)%is_male()) // ".")

      call LOG_DBG ("Agent body length: " //                            &
              TOSTR(proto_parents%individual(ind)%body_length) //       &
              ", body mass: " //                                        &
              TOSTR(proto_parents%individual(ind)%body_mass) //         &
              ", energy: " //                                           &
              TOSTR(proto_parents%individual(ind)%get_energy())    )

      surlig_log_dt = light_surface(Global_Time_Step_Model_Current)
      surlig_log_st = light_surface(Global_Time_Step_Model_Current, DAYLIGHT_STOCHASTIC)

      call LOG_DBG ("  Light at surface deterministic: " //                 &
                    TOSTR(surlig_log_dt) //                                 &
                    ", stochastic: " //                                     &
                    TOSTR(surlig_log_st) //                                 &
                    ", light at depth: " //                                 &
                    TOSTR(proto_parents%individual(ind)%dpos()) //          &
                    " is :" //                                              &
                    TOSTR(light_depth(proto_parents%individual(ind)%dpos(), &
                            light_surface(Global_Time_Step_Model_Current,   &
                                                  DAYLIGHT_STOCHASTIC)))    )

      agent_in = proto_parents%individual(ind)%find_environment()

      call LOG_DBG( LTAG_INFO // "Agent is in the environment " //          &
                      TOSTR(agent_in) // ", with name " //                  &
                      Global_Habitats_Available(agent_in)%get_label() )

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! PRODUCE PERCEPTIONS
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !===================================================
      ! Inner perceptions: stomach, bodymass, energy, age, reproductive factor
      call proto_parents%individual(ind)%perceptions_inner()

      !===================================================
      ! Environmental perceptions: light, depth
      call proto_parents%individual(ind)%perceptions_environ()
      call LOG_DBG("Environmental perceptions: light " //               &
        TOSTR(proto_parents%individual(ind)%perceive_light%get_current()) // &
        ", depth " //                                                   &
        TOSTR(proto_parents%individual(ind)%perceive_depth%get_current()) )

      call LOG_DBG("Agent's data: depth: " // TOSTR(proto_parents%individual(ind)%dpos()) )

      !===================================================
      ! Spatial perceptions food, conspecifics, predators
      call proto_parents%individual(ind)%see_food(                          &
                                  Global_Habitats_Available( agent_in )%food )
      !call proto_parents%individual(ind)%see_food( habitat_safe%food )
      !call proto_parents%individual(ind)%see_food(joined_food_res_tmp)
      call proto_parents%individual(ind)%see_consp( proto_parents%individual )
      call proto_parents%individual(ind)%see_pred( habitat_safe%predators )
      !> add perceptions to the memory
      call proto_parents%individual(ind)%perception_to_memory()

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! PRODUCE MOTIVATIONS AND GOS
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      !===================================================
      call proto_parents%individual(ind)%motivations_percept_components()
      call proto_parents%individual(ind)%motivations_primary_calc()
      !> Primary motivations are logged in the @ref intro_debug_mode
      !! "debug mode".
      call LOG_DBG( LTAG_INFO // "Primary motivations: " //                                         &
                "hunger: " //                                                                         &
                  TOSTR(proto_parents%individual(ind)%motivations%hunger%motivation_prim)  //         &
                ", avoid_passive: "//                                                                 &
                  TOSTR(proto_parents%individual(ind)%motivations%avoid_passive%motivation_prim) //   &
                ", avoid_active: " //                                                                 &
                  TOSTR(proto_parents%individual(ind)%motivations%avoid_active%motivation_prim) //    &
                ", reproduce: " //                                                                    &
                  TOSTR(proto_parents%individual(ind)%motivations%reproduction%motivation_prim),      &
                PROCNAME, MODNAME )
      call proto_parents%individual(ind)%modulation()
      call LOG_DBG( LTAG_INFO // "Motivations post modulation: " //                                   &
                "hunger: " //                                                                         &
                  TOSTR(proto_parents%individual(ind)%motivations%hunger%motivation_finl)  //         &
                ", avoid_passive: "//                                                                 &
                  TOSTR(proto_parents%individual(ind)%motivations%avoid_passive%motivation_finl) //   &
                ", avoid_active: " //                                                                 &
                  TOSTR(proto_parents%individual(ind)%motivations%avoid_active%motivation_finl) //    &
                ", reproduce: " //                                                                    &
                  TOSTR(proto_parents%individual(ind)%motivations%reproduction%motivation_finl),      &
                PROCNAME, MODNAME )
      call proto_parents%individual(ind)%motivations_to_memory()
      call proto_parents%individual(ind)%gos_find()

      ! Determine population-wise maximum motivation for threshold.
      Global_Rescale_Maximum_Motivation =                                   &
              maxval( proto_parents%individual%motivations%max_perception() )

      call LOG_DBG( LTAG_INFO // "*** Maximum motivation for rescale:" //   &
                    TOSTR(Global_Rescale_Maximum_Motivation) //             &
                    ", whole population." )

      call LOG_DBG(LTAG_INFO // "*** Agent can see foods: " //              &
              TOSTR(proto_parents%individual(ind)%perceive_food%get_count()))
      if ( proto_parents%individual(ind)%has_food() ) then
        call LOG_DBG("  distance     >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_distances))
        call LOG_DBG("  size         >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen%get_size() ))
        call LOG_DBG("  dist. (d/sl) >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_distances &
                        / proto_parents%individual(ind)%get_length()))
        block
          integer :: i
          real(SRP), allocatable, dimension(:) :: visibility, p_capture
          allocate(visibility(proto_parents%individual(ind)%perceive_food%get_count()))
          allocate(p_capture(proto_parents%individual(ind)%perceive_food%get_count()))
          do i=1,proto_parents%individual(ind)%perceive_food%get_count()
            visibility(i)=proto_parents%individual(ind)%perceive_food%foods_seen(i)%visibility()
            p_capture(i)=                                                                      &
              proto_parents%individual(ind)%perceive_food%foods_seen(i)%capture_probability(   &
                     distance=proto_parents%individual(ind)%perceive_food%foods_distances(i) )
          end do
          call LOG_DBG("  vis range    >" //  TOSTR(visibility))
          call LOG_DBG("  dist. (d/vr) >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_distances &
                        / visibility ))
          call LOG_DBG("  P capture    >" //  TOSTR(p_capture))
        end block
      end if
      call LOG_DBG("  average distance: " //                                      &
                    TOSTR(proto_parents%individual(ind)%perceive_food%get_meandist()))
      call LOG_DBG("  memory food N: " //                            &
                    TOSTR(proto_parents%individual(ind)%memory_stack%memory_food))
      call LOG_DBG("  memory food dist: " //                            &
                    TOSTR(proto_parents%individual(ind)%memory_stack%memory_foodist))
      call LOG_DBG("  memory food size: " //                            &
                    TOSTR(proto_parents%individual(ind)%memory_stack%memory_foodsiz))
      call LOG_DBG("  average distance in memory: " //                            &
                    TOSTR(proto_parents%individual(ind)%memory_stack%get_food_mean_dist()))
      call LOG_DBG("  probability of capture subjective: " //                            &
                    TOSTR(proto_parents%individual(ind)%food_probability_capture_subjective()))

      call LOG_DBG(LTAG_INFO // "*** Agent can see conspecifics: " //       &
              TOSTR(proto_parents%individual(ind)%perceive_consp%get_count()))

      if ( proto_parents%individual(ind)%has_consp() ) then
        call LOG_DBG("  iid         >" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_cid()))
        !call LOG_DBG("  sex (is male), subjective >" //                              &
        !                 TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen%is_male()))
        call LOG_DBG("  sex (is male), objective  >" //                              &
                         TOSTR(proto_parents%individual(               &
                         proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_cid() )%is_male()))
        !call LOG_DBG("  mass subjective >" //                              &
        !                 TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_mass()))
        call LOG_DBG("  mass objective  >" //                              &
                         TOSTR(proto_parents%individual(               &
                         proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_cid() )%get_mass()))
        call LOG_DBG( "Probability of reproduction: " //                 &
            TOSTR(proto_parents%individual(ind)%probability_reproduction()) )
      end if

      call LOG_DBG(LTAG_INFO // "*** Agent can see predators: " //          &
              TOSTR(proto_parents%individual(ind)%perceive_predator%get_count()))

      if (proto_parents%individual(ind)%starved_death()) then
        call proto_parents%individual(ind)%dies()
        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_SECTION)
        call LOG_DBG ( "INFO: Agent dies due to starvation, ID: " //     &
                       TOSTR(proto_parents%individual(ind)%get_id()) //  &
                       "at time step " // TOSTR(i) )
        call LOG_DBG ("      Body length: " //                          &
              TOSTR(proto_parents%individual(ind)%body_length) //       &
              ", body mass: " //                                        &
              TOSTR(proto_parents%individual(ind)%body_mass) //         &
              ", maximum mass: " //                                     &
              TOSTR(proto_parents%individual(ind)%body_mass_maximum) // &
              ", birth mass : " //                                      &
              TOSTR(proto_parents%individual(ind)%body_mass_birth)     )
        call LOG_DBG("       Energy :" //                               &
              TOSTR(proto_parents%individual(ind)%energy_current) //    &
              ", energy maximum: " //                                   &
              TOSTR(proto_parents%individual(ind)%energy_maximum)    )
        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_SECTION)
        call LOG_DBG(stopwatch_walk_wise%log())
        exit WALKS_BEH
      end if

      call LOG_DBG( "GOS is      :" // proto_parents%individual(ind)%gos_label() )
      call LOG_DBG( "GOS arousal :" // TOSTR(proto_parents%individual(ind)%arousal()) )


      call LOG_DBG("Before walk:")
      call LOG_DBG("   +++ Current mass: " // TOSTR(proto_parents%individual(ind)%mass()) //  &
                   ", length: " // TOSTR(proto_parents%individual(ind)%length()) //       &
                   ", energy: " // TOSTR(proto_parents%individual(ind)%get_energy())  )

      call LOG_DBG( LTAG_INFO // "Mean distance to food items: " //                   &
            TOSTR(proto_parents%individual(ind)%memory_stack%get_food_mean_dist()) ,  &
            PROCNAME, MODNAME )
      block
        real(SRP) :: dist_food_sl
        dist_food_sl = proto_parents%individual(ind)%memory_stack%get_food_mean_dist() /    &
                          proto_parents%individual(ind)%get_length()
        call LOG_DBG( LTAG_INFO // "Mean distance to food items: " //                       &
                      TOSTR(dist_food_sl) // " SL units of agent ",                         &
                      PROCNAME, MODNAME )
      end block

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! PRODUCE BEHAVIOURS
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
      call LOG_DBG(LTAG_INFO // "START BEHAVIOUR (do_behave)")

      call proto_parents%individual(ind)%do_behave(                         &
                                  rescale_max_motivation=                   &
                                         Global_Rescale_Maximum_Motivation)

      if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
      call LOG_DBG(LTAG_INFO // "EXECUTED BEHAVIOUR: " //                   &
                            proto_parents%individual(ind)%behaviour_is() )
      if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      call LOG_DBG("After walk:")
      call LOG_DBG("   +++ Current mass: " // TOSTR(proto_parents%individual(ind)%mass()) //  &
                   ", length: " // TOSTR(proto_parents%individual(ind)%length()) //       &
                   ", energy: " // TOSTR(proto_parents%individual(ind)%get_energy())  )

      call LOG_DBG( LTAG_INFO // "Current testosterone: " //                      &
                    TOSTR(proto_parents%individual(ind)%testosterone_level) //    &
                    ", current estrogen: " //                                     &
                    TOSTR(proto_parents%individual(ind)%estrogen_level ) )

      if ( proto_parents%individual(ind)%is_male() )   &
        call LOG_DBG( LTAG_INFO // "Testosterone: " // TOSTR(proto_parents%individual(ind)%testosterone_history ) )

      if ( proto_parents%individual(ind)%is_female() )   &
        call LOG_DBG( LTAG_INFO // "Estrogen: " // TOSTR(proto_parents%individual(ind)%estrogen_history ) )

      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      ! Update sex steroids, subtract living cost
      !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      call proto_parents%individual(ind)%sex_steroids_update()
      call proto_parents%individual(ind)%subtract_living_cost()
      call proto_parents%individual(ind)%energy_update()


      call proto_parents%individual(ind)%age_increment()

    end do WALKS_BEH

  end do DO_INDS

  call generation_one%fitness_calc()

  !> @important Save the individual data for the whole parent population,
  !!            generation one, to a csv file.
  SAVE_DATA_INDS_DEBUG_END: block
    character(len=:), allocatable :: output_data_file
    !> Save individual data
    output_data_file = "agents_" // MODEL_NAME // "_" // MMDD //            &
                        "_gen_" // TOSTR(Global_Generation_Number_Current,  &
                        GENERATIONS) // "_end" // csv
    call proto_parents%save_csv(output_data_file, is_logging=.TRUE.)
  end block SAVE_DATA_INDS_DEBUG_END

  ! Note that disassemble is necessary to update the original habitats
  ! from the global habitat array
  call disassemble ( habitat_safe, habitat_dangerous )

  call joined_food_res_tmp%save_csv(csv_file_name="foods_joined_after.csv")
  call habitat_safe%food%save_csv(csv_file_name="foods_safe_after.csv")
  call habitat_dangerous%food%save_csv(csv_file_name="foods_dangerous_after.csv")



  call system_halt(message="NORMAL TERMINATION DEBUG TEST PROCEDURE")

  !###########################################################################

  INDS: do ind=1, 20 !! proto_parents%population_size

    if (proto_parents%individual(ind)%is_dead()) exit INDS

    way_passed = MISSING

    !call LOG_DBG("**** ind=" // TOSTR(ind))
    ! place agent from zero point
    !call proto_parents%individual(ind)%position( SPATIAL(0., 0., 0.) )
    ! place agent randomly not needed as they are positioned uniformly at
    !   GENERATIONS_LOOP_GA
    !call proto_parents%individual(ind)%place_uniform(habitat_safe)

    if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_VOLUME)
    call LOG_DBG("********** Agent " // TOSTR(ind) // "*****************")
    call LOG_DBG("Agent location one:" //                               &
                    TOSTR(proto_parents%individual(ind)%location(.TRUE.)))
    call LOG_DBG ("Agent body length: " //                              &
              TOSTR(proto_parents%individual(ind)%body_length) //       &
              ", body mass: " //                                        &
              TOSTR(proto_parents%individual(ind)%body_mass)  //        &
              ", energy: " //                                           &
              TOSTR(proto_parents%individual(ind)%get_energy()) // "=" //   &
              TOSTR(energy_reserve(proto_parents%individual(ind)%body_mass, &
                               proto_parents%individual(ind)%body_length))  )

    WALKS: do i=1, 100

      !> Initialise stopwatch for timing each walk.
      call stopwatch_walk_wise%start("Walk # " // TOSTR(i))

      if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_CHAPTER)
      call LOG_DBG("Agent walk no=" // TOSTR(i) // " , agent ID " //    &
                  TOSTR(proto_parents%individual(ind)%get_id()) //      &
                  " (# " // TOSTR(ind) // "), name:"                    &
                  // trim(proto_parents%individual(ind)%individ_label())&
                  // ", is male: " // TOSTR(proto_parents%individual(ind)%is_male()) // ".")

      call LOG_DBG ("Agent body length: " //                            &
              TOSTR(proto_parents%individual(ind)%body_length) //       &
              ", body mass: " //                                        &
              TOSTR(proto_parents%individual(ind)%body_mass) //         &
              ", energy: " //                                           &
              TOSTR(proto_parents%individual(ind)%get_energy())    )

      ! do random walk
      call stopwatch_op_current%start()

      step_rwalk = dist2step(400.0)
      call LOG_DBG("  Step size for random walk: " // TOSTR(step_rwalk) // &
                    ", " // TOSTR(step_rwalk / proto_parents%individual(ind)%get_length()) // &
                    " agent's body sizes." )

      call proto_parents%individual(ind)%rwalk( step_rwalk, 0.50, &
                                                             habitat_safe)

      call LOG_DBG("  cycle ind:walk "// TOSTR(ind) // ":"// TOSTR(i) // &
                   ", Loc: " // TOSTR(proto_parents%individual(ind)%location(.TRUE.)))
      call LOG_DBG("            way "//                                 &
                               TOSTR(proto_parents%individual(ind)%way()))

      !> Update the i-th walk distance/way for distribution plot.
      way_passed(i) = proto_parents%individual(ind)%way()

      cost_step = proto_parents%individual(ind)%cost_swim(distance=way_passed(i))
      call LOG_DBG("  Cost of random walk step: " // TOSTR(cost_step) // &
                    " is " // TOSTR(100.0_SRP * cost_step / proto_parents%individual(ind)%body_mass ) // &
                    "% of agent's body mass." )

      !> Introduce the cost of swimming here:
      proto_parents%individual(ind)%body_mass=proto_parents%individual(ind)%body_mass - &
                                cost_step

      !===================================================
      ! Inner perceptions: stomach, bodymass, energy, age, reproductive factor
      call proto_parents%individual(ind)%perceptions_inner()

      !===================================================
      ! Environmental perceptions: light, depth
      call proto_parents%individual(ind)%perceptions_environ()
      call LOG_DBG("Environmental perceptions: light " //               &
        TOSTR(proto_parents%individual(ind)%perceive_light%get_current()) // &
        ", depth " //                                                   &
        TOSTR(proto_parents%individual(ind)%perceive_depth%get_current()) )

      call LOG_DBG("Agent's data: depth: " // TOSTR(proto_parents%individual(ind)%dpos()) )

      !===================================================
      ! Spatial perceptions food, conspecifics, predators

      !call proto_parents%individual(ind)%see_food(habitat_safe%food)
      call proto_parents%individual(ind)%see_food(joined_food_res_tmp)

      call proto_parents%individual(ind)%see_consp( proto_parents%individual )

      call proto_parents%individual(ind)%see_pred( habitat_safe%predators )

      !===================================================
      call proto_parents%individual(ind)%motivations_percept_components()
      call proto_parents%individual(ind)%motivations_primary_calc()
      call proto_parents%individual(ind)%modulation()
      call proto_parents%individual(ind)%motivations_to_memory()
      call proto_parents%individual(ind)%gos_find()

      if (proto_parents%individual(ind)%starved_death()) then
        call proto_parents%individual(ind)%dies()
        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_SECTION)
        call LOG_DBG ("INFO: Agent dies due to starvation, ID: " //     &
                            TOSTR(proto_parents%individual(ind)%get_id()))
        call LOG_DBG ("      Body length: " //                          &
              TOSTR(proto_parents%individual(ind)%body_length) //       &
              ", body mass: " //                                        &
              TOSTR(proto_parents%individual(ind)%body_mass) //         &
              ", maximum mass: " //                                     &
              TOSTR(proto_parents%individual(ind)%body_mass_maximum) // &
              ", birth mass : " //                                      &
              TOSTR(proto_parents%individual(ind)%body_mass_birth)     )
        call LOG_DBG("       Energy :" //                               &
              TOSTR(proto_parents%individual(ind)%energy_current) //    &
              ", energy maximum: " //                                   &
              TOSTR(proto_parents%individual(ind)%energy_maximum)    )
        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_SECTION)
        call LOG_DBG(stopwatch_walk_wise%log())
        exit WALKS
      end if

      call LOG_DBG( "GOS is      :" // proto_parents%individual(ind)%gos_label() )
      call LOG_DBG( "GOS arousal :" // TOSTR(proto_parents%individual(ind)%arousal()) )

      call LOG_DBG("**** can see food:  " // TOSTR(proto_parents%individual(ind)%perceive_food%get_count()))
      HAS_FOOD_CHECK: if ( proto_parents%individual(ind)%has_food() ) then
        call LOG_DBG("  coord(1)    >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(1)%location(.TRUE.)))
        call LOG_DBG("  distance    >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_distances))
        call LOG_DBG("  dist. (d/l) >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_distances &
                        / proto_parents%individual(ind)%get_length()))
        call LOG_DBG("  available   >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen%is_available()))
        call LOG_DBG("  iid         >" //                              &
                        TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen%get_iid()))

        do iii=1, proto_parents%individual(ind)%perceive_food%get_count()
          call LOG_DBG("   Prob:" // TOSTR(iii) // "=" // &
            TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(iii)%capture_probability( &
            distance=proto_parents%individual(ind)%perceive_food%foods_distances(iii) ) ) // &
            " dist:" // TOSTR(proto_parents%individual(ind)%perceive_food%foods_distances(iii)) // &
            " size:" // TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(iii)%get_size()) // &
            " mass:" // TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(iii)%get_mass())  &
            )
        end do

        ! Test above and below functions for spatial objects.
        call LOG_DBG("   Food items are above: " //                         &
                TOSTR( proto_parents%individual(ind)%is_above(proto_parents%individual(ind)%perceive_food%foods_seen) ) )

        call LOG_DBG("   Food items are below: " //                         &
                TOSTR( proto_parents%individual(ind)%is_below(proto_parents%individual(ind)%perceive_food%foods_seen) ) )

        call LOG_DBG("   **Food items are above (user operator test): " //    &
                TOSTR( proto_parents%individual(ind) .above. proto_parents%individual(ind)%perceive_food%foods_seen ) )
        if (proto_parents%individual(ind) .above. proto_parents%individual(ind)%perceive_food%foods_seen(1)) &
          call LOG_DBG("   **Test operator .above. :: agent is above the first item TRUE; " //    &
                        "agent dpos: " // TOSTR(proto_parents%individual(ind)%dpos()) //          &
                        ", food item dpos: " // TOSTR( proto_parents%individual(ind)%perceive_food%foods_seen(1)%dpos() ) )

        call LOG_DBG("   ++Food items are below (user operator test): " //    &
                TOSTR( proto_parents%individual(ind) .below. proto_parents%individual(ind)%perceive_food%foods_seen ) )
        if (proto_parents%individual(ind) .below. proto_parents%individual(ind)%perceive_food%foods_seen(1)) &
          call LOG_DBG("   ++Test operator .below. :: agent is below the first item TRUE; " //    &
                        "agent dpos: " // TOSTR(proto_parents%individual(ind)%dpos()) //          &
                        ", food item dpos: " // TOSTR( proto_parents%individual(ind)%perceive_food%foods_seen(1)%dpos() ) )

        call LOG_DBG("Relative functions test:")
        call LOG_DBG("    Food items above: " // TOSTR(proto_parents%individual(ind)%food_items_above()))
        call LOG_DBG("    Food items mass above: " // TOSTR(proto_parents%individual(ind)%food_mass_above()))
        call LOG_DBG("    Food items below: " // TOSTR(proto_parents%individual(ind)%food_items_below()))
        call LOG_DBG("    Food items mass below: " // TOSTR(proto_parents%individual(ind)%food_mass_below()))

        !exit INDS
        call LOG_DBG( "Finding food took " // TOSTR(stopwatch_op_current%elapsed()) )

        !===================================================
        call LOG_DBG("   +++ Current mass: " // TOSTR(proto_parents%individual(ind)%mass()) //  &
                     ", length: " // TOSTR(proto_parents%individual(ind)%length()) //       &
                     ", energy: " // TOSTR(proto_parents%individual(ind)%get_energy())  )

        !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        !++++++++++++ select food item +++++++++++++++++++++++
        food_item_selected = proto_parents%individual(ind)%food_item_select(rescale_max_motivation=6.0_SRP)
        !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        !call proto_parents%individual(ind)%do_eat_food_item(food_item_selected, habitat_safe%food)

        ! ++++ EAT FOOD ITEM
        call proto_parents%individual(ind)%do_eat_food_item(food_item_selected, joined_food_res_tmp)

        call LOG_DBG("**** Tried to eat food item: " // TOSTR(food_item_selected))
        call LOG_DBG("   +++ Updated mass: " // TOSTR(proto_parents%individual(ind)%mass()) //  &
                     ", length: " // TOSTR(proto_parents%individual(ind)%length()) //       &
                     ", energy: " // TOSTR(proto_parents%individual(ind)%get_energy())  )

        call LOG_DBG("Selected food iid " //  &
          TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(food_item_selected)%food_iid) )

        call LOG_DBG("Selected item status " // &
          TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(food_item_selected)%eaten) )

        iii = proto_parents%individual(ind)%perceive_food%foods_seen(food_item_selected)%food_iid
        call LOG_DBG("Selected food iid " // TOSTR(iii) )
        call LOG_DBG("Selected item status " // &
          TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen(food_item_selected)%eaten) )

        !call LOG_DBG("Selected food item iid in resource " // TOSTR(habitat_safe%food%food(iii)%food_iid) )
        call LOG_DBG("Selected food item iid in resource " // TOSTR(joined_food_res_tmp%food(iii)%food_iid) )
        !call LOG_DBG("Selected food item status in resource " // TOSTR(habitat_safe%food%food(iii)%eaten) )
        call LOG_DBG("Selected food item status in resource " // TOSTR(joined_food_res_tmp%food(iii)%eaten) )

      else HAS_FOOD_CHECK
        !> If no food objects were encountered we still grow with zero food gain.
        call proto_parents%individual(ind)%mass_grow(0.0_SRP)
        call proto_parents%individual(ind)%len_grow(0.0_SRP)
      end if HAS_FOOD_CHECK

      call LOG_DBG("Food items status " // TOSTR(proto_parents%individual(ind)%perceive_food%foods_seen%eaten)  )
      !print *, "********** Food items status ", proto_parents%individual(ind)%perceive_food%foods_seen%eaten

      call LOG_DBG("**** can see consp: " // TOSTR(proto_parents%individual(ind)%perceive_consp%get_count() ) )
      HAS_CONSP_CHK: if ( proto_parents%individual(ind)%has_consp() ) then
        call LOG_DBG("  coord(1)    >" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(1)%location(.TRUE.)))
        call LOG_DBG("  iid         >" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_cid()))
        call LOG_DBG("  sex (male)  >" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen%is_male()))
        call LOG_DBG("  sex (male) ?>" //                              &
                         TOSTR(proto_parents%individual(               &
                         proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_cid() )%is_male()))

        call LOG_DBG("  mass        >" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_mass()))
        call LOG_DBG("  mass       ?>" //                              &
                         TOSTR(proto_parents%individual(               &
                         proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_cid() )%get_mass()))
        call LOG_DBG( "Probability of reproduction: " //                 &
            TOSTR(proto_parents%individual(ind)%probability_reproduction()) )

        call LOG_DBG( "Absolute difference in mass (all sex agents): " //   &
            TOSTR( proto_parents%individual(ind)%get_mass() -               &
                  average( proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_mass() ) ) &
              )
        call LOG_DBG( "Delta mass relative (all sex agents): "  //          &
        TOSTR(                                                              &
          within( proto_parents%individual(ind)%get_mass() /                                         &
              average( proto_parents%individual(ind)%perceive_consp%conspecifics_seen%get_mass() ),  &
            minval(PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA),           &
            maxval(PROBABILITY_REPRODUCTION_DELTA_MASS_ABSCISSA)            &
          ) ) )

        !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        ! ++++++++++++++++++++++ slect conspecific ++++++++++++++++++++++++++++
        conspecif_selected = proto_parents%individual(ind)%consp_select(rescale_max_motivation=6.0_SRP)
        !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        call LOG_DBG("Selected conspecific No: " // TOSTR(conspecif_selected) )
        call LOG_DBG("Selected consp iid: " //                                                     &
          TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(conspecif_selected)%cid) )

        call LOG_DBG("Selected consp is male: " //                                                 &
          TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(conspecif_selected)%sex_is_male ) )

        call LOG_DBG("Selected consp mass: " //                                                 &
          TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(conspecif_selected)%get_mass() ) )

        call LOG_DBG("Selected conspecific coordinates: " //                                                    &
          TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(conspecif_selected)%x) // "," // &
          TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(conspecif_selected)%y) // "," // &
          TOSTR(proto_parents%individual(ind)%perceive_consp%conspecifics_seen(conspecif_selected)%depth)   )

        !  stop
        !exit INDS
      end if HAS_CONSP_CHK

      call LOG_DBG("**** can see pred:  " // TOSTR(proto_parents%individual(ind)%perceive_predator%get_count() ) )
      if ( proto_parents%individual(ind)%has_pred() ) then
         call LOG_DBG("  coord(1)    =" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_predator%predators_seen(1)%location(.TRUE.)))
         call LOG_DBG("  iid         =" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_predator%predators_seen%get_cid()))
         call LOG_DBG("  dist        =" //                              &
                         TOSTR(proto_parents%individual(ind)%perceive_predator%predators_seen%get_dist()))
         call LOG_DBG("  attack rates=" //                             &
                         TOSTR(proto_parents%individual(ind)%perceive_predator%predators_attack_rates))

         call LOG_DBG("   Probability of capture by predator 1: " //        &
              TOSTR(proto_parents%individual(ind)%risk_pred(                &
                  proto_parents%individual(ind)%perceive_predator%predators_seen(1), &
                  proto_parents%individual(ind)%perceive_predator%predators_attack_rates(1) )))
         !  stop
        !exit INDS
        ! Calculate visual range for the predator
        irradiance =                                        &
              light_depth ( depth=proto_parents%individual(ind)%dpos(), &
                      surface_light =                                   &
                          light_surface(tstep=Global_Time_Step_Model_Current,  &
                                        is_stochastic=DAYLIGHT_STOCHASTIC) )
        visrange = m2cm (  visual_range (                      &
              irradiance = irradiance,                      &
              prey_area =                                                   &
                carea( cm2m( proto_parents%individual(ind)%                 &
                  perceive_predator%predators_seen(1)%get_size() ) ),       &
              prey_contrast = PREYCONTRAST_DEFAULT ) )
        call LOG_DBG("  visual range =" // TOSTR(visrange) )
        !stop

      end if

      !call LOG_DBG( "**** Probability of reproduction: " //                 &
      !      TOSTR(proto_parents%individual(ind)%probability_reproduction()) )

      !> add perceptions to the memory
      call proto_parents%individual(ind)%perception_to_memory()


      call LOG_DBG("INFO: Subtracting cost of living for agent # " //     &
              TOSTR(ind) // " and add weight and length to the history.")

      !> Subtract the cost of living
      call proto_parents%individual(ind)%subtract_living_cost()

      !> Update increments to sex steroids.
      call proto_parents%individual(ind)%sex_steroids_update()

      call add_to_history(proto_parents%individual(ind)%body_length_history, &
                          proto_parents%individual(ind)%body_length)

      call add_to_history(proto_parents%individual(ind)%body_mass_history, &
                          proto_parents%individual(ind)%body_mass)

      call LOG_DBG(stopwatch_walk_wise%log())

      call proto_parents%individual(ind)%destroy_perception()
      call LOG_DBG("Perceptions destroyed for ind " // TOSTR(ind) // "("// TOSTR(i) // ")")

    end do WALKS

    !> Saving histograms of `way_passed`, the distance of each walk.
    call LOG_DBG("Average walk step distance: " // TOSTR(average(way_passed)) )
    call debug_histogram_save(x_data=way_passed,                        &
            csv_out_file="debug_hist_way_" // MMDD // "_g" //           &
                TOSTR(Global_Generation_Number_Current) // "_ind_" //   &
                TOSTR(ind) // csv,                                      &
            delete_csv=.FALSE., enable_non_debug=.TRUE. )

     call LOG_DBG("INFO: Subtracting cost of living for agent # " //     &
               TOSTR(ind) // " and add weight and length to the history.")

     !> Subtract the cost of living
     call proto_parents%individual(ind)%subtract_living_cost()

     call proto_parents%individual(ind)%sex_steroids_update()

     call add_to_history(proto_parents%individual(ind)%body_length_history, &
                         proto_parents%individual(ind)%body_length)

     call add_to_history(proto_parents%individual(ind)%body_mass_history, &
                         proto_parents%individual(ind)%body_mass)

    if (proto_parents%individual(ind)%starved_death()) then
        call proto_parents%individual(ind)%dies_debug()
        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_SECTION)
        call LOG_DBG ("INFO: Agent dies due to starvation, ID: " //     &
                            TOSTR(proto_parents%individual(ind)%get_id()))
        call LOG_DBG ("      Body length: " //                          &
              TOSTR(proto_parents%individual(ind)%body_length) //       &
              ", body mass: " //                                        &
              TOSTR(proto_parents%individual(ind)%body_mass) //         &
              ", maximum mass: " //                                     &
              TOSTR(proto_parents%individual(ind)%body_mass_maximum) // &
              ", birth mass : " //                                      &
              TOSTR(proto_parents%individual(ind)%body_mass_birth)     )
        call LOG_DBG("       Energy :" //                               &
              TOSTR(proto_parents%individual(ind)%energy_current) //    &
              ", energy maximum: " //                                   &
              TOSTR(proto_parents%individual(ind)%energy_maximum)    )
        if (IS_DEBUG) call LOG_DELIMITER(LOG_LEVEL_SECTION)
      end if



    ! DEBUG: save movement history of each agent.
    call CSV_MATRIX_WRITE ( reshape(                                    &
                  [proto_parents%individual(ind)%history%x,             &
                    proto_parents%individual(ind)%history%y,            &
                    proto_parents%individual(ind)%history%depth],       &
                  [HISTORY_SIZE_SPATIAL, 3]),                           &
                  "zz_move_hist_" // MMDD //                            &
                  "_gen_" // TOSTR(Global_Generation_Number_Current, GENERATIONS) // &
                  "_ind_" // TOSTR(ind) //                    csv,      &
                  ["X","Y", "Z"]                                        &
                  )

    ! DEBUG: save body mass history of each agent.
    call CSV_MATRIX_WRITE ( reshape(                                    &
                  [proto_parents%individual(ind)%body_length_history,   &
                   proto_parents%individual(ind)%body_mass_history],    &
                  [HISTORY_SIZE_AGENT_PROP, 2]),                        &
                  "zz_length_mass_hist_" // MMDD //                     &
                  "_gen_" // TOSTR(Global_Generation_Number_Current, GENERATIONS) //  &
                  "_ind_" // TOSTR(ind) //                    csv,      &
                  ["LENGTH","MASS  "]                                   &
                  )

    ! DEBUG: save perception history of each agent.
    call CSV_MATRIX_WRITE ( reshape(                                    &
                  [real(proto_parents%individual(ind)%memory_stack%memory_food), &
                   proto_parents%individual(ind)%memory_stack%memory_foodsiz,    &
                   proto_parents%individual(ind)%memory_stack%memory_reprfac ],  &
                  [HISTORY_SIZE_PERCEPTION, 3]),                        &
                  "zz_percept_" // MMDD //                              &
                  "_gen_" // TOSTR(Global_Generation_Number_Current, GENERATIONS) // &
                  "_ind_" // TOSTR(ind) // "_name_" //                  &
                  trim(proto_parents%individual(ind)%individ_label()) //  csv, &
                  ["PRC_FOOD    ","PRC_FOOD_SIZ","PRC_REPRFAC "]         &
                  )


  end do INDS

  print *, "************** INDS ended "
  !stop

  !>  Save the individual data for the whole test parent population,
  !!  generation one, to a csv file. This is after the walk states of
  !! the agents.
  SAVE_DATA_INDS_INIT: block
    character(len=:), allocatable :: output_data_file
    !> Save individual data
    output_data_file = "agents_" // MODEL_NAME // "_" // MMDD //            &
                        "_gen_" // TOSTR(Global_Generation_Number_Current,  &
                        GENERATIONS) // "_z_end" // csv
    call proto_parents%save_csv(output_data_file, is_logging=.TRUE.)
  end block SAVE_DATA_INDS_INIT

  call CSV_MATRIX_WRITE ( reshape(                                        &
                        [ joined_food_res_tmp%food%x,                     &
                          joined_food_res_tmp%food%y,                     &
                          joined_food_res_tmp%food%depth,                 &
                          joined_food_res_tmp%food%size,                  &
                          conv_l2r(joined_food_res_tmp%food%eaten),       &
                          real(joined_food_res_tmp%food%food_iid,SRP)],   &
                        [joined_food_res_tmp%number_food_items, 6]),      &
                         "zzz_zzz_food_ALL_after_" // MODEL_NAME // "_" // MMDD // &
                           "_gen_" // TOSTR(Global_Generation_Number_Current, GENERATIONS) // csv, &
                        ["X   ","Y   ", "D   ", "SIZE", "EATN", "IID "]  &
                         )

  call joined_food_res_tmp%unjoin( habitat_safe%food, habitat_dangerous%food, reindex=.TRUE. )

  call joined_food_res_tmp%destroy()

  call CSV_MATRIX_WRITE ( reshape(                                      &
                        [ habitat_safe%food%food%x,                     &
                          habitat_safe%food%food%y,                     &
                          habitat_safe%food%food%depth,                 &
                          habitat_safe%food%food%size,                  &
                          conv_l2r(habitat_safe%food%food%eaten),       &
                          real(habitat_safe%food%food%food_iid,SRP)],   &
                        [habitat_safe%food%number_food_items, 6]),      &
                         "zzz_food_safe_" // MODEL_NAME // "_" // MMDD // &
                           "_gen_" // TOSTR(Global_Generation_Number_Current, GENERATIONS) // csv, &
                        ["X   ","Y   ", "D   ", "SIZE", "EATN", "IID "]  &
                         )

  call CSV_MATRIX_WRITE ( reshape(                                     &
                        [ habitat_dangerous%food%food%x,                 &
                          habitat_dangerous%food%food%y,                 &
                          habitat_dangerous%food%food%depth,             &
                          habitat_dangerous%food%food%size,              &
                          conv_l2r(habitat_dangerous%food%food%eaten),   &
                          real(habitat_dangerous%food%food%food_iid,SRP)],&
                        [habitat_dangerous%food%number_food_items, 6]),   &
                         "zzz_food_dang_" // MODEL_NAME // "_" // MMDD // &
                           "_gen_" // TOSTR(Global_Generation_Number_Current, GENERATIONS) // csv, &
                        ["X   ","Y   ", "D   ", "SIZE", "EATN", "IID "]  &
                         )

  call system_halt(message="NORMAL TERMINATION DEBUG TEST PROCEDURE")

end subroutine life_cycles_DEBUG_TEST

!=============================================================================
