!> @file m_genome.f90
!! The Genome objects of the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition the genetic architecture of the agent
!> @section the_genome_module THE_GENOME module
!> This module defines the genetic architecture objects of the agent. See
!! @ref aha_buildblocks_genome "The genome structure" for an overview.
module THE_GENOME

  use COMMONDATA  !> @note We need Global Data in every module!

  !> @note We don't need all environmental objects definitions here!
  !!       But the individual genome `INDIVIDUAL_GENOME` is an extension
  !!       of `SPATIAL_MOVING`.
  use THE_ENVIRONMENT, only : SPATIAL_MOVING

  implicit none

  character (len=*), parameter, private :: MODNAME = "(THE_GENOME)"

  !-----------------------------------------------------------------------------
  !> @brief   This describes an individual gene object. See
  !!          @ref aha_buildblocks_genome "the genome structure" for as general
  !!          description and @ref aha_buildblocks_genome_gene "gene" for
  !!          details.
  type, public :: GENE
    !> sets a descriptive label of the allele, e.g its role and purpose
    character(len=LABEL_LENGTH)  :: allele_label
    !> @brief Sets the value of the allele that is stored and evolved.
    !! @note In the new version allele values are *INTEGER*
    !!       rather than *REAL*. Integer genome is not affected by the CPU
    !!       precision and does not suffer from FPU rounding errors. This is
    !!       what is expected from the genome: genes should be atomic, fixed,
    !!       and never subject to any uncontrollable fluctuations and drift.
    !!       Otherwise no "inheritance" is guaranteed. Only controlled
    !!       mutations are allowed. Integer calculations will also have higher
    !!       calculation speed and may hopefully avoid IEEE float point errors
    !!       (overflow/underflow). Also, we may in future use more realistic
    !!       limited-range allele functions to mimic real DNA structure.
    !!       If we have sufficiently large range of possible allele values,
    !!       e.g. 1:10000 and integer-to-real conversion function for
    !!       converting these true integer allele values to real values
    !!       within 0.:1. in the gamma neural response function, this would
    !!       not have a much different effect compared with the old
    !!       real-value gene implementation.
    integer, dimension(ADDITIVE_COMPS) :: allele_value
    !> sets if the allele is dominant
    logical :: dominant
    !> sets the multiplicative dominance weight
    real(SRP) :: dominance_weight
    !> rank_id of the gene, needed for sorting alleles within the chromosome
    integer :: rank_id
    contains
        !> init alleles with random values, labels not set here,
        !! use this function for startup initialisations of random agents
        !! See `the_genome::allele_init_random()`
        procedure, public :: init_allele => allele_init_random
        !> create empty zero allele object, should be used for offspring inits
        !! as we do not need to init them with random values, they will get
        !! them from the parents using inherit function set
        !! See `the_genome::allele_create_zero()`
        procedure, public :: create_allele => allele_create_zero
        !> init label alleles random
        !! See `the_genome::allele_label_init_random()`
        procedure, public :: label_random => allele_label_init_random
        !> set labels for the allele
        !! See `the_genome::allele_label_set()`
        procedure, public :: labels =>  allele_label_set
        !> get the allele label
        !! See `the_genome::allele_label_get()`
        procedure, public :: label_get =>  allele_label_get
        !> set individual value of allele
        !! See `the_genome::allele_value_set()`
        procedure, public :: set =>  allele_value_set
        !> set the vector of additive allele components
        !! See `the_genome::alleles_value_vector_set()`
        procedure, public :: set_vector => alleles_value_vector_set
        !> get the value of the allele
        !! See `the_genome::allele_value_get()`
        procedure, public :: get =>  allele_value_get
        !> get the vector of additive allele components
        !! See `the_genome::allele_values_vector_get()`
        procedure, public :: get_vector => allele_values_vector_get
        !> set rank_id for the allele
        !! See `the_genome::allele_rank_id_set()`
        procedure, public :: rank => allele_rank_id_set
        !> Introduce a random point mutation to one (random) of the alleles
        !! See `the_genome::allele_mutate_random()`
        procedure, public :: mutate_point => allele_mutate_random
        !> Introduce random mutations to the whole allele components set
        !! See `the_genome::allele_mutate_random_batch()`
        procedure, public :: mutate_set => allele_mutate_random_batch
  end type GENE

  !-----------------------------------------------------------------------------
  !> This type describes the chromosome object.
  !! Chromosome consists of an array of alleles and a descriptive
  !! string label. See
  !! @ref aha_buildblocks_genome "\"the genome structure\"" for as general
  !! description and @ref aha_buildblocks_genome_chromosome
  !! "\"chromosome\"" for details.
  type, public :: CHROMOSOME
    !> chromosome label
    character(len=LABEL_LENGTH) :: chromosome_label
    !> chromosome length, i.e. N of alleles here
    integer :: clength
    !> array of alleles of the size `clength`
    type(GENE), allocatable, dimension(:) :: allele
    contains
      !> This subroutine initialises the chromosome with, and allocates, random
      !! alleles, sets one of them randomly dominant and optionally defines the
      !! chromosome label.
      !! See `the_genome::chromosome_init_allocate_random()`
      procedure, public :: init_chromosome => chromosome_init_allocate_random
      !> Init a new chromosome, zero, non-random.
      !! See `the_genome::chromosome_create_allocate_zero()`
      procedure, public :: create_chromosome => chromosome_create_allocate_zero
      !> This subroutine recalculates rank_id indices for consecutive gene objects
      !! within the chromosome. This may be necessary after reordering by random
      !! relocation mutation.
      !! See `the_genome::chromosome_recalculate_rank_ids()`
      procedure, public :: recalc_rank_id => chromosome_recalculate_rank_ids
      !> mutate within the same chromosome, relocate a gene (unit of alleles) to a
      !! different random position within the same chromosome, the misplaced gene
      !! moves to the relocated gene position, so they are just swap.
      !! See `the_genome::chromosome_mutate_relocate_swap_random()`
      procedure, public :: mutate_swap => chromosome_mutate_relocate_swap_random
      !> Mutate within the same chromosome, relocate a gene (unit of alleles) to a
      !! different random position within the same chromosome, shifting all other
      !! genes within the chromosome down one position. This works as
      !! follows: first, we randomly determine the gene to relocate, assign it
      !! a new random rank_id. Then re-sort the chromosome according to the new
      !! ranks with `qsort` with the `qs_partition_rank_id`  backend.
      !! See `the_genome::chromosome_mutate_relocate_shift_random()`
      procedure, public :: mutate_shift => chromosome_mutate_relocate_shift_random
      !> Sort GENE objects within the CHROMOSOME by their rank_id
      !! The two subroutines below are a variant of the recursive quick-sort
      !! algorithm adapted for sorting integer components of the the `CHROMOSOME`
      !! object.
      !! See `the_genome::chromosome_sort_rank_id()`
      procedure, public :: sort_rank_id => chromosome_sort_rank_id
  end type CHROMOSOME

  !-----------------------------------------------------------------------------
  !> This type describes parameters of the individual agent's genome
  !! The genome is an array of allocatable the_genome::chromosome objects,
  !! different kinds of agents may have different genomes with
  !! different number of chromosomes. See
  !! @ref aha_buildblocks_genome "\"the genome structure\"" for as general
  !! description and @ref aha_buildblocks_genome_genome "\"genome\"" for
  !! details.
  type, public, extends(SPATIAL_MOVING) :: INDIVIDUAL_GENOME
    !> label for the genome
    character(len=LABEL_LENGTH) :: genome_label
    !> the size of the genome, i.e. N of chromosomes = N_CHROMOSOMES
    !! in this version it is constant, can implement variable genomes later.
    integer :: genome_size = N_CHROMOSOMES
    !> array of chromosome objects, the two dimensions refer to
    !!   (1) chromosome number in the genome and (2) the number of homologs
    !!   (1:2 for diploid), so chromosome is a 2D array.
    type(CHROMOSOME), allocatable, dimension(:,:) :: chromosome
    !> The sex of the individual: is male = TRUE or female = FALSE
    !! this is the main sex identifier. The sex_label defined below
    !! should only  be used for outputs and similar purposes.
    logical :: sex_is_male
    !> Verbal label for sex ("male" or "female").
    character(len=LABEL_LENGTH) :: sex_label
    !> Flag the agent is alive (TRUE) or dead (False).
    logical :: alive
    contains
      !> Initialise the genome at random, and set sex as determined by the sex
      !!   determination locus.
      !! See `the_genome::genome_init_random()`
      procedure, public :: init_genome => genome_init_random
      !> Create a new empty genome, and set sex as determined by the sex
      !!   determination locus. Genome values are from parents using inherit
      !!   functions.
      !! See `the_genome::genome_create_zero()`
      procedure, public :: create_genome => genome_create_zero
      !> Label genome. If label is not provided, make a random string.
      !! @note TMP NOTE: label setting removed from the create function
      !!       as it will now  override create for `SPATIAL_MOVING`.
      !! See `the_genome::genome_label_set()`
      procedure, public :: label => genome_label_set
      !> Accessor function to get the genome label. The label is a kind of a
      !! (random) text string name of the genome and the individual agent.
      !! @note We especially need this accessor function because the genome (and
      !!       individual) name is used in other modules for file names ids etc.
      !! See `the_genome::genome_label_get()`
      procedure, public :: individ_label => genome_label_get

      !> Sex has a separate status from all other genetically determined traits.
      !! It is initialised here, at the genotype level of the class hierarchy.
      !! See `the_genome::genome_sex_determine_init()`
      procedure, public :: sex_init => genome_sex_determine_init ! init sex
      !> Get the logical sex ID of the genome object component.
      !! See `the_genome::genome_get_sex_is_male()`
      procedure, public :: is_male => genome_get_sex_is_male
      !> Get the logical sex ID of the genome object component.
      !! See `the_genome::genome_get_sex_is_female()`
      procedure, public :: is_female => genome_get_sex_is_female
      !> Get the descriptive sex label: male or female.
      !! See `the_genome::genome_get_sex_label()`
      procedure, public :: label_sex => genome_get_sex_label

      !> Init a trait from the genotype, trait can be any object in any of
      !! the up level class hierarchy that is determined from the boolean
      !! genotype x phenotype matrix.
      !! See `the_genome::trait_init_genotype_gamma2gene()`
      procedure, public :: trait_init => trait_init_genotype_gamma2gene
      !> Set an **individual trait** of the agent that depends on the
      !! genotype. This can be any trait upwards in the class hierarchy.
      !! See `the_genome::trait_set_genotype_gamma2gene()`
      procedure, public :: trait_set => trait_set_genotype_gamma2gene
        !> Generic interface to the neuronal response function.
        !! See `the_genome::trait_init_genotype_gamma2gene()` and
        !! `the_genome::trait_set_genotype_gamma2gene()`.
        generic, public :: neuro_resp => trait_init, trait_set

      !> Init a trait from the genotype, trait can be any object in any of
      !! the up level class hierarchy that is determined from the boolean
      !! genotype x phenotype matrix. Note that this method is based on
      !! simple linear rescale rather than neuronal response.
      !! See `the_genome::trait_init_linear_sum_additive_comps_2genes_r()`
      procedure, public :: trait_init_linear =>                               &
                                  trait_init_linear_sum_additive_comps_2genes_r
      !> Set an **individual trait** of the agent that depends on the
      !! genotype. This can be any trait upwards in the class hierarchy.
      !!  Note that this method is based on simple linear rescale rather than
      !! neuronal response.
      !! See `the_genome::trait_set_linear_sum_additive_comps_2genes_r()`
      procedure, public :: trait_set_linear =>                                &
                                  trait_set_linear_sum_additive_comps_2genes_r
        !> Generic interface to the simple linear genotype to phenotype
        !! transformation functions. See
        !! `the_genome::trait_init_linear_sum_additive_comps_2genes_r()` and
        !! `the_genome::trait_set_linear_sum_additive_comps_2genes_r()`.
        generic, public :: linear_g2p => trait_init_linear, trait_set_linear

      !> Set the individual to be alive, normally this function is used after
      !! init or birth.
      !! See `the_genome::genome_individual_set_alive()`
      procedure, public :: lives => genome_individual_set_alive
      !> Set the individual to be **dead**. Note that this function does not
      !! deallocate the individual agent object, this may be a separate
      !! destructor function.
      !! The `dies` method is implemented at the following levels
      !! of the agent object hierarchy (upper overrides the lower level):
      !! - the_genome::individual_genome::dies();
      !! - the_neurobio::appraisal::dies();
      !! - the_neurobio::gos_global::dies();
      !! - the_individual::individual_agent::dies().
      !! .
      !! See `the_genome::genome_individual_set_dead()`
      procedure, public :: dies => genome_individual_set_dead
      !> Set the individual to be **dead**. Note that in this class this method
      !! implementation points to the same procedure as
      !! `the_genome::individual_genome::dies(). However the `dies` method is
      !! overriden upwards in the class hierarchy to also nullify
      !! neurobiological and behavioural objects. So this method should be
      !! only called in procedures that specifically implemented override of
      !! the `dies` method:
      !! - the_neurobio::gos_global::dies()
      !! - the_individual::individual_agent::dies()
      !! .
      !! See `the_genome::genome_individual_set_dead()`
      procedure, public :: set_dead => genome_individual_set_dead
      !> Check if the individual is alive.
      !! See `the_genome::genome_individual_check_alive()`
      procedure, public :: is_alive => genome_individual_check_alive
      !> Check if the individual is dead (the opposite of `is_alive`).
      !! See `the_genome::genome_individual_check_dead()`
      procedure, public :: is_dead => genome_individual_check_dead

      !> Internal **genetic recombination backend**, exchange individual alleles
      !! between homologous chromosomes in mother and father genomes to form
      !! the `this` (offspring) genome. Fully random recombination. See
      !! `the_genome::genome_individual_recombine_homol_full_rand_alleles()`.
      procedure, public :: recombine_random =>                                &
                            genome_individual_recombine_homol_full_rand_alleles
      !> Internal genetic recombination backend, exchange individual alleles
      !! between homologous chromosomes in mother and father genomes to form
      !! the `this` (offspring) genome. Partially random recombination. See
      !! `the_genome::genome_individual_recombine_homol_part_rand_alleles()`.
      procedure, public :: recombine_partial =>                               &
                            genome_individual_recombine_homol_part_rand_alleles
      !> Internal **fixed genetic crossover** backend, exchange blocks of
      !! alleles between homologous chromosomes in mother and father genomes
      !! to form the `this` (offspring) genome.
      !! See `the_genome::genome_individual_crossover_homol_fix()`.
      procedure, public :: crossover =>                                       &
                            genome_individual_crossover_homol_fix

      !> Perform a probabilistic random mutation(s) on the individual genome.
      !! This is a high level wrapper to build mutations from various
      !! components. See `the_genome::genome_mutate_wrapper()`.
      procedure, public :: mutate => genome_mutate_wrapper
  end type INDIVIDUAL_GENOME

  ! qsort and partition are only for sorting genetic objects within this
  ! module, so they are private. We also don't use real procedure names, refer
  ! them by their intarface names (left of =>).
  ! private :: genome_init_random

contains ! ........ implementation of procedures for this level ................

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! General sorting functions for THE_GENOME module objects
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !> Sort GENE objects within the CHROMOSOME by their `rank_id`.
  !! The two subroutines `qsort` and `qs_partition_rank_id` are a variant of
  !! the recursive quick-sort algorithm adapted for sorting integer components
  !! of the the `CHROMOSOME` object.
  elemental subroutine chromosome_sort_rank_id(this)
    class(CHROMOSOME), intent(inout) :: this

    call qsort(this%allele) ! This is the array component we sort.

    contains

    !...........................................................................
    !> `qsort` and `qs_partition_` are the two parts of the recursive sort
    !! algorithm `qsort` is the recursive frontend. Sorts genes within the
    !! chromosome by components of the array of `allele`s.
    recursive pure subroutine qsort(A)

      !> @param[inout] input array to be sorted. It has the same type as
      !!               the individual component objects of the array-object
      !!               that we are going to sort.
      type(GENE), intent(in out), dimension(:) :: A
      integer :: iq

      if(size(A) > 1) then
        call qs_partition_rank_id(A, iq)
        call qsort(A(:iq-1))
        call qsort(A(iq:))
      endif

    end subroutine qsort

    !...........................................................................
    !> `qsort` and `qs_partition_` are the two parts of the recursive sort
    !! algorithm `qs_partition_rank_id` is a pivot backend, here it sorts
    !! genes within the chromosome object by integer `rank_id` components of
    !! the genes.
    pure subroutine qs_partition_rank_id(A, marker)

      !> @param[inout] input array to be sorted.
      type(GENE), intent(in out), dimension(:) :: A
      !> @param[out] internal pivot marker.
      integer, intent(out) :: marker

      integer :: i, j
      type(GENE) :: temp
      !> @note Pivot point `x`, has the same type **as
      !!       the sorted object component**.
      integer :: x

      ! We sort GENE objects within the CHROMOSOME by their `rank_id`
      ! components (hardwired).
      x = A(1)%rank_id
      i= 0
      j= size(A) + 1

      do
        j = j-1
        do
            if (A(j)%rank_id <= x) exit
            j = j-1
        end do
        i = i+1
        do
            if (A(i)%rank_id >= x) exit
            i = i+1
        end do
        if (i < j) then
            ! exchange A(i) and A(j)
            temp = A(i)
            A(i) = A(j)
            A(j) = temp
        elseif (i == j) then
            marker = i+1
            return
        else
            marker = i
            return
        endif
      end do

    end subroutine qs_partition_rank_id

  end subroutine chromosome_sort_rank_id

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with the object GENE
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initialises allele with a random integer. Note that we do **not** set
  !! the labels for the alleles here during the random initialisation.
  subroutine allele_init_random(this)
    class(GENE), intent(inout) :: this

    !> The allele components are initialised by a random integers within
    !! the range `ALLELERANGE_MIN` and `ALLELERANGE_MAX` parameetr values.
    call RAND_ARRAY(this%allele_value, ALLELERANGE_MIN, ALLELERANGE_MAX)

    ! create random labels for the allele initially (disabled)
    ! call this%label_random()

  end subroutine allele_init_random

  !-----------------------------------------------------------------------------
  !> Create allele with zero value. We don't set labels for alleles here
  elemental subroutine allele_create_zero(this)
    class(GENE), intent(inout) :: this

    !> @note Note that there is no need to allocate `allele_value` array
    !!       as it has fixed shape.
    this%allele_value = 0

  end subroutine allele_create_zero

  !-----------------------------------------------------------------------------
  !> The (pair of) alleles here are assigned random string labels
  !! Not sure if that is necessary for any application though
  subroutine allele_label_init_random(this)
    class(GENE), intent(inout) :: this

    this%allele_label = RAND_STRING(LABEL_LENGTH,LABEL_CST,LABEL_CEN)

  end subroutine allele_label_init_random

  !-----------------------------------------------------------------------------
  !> Set labels for the alleles. The subroutine parameter is array of labels
  elemental subroutine allele_label_set(this, label)
    class(GENE), intent(inout) :: this
    !> @param[in] label, provides an array of labels to set for the allele.
    character(len=*), intent(in)  :: label

    this%allele_label = label

  end subroutine allele_label_set

  !-----------------------------------------------------------------------------
  !> Get the i-th allele label.
  elemental function allele_label_get(this) result(label)
    class(GENE), intent(in) :: this
    !> @returns Returns the label of the allele.
    character(len=LABEL_LENGTH) :: label

    label = this%allele_label

  end function allele_label_get

  !-----------------------------------------------------------------------------
  !> Set a single value of the allele additive component.
  elemental subroutine allele_value_set(this, set_value, nr)
    class(GENE), intent(inout) :: this
    !> @param[in] value, provides the value to set for the allele and the
    !!            allele number.
    integer, intent(in) :: set_value

    !> @param[in] number, provides the number of the allele component to set.
    integer, intent(in) :: nr

    this%allele_value(nr) = set_value

  end subroutine allele_value_set

  !-----------------------------------------------------------------------------
  !> Set values of the alleles as a vector, i.e. sets the whole gene values.
  pure subroutine alleles_value_vector_set(this, values)
    class(GENE), intent(inout) :: this
    !> @param[in] values, provides vector of values to set for the alleles.
    integer, dimension(ADDITIVE_COMPS), intent(in) :: values

    this%allele_value = values

  end subroutine alleles_value_vector_set

  !-----------------------------------------------------------------------------
  !> Get the value of the allele
  elemental function allele_value_get(this, nr) result(avalue)
    class(GENE), intent(in) :: this

    !> @param[in] number, provides the number of the allele component to set.
    integer, intent(in) :: nr

    !> @returns Returns the value of the `nr`'s allele.
    integer :: avalue

    avalue = this%allele_value(nr)

  end function allele_value_get

  !-----------------------------------------------------------------------------
  !> Get the vector of all values of the alleles, i.e. gets the gene values.
  pure subroutine allele_values_vector_get(this, values)
    class(GENE), intent(in) :: this
    !> @param[out] values, Gets the vector of the values for the alleles.
    integer, dimension(ADDITIVE_COMPS), intent(out) :: values

    values = this%allele_value

  end subroutine allele_values_vector_get

  !-----------------------------------------------------------------------------
  elemental subroutine allele_rank_id_set(this, value_id)
    class(GENE), intent(inout) :: this

    !> @param rank_id, set this value to the allele `rank_id`.
    integer, intent(in) :: value_id

    this%rank_id = value_id

  end subroutine allele_rank_id_set

  !-----------------------------------------------------------------------------
  !> Introduce a random point mutation to a random allele component.
  subroutine allele_mutate_random(this, prob)
    class(GENE), intent(inout) :: this
    !> @param[in] prob optional probability of mutation, if absent, the default
    !!            value commondata::mutationrate_point is used.
    real(SRP), optional, intent(in) :: prob

    ! Local copies of optionals.
    real(SRP) :: prob_mut

    integer :: this_allele_mutates

    ! Check optional probability of mutation.
    if (present(prob)) then
      prob_mut = prob
    else
      prob_mut = MUTATIONRATE_POINT
    end if

    !> ### Implementation details ###
    !> Do mutate if a random value is smaller than the commondata::mutationrate
    !! parameter constant.
    if (RAND_R4() < prob_mut) then
      !> First, determine which of the alleles components gets mutation.
      this_allele_mutates = RAND_I(1, ADDITIVE_COMPS)
      !> Second, change the value of this allele component to an random integer.
      call this%set(RAND_I(ALLELERANGE_MIN,ALLELERANGE_MAX),this_allele_mutates)
    end if

  end subroutine allele_mutate_random

  !-----------------------------------------------------------------------------
  !> Introduce a random mutation of the whole set of additive allele components.
  subroutine allele_mutate_random_batch(this, prob)
    class(GENE), intent(inout) :: this
    !> @param[in] prob optional probability of mutation, if absent, the default
    !!            value commondata::mutationrate_batch is used.
    real(SRP), optional, intent(in) :: prob

    ! Local copies of optionals.
    real(SRP) :: prob_mut

    ! Check optional probability of mutation.
    if (present(prob)) then
      prob_mut = prob
    else
      prob_mut = MUTATIONRATE_BATCH
    end if

    if (RAND_R4() < prob_mut) then
      !> ### Implementation details ###
      !> This mutation just re-init the whole allele set as random.
      call this%init_allele()
    end if

  end subroutine allele_mutate_random_batch

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with the object CHROMOSOME
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> This subroutine initialises the chromosome with, and allocates, random
  !! alleles, sets one of them randomly dominant and optionally defines the
  !! chromosome label.
  !! @param[in] length, sets the length of the chromosome object, N of alleles.
  !! @param[in] label, sets the label for the chromosome object, optional.
  subroutine chromosome_init_allocate_random(this, length, label)
    class(CHROMOSOME), intent(inout) :: this
    ! @param[in] length, sets the length of the chromosome object, N of alleles.
    integer, intent(in) :: length
    ! @param[in] label, sets the label for the chromosome object, optional.
    character(len=*), optional, intent(in) :: label

    ! local cycle counters
    integer :: i

    !> ### Implementation details ###
    !> We set the chromosome label if such a parameter is provided, or
    !! a random string if not.
    if (present(label)) then
      this%chromosome_label = label
    else
      ! label chromosome with a random string here
      this%chromosome_label = RAND_STRING(LABEL_LENGTH,LABEL_CST,LABEL_CEN)
    end if

    !> First, set the chromosome length using the procedure parameter `length`.
    this%clength = length

    !> Then, allocate the array of the allele objects with this length.
    if (.not. allocated(this%allele)) allocate(this%allele(length))

    !> Initialise all the alleles within this chromosome.
    do i=1, length
      !> Specifically, initialise the allele.
      call this%allele(i)%init_allele()
      !> Set initial rank_id ID of the allele.
      call this%allele(i)%rank(i)
      !> Finally, set the label for the alleles within this chromosome.
      !! Labels can be set random using this function (disabled):
      !! @code
      !! call this%allele(i)%label_random()
      !! @endcode
      !! But in this implementation we construct the label for the allele from
      !! the *chromosome label* and the *allele number*;
      call this%allele(i)%labels( label( 1:len_trim(label)-                   &
                                           max(0, len_trim(label)+            &
                                           len(TOSTR(length))-LABEL_LENGTH)-1 &
                                       ) //  "_" // TOSTR(i,length)           &
                                )
      !> Long chromosome labels are trimmed at right to fit the allele number.
    end do

  end subroutine chromosome_init_allocate_random

  !-----------------------------------------------------------------------------
  !> Init a new chromosome, zero, non-random.
  subroutine chromosome_create_allocate_zero(this, length, label)
    class(CHROMOSOME), intent(inout) :: this
    ! @param[in] length, sets the length of the chromosome object,N of alleles.
    integer, intent(in) :: length
    ! @param[in] label, sets the label for the chromosome object, optional.
    character(len=*), optional, intent(in) :: label

    ! local cycle counters
    integer :: i

    !> ### Implementation details ###
    !> Set the chromosome label if provided as parameter, or random string
    !! if not.
    if (present(label)) then
      this%chromosome_label = label
    else
      ! label chromosome with a random string here
      this%chromosome_label = RAND_STRING(LABEL_LENGTH,LABEL_CST,LABEL_CEN)
    end if

    !> First, set the chromosome length using the procedure parameter `length`.
    this%clength = length

    !> Then, we allocate the array of the allele objects with this length.
    if (.not. allocated(this%allele)) allocate(this%allele(length))

    !> Initialise all the alleles within this chromosome.
    !! @note Parallel `do concurrent` construct is used here.
    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    do concurrent ( i=1:length )
      ! initialise the allele with zeros
      call this%allele(i)%create_allele()
      ! set initial rank_id ID of the allele
      this%allele(i)%rank_id = i
      ! make labels for the alleles within this chromosome.
      ! 1. we can set random labels for the alleles using this
      !    function below:
      !call this%allele(i)%label_random()
      ! 2. or construct the vector of labels for the alleles from the chromosome
      !   label and allele number:
      call this%allele(i)%labels( trim(label) // TOSTR(i,length) )
    end do

  end subroutine chromosome_create_allocate_zero

  !-----------------------------------------------------------------------------
  !> This subroutine recalculates rank_id indices for consecutive gene objects
  !! within the chromosome. This may be necessary after reordering by random
  !! relocation mutation.
  elemental subroutine chromosome_recalculate_rank_ids(this)
    class(CHROMOSOME), intent(inout) :: this
    integer :: i

    ! @warning The `do concurrent` construct is F2008 and can not (yet) be
    !          implemented in all compilers. Use normal `do` in such a case.
    do concurrent ( i = 1:this%clength )
      ! Reset rank_id ID of each.
      this%allele(i)%rank_id = i
    end do

  end subroutine chromosome_recalculate_rank_ids

  !-----------------------------------------------------------------------------
  !> Mutate within the same chromosome, relocate a gene (unit of alleles) to a
  !! different random position within the same chromosome, the misplaced gene
  !! moves to the relocated gene position, so they are just **swap**.
  subroutine chromosome_mutate_relocate_swap_random(this, prob)
    class(CHROMOSOME), intent(inout) :: this
    !> @param[in] prob optional probability of mutation, if absent, the default
    !!            value commondata::relocation_swap_rate is used.
    real(SRP), optional, intent(in) :: prob
    ! Local copies of optionals.
    real(SRP) :: prob_mut

    type(GENE) :: temp_shift
    integer :: gene_move, gene_swap

    ! Check optional probability of mutation.
    if (present(prob)) then
      prob_mut = prob
    else
      prob_mut = RELOCATION_SWAP_RATE
    end if

    !> ### Implementation details ###
    !> Do mutate if a random value is smaller than the
    !! commondata::relocation_swap_rate parameter constant value.
    if (RAND_R4() < prob_mut) then

      !> If yes, randomly determine the gene (`gene_move`) that initiates
      !! the mutation move within the chromosome.
      gene_move = RAND_I(1,this%clength)

      !> Randomly determine the gene that will be swapped with the `gene_move`.
      gene_swap = RAND_I(1,this%clength)

      !> Then, cycle through the alleles and select new random allele if
      !! it happens to coincide with `gene_move`.
      do while (gene_swap == gene_move)
        gene_swap = RAND_I(1,this%clength)
      end do

      !> After this, do the gene swap, and gene rank_id ID swap.
      temp_shift = this%allele(gene_move)

      ! swap objects
      this%allele(gene_move) = this%allele(gene_swap)
      this%allele(gene_swap) = temp_shift

      ! and swap their rank_id's
      this%allele(gene_move)%rank_id = gene_move
      this%allele(gene_swap)%rank_id = gene_swap

    end if

  end subroutine chromosome_mutate_relocate_swap_random

  !-----------------------------------------------------------------------------
  !> Mutate within the same chromosome, relocate a gene (unit of alleles) to a
  !! different random position within the same chromosome, **shifting** all
  !! other genes within the chromosome down one position. This works as
  !! follows: first, we randomly determine the gene to relocate, assign it
  !! a new random rank_id. Then re-sort the chromosome according to the new
  !! ranks with `qsort` with the `qs_partition_rank_id`  backend.
  subroutine chromosome_mutate_relocate_shift_random(this, prob)
    class(CHROMOSOME), intent(inout) :: this
    !> @param[in] prob optional probability of mutation, if absent, the default
    !!            value commondata::relocation_shift_rate is used.
    real(SRP), optional, intent(in) :: prob
    ! Local copies of optionals.
    real(SRP) :: prob_mut

    integer :: gene_move, gene_moveto

    ! Check optional probability of mutation.
    if (present(prob)) then
      prob_mut = prob
    else
      prob_mut = RELOCATION_SHIFT_RATE
    end if

    !> ### Implementation details ###
    !> Do mutate if a random value is smaller than the
    !! commondata::relocation_shift_rate parameter constant value.
    if (RAND_R4() < prob_mut) then

      !> If yes, randomly determine the gene that initiates move within
      !! the chromosome.
      gene_move = RAND_I(1,this%clength)

      !> Randomly determine the new position of this gene.
      gene_moveto = RAND_I(1,this%clength)

      !> Then, cycle through alleles and select new random allele if
      !! it happens to coincide with `gene_move`.
      do while (gene_moveto == gene_move)
        gene_moveto = RAND_I(1,this%clength)
      end do

      !> After the cycle, adjust rank_id's of the alleles.
      this%allele(gene_move)%rank_id = gene_moveto

      !> Then re-sort the allele objects by their updated rank_id's.
      call this%sort_rank_id()

      !> Finally, recalculate rank_id's so they are again ordered.
      call this%recalc_rank_id()

    end if

  end subroutine chromosome_mutate_relocate_shift_random

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Functions linked with the object INDIVIDUAL_GENOME
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !-----------------------------------------------------------------------------
  !> Initialise the genome at random, and set sex as determined by the sex
  !! determination locus.
  subroutine genome_init_random(this, label)
    class(INDIVIDUAL_GENOME), intent(inout) :: this
    !! @param[in] label, set an optional label of the genome, if unset,
    !!            generate random string.
    character(len=*), optional, intent(in) :: label

    ! Local variables
    integer :: i,j

    !> ### Implementation details ###
    !> First, create spatial moving object component of the individual genome.
    !! But we do not yet position the genome object.
    call this%create()

    !> Allocate the genome object, it  must have `genome_size` chromosomes and
    !! `CHROMOSOME_PLOIDY` homologs.
    if (.not.  allocated(this%chromosome))                                    &
                allocate(this%chromosome(this%genome_size,CHROMOSOME_PLOIDY))

    !> Now cycle over all the chromosomes and homologs and initialise each
    !! of them.
    do j=1, CHROMOSOME_PLOIDY
      do i=1, this%genome_size
        call this%chromosome(i,j)%init_chromosome( LEN_CHROMOSOMES(i),        &
                                                   LAB_CHROMOSOMES(i)  )
      end do
    end do

    !> On exit from the cycle, set the genome label if provided, or random
    !! string if not.
    if (present(label)) then
      call this%label( label )
    else
      ! label the genome with a random string here
      call this%label ()
    end if

    !> Then, determine the sex of the genome by the genome sex
    !! determination locus taking into account the sex ratio.
    call this%sex_init()

  end subroutine genome_init_random

  !-----------------------------------------------------------------------------
  !> Create a new empty genome, and set sex as determined by the sex
  !! determination locus. Genome values are from parents using inherit
  !! functions.
  subroutine genome_create_zero(this)
    class(INDIVIDUAL_GENOME), intent(inout) :: this

    ! Local variables
    integer :: i,j

    !> ### Implementation details ###
    !> First of all, Create spatial moving object component of the
    !! individual genome.
    call this%create()

    !> Allocate the genome object, it must have `genome_size` chromosomes and
    !! `CHROMOSOME_PLOIDY` homologs
    if (.not. allocated(this%chromosome))                                     &
               allocate(this%chromosome(this%genome_size,CHROMOSOME_PLOIDY))

    !> Now cycle over all the chromosomes and homologs and initialise each
    !! of them with empty genes (zero).
    do j=1, CHROMOSOME_PLOIDY
      do i=1, this%genome_size
        call this%chromosome(i,j)%create_chromosome( LEN_CHROMOSOMES(i),      &
                                                     LAB_CHROMOSOMES(i)  )
      end do
    end do

    !> Initialise the label the genome with a random string.
    call this%label()

    !> Determine the sex of the genome by the genome sex determination locus
    !! taking into account the sex ratio.
    call this%sex_init()

  end subroutine genome_create_zero

  !-----------------------------------------------------------------------------
  !> Label genome. If label is not provided, make a random string.
  ! @note TMP NOTE: label setting removed from the create function
  !       as it will now  override create for `SPATIAL_MOVING`.
  subroutine genome_label_set(this, label)
    class(INDIVIDUAL_GENOME), intent(inout) :: this
    !! @param[in] label, set an optional label of the genome, if unset,
    !!            generate random string.
    character(len=*), optional, intent(in) :: label

    !> ### Implementation details ###
    !> Set the genome label if provided, or random string if not.
    if (present(label)) then
      this%genome_label = label
    else
      ! label the genome with a random string here
      this%genome_label = RAND_STRING(LABEL_LENGTH,LABEL_CST,LABEL_CEN)
    end if

  end subroutine genome_label_set

  !-----------------------------------------------------------------------------
  !> Accessor function to get the genome label. The label is a kind of a
  !! (random) text string name of the genome and the individual agent.
  !! @note We especially need this accessor function because the genome (and
  !!       individual) name is used in other modules for file names ids etc.
  !! @returns String label.
  elemental function genome_label_get(this) result (label_str)
    class(INDIVIDUAL_GENOME), intent(in) :: this
    ! @warning Intel Fortran 17 does not allow setting allocatable attribute
    !          for an elemental function. GNU gfortran allows.
    ! @warning Use intrinsic `trim` function to avoid blanks, especially when
    !          building file names. For example:
    !          `MMDD // "_a_"// trim(this%individ_label()) `
    character(len=LABEL_LENGTH) :: label_str

    label_str = this%genome_label

  end function genome_label_get

  !-----------------------------------------------------------------------------
  !> @brief   Sex determination initialisation subroutine.
  !! @details Determine the genome's sex, sex is set by a logical identifier,
  !!          `sex_is_male` TRUE is **male**. Sex is calculated from the genome
  !!          and based on the average values of the sex determination alleles
  !!          in homologous chromosomes, rescaled to 0:1. This rescaled
  !!          value is then compared with the sex ratio parameter.
  subroutine genome_sex_determine_init(this)
    class(INDIVIDUAL_GENOME), intent(inout) :: this

    integer :: i, j, k
    integer :: sex_locus_sum  ! average_sex_locus
    integer :: sex_locus_num  ! counter
    integer, dimension(ADDITIVE_COMPS) :: values_from_allele

    !---------------------------------------------------------------------------
    !> ### Implementation details ###
    !> The implementation is based on **genotype** x **phenotype** matrix
    !! (logical type): commondata::sex_genotype_phenotype.
    !!
    !> First, initialise the average sex locus sum across the homologous
    !! chromosomes.
    sex_locus_sum = 0; sex_locus_num = 0
    !> #### Loops ####
    !> Then loop across **homologs**, **chromosomes** and **alleles** until
    !! the value of `SEX_GENOTYPE_PHENOTYPE` gets TRUE. This means it is the
    !! *sex locus*.
    CHOMOLOGS: do k=1,CHROMOSOME_PLOIDY
      CCHROMOS: do i=1,this%genome_size
        CALLALES: do j=1, this%chromosome(i,k)%clength
          if ( SEX_GENOTYPE_PHENOTYPE(j,i) ) then
            !> - If this condition is met, set label to the sex locus allele
            !!   ("SEX_LOCUS").
            call this%chromosome(i,k)%allele(j)%labels(SEXLOCUS_LABEL)
            !> - Sex is determined by an average of the sex loci of the
            !!   homologous chromosomes.
            !!   Therefore, first get the vector of additive allele components.
            call this%chromosome(i,k)%allele(j)%get_vector(values_from_allele)
            !> - And sum it up to finally get the total sum for all chromosomes.
            sex_locus_sum = sex_locus_sum + sum(values_from_allele)
            !> - Finally, also update the counter of the totals.
            !! .
            sex_locus_num = sex_locus_num + ADDITIVE_COMPS
          end if
        end do CALLALES
      end do CCHROMOS
    end do CHOMOLOGS
    !! -------------------------------------------------------------------------

    !> Upon exit from the loop, check if the average sex locus across all
    !! homologous chromosomes and additive allele components, scaled to 0:1
    !! is less than the `SEX_RATIO`, the subject becomes the **male** genotype.
    if ( ((real(sex_locus_sum,SRP)/real(sex_locus_num,SRP)) /       &
           (ALLELERANGE_MAX - ALLELERANGE_MIN)) <= SEX_RATIO ) then
      ! set the main sex id of the object
      this%sex_is_male = .TRUE.
      ! set the sex label for the object
      this%sex_label = MALE
    !> Otherwise, the subject becomes the **female**.
    else
      this%sex_is_male = .FALSE.
      this%sex_label = FEMALE
    end if

  end subroutine genome_sex_determine_init

  !-----------------------------------------------------------------------------
  !> Get the logical sex ID of the genome object component.
  elemental function genome_get_sex_is_male(this) result (male)
    class(INDIVIDUAL_GENOME), intent(in) :: this

    !> @return Returns logical value for sex: is it **male**?
    logical :: male

    male = this%sex_is_male

  end function genome_get_sex_is_male

  !-----------------------------------------------------------------------------
  !> Get the logical sex ID of the genome object component.
  elemental function genome_get_sex_is_female(this) result (female)
    class(INDIVIDUAL_GENOME), intent(in) :: this

    !> @return Returns logical value for sex: is it a **female**?
    logical :: female

    if (this%sex_is_male) then
      female = .FALSE.
    else
      female = .TRUE.
    end if

  end function genome_get_sex_is_female

  !-----------------------------------------------------------------------------
  !> Get the descriptive sex label: male or female.
  elemental function genome_get_sex_label(this) result (sex_label)
    class(INDIVIDUAL_GENOME), intent(in) :: this

    !> @return Returns the the descriptive sex label
    character(len=LABEL_LENGTH) :: sex_label

    sex_label = this%sex_label

  end function genome_get_sex_label

  !-----------------------------------------------------------------------------
  !> Set the individual to be **alive**, normally this function is used after
  !! init or birth.
  elemental subroutine genome_individual_set_alive(this)
    class(INDIVIDUAL_GENOME), intent(inout) :: this

    this%alive = .TRUE.

  end subroutine genome_individual_set_alive

  !-----------------------------------------------------------------------------
  !> Set the individual to be **dead**. Note that this function does not
  !! deallocate the individual agent object, this may be a separate destructor
  !! function.
  !!
  !! The `dies` method is implemented at the following levels
  !! of the agent object hierarchy (upper overrides the lower level):
  !! - the_genome::individual_genome::dies();
  !! - the_neurobio::appraisal::dies();
  !! - the_neurobio::gos_global::dies();
  !! - the_individual::individual_agent::dies().
  !! .
  elemental subroutine genome_individual_set_dead(this)
    class(INDIVIDUAL_GENOME), intent(inout) :: this

    this%alive = .FALSE.

  end subroutine genome_individual_set_dead

  !-----------------------------------------------------------------------------
  !> Check if the individual is **alive**.
  elemental function genome_individual_check_alive(this) result(is_alive_now)
    class(INDIVIDUAL_GENOME), intent(in) :: this
    logical :: is_alive_now   !> @return Logical flag for **alive**.

    is_alive_now = this%alive

  end function genome_individual_check_alive

  !-----------------------------------------------------------------------------
  !> Check if the individual is **dead** (the opposite of `is_alive`).
  elemental function genome_individual_check_dead(this) result(is_dead_now)
    class(INDIVIDUAL_GENOME), intent(in) :: this
    logical :: is_dead_now    !> @return Logical flag for **dead**.

    is_dead_now = .TRUE.
    if (this%alive) is_dead_now = .FALSE.

  end function genome_individual_check_dead

  !-----------------------------------------------------------------------------
  !> Internal genetic recombination backend, exchange individual alleles
  !! between homologous chromosomes in mother and father genomes to form
  !! the `this` (offspring) genome. Fully random recombination.
  !! @image html aha_genome_recombine_full.svg
  !! @image latex aha_genome_recombine_full.eps "Scheme of random genetic recombination" width=14cm
  !> @note Note that in this procedure, each of the individual alleles is
  !!       copied from the mother or from the farther **independently** and
  !!       **randomly**. This means that genetic distances are equal
  !!       and there is no linkage disequilibrium.
  !> @note Note also that recombinations across the homologs of the same
  !!       chromosome are also randomised, i.e. different in all homologs.
  subroutine genome_individual_recombine_homol_full_rand_alleles(this,        &
                                               mother, father, exchange_ratio)
    class(INDIVIDUAL_GENOME), intent(inout) :: this
    !> @param[in] mother The **mother** object the_genome::individual_genome
    !!            class.
    class(INDIVIDUAL_GENOME), intent(in) :: mother
    !> @param[in] father The **father** object the_genome::individual_genome
    !!            class.
    class(INDIVIDUAL_GENOME), intent(in) :: father
    real(SRP), optional, intent(in) :: exchange_ratio

    ! Local copies of optionals
    real(SRP) :: exchange_ratio_here

    ! Local counters
    integer :: i, j, k

    !> ### Notable local variables ###
    !> - **n_alleles** is the number of alleles for each of the chromosomes,
    !!   dynamically updated within the loops.
    integer :: n_alleles

    !> - **acomp_vect_mother** and **acomp_vect_father** are the vectors of
    !!   additive allele components that are obtained from the mother and
    !!   the father.
    !! .
    integer, dimension(ADDITIVE_COMPS) :: acomp_vect_mother, acomp_vect_father

    !> ### Implementation details ###
    !> Check optional `exchange_ratio` parameter that defines the ratio of the
    !! alleles that are inherited from the mother. If absent, get the default
    !! value from the commondata::genome_recombination_ratio_mother parameter.
    if (present(exchange_ratio)) then
      exchange_ratio_here = exchange_ratio
    else
      exchange_ratio_here = GENOME_RECOMBINATION_RATIO_MOTHER
    end if

    !> #### Nested loops: HOMOLOGS, CHROMOSOMES, ALLELES ####
    !> Loop through the chromosomes (`CHROMOSOMES` block over `i`) and their
    !! homologues (`HOMOLOGS` block over `j`) and set the genetic make up of
    !! the `this` object from the genes of the `mother` and the `father`
    !! objects.
    !!
    !! The outline of the main loop is:
    !! - homologues
    !!   - chromosomes
    !!     - alleles
    !!     .
    !!   .
    !! .
    !!
    !! @warning Note that it is not possible to use parallel `do concurrent`
    !!          loops here due to non-pure random call of the `RAND_R4()`
    !!          function in the innermost loop (`ALLELES` block over `k`).
    HOMOLOGS: do j=1, CHROMOSOME_PLOIDY
      CHROMOSOMES: do i=1, this%genome_size
        !> First, get the number of alleles for each of the chromosomes:
        !! `n_alleles`.
        n_alleles = this%chromosome(i,j)%clength
        !> Then, loop over the `n_alleles` alleles (`ALLELES` block over `k`):
        ALLELES: do k=1, n_alleles
          !> Randomly select if this specific allele (k) is copied from the
          !! **mother** or is a subject to (random) recombination and gets
          !! the values from the **father**. This is determined stochastically
          !! using the `exchange_ratio` ratio dummy argument or the
          !! `commondata::genome_recombination_ratio_mother` parameter as the
          !! probability to get the allele from the mother.
          if ( RAND_R4() < exchange_ratio_here ) then
            !> - Get the vectors of additive allele components for the
            !!   **mother**.
            !    @note This step is necessary because the
            !          `the_genome::gene::get_vector()` is a subroutine rather
            !          than function. Therefore, it is not possible to place
            !          the values just inline as a function into the next call.
            call mother%chromosome(i,j)%allele(k)%get_vector(acomp_vect_mother)
            !>   Finally, **set** the vector of additive allele components of
            !!   the `this` agent from  the mother's vector.
            call this%chromosome(i,j)%allele(k)%set_vector(acomp_vect_mother)
          else
            !> - Get the vectors of additive allele components for the
            !!   **father**.
            !    @note This step is necessary because the
            !          `the_genome::gene::get_vector()` is a subroutine rather
            !          than function. Therefore, it is not possible to place
            !          the values just inline as a function into the next call.
            call father%chromosome(i,j)%allele(k)%get_vector(acomp_vect_father)
            !>   Finally, **set** the vector of additive allele components of
            !!   the `this` agent from  the father's vector.
            !! .
            call this%chromosome(i,j)%allele(k)%set_vector(acomp_vect_father)
          end if
        end do ALLELES
      end do CHROMOSOMES
    end do HOMOLOGS

  end subroutine genome_individual_recombine_homol_full_rand_alleles

  !-----------------------------------------------------------------------------
  !> Internal genetic recombination backend, exchange individual alleles
  !! between homologous chromosomes in mother and father genomes to form
  !! the `this` (offspring) genome. Partially random recombination, identical
  !! across the homologous chromosomes.
  !! @image html aha_genome_recombine_part.svg
  !! @image latex aha_genome_recombine_part.eps "Scheme of partially random genetic recombination" width=14cm
  !> @note Note that in this procedure, each of the individual alleles is
  !!       copied from the mother or from the farther **independently** and
  !!       **partially randomly**. This means that genetic distances are equal
  !!       and there is no linkage disequilibrium.
  !> @note Note also that recombination is identical across all homologs of
  !!       the same chromosome.
  subroutine genome_individual_recombine_homol_part_rand_alleles(this,        &
                                               mother, father, exchange_ratio)
    class(INDIVIDUAL_GENOME), intent(inout) :: this
    !> @param[in] mother The **mother** object the_genome::individual_genome
    !!            class.
    class(INDIVIDUAL_GENOME), intent(in) :: mother
    !> @param[in] father The **father** object the_genome::individual_genome
    !!            class.
    class(INDIVIDUAL_GENOME), intent(in) :: father
    real(SRP), optional, intent(in) :: exchange_ratio

    ! Local copies of optionals
    real(SRP) :: exchange_ratio_here

    ! Local counters
    integer :: i, j, k

    !> ### Notable local variables ###
    !> - **n_alleles** is the number of alleles for each of the chromosomes,
    !!   dynamically updated within the loops.
    integer :: n_alleles

    !> - **acomp_vect_mother** and **acomp_vect_father** are the vectors of
    !!   additive allele components that are obtained from the mother and
    !!   the father.
    !! .
    integer, dimension(ADDITIVE_COMPS) :: acomp_vect_mother, acomp_vect_father

    !> ### Implementation details ###
    !> Check optional `exchange_ratio` parameter that defines the ratio of the
    !! alleles that are inherited from the mother. If absent, get the default
    !! value from the commondata::genome_recombination_ratio_mother parameter.
    if (present(exchange_ratio)) then
      exchange_ratio_here = exchange_ratio
    else
      exchange_ratio_here = GENOME_RECOMBINATION_RATIO_MOTHER
    end if

    !> #### Nested loops: CHROMOSOMES, ALLELES, HOMOLOGS ####
    !> Loop through the chromosomes (`CHROMOSOMES` block over `i`) and their
    !! and set the genetic make up of
    !! the `this` object from the genes of the `mother` and the `father`
    !! objects.
    !!
    !! The outline of the main loop is:
    !! - chromosomes
    !!   - alleles
    !!     - homologues (within if block)
    !!     .
    !!   .
    !! .
    !!
    !! @warning Note that it is not possible to use parallel `do concurrent`
    !!          loops here due to non-pure random call of the `RAND_R4()`
    !!          function in the innermost loop (`ALLELES` block over `k`).
    CHROMOSOMES: do i=1, this%genome_size
      !> First, get the number of alleles for each of the chromosomes:
      !! `n_alleles`.
      n_alleles = this%chromosome(i,j)%clength
      !> Then, loop over the `n_alleles` alleles (`ALLELES` block over `k`):
      ALLELES: do k=1, n_alleles
        !> Randomly select if this specific allele (k) is copied from the
        !! **mother** or is a subject to (random) recombination and gets
        !! the values from the **father**. This is determined stochastically
        !! using the `exchange_ratio` ratio dummy argument or the
        !! `commondata::genome_recombination_ratio_mother` parameter as the
        !! probability to get the allele from the mother.
        if ( RAND_R4() < exchange_ratio_here ) then
          !> Finally, loop through the homologues of the `i`th chromosome
          !! (`HOMOLOGS` block over `j`) and set the same allele across all
          !! homologues the same way (transferred from mother or the father).
          !! Thus, all homologues have identical non-random recombination
          !! pattern, either from the mother or from the father.
          HOMOLOGS_MO: do j=1, CHROMOSOME_PLOIDY
            !> - Get the vectors of additive allele components for the
            !!   **mother**.
            !    @note This step is necessary because the
            !          `the_genome::gene::get_vector()` is a subroutine rather
            !          than function. Therefore, it is not possible to place
            !          the values just inline as a function into the next call.
            call mother%chromosome(i,j)%allele(k)%get_vector(acomp_vect_mother)
            !>   Finally, **set** the vector of additive allele components of
            !!   the `this` agent from  the mother's vector.
            call this%chromosome(i,j)%allele(k)%set_vector(acomp_vect_mother)
          end do HOMOLOGS_MO
        else
          HOMOLOGS_FA: do j=1, CHROMOSOME_PLOIDY
            !> - Get the vectors of additive allele components for the
            !!   **father**.
            !    @note This step is necessary because the
            !          `the_genome::gene::get_vector()` is a subroutine rather
            !          than function. Therefore, it is not possible to place
            !          the values just inline as a function into the next call.
            call father%chromosome(i,j)%allele(k)%get_vector(acomp_vect_father)
            !>   Finally, **set** the vector of additive allele components of
            !!   the `this` agent from  the father's vector.
            !! .
            call this%chromosome(i,j)%allele(k)%set_vector(acomp_vect_father)
          end do HOMOLOGS_FA
        end if
      end do ALLELES
    end do CHROMOSOMES

  end subroutine genome_individual_recombine_homol_part_rand_alleles

  !-----------------------------------------------------------------------------
  !> Internal fixed genetic crossover backend, exchange blocks of alleles
  !! between homologous chromosomes in mother and father genomes to form
  !! the `this` (offspring) genome.
  !! @image html aha_genome_crossover.svg
  !! @image latex aha_genome_crossover.eps "Scheme of genetic crossover" width=14cm
  !> @note Note that in this procedure, blocks of alleles (i.e. sections of
  !!       the chromosomes) are copied from the mother or from the farther.
  !!       The blocks are fixed and defined by the boolean parameter matrix
  !!       commondata::genome_crossover_fixed_mother. This means that genetic
  !!       recombination is non-random, genetic distances are small within the
  !!       blocks that are inherited from the same parent and relatively large
  !!       between the alleles that belong to different blocks (i.e. the
  !!       genetic distances depend on the proximity of the alleles). This
  !!       makes linkage disequilibrium possible.
  subroutine genome_individual_crossover_homol_fix(this,                      &
                                               mother, father, pattern_matrix)
    class(INDIVIDUAL_GENOME), intent(inout) :: this
    !> @param[in] mother The **mother** object the_genome::individual_genome
    !!            class.
    class(INDIVIDUAL_GENOME), intent(in) :: mother
    !> @param[in] father The **father** object the_genome::individual_genome
    !!            class.
    class(INDIVIDUAL_GENOME), intent(in) :: father
    !> @param[in] pattern_matrix is an optional boolean pattern matrix that
    !!            determines the pattern of fixed chromosome crossover. For
    !!            each chromosome, the alleles that are marked with the
    !!            TRUE (YES) values are inherited from the **mother** whereas
    !!            those marked FALSE (NO) are inherited from the **father**.
    !!            see commondata::genome_crossover_fixed_mother.
    logical, dimension(MAX_NALLELES,N_CHROMOSOMES),                           &
                                        optional, intent(in) :: pattern_matrix

    ! Local copies of optionals
    logical, dimension(MAX_NALLELES,N_CHROMOSOMES) :: pattern_matrix_loc

    ! Local counters
    integer :: i, j, k

    !> ### Notable local variables ###
    !> - **n_alleles** is the number of alleles for each of the chromosomes,
    !!   dynamically updated within the loops.
    integer :: n_alleles

    !> - **acomp_vect_mother** and **acomp_vect_father** are the vectors of
    !!   additive allele components that are obtained from the mother and
    !!   the father.
    !! .
    integer, dimension(ADDITIVE_COMPS) :: acomp_vect_mother, acomp_vect_father

    !> ### Implementation details ###
    !> Check optional `pattern_matrix` parameter matrix. If it is not provided,
    !! use the default commondata::genome_crossover_fixed_mother.
    if (present(pattern_matrix)) then
      pattern_matrix_loc = pattern_matrix
    else
      pattern_matrix_loc = GENOME_CROSSOVER_FIXED_MOTHER
    end if

    !> #### Nested parallel loops: HOMOLOGS, CHROMOSOMES, ALLELES ####
    !> Loop through the chromosomes (`CHROMOSOMES` block over `i`) and their
    !! homologues (`HOMOLOGS`block over `j`) and set the genetic make up of
    !! the `this` object from the genes of the `mother` and the `father`
    !! objects.
    HOMOLOGS: do concurrent (j=1:CHROMOSOME_PLOIDY)
      CHROMOSOMES: do concurrent (i=1:this%genome_size)
        !> First, get the number of alleles for each of the chromosomes:
        !! `n_alleles`.
        n_alleles = this%chromosome(i,j)%clength
        !> Then, loop over the `n_alleles` alleles (`ALLELES` block over `k`):
        ALLELES: do concurrent (k=1:n_alleles)
          !> Check the boolean pattern matrix `pattern_matrix_loc`value,
          !! if it is TRUE, the allele value is copied from the **mother**,
          !! otherwise from the **father**.
          !! ##### If block check #####
          if ( pattern_matrix_loc(k,i) ) then
            !> - Get the vectors of additive allele components for the
            !!   **mother**.
            !    @note This step is necessary because the
            !          `the_genome::gene::get_vector()` is a subroutine rather
            !          than function. Therefore, it is not possible to place
            !          the values just inline as a function into the next call.
            call mother%chromosome(i,j)%allele(k)%get_vector(acomp_vect_mother)
            !>   Finally, **set** the vector of additive allele components of
            !!   the `this` agent from  the mother's vector.
            call this%chromosome(i,j)%allele(k)%set_vector(acomp_vect_mother)
          else
            !> - Get the vectors of additive allele components for the
            !!   **father**.
            !    @note This step is necessary because the
            !          `the_genome::gene::get_vector()` is a subroutine rather
            !          than function. Therefore, it is not possible to place
            !          the values just inline as a function into the next call.
            call father%chromosome(i,j)%allele(k)%get_vector(acomp_vect_father)
            !>   Finally, **set** the vector of additive allele components of
            !!   the `this` agent from  the father's vector.
            !! .
            call this%chromosome(i,j)%allele(k)%set_vector(acomp_vect_father)
          end if
        end do ALLELES
      end do CHROMOSOMES
    end do HOMOLOGS

  end subroutine genome_individual_crossover_homol_fix

  !-----------------------------------------------------------------------------
  !> @name   Neuronal response functions
  !!         There are two separate functions that produce a trait value from
  !!         the genotype. The procedure `trait_init_genotype_gamma2gene` does
  !!         modify the agent (`this`, intent[inout]) as it sets the label.
  !!         On the other hand, a similar procedure
  !!         `the_genome::trait_set_genotype_gamma2gene()` does *not* affect the
  !!         agent, it has the intent [in].
  !! @{

  !> @brief   Initialise an **individual trait** of the agent that depends on
  !!          the genotype. This can be any trait upwards in the class
  !!          hierarchy.
  !! @details Create a new **trait** object from the genotype according to the
  !!          boolean genotype x phenotype matrix `g_p_matrix` by
  !!          `commondata::gamma2gene()` function and the `init_val` baseline
  !!          value. It also introduces an initial Gaussian variance with the
  !!          coefficient of variation=`gerror_cv`. Normally, `g_p_matrix`,
  !!          `init_val` and `gerror_cv` are parameters set in `commondata` for
  !!          each specific trait.  For example, for *thyroid* hormone
  !!          (`the_hormones::hormones` class level up) they are:
  !!          - `commondata::thyroid_genotype_phenotype`,
  !!          - `commondata::thyroid_init` and
  !!          - `commondata::thyroid_gerror_cv`.
  !!          .
  !!
  !! @note    This procedure has the **intent [inout]** for `this`, the agent
  !!          as it modifies the label. Therefore, it should be mainly used
  !!          for initialisation of agent traits.
  !! @warning This code does not (yet) work with *haploid* genotype
  !!          `commondata::chromosome_ploidy = 1`, in such a case there is no
  !!          need to select random homologous chromosome and it is impossible
  !!          to set the two parameters of the `commondata::gamma2gene`
  !!          function (there is only a single chromosome).
  subroutine trait_init_genotype_gamma2gene(this, this_trait, g_p_matrix,     &
                                                    init_val, gerror_cv, label)
    class(INDIVIDUAL_GENOME), intent(inout) :: this

    !> @param[out] this_trait the component of the individual agent object
    !!             hierarchy that we are initialising in.
    real(SRP), intent(out) :: this_trait
    !> @param[in] g_p_matrix matrix genotype x phenotype, the matrix setting
    !!            the correspondence between the genome and the phenotype.
    logical, dimension(:,:), intent(in) :: g_p_matrix
    !> @param[in] init_val start value, trait initialisation baseline value
    !!        parameter, e.g. for *thyroid* it is `commondata::thyroid_init`.
    real(SRP), intent(in)  ::  init_val
    !> @param[in] gerror_cv error value, trait Gaussian error value, e.g.
    !!        for *thyroid* it is `commondata::thyroid_gerror_cv`, if absent,
    !!        we don't introduce random error and the initial trait values are
    !!        deterministic by the genome.
    real(SRP), optional, intent(in) :: gerror_cv
    !> @param[in] label a label for the allele locus that sets the
    !!        phenotypic object.
    character(len=*), intent(in) :: label

    ! Local variables
    integer :: i, j, k1, k2
    integer, dimension(ADDITIVE_COMPS) :: additive_vals_1, additive_vals_2

    !> ### Implementation details ###
    !> Set **trait** values from the genome using the `g_p_matrix` matrix.
    !> We first need to select two chromosomes from the available set (normally
    !! two, but possibly more) for input into commondata::gamma2gene() function.
    !! We do it random.
    !   - Assign the first chromosome.
    k1 = RAND_I(1,CHROMOSOME_PLOIDY)
    !   - Also assign the second chromosome.
    k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    !> Then, cycle through and select a different chromosomes (i.e. cycle
    !! if happens to coincide with the first).
    !! (Use `do while chromosome1 = chromosome2` construct.)
    do while (k1 == k2)
      k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    end do
    !> As a result there are two distinct chromosomes `k1` and `k2`.
    !! This unique chromosomes selection part of the code precludes
    !! the use of haploid genome.
    !> #### Loop over chromosomes and alleles ####
    CCHROMOS: do i=1,this%genome_size
      CALLELES: do j=1, this%chromosome(i,k1)%clength
        if ( g_p_matrix(j,i) ) then
          !> - set label to the **trait** locus allele
          call this%chromosome(i,k1)%allele(j)%labels(label)
          call this%chromosome(i,k2)%allele(j)%labels(label)
          !> - The initial trait value is determined using the
          !!   commondata::gamma2gene() function using additive allele
          !!   components. So, we first get the two vectors of additive
          !!   allele components.
          call this%chromosome(i,k1)%allele(j)%get_vector(additive_vals_1)
          call this%chromosome(i,k2)%allele(j)%get_vector(additive_vals_2)
          !> - And get the value of the trait from commondata::gamma2gene().
          !!   @note commondata::gamma2gene() now accepts vectors of
          !!       additive allele  components.
          if (present(gerror_cv)) then
            this_trait = gamma2gene( additive_vals_1, additive_vals_2,        &
                                    init_val, gerror_cv )
          else
            this_trait = gamma2gene( additive_vals_1, additive_vals_2,        &
                                    init_val)
          end if
        end if
      end do CALLELES
    end do CCHROMOS

  end subroutine trait_init_genotype_gamma2gene

  !-----------------------------------------------------------------------------
  !> @brief   Set an **individual trait** of the agent that depends on
  !!          the genotype. This can be any trait upwards in the class hierarchy.
  !! @details This is almost the same as
  !!          `the_genome::trait_init_genotype_gamma2gene()`, but does
  !!          *not* modify the `this` object (it has **intent [in]**).
  !!          Therefore it should be used for setting such traits as
  !!          behavioural expectancies.
  !!
  !! @note    This procedure has the intent [in] for `this`, the agent as it
  !!          does not modify the object (e.g. does not set label). Therefore,
  !!          it can be used in assessing the subjective expectancies.
  !! @warning This code does not (yet) work with *haploid* genotype
  !!          `commondata::chromosome_ploidy = 1`, in such a case there is no
  !!          need to select random homologous chromosome and it is impossible
  !!          to set the two parameters of the `commondata::gamma2gene()`
  !!          function (there is only a single chromosome).
  subroutine trait_set_genotype_gamma2gene(this, this_trait, g_p_matrix,     &
                                                          init_val, gerror_cv)
    class(INDIVIDUAL_GENOME), intent(in) :: this

    !> @param[out] this_trait the component of the individual agent
    !!        object hierarchy that we are initialising.
    real(SRP), intent(out) :: this_trait
    !> @param[in] matrix genotype x phenotype, the matrix setting the
    !!               correspondence between the genome and the phenotype.
    logical, dimension(:,:), intent(in) :: g_p_matrix
    !> @param[in] init_val start value, trait initialisation baseline value
    !!        parameter, e.g. for *thyroid* it is `commondata::thyroid_init`.
    real(SRP), intent(in)  ::  init_val
    !> @param[in] gerror_cv error value, trait Gaussian error value, e.g. for
    !!        *thyroid* it is `commondata::thyroid_gerror_cv`, if absent,
    !!        we don't introduce random error and the initial trait values are
    !!        deterministic by the genome.
    real(SRP), optional, intent(in) :: gerror_cv

    ! Local variables
    integer :: i, j, k1, k2
    integer, dimension(ADDITIVE_COMPS) :: additive_vals_1, additive_vals_2

    !> ### Implementation details ###
    !> Set **trait** values from the genome using the `g_p_matrix` matrix.
    !> We first need to select two chromosomes from the available set (normally
    !! two, but possibly more) for input into commondata::gamma2gene() function.
    !! We do it random.
    !   - Assign the first chromosome.
    k1 = RAND_I(1,CHROMOSOME_PLOIDY)
    !   - Also assign the second chromosome.
    k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    !> Then, we cycle through and select a different chromosomes (i.e. cycle
    !! if happens to coincide with the first.
    do while (k1 == k2)
      k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    end do
    !> As a result there are two distinct chromosomes `k1` and `k2`.
    !! This unique chromosomes selection part of the code precludes
    !! the use of haploid genome.
    !> #### Loop over chromosomes and alleles ####
    CCHROMOS: do i=1,this%genome_size
      CALLELES: do j=1, this%chromosome(i,k1)%clength
        if ( g_p_matrix(j,i) ) then
          !> - The initial trait value is determined using the
          !!   commondata::gamma2gene() function using additive allele
          !!   components. So, we first get the two vectors of additive
          !!   allele components.
          call this%chromosome(i,k1)%allele(j)%get_vector(additive_vals_1)
          call this%chromosome(i,k2)%allele(j)%get_vector(additive_vals_2)
          !> - And get the value of the trait from `commondata::gamma2gene()`.
          !!   @note commondata::gamma2gene() now accepts vectors of additive
          !!         allele components.
          if (present(gerror_cv)) then
            this_trait = gamma2gene( additive_vals_1, additive_vals_2,        &
                                    init_val, gerror_cv )
          else
            this_trait = gamma2gene( additive_vals_1, additive_vals_2,        &
                                    init_val )
          end if
        end if
      end do CALLELES
    end do CCHROMOS

  end subroutine trait_set_genotype_gamma2gene

  !> @}

  !-----------------------------------------------------------------------------
  !> @name   The genotype to phenotype functions based on fixed linear
  !!         transformation.
  !! @{

  !-----------------------------------------------------------------------------
  !> @brief   Initialise an **individual trait** of the agent that depends on
  !!          the genotype. This can be any trait upwards in the class
  !!          hierarchy.
  !! @details Create a new **trait** object from the genotype according to the
  !!          boolean genotype x phenotype matrix `g_p_matrix` by a simple
  !!          linear scaling transformation function. Normally, `g_p_matrix`,
  !!          is a parameters set in @ref commondata for each specific trait.
  !! @note    This version uses only two chromosomes for compatibility with
  !!          the counterpart the_genome::trait_init_genotype_gamma2gene()
  !!          procedure; the code logic is almost the same as in that based on
  !!          the neuronal response function.
  subroutine trait_init_linear_sum_additive_comps_2genes_r(this,              &
                                          this_trait, g_p_matrix,             &
                                          phenotype_min, phenotype_max, label)
    class(INDIVIDUAL_GENOME), intent(inout) :: this

    !> @param[out] this_trait the component of the individual agent object
    !!             hierarchy that we are initialising in.
    real(SRP), intent(out) :: this_trait
    !> @param[in] g_p_matrix matrix genotype x phenotype, the matrix setting
    !!            the correspondence between the genome and the phenotype.
    logical, dimension(:,:), intent(in) :: g_p_matrix
    !> @param[in] init_val start value, trait initialisation baseline value
    !!        parameter, e.g. for *thyroid* it is `commondata::thyroid_init`.
    real(SRP), intent(in)  ::  phenotype_min
    real(SRP), intent(in)  ::  phenotype_max
    !> @param[in] label a label for the allele locus that sets the
    !!        phenotypic object.
    character(len=*), intent(in) :: label

    ! Local variables
    integer :: i, j, k1, k2
    integer, dimension(ADDITIVE_COMPS) :: additive_vals_1, additive_vals_2

    ! The Minimum possible value of the genome, assuming each of the
    ! commondata::additive_comps additive components of two homologous
    ! (genes) has the minimum value commondata::allelerange_min.
    integer, parameter :: SCALE_GENOME_MIN = ALLELERANGE_MIN*ADDITIVE_COMPS * 2

    ! The Maximum possible value of the genome, assuming each of the
    ! commondata::additive_comps additive components of two homologous
    ! (genes) has the maximum value commondata::allelerange_max.
    integer, parameter :: SCALE_GENOME_MAX = ALLELERANGE_MAX*ADDITIVE_COMPS * 2

    !> ### Implementation details ###
    !> Set **trait** values from the genome using the `g_p_matrix` matrix.
    !> We first need to select two chromosomes from the available set (normally
    !! two, but possibly more) for input into `gamma2gene`. We do it random.
    !> Assign the first chromosome.
    k1 = RAND_I(1,CHROMOSOME_PLOIDY)
    !> Also assign the second chromosome.
    k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    !> Then, cycle through and select a different chromosomes (i.e. cycle
    !! if happens to coincide with the first).
    !! (Use do while chromosome1 = chromosome2` construct.)
    do while (k1 == k2)
      k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    end do
    !> As a result there are two distinct chromosomes `k1` and `k2`.
    !! This unique chromosomes selection part of the code precludes
    !! the use of haploid genome.
    !> #### Loop over chromosomes and alleles ####
    CCHROMOS: do i=1,this%genome_size
      CALLELES: do j=1, this%chromosome(i,k1)%clength
        if ( g_p_matrix(j,i) ) then
          !> - set label to the **trait** locus allele
          call this%chromosome(i,k1)%allele(j)%labels(label)
          call this%chromosome(i,k2)%allele(j)%labels(label)
          !> - The initial trait value is determined using the gamma2gene
          !!   function using additive allele components.
          !!   So, we first get the two vectors of additive allele components.
          call this%chromosome(i,k1)%allele(j)%get_vector(additive_vals_1)
          call this%chromosome(i,k2)%allele(j)%get_vector(additive_vals_2)
          !> - Unlike ::trait_init_genotype_gamma2gene, the output phenotypic
          !!   value is obtained by simple linear commondata::rescale from the
          !!   minimum genotype range to the phenotypic range set by the
          !!   `phenotype_min` and `phenotype_max` parameters.
          !! .
          this_trait = rescale( real(sum(additive_vals_1) +                   &
                                     sum(additive_vals_2), SRP),              &
                                real(SCALE_GENOME_MIN, SRP),                  &
                                real(SCALE_GENOME_MAX, SRP),                  &
                                phenotype_min,                                &
                                phenotype_max )
        end if
      end do CALLELES
    end do CCHROMOS

  end subroutine trait_init_linear_sum_additive_comps_2genes_r

  !-----------------------------------------------------------------------------
  !> @brief   Set an **individual trait** of the agent that depends on
  !!          the genotype. This can be any trait upwards in the class
  !!          hierarchy.
  !! @details This is almost the same as
  !!          the_genome::trait_init_linear_sum_additive_comps_2genes_r(), but
  !!          does *not* modify the `this` object (it has **intent [in]**).
  !!          Therefore it should be used for setting such traits as
  !!          behavioural expectancies.
  !!          Create a new **trait** object from the genotype according to the
  !!          boolean genotype x phenotype matrix `g_p_matrix` by a simple
  !!          linear scaling transformation function. Normally, `g_p_matrix`,
  !!          is a parameters set in @ref commondata for each specific trait.
  !! @note    This version uses only two chromosomes for compatibility with
  !!          the counterpart the_genome::trait_init_genotype_gamma2gene()
  !!          procedure; the code logic is almost the same as in that based on
  !!          the neuronal response function.
  subroutine trait_set_linear_sum_additive_comps_2genes_r( this,              &
                                          this_trait, g_p_matrix,             &
                                          phenotype_min, phenotype_max )
    class(INDIVIDUAL_GENOME), intent(in) :: this

    !> @param[out] this_trait the component of the individual agent object
    !!             hierarchy that we are initialising in.
    real(SRP), intent(out) :: this_trait
    !> @param[in] g_p_matrix matrix genotype x phenotype, the matrix setting
    !!            the correspondence between the genome and the phenotype.
    logical, dimension(:,:), intent(in) :: g_p_matrix
    !> @param[in] init_val start value, trait initialisation baseline value
    !!        parameter, e.g. for *thyroid* it is `commondata::thyroid_init`.
    real(SRP), intent(in)  ::  phenotype_min
    real(SRP), intent(in)  ::  phenotype_max

    ! Local variables
    integer :: i, j, k1, k2
    integer, dimension(ADDITIVE_COMPS) :: additive_vals_1, additive_vals_2

    ! The Minimum possible value of the genome, assuming each of the
    ! commondata::additive_comps additive components of two homologous
    ! (genes) has the minimum value commondata::allelerange_min.
    integer, parameter :: SCALE_GENOME_MIN = ALLELERANGE_MIN*ADDITIVE_COMPS * 2

    ! The Maximum possible value of the genome, assuming each of the
    ! commondata::additive_comps additive components of two homologous
    ! (genes) has the maximum value commondata::allelerange_max.
    integer, parameter :: SCALE_GENOME_MAX = ALLELERANGE_MAX*ADDITIVE_COMPS * 2

    !> ### Implementation details ###
    !> Set **trait** values from the genome using the `g_p_matrix` matrix.
    !> We first need to select two chromosomes from the available set (normally
    !! two, but possibly more) for input into `gamma2gene`. We do it random.
    !> Assign the first chromosome.
    k1 = RAND_I(1,CHROMOSOME_PLOIDY)
    !> Also assign the second chromosome.
    k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    !> Then, cycle through and select a different chromosomes (i.e. cycle
    !! if happens to coincide with the first).
    !! (Use do while chromosome1 = chromosome2` construct.)
    do while (k1 == k2)
      k2 = RAND_I(1,CHROMOSOME_PLOIDY)
    end do
    !> As a result there are two distinct chromosomes `k1` and `k2`.
    !! This unique chromosomes selection part of the code precludes
    !! the use of haploid genome.
    !> #### Loop over chromosomes and alleles ####
    CCHROMOS: do i=1,this%genome_size
      CALLELES: do j=1, this%chromosome(i,k1)%clength
        if ( g_p_matrix(j,i) ) then
          !> - The initial trait value is determined using the gamma2gene
          !!   function using additive allele components.
          !!   So, we first get the two vectors of additive allele components.
          call this%chromosome(i,k1)%allele(j)%get_vector(additive_vals_1)
          call this%chromosome(i,k2)%allele(j)%get_vector(additive_vals_2)
          !> - Unlike ::trait_init_genotype_gamma2gene, the output phenotypic
          !!   value is obtained by simple linear commondata::rescale from the
          !!   minimum genotype range to the phenotypic range set by the
          !!   `phenotype_min` and `phenotype_max` parameters.
          !! .
          this_trait = rescale( real(sum(additive_vals_1) +                   &
                                     sum(additive_vals_2), SRP),              &
                                real(SCALE_GENOME_MIN, SRP),                  &
                                real(SCALE_GENOME_MAX, SRP),                  &
                                phenotype_min,                                &
                                phenotype_max )
        end if
      end do CALLELES
    end do CCHROMOS

  end subroutine trait_set_linear_sum_additive_comps_2genes_r

  !> @}

  !-----------------------------------------------------------------------------
  !> Perform a probabilistic random mutation(s) on the individual genome.
  !! This is a high level wrapper to build mutations from various components.
  subroutine genome_mutate_wrapper(this, p_point, p_set, p_swap, p_shift)
    class(INDIVIDUAL_GENOME), intent(inout) :: this
    !> @param[in] p_point optional probability of mutation, if absent, the
    !!            default value commondata::mutationrate_point is used.
    real(SRP), optional, intent(in) :: p_point
    !> @param[in] p_set optional probability of mutation, if absent, the
    !!            default value commondata::mutationrate_batch is used.
    real(SRP), optional, intent(in) :: p_set
    !> @param[in] p_swap optional probability of mutation, if absent, the
    !!            default value commondata::mutationrate_batch is used.
    real(SRP), optional, intent(in) :: p_swap
    !> @param[in] p_shift optional probability of mutation, if absent, the
    !!            default value commondata::mutationrate_batch is used.
    real(SRP), optional, intent(in) :: p_shift

    ! Local variables that randomly set the location for the mutation.
    integer :: mutate_chromosome, mutate_homolog, mutate_allele

    !> ### Implementation notes ###
    !> Each of the mutations below is a random process that occurs with a
    !! specific probability that is set by its respective mutation rate
    !! parameter.
    !> - Call for a point mutation on a single random allele component of a
    !!   randomly chosen chromosome using the the_genome::gene::mutate_point()
    !!   method.
    mutate_chromosome = RAND_I(1, this%genome_size)
    mutate_homolog = RAND_I(1, CHROMOSOME_PLOIDY)
    mutate_allele = RAND_I(1, this%chromosome(                                &
                                mutate_chromosome,mutate_homolog)%clength)
    if (present(p_point)) then
      call this%chromosome(mutate_chromosome,mutate_homolog)%allele(          &
                                            mutate_allele)%mutate_point(p_point)
    else
      call this%chromosome(mutate_chromosome,mutate_homolog)%allele(          &
                                            mutate_allele)%mutate_point()
    end if

    !> - Call for a point mutation on a whole random allele (batch of allele
    !!   components) of a randomly chosen chromosome using the
    !!   the_genome::gene::mutate_set() method.
    mutate_chromosome = RAND_I(1, this%genome_size)
    mutate_homolog = RAND_I(1, CHROMOSOME_PLOIDY)
    mutate_allele = RAND_I(1, this%chromosome(                                &
                                mutate_chromosome,mutate_homolog)%clength)
    if (present(p_set)) then
      call this%chromosome(mutate_chromosome,mutate_homolog)%allele(          &
                                                mutate_allele)%mutate_set(p_set)
    else
      call this%chromosome(mutate_chromosome,mutate_homolog)%allele(          &
                                                mutate_allele)%mutate_set()
    end if

    ! > - Call for a  relocation swap mutation; this calls the
    ! !   the_genome::chromosome::mutate_swap() method.
    ! mutate_chromosome = RAND_I(1, this%genome_size)
    ! mutate_homolog = RAND_I(1, CHROMOSOME_PLOIDY)
    ! if (present(p_swap)) then
    !   call this%chromosome(mutate_chromosome, mutate_homolog)%              &
    !                                                     mutate_swap(p_swap)
    ! else
    !   call this%chromosome(mutate_chromosome, mutate_homolog)%mutate_swap()
    ! end if

    ! > - Call for a relocation shift mutation: this calls the
    ! !   the_genome::chromosome::mutate_shift() method.
    ! ! .
    ! mutate_chromosome = RAND_I(1, this%genome_size)
    ! mutate_homolog = RAND_I(1, CHROMOSOME_PLOIDY)
    ! if (present(p_shift)) then
    !   call this%chromosome(mutate_chromosome, mutate_homolog)%              &
    !                                                   mutate_shift(p_shift)
    ! else
    !   call this%chromosome(mutate_chromosome, mutate_homolog)%mutate_shift()
    ! end if

  end subroutine genome_mutate_wrapper

end module THE_GENOME
