!> @file m_fileio.f90
!! File objects for the AHA Model.
!! @author Sergey Budaev <sergey.budaev@uib.no>
!! @author Jarl Giske <jarl.giske@uib.no>
!! @date 2016-2017

!-------------------------------------------------------------------------------
! $Id$
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!> @brief Definition of high level file objects
!> @section file_io_module FILE_IO module
!> This module defines input and output to external files in various formats.
!! The main format for output files for numerical data is CSV. It is useful
!! for one-dimensional vectors and two-dimensional matrices. More complex
!! data structures can be output in other formats (to be implemented) such as
!! binary, XML or HDF. This module provides an unitary object-oriented
!! interface to all file types and objects.
!!
!! **Current status:** Only [CSV](http://ahamodel.uib.no/doc/ar01s08.html) and
!! plain text (TXT) files are implemented so far. And even here, only the file
!! object is implemented in the object oriented style, record (string) is used
!! exactly as in the [CSV_IO](http://ahamodel.uib.no/doc/ar01s08.html) in HEDTOOLS.
!!
!! @subsection file_io_module_csv CSV format
!! Notably, standard HEDTOOLS whole array procedure
!! [CSV_MATRIX_WRITE](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_matrix_write)
!! can save a single vector or 2D matrix into a separate CSV file.
!!
!! **Example:** @anchor file_handle_example_code
!! @code
!!   ! File handle object identifies a specific file.
!!   type(FILE_HANDLE) :: test_file
!!   ! String variable that is used to build each record (row)
!!   ! it must fit the whole record into its length;
!!   character(len=:), allocatable :: record_string
!!
!!   ! Open the file for writing
!!   call test_file%open_write( "test_file.csv", FORMAT_CSV )
!!
!!   ! Write optional header.
!!   call test_file%header_write("Header is the first line of the file")
!!
!!   ! The current record must be cleared before it is built. Note that
!!   ! if the record string is allocatable, it must be whole blank.
!!   record_string=repeat(" ", max_len)
!!   ! Append values to the first record. The first record is
!!   ! here the variable names.
!!   call csv_record_append( record_string, ["No", "V1", "V2", "V3"] )
!!   ! Write this record 'record_string' physically to the disk.
!!   call test_file%record_write(record_string)
!!
!!   ! Now write the data beyond the first row.
!!   do i=1, 100
!!     record_string=repeat(" ", max_len)  ! clean each new record string
!!     ! Append values of various types to the current
!!     ! record string 'record_string'
!!     call csv_record_append( record_string, i )
!!     call csv_record_append( record_string, [ 1.1, 2.2 ] )
!!     call csv_record_append( record_string, 3.3 )
!!     ! Once the record is built, write it to the disk.
!!     call test_file%record_write(record_string)
!!   end do
!!
!!   ! Close file at the end.
!!   call test_file%close()
!! @endcode
!! See code of the the_population::population_save_data_all_agents_csv() for
!! another example of writing data to CSV file. But note that these codes use
!! the standard non-object-oriented procedures from HEDTOOLS, not these
!! wrappers.
!!
!! @subsection file_io_module_txt Plain text (TXT) format
!! The file handler object file_io::file_handle defined in this module can be
!! easily used to write arbitrary plain text files. Here is an example.
!!
!! **Example:**
!! @code
!!   ! File handle object identifies a specific file.
!!   type(FILE_HANDLE) :: test_file
!!
!!   ! Open the file for writing
!!   call test_file%open_write( "test_file.txt", FORMAT_TXT )
!!
!!   ! Write an arbitrary row of data to the file, character text string:
!!   call test_file%record_write("This is a test string to output")
!!
!!   ! Standard Fortran intrinsic 'write' can be combined with the 'get_unit()'
!!   ! accessor function for unit.
!!   write( test_file%get_unit(), * ) "Raw string 1, success=", test_file%is_success()
!!   write( test_file%get_unit(), * ) "Raw string 2, success=", test_file%is_success()
!!   write( test_file%get_unit(), * ) "Raw string 3, success=", test_file%is_success()
!!
!!   ! Close file at the end.
!!   call test_file%close()
!! @endcode
!!
!! Thus, the wrappers implemented in this unit allow to use unitary file
!! handler object to work with specific files, even though they are not fully
!! object oriented. Using a single file handle is simpler and more
!! understandable than different Fortran file identifiers (file name, unit).
!!
!! The logger commondata::logger_init() is the standard normal method to
!! report everything in the model during the runtime. Therefore, using
!! separate plain text file(s) to output any reports should be very rare if
!! needed at all.
!> ### Accessibility of objects ###
!! By default, all objects in `FILE_IO` are *private*.
module FILE_IO
  use COMMONDATA
  use CSV_IO
  implicit none

  private   ! All objects here are private except explicitly stated public.

  !> @name Public enumeration constants defining supported file types
  !! @{
  !> Define the file types that are supported by this module.
  enum, bind(C)
    enumerator :: UNDEFINED, FORMAT_CSV, FORMAT_TXT
  end enum

  public :: UNDEFINED, FORMAT_CSV, FORMAT_TXT
  !> @}

  !> `FILE_HANDLE` is the basic file handle object. It provides an
  !! unitary object oriented interface for operations with any supported file
  !! types.
  !!
  !! **Example:**
  !! @code
  !! type(FILE_HANDLE) :: data_file
  !! @endcode
  !> Only CSV format is supported so far.
  !> @note It would be even more useful to add the CSV file record into the
  !!       type as an allocatable component and implement simple record_clean
  !!       record_append functions. However, allocatable components
  !!       are not supported in GNU gfortran 4.8, issues error:
  !!       `Error: Deferred-length character component 'record_string' at
  !!       (1) is not yet supported`. Therefore, raw operations with the CSV
  !!       file record are left here as in the non-OO versions. See
  !!       [extended example](http://ahamodel.uib.no/doc/ar01s08.html#_extended_example)
  !!       in HEDTOOLS and the code of this procedure:
  !!       the_population::population_save_data_all_agents_csv(). But note
  !!       that these codes use standard non-object-oriented procedures from
  !!       HEDTOOLS, not these object oriented wrappers.
  type, public :: FILE_HANDLE
    !> The format of the file, i.e. the file type. The following file types
    !! are implemented:
    !! - `FORMAT_CSV` -- Comma Separated Values is a plain text;
    !! - `FORMAT_TXT` -- Plain text file.
    !! .
    integer, private :: format
    !> `file_object_csv` is the standard `CSV_FILE` type from CSV_IO module
    !! HEDTOOLS.  For details on this component type see
    !! [CSV_FILE](http://ahamodel.uib.no/doc/ar01s08.html#_derived_type_csv_file)
    !! `CSV_FILE` has the following sub-components:
    !! - `character (len=MAX_FILENAME) :: name` -- file name;
    !! - `integer :: unit` -- Fortran file unit;
    !! - `logical :: status` -- Logical status of the last operation.
    !! .
    !! @note In the wrapper implementation routines, just substitute the
    !!       original CSV file handle object with this%file_object_csv.
    type (CSV_FILE), private :: file_object_csv
    contains
      ! All objects here are private except explicitly stated public.
      private
      !> Obtain the success (TRUE) or failure (FALSE) status of the latest
      !! file operation.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! if ( data_file%is_success() ) then
      !! @endcode
      procedure, public :: is_success => file_operation_last_is_success
      !> Get the file name associated with the file handle.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! print *, data_file%get_name()
      !! @endcode
      procedure, public :: get_name => file_hangle_get_name_string
      !> Get the Fortran unit number associated with the
      !! file handle object.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! print *, data_file%get_unit()
      !! @endcode
      procedure, public :: get_unit => file_object_get_associated_unit
      !> Check if the file format is CSV.
      !! See @ref file_handle_example_code "example code".
      procedure, public :: is_csv => file_object_format_is_csv
      !> Check if the file format is TXT.
      !! See @ref file_handle_example_code "example code".
      procedure, public :: is_txt => file_object_format_is_txt
      !> Open file for physical writing on the disk.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! call data_file%open_write( "file_001.csv" )
      !! @endcode
      procedure, public :: open_write => csv_open_write_this
      !> Physically write an information header (metadata) about the file.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! call data_file%header_write("Agent data at start of the simulation")
      !! @endcode
      procedure, public :: header_write => csv_header_line_write_this
      !> Closes the file for reading or writing.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! call data_file%close()
      !! @endcode
      procedure, public :: close => csv_close_this
      !> Physically write a single string CSV data record (row) to the file.
      !! See @ref file_handle_example_code "example code".
      !!
      !! **Example:**
      !! @code
      !! call data_file%record_write( record_string )
      !! @endcode
      procedure, public :: record_write => csv_record_string_write_this

  end type

contains ! ........ implementation of procedures for this level ................

  !-----------------------------------------------------------------------------
  !> Get the success or error status of the latest file operation.
  !! **Example:**
  !! @code
  !! if ( data_file%is_success() ) then
  !! @endcode
  function file_operation_last_is_success(this) result (is_success)
    class(FILE_HANDLE), intent(inout) :: this
    !> @return TRUE if the latest file operation was successful,
    !!         FALSE otherwise.
    logical :: is_success
    is_success = this%file_object_csv%status
  end function file_operation_last_is_success

  !-----------------------------------------------------------------------------
  !> Get the file name associated with the file handle. If the file name is
  !! (yet) undefined, the latest operation success flag
  !! (see file_io::is_success()) is FALSE.
  !! **Example:**
  !! @code
  !! print *, data_file%get_name()
  !! @endcode
  function file_hangle_get_name_string(this) result (name_file)
    class(FILE_HANDLE), intent(inout) :: this
    !> @return the name of the file.
    character(len=:), allocatable :: name_file
    name_file = this%file_object_csv%name
    if ( len_trim(name_file)==0 ) this%file_object_csv%status = .FALSE.
  end function file_hangle_get_name_string

  !-----------------------------------------------------------------------------
  !> A Low level function to get the Fortran unit number associated with the
  !! file handle object.
  !! @note This function is useful only for diagnostics because Fortran unit is
  !!       treated automatically and transparently in all the routines of this
  !!       module. Of course, it could also be useful for low-level code.
  !! **Example:**
  !! @code
  !! print *, data_file%get_unit()
  !! @endcode
  function file_object_get_associated_unit(this) result (unit_n)
    class(FILE_HANDLE), intent(inout) :: this
    !> @return the Furtran file unit.
    integer :: unit_n
    unit_n = GET_FILE_UNIT( this%file_object_csv%name,                        &
                            this%file_object_csv%status )
  end function file_object_get_associated_unit

  !-----------------------------------------------------------------------------
  !> Check if the file format is CSV.
  function file_object_format_is_csv(this) result (is_type)
    class(FILE_HANDLE), intent(in) :: this
    !> @return TRUE if the file format is CSV, FALSE otherwise.
    logical :: is_type
    if (this%format == FORMAT_CSV) then
      is_type = .TRUE.
    else
      is_type = .FALSE.
    end if
  end function file_object_format_is_csv

  !-----------------------------------------------------------------------------
  !> Check if the file format is CSV.
  function file_object_format_is_txt(this) result (is_type)
    class(FILE_HANDLE), intent(in) :: this
    !> @return TRUE if the file format is TXT, FALSE otherwise.
    logical :: is_type
    if (this%format == FORMAT_TXT) then
      is_type = .TRUE.
    else
      is_type = .FALSE.
    end if
  end function file_object_format_is_txt

  !-----------------------------------------------------------------------------
  !> This is an object oriented wrapper for `CSV_OPEN_WRITE()`. For details see
  !! [CSV_OPEN_WRITE](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_header_write).
  !! @note This procedure also automatically and transparently assigns the
  !!       Fortran unit.
  !! **Example:**
  !! @code
  !! call data_file%open_write( "file_001.csv" )
  !! @endcode
  subroutine csv_open_write_this(this, name, format)
    class(FILE_HANDLE), intent(inout) :: this
    !> @param[in] name the name of the file.
    character(len=*), intent(in) :: name
    !> @param[in] format optional data format type of the file:
    !!            - FORMAT_CSV (default)
    !!            - FORMAT_TXT
    !!            .
    integer, optional, intent(in) :: format

    ! Local copies of optionals
    integer :: format_def

    !> ### Implementation notes ###
    !> Set name from the mandatory `name` argument.
    this%file_object_csv%name = name

    !> Default format is `FORMAT_CSV`.
    if (present(format)) then
      format_def = format
    else
      format_def = FORMAT_CSV
    end if

    !> Providing a non-supported format results in FALSE status flag.
    select case (format_def)
      case (FORMAT_CSV)
        this%format = format_def
        this%file_object_csv%status = .TRUE.
      case (FORMAT_TXT)
        this%format = format_def
        this%file_object_csv%status = .TRUE.  ! Not implemented so far.
      case default
        this%format = UNDEFINED
        this%file_object_csv%status = .FALSE. ! Unknown format, error.
        return
    end select

    !> Open the file for writing physically.
    call CSV_OPEN_WRITE( this%file_object_csv )

  end subroutine csv_open_write_this

  !-----------------------------------------------------------------------------
  !> This is an object oriented wrapper for `CSV_CLOSE()`. For details see
  !! [CSV_CLOSE](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_close).
  !! @note This procedure also automatically ans transparently assigns the
  !!       Fortran unit.
  !! **Example:**
  !! @code
  !! call data_file%close()
  !! @endcode
  subroutine csv_close_this(this)
    class(FILE_HANDLE), intent(inout) :: this
    call CSV_CLOSE( this%file_object_csv )
  end subroutine csv_close_this

  !-----------------------------------------------------------------------------
  !> This is an object oriented wrapper for `CSV_HEADER_WRITE()`. See
  !! [CSV_HEADER_WRITE](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_header_write)
  !! for details.
  !! @note File header is optional in CSV files and is not used in most cases.
  !! **Example:**
  !! @code
  !! call data_file%header_write("Agent data at start of the simulation")
  !! @endcode
  subroutine csv_header_line_write_this(this, header)
    class(FILE_HANDLE), intent(inout) :: this
    !> @param[in] header is the optional header line for the CSV file. If
    !!            header is absent, it is automatically generated from the
    !!            file name.
    character(len=*), optional, intent(in) :: header
    if (present(header)) then
      call CSV_HEADER_WRITE( header, this%file_object_csv )
    else
      call CSV_HEADER_WRITE( "File " // this%file_object_csv%name,            &
                             this%file_object_csv )
    end if
  end subroutine csv_header_line_write_this

  !-----------------------------------------------------------------------------
  !> Physically write a single string CSV data record to the file. See
  !! [CSV_RECORD_WRITE](http://ahamodel.uib.no/doc/ar01s08.html#_subroutine_csv_record_write)
  !! **Example:**
  !! @code
  !! call data_file%record_write( record_string )
  !! @endcode
  subroutine csv_record_string_write_this(this, csv_record)
    class(FILE_HANDLE), intent(inout) :: this
    !> @param[in] csv_record character string that keeps the whole record
    !!            (row) of the CSV data spreadsheet.
    character(len=*), intent(in) :: csv_record
    call CSV_RECORD_WRITE( csv_record, this%file_object_csv )
  end subroutine csv_record_string_write_this


end module FILE_IO
