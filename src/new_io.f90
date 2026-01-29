!>\mainpage Sketchs of loadtxt for fortran_libstd

!> new_io provides a framework for
module new_io

  USE stdlib_kinds, only: dp
  use stdlib_ascii, only: is_blank, CR, LF, TAB
  USE stdlib_io, only: get_file
  use stdlib_optval, only: optval
  use stdlib_str2num, only: to_num, to_num_from_stream

  implicit none

  character(len=1), parameter :: delimiter_default = " "
  character(len=1), parameter :: comment_default = "#"
  character(len=2), parameter :: nl = CR//LF

contains

  subroutine loadtxt2(filename, d, comments, delimiter, skiprows, max_rows, fast)
    !! Filename to load the array from
    character(len=*), intent(in) :: filename
    !! The array 'd' will be automatically allocated with the correct dimensions
    real(dp), allocatable, intent(out) :: d(:, :)
    !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
    integer, intent(in), optional :: skiprows
    !! Read `max_rows` lines of content after `skiprows` lines.
    !! A negative value results in reading all lines.
    !! A value of zero results in no lines to be read.
    !! The default value is -1.
    integer, intent(in), optional :: max_rows
    ! character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: comments
    character(len=1), intent(in), optional :: delimiter
    logical, intent(in), optional :: fast !< Faster version for well formatted data. Default: True
    !
    character(len=1) :: comments_
    integer :: skiprows_
    integer :: max_rows_
    character(len=1) :: delimiter_
    logical :: fast_
    !
    character(len=:), allocatable, target :: fcontents
    character(len=:), pointer :: sp
    character(len=:), pointer :: spp
    ! character(len=:), allocatable :: parsed_contents
    character(len=:), allocatable :: line
    integer :: nrows, ncols
    integer :: i
    skiprows_ = max(optval(skiprows, 0), 0)

    call get_file(filename, fcontents)
    sp => fcontents(1:)

    do i = 1, skiprows_
      call next_line(sp, line)
      if ((sp == '') .and. (line == '')) then ! Exhausted the string
        ! Should we give a warning that it is an empty array? numpy does it
        allocate (d(0, 0))
        return
      end if
    end do

    comments_ = optval(comments, comment_default)
    fast_ = optval(fast, .TRUE.)
    ! ----------------------------------------------------------------------
    ! Remove comments and empty lines
    ! This is currently the bottleneck in speed
    if (fast_) then
      call clean_contents(sp, comments_)
      ! call skip_header(sp, comments_)
      ! call skip_footer(sp, comments_)
    else
      call clean_contents_full(sp, comments_)
    end if

    ! ----------------------------------------------------------------------
    !
    spp => sp
    ! Determine number of rows
    nrows = count_lines(sp, count_empty=.False.)
    ! Determine number of colums
    delimiter_ = optval(delimiter, delimiter_default)
    line = get_first_line(sp)
    ncols = number_cols_line(line, delimiter_)

    max_rows_ = min(optval(max_rows, nrows), nrows)
    if (max_rows_ <= 0) then
      allocate (d(0, 0))
      return
    end if

    allocate (d(max_rows_, ncols))
    do i = 1, max_rows_
      call next_line(sp, line)
      call read_line_cols(line, delimiter_, ncols, d(i, :))
    end do

  end subroutine loadtxt2

  !> skip_header
  !!
  !! Fast version. Only considers whole line comments as header
  subroutine skip_header(sp, comment)
    implicit none
    character(len=:), intent(INOUT), pointer :: sp !< in/out string
    character(len=*), intent(in) :: comment !< char or string comment
    character(len=:), allocatable :: line
    !
    integer :: eol
    do
      eol = scan(sp, nl) ! find position of end of line
      line = trim(sp(:eol - 1))
      if ((line == '') .or. (index(line, comment) == 1)) then ! Empty line or commented line
        sp => sp(eol + 1:)! Remove line
      else
        return
      end if
    end do
  end subroutine skip_header  !> clean_contents
  !!
  !! Faster version. Only considers whole line comments as footer
  subroutine skip_footer(sp, comment)
    implicit none
    character(len=*), intent(INOUT) :: sp !< in/out string
    character(len=*), intent(in) :: comment !< char or string comment
    character(len=:), allocatable :: line
    integer :: bol
    !
    if (scan(sp, nl, back=.True.) == len(sp)) sp(len(sp):) = ''
    do
      bol = scan(sp, nl, back=.True.) ! find position of end of line
      line = trim(sp(bol + 1:))
      if ((line == '') .or. (index(line, comment) == 1)) then ! Empty line or commented line
        sp(bol:) = '' ! Remove line
      else
        sp(len(sp):) = new_line('a') ! Add last newline
        return
      end if
    end do
  end subroutine skip_footer

  !> clean_contents
  !!
  !! Faster version. Only considers whole line comments as header
  subroutine clean_contents(sp, comment)
    implicit none
    character(len=:), intent(INOUT), pointer :: sp !< in/out string
    character(len=*), intent(in) :: comment !< char or string comment

    call skip_header(sp, comment)
    call skip_footer(sp, comment)
  end subroutine clean_contents

  !> clean_contents
  !!
  !! Versatile version that cleans all text left of comment chars
  !! and all empty lines.
  !! As it checks all the lines it is more robust but slower
  subroutine clean_contents_full(s, comment)
    implicit none
    character(len=:), intent(INOUT), pointer :: s !< in/out string
    character(len=*) :: comment !< char or string comment
    !
    character(len=:), pointer :: sp
    integer :: eol, pos_comm
    sp => s
    do
      eol = scan(sp, nl) ! find position of end of line

      if (trim(sp(:eol - 1)) == '') then ! Empty line
        sp(:) = sp(eol + 1:)! Remove empty line
        cycle
      end if

      pos_comm = index(sp(:eol), comment) ! Find comment in line
      if (pos_comm /= 0) then
        if (trim(sp(:pos_comm - 1)) == '') then ! Empty line
          sp(1:) = sp(eol + 1:) ! Overwrite commented text
        else
          sp(pos_comm:) = sp(eol:) ! Overwrite commented text
          sp => sp(pos_comm + 1:)       ! move to next line
        end if
      else                       ! No comments
        sp => sp(eol + 1:)       ! move to next line
      end if

      if (trim(sp) == '') return     ! exhausted string
    end do
  end subroutine clean_contents_full

  !> clean_contents_string
  !!
  !! Here we create a new string. A versatile but slower version
  subroutine clean_contents_string(s, comment)
    implicit none
    character(len=:), intent(INOUT), pointer :: s !< in/out string
    character(len=*) :: comment !< char or string comment
    !
    character(len=:), pointer :: sp
    character(len=:), allocatable :: line
    character(len=:), allocatable :: sout
    integer :: pos_comm
    character(len=1), parameter :: newline = new_line('a')
    sp => s
    sout = ''
    do
      call next_line(s, line)
      if (line == '') then
        if (trim(s) == '') then
          return
        else
          cycle
        end if
      end if

      pos_comm = index(line, comment)
      if (pos_comm /= 0) then
        line = line(:pos_comm - 1)
        if (line == '') cycle
      end if
      sout = sout//line//newline
    end do
  end subroutine clean_contents_string

  !> get_next_line returns the next
  pure function get_first_line(s) result(line)
    implicit none
    character(len=*), intent(IN) :: s !< input string
    character(len=:), allocatable :: line
    line = s(:scan(s, nl) - 1) ! Line without the newline char
  end function get_first_line

  !> Return the first line in a stream and move the pointer to the next
  subroutine next_line(s, line)
    implicit none
    character(len=:), intent(INOUT), pointer :: s               ! string
    character(len=:), intent(OUT), allocatable :: line
    !
    line = get_first_line(s)
    s => s(len(line) + 2:)   ! Move the pointer after newline
  end subroutine next_line

  !> count_lines count all the lines in a string
  function count_lines(sp, count_empty) result(nlines)
    implicit none
    integer :: nlines           !< Number of lines in string
    character(len=:), intent(in), pointer :: sp               !< input string
    logical, intent(in), optional :: count_empty !< Should count empty lines? Default: True
    !
    logical :: count_empty_ ! Default: True
    character(len=:), allocatable :: line
    character(len=:), pointer :: s
    !
    s => sp
    count_empty_ = optval(count_empty, .TRUE.)
    nlines = 0
    do
      call next_line(s, line)
      if ((s == '') .and. (line == '')) return ! Exhausted the string
      if ((trim(line) /= '') .or. count_empty_) nlines = nlines + 1
    end do
  end function count_lines

  !> number_cols_line
  !!
  elemental function number_cols_line(line, delimiter) result(ncols)
    implicit none
    character(len=*), intent(in) :: line !<
    character(len=1), intent(in) :: delimiter !<
    integer :: ncols
    character(len=1) :: c
    logical :: last_delim
    integer :: i

    !! Examples:
    !!
    ncols = 0
    if (delimiter == delimiter_default) then
      last_delim = .true.
      do i = 1, len(line)
        c = line(i:i)
        if (last_delim .and. .not. is_blank(c)) ncols = ncols + 1
        last_delim = is_blank(c)
      end do
    else
      do i = 1, len(line)
        if (line(i:i) == delimiter) ncols = ncols + 1
      end do
      if (ncols == 0) then
        if (len_trim(line) /= 0) ncols = 1
      else
        ncols = ncols + 1
      end if
    end if
  end function number_cols_line

  !> read_line_cols Reads ncols cols from a line
  subroutine read_line_cols(line, delimiter, ncols, cols)
    implicit none
    character(len=*), intent(in), target :: line !<
    character(len=1), intent(in) :: delimiter !<
    integer, intent(in) :: ncols
    real(dp), intent(out) :: cols(:) !<
    integer :: i
    character(len=:), pointer :: sl !<
    integer :: p

    !! Examples:
    !!
    sl => line
    if (delimiter /= delimiter_default) then
      do i = 1, ncols
        cols(i) = to_num(sl, cols(i))
        p = scan(sl, delimiter) + 1 ! Move pointer to position after delimiter
        sl => sl(p:)
      end do
    else
      do i = 1, ncols
        cols(i) = to_num_from_stream(sl, cols(i))
      end do
    end if
  end subroutine read_line_cols

end module new_io
