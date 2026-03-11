module mod_loadtxt

    use stdlib_kinds, only: int8, dp
    !JF: new
    ! use stdlib_ascii, only: is_blank
    use stdlib_ascii, only: is_blank, CR, LF, TAB
    use stdlib_strings, only: starts_with
    use stdlib_optval, only: optval
    use stdlib_str2num, only: to_num, to_num_from_stream

    implicit none
    private
    public :: loadtxt

    character(len=1), parameter :: delimiter_default = " "
    character(len=1), parameter :: comment_default = "#"
    ! JF: new for comparing directly chars instead of integers
    integer(int8), parameter :: iLF = 10, iCR = 13
    character(len=2), parameter :: nl = CR//LF
    character(len=*), parameter :: blanks = " "//TAB

contains

    subroutine loadtxt(filename, d, comments, delimiter, skiprows, max_rows, usecols)
        character(*), intent(in)   :: filename !! Filename to load the array from
        real(dp), allocatable, intent(out) :: d(:, :) !! The array 'd' will be automatically allocated with the correct dimensions
        integer, intent(in), optional :: skiprows !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
        integer, intent(in), optional :: max_rows !! Read `max_rows` lines of content after `skiprows` lines. A negative value results in reading all lines. A value of zero results in no lines to be read. The default value is -1.
        character(len=*), intent(in), optional :: comments !! from comments symbol until line end everything else will be ignored. The default is '#'.
        character(len=*), intent(in), optional :: delimiter !! Character used to separate values in a line. The default is a space.
        integer, intent(in), optional :: usecols(:) !! Array of column indices to read. If not provided, all columns are read.
        !-----------------------------------------------------------------------------
        integer :: skiprows_, max_rows_
        integer :: u, fsze, nrows, nrows_effective, ncols, j, start_effective
        character(:), allocatable, target :: ff
        character(len=:), pointer :: ffp
        integer :: line_start, line_end
        ! integer :: pos, line_start, line_end
        character(len=:), allocatable :: delim_
        character(len=:), allocatable :: comment_
        integer :: row, row_effective, err
        integer :: len_comment, len_delim
        real(dp) :: val
        !=============================================================================
        err = 1
        comment_ = optval(comments, comment_default)
        len_comment = len(comment_)
        delim_ = optval(delimiter, delimiter_default)
        len_delim = len(delim_)
        skiprows_ = optval(skiprows, 0)
        max_rows_ = optval(max_rows, -1)
        !----------------------------------------- Load file in a single string
        open (newunit=u, file=filename, status='old', access='stream', action="read", iostat=err)
        if (err /= 0) return
        inquire (unit=u, size=fsze)
        allocate (character(fsze) :: ff)
        read (u) ff
        close (u)
        ffp => ff
        start_effective = 1              ! Used after skiprows (is it worth?)
        !----------------------------------------- Count lines and columns
        nrows = 0; nrows_effective = 0
        ncols = 0
        do while (len(ffp) > 0)
            line_end = shift_to_eol(ffp)
            if (line_end > len(ffp)) exit  ! No more lines
            line_start = verify(ffp(:line_end), blanks) ! Skip initial blanks in line

            nrows = nrows + 1
            if (nrows <= skiprows_) then
                start_effective = start_effective + line_end ! JF: Remember this position in order to not repeat when reading
                ffp => ffp(line_end + 1:) ! Skip the line
                cycle
            end if
            ! JF: Encapsulated conditions for easier modification
            if (should_skip_line(ffp, line_start, line_end)) then
                ffp => ffp(line_end + 1:) ! Skip comment lines and blank lines
                cycle
            end if
            nrows_effective = nrows_effective + 1
            ! if ncols is not set yet, determine the number of columns from the first numerical line by counting the number of delimiters+1
            ! JF: Here I think we should add possibility for:
            ! 1. blank delimiters that could include one or more of (space, tab)
            ! 2. multi-character comments
            ! 3. blanks at beginning or end of line (when blanks are delimiters)
            if (ncols == 0) then
                ncols = number_cols_line(ffp(line_start:line_end), delim_, comment_)
            end if

            ffp => ffp(line_end + 1:)
        end do

        if (ncols == 0 .or. nrows_effective == 0) return
        !----------------------------------------- Allocate and read data
        if (.not. present(usecols)) then
            allocate (d(nrows_effective, ncols))
        else
            allocate (d(nrows_effective, size(usecols)))
        end if

        row_effective = 0
        ! ffp => ff ! Reset pointer to the beginning of the file content
        ffp => ff(start_effective:) ! Reset pointer to the beginning of the file after skiprows
        nrows = nrows - skiprows_
        do row = 1, nrows
            ! line_start = 1
            line_end = shift_to_eol(ffp)
            line_start = verify(ffp(:line_end), blanks) ! Avoid initial blanks in line
            if (should_skip_line(ffp, line_start, line_end)) then
                ffp => ffp(line_end + 1:) ! Skip comment lines and blank lines
                cycle
            end if

            row_effective = row_effective + 1
            if (.not. present(usecols)) then
                do j = 1, ncols
                    val = to_num_from_stream(ffp, val)
                    if (starts_with(ffp, delim_)) ffp => ffp(len(delim_) + 1:)
                    d(row_effective, j) = val
                end do
            else
                do j = 1, ncols
                    val = to_num_from_stream(ffp, val)
                    if (starts_with(ffp, delim_)) ffp => ffp(len(delim_) + 1:)
                    if (any(usecols == j)) d(row_effective, j) = val
                end do
            end if
            line_end = shift_to_eol(ffp)
            ffp => ffp(line_end + 1:)
        end do

        err = 0
    contains

        ! JF: Here I think we could modify it to accept:
        ! 1. multi-character comments
        ! 2. lines starting with blanks (tabs or spaces) but are not empty
        ! 3. empty lines that contain blanks
        pure function should_skip_line(ffp, line_start, line_end) result(skip)
            character(len=:), intent(inout), pointer :: ffp
            integer, intent(in) :: line_start, line_end

            logical :: skip
            skip = (ffp(line_start:line_start + len_comment - 1) == comment_ .or. &
                    (line_start == line_end))
        end function should_skip_line

    end subroutine

    ! JF: Changed to use intrinsic procedure scan().
    ! Tested with gfortran: it seems to be slightly faster,
    ! but also is simpler and leans on potential improvements on intrinsic
    !
    ! elemental function shift_to_eol(s) result(p)
    !     !! move string to position of the next end-of-line character
    !     character(*), intent(in) :: s !! character chain
    !     integer :: p !! position
    !     !----------------------------------------------
    !     p = 1
    !     do while (p < len(s) .and. .not. (iachar(s(p:p)) == iLF .or. iachar(s(p:p)) == iCR))
    !         p = p + 1
    !     end do
    !     ! If CRLF, move to LF
    !     if (p < len(s)) then
    !         if (iachar(s(p:p)) == iCR .and. iachar(s(p + 1:p + 1)) == iLF) then
    !             p = p + 1
    !         end if
    !     end if
    ! end function

    elemental function shift_to_eol(s) result(p)
        !! move string to position of the next end-of-line character
        character(*), intent(in) :: s !! character chain
        integer :: p !! position
        !----------------------------------------------
        p = scan(s, nl)
        ! If CRLF, move to LF
        if ((p < len(s)) .and. (s(p:p + 1) == nl)) p = p + 1
    end function shift_to_eol

    !> number_cols_line
  !!
    ! elemental function number_cols_line(ffp, delimiter, comment) result(ncols)
    function number_cols_line(ffp, delimiter, comment) result(ncols)
        implicit none
        character(len=*), intent(in) :: ffp !<
        character(len=*), intent(in) :: delimiter !<
        character(len=*), intent(in) :: comment !<
        integer :: ncols
        logical :: last_delim
        integer :: pos, len_comment

        ncols = 0
        len_comment = len(comment)
        if (delimiter == delimiter_default) then
            last_delim = .true.
            do pos = 1, len(ffp) - 1
                if (ffp(pos:pos + len_comment - 1) == comment) then
                    return
                else if (last_delim .and. .not. is_blank(ffp(pos:pos))) then
                    ncols = ncols + 1
                end if
                last_delim = is_blank(ffp(pos:pos))
            end do
        else
            do pos = 1, len(ffp) - 1
                if (ffp(pos:pos + len_comment - 1) == comment) then
                    return
                else if (starts_with(ffp(pos:), delimiter)) then
                    ncols = ncols + 1
                end if
            end do
            ncols = ncols + 1
        end if
    end function number_cols_line

end module mod_loadtxt

