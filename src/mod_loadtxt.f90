module mod_loadtxt

    use stdlib_kinds, only: int8, dp
    use stdlib_ascii, only: is_blank, CR, LF, TAB
    use stdlib_strings, only: starts_with
    use stdlib_optval, only: optval
    use stdlib_str2num, only: to_num_from_stream

    implicit none
    private
    public :: loadtxt

    character(len=*), parameter :: delimiter_default = "BLANK"
    character(len=1), parameter :: comment_default = "#"
    character(len=2), parameter :: nl = CR//LF
    character(len=*), parameter :: blanks = " "//TAB

contains

    subroutine loadtxt(filename, d, comments, delimiter, skiplines, max_rows, usecols)
        character(*), intent(in)   :: filename !! Filename to load the array from
        real(dp), allocatable, intent(out) :: d(:, :) !! The array 'd' will be automatically allocated with the correct dimensions
        integer, intent(in), optional :: skiplines !! Skip the first `skiplines` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
        integer, intent(in), optional :: max_rows !! Read `max_rows` lines of content after `skiplines` lines. A negative value results in reading all lines. A value of zero results in no lines to be read. The default is to read all rows.
        character(len=*), intent(in), optional :: comments !! from comments symbol until line end everything else will be ignored. The default is '#'.
        character(len=*), intent(in), optional :: delimiter !! Character used to separate values in a line. The default is any number of spaces and or tabs.
        integer, intent(in), optional :: usecols(:) !! Array of column indices to read. If not provided, all columns are read.
        !-----------------------------------------------------------------------------
        integer :: skiplines_, max_rows_
        integer :: u, fsze, nrows, nrows_effective, ncols, j, start_effective
        character(:), allocatable, target :: ff
        character(len=:), pointer :: ffp
        integer :: line_start, line_end
        character(len=:), allocatable :: delim_
        character(len=:), allocatable :: comment_
        integer :: row, row_effective, err
        real(dp) :: val
        real(dp), allocatable :: cols(:)
        integer, allocatable ::  usecols_(:)
        !=============================================================================
        ! err = 1
        comment_ = optval(comments, comment_default)
        delim_ = optval(delimiter, delimiter_default)
        skiplines_ = optval(skiplines, 0)
        ! max_rows will be set later, after determining number of rows
        !----------------------------------------- Load file in a single string
        open (newunit=u, file=filename, status='old', access='stream', action="read", iostat=err)
        if (err /= 0) then
            allocate (d(0, 0))
            return
        end if
        inquire (unit=u, size=fsze)
        allocate (character(fsze) :: ff)
        read (u) ff
        close (u)
        ffp => ff
        start_effective = 1     ! Start after skiplines
        !----------------------------------------- Count lines and columns
        nrows = 0               ! Total number of rows (including empty and commented lines)
        nrows_effective = 0     ! rows with data
        ncols = 0
        do while (len(ffp) > 0)
            line_end = shift_to_eol(ffp)
            if (line_end > len(ffp)) exit  ! No more lines
            line_start = shift_to_nonwhitespace(ffp(:line_end)) ! Skip initial blanks in line
            nrows = nrows + 1
            if (nrows <= skiplines_) then
                start_effective = start_effective + line_end ! Remember position to use aS starting point when reading
                ffp => ffp(line_end + 1:) ! Skip the line
                cycle
            end if

            if (starts_with(ffp(line_start:), comment_) .or. &
                (line_start == line_end)) then
                ffp => ffp(line_end + 1:) ! Skip comment lines and blank lines
                cycle
            end if
            nrows_effective = nrows_effective + 1
            !
            ! if ncols is not set yet, determine the number of columns
            if (ncols == 0) ncols = number_cols_line(ffp(line_start:line_end), delim_, comment_)
            ffp => ffp(line_end + 1:) ! go to next line
        end do

        !----------------------------------------- Allocate and read data
        max_rows_ = min(optval(max_rows, nrows_effective), nrows_effective)
        ! If there is no data we will return an empty array
        if ((max_rows_ <= 0) .or. (ncols == 0)) then
            allocate (d(0, 0))
            return
        end if

        if (present(usecols)) then
            usecols_ = usecols
        else
            usecols_ = [(j, j=1, ncols)]
        end if
        allocate (d(max_rows_, size(usecols_)))
        allocate (cols(ncols))  ! Used to hold each row

        row_effective = 0
        ffp => ff(start_effective:) ! Reset pointer to the beginning of the file after skiplines
        nrows = nrows - skiplines_

        do row = 1, nrows
            line_end = shift_to_eol(ffp)
            line_start = shift_to_nonwhitespace(ffp(:line_end)) ! Avoid initial blanks in line

            if (starts_with(ffp(line_start:), comment_) .or. &
                (line_start == line_end)) then
                ffp => ffp(line_end + 1:) ! Skip comment lines and blank lines
                cycle
            end if

            row_effective = row_effective + 1
            do j = 1, ncols     ! Read a row
                val = to_num_from_stream(ffp, val)
                if (in_delim(ffp, delim_)) then
                    ffp => ffp(shift_to_nondelim(ffp, delim_):)
                end if
                cols(j) = val
                if (scan(ffp(1:1), nl) /= 0) then ! If EOL => no more cols
                    exit
                end if
            end do
            ! Copy the columns of the current row to d(row_effective,:)
            do j = 1, size(usecols_)
                d(row_effective, j) = cols(usecols_(j))
            end do

            if (row_effective >= max_rows_) return
            line_end = shift_to_eol(ffp)
            ffp => ffp(line_end + 1:)
        end do

    end subroutine

    !>  Determine the number of columns from the numerical line
    !! by counting the number of delimiters + 1
    function number_cols_line(row, delimiter, comment) result(ncols)
        implicit none
        character(len=*), intent(in) :: row !<
        character(len=*), intent(in) :: delimiter !<
        character(len=*), intent(in) :: comment !<
        character(len=:), allocatable :: line
        integer :: ncols
        integer :: pos, p
        ncols = 0
        pos = index(row, comment)
        if (pos == 0) pos = len(row)
        line = trim(adjustl(row(:pos))) ! Line with no blanks around and no comments
        pos = 1
        do
            p = shift_after_next_delim(line(pos:), delimiter)
            pos = pos + p - 1 ! Find delimiter
            if (pos >= len(line)) exit
            ncols = ncols + 1
        end do
        ncols = ncols + 1
    end function number_cols_line

    elemental function in_delim(s, delim) result(m)
        !! Check if current position is the init of a delimiter
        character(len=*), intent(in) :: s !! character chain
        character(len=*), intent(in) :: delim !! character chain
        logical :: m !! True or False
        !----------------------------------------------
        if (delim == delimiter_default) then
            m = (scan(s(1:1), blanks) /= 0)
        else
            m = starts_with(s(shift_to_nonwhitespace(s):), delim)
        end if
    end function in_delim

    elemental function shift_to_eol(s) result(p)
        !! move string to position of the next end-of-line character
        character(len=*), intent(in) :: s !! character chain
        integer :: p !! position
        !----------------------------------------------
        p = scan(s, nl)
        if (p < len(s)) then ! If CRLF, move to LF
            if (s(p:p + 1) == nl) p = p + 1
        end if

    end function shift_to_eol

    function shift_after_next_delim(s, delim) result(p)
      !! move string to position of the next non delimiter character
        character(len=*), intent(in) :: s !! character chain
        character(len=*), intent(in) :: delim !! character chain
        integer :: p !! position
        !----------------------------------------------
        if (delim == delimiter_default) then
            p = 1
            if (.not. is_blank(s(p:p))) p = shift_to_whitespace(s)
            p = p + shift_to_nonwhitespace(s(p:)) - 1
        else
            p = index(s, delim)
            if (p == 0) then
                p = len(s)
            else
                p = p + len(delim)
            end if
        end if
        if (p > len(s)) p = len(s)
    end function shift_after_next_delim

    elemental function shift_to_nondelim(s, delim) result(p)
      !! move string to position of the next non delimiter character
      !! Assumes that it is in a delim
        character(len=*), intent(in) :: s !! character chain
        character(len=*), intent(in) :: delim !! character chain
        integer :: p !! position
        !----------------------------------------------
        if (delim == delimiter_default) then
            p = shift_to_nonwhitespace(s)
        else
            ! Check first if we are at the beginning of a delimiter
            p = index(s, delim)
            if (p == 0) then
                p = len(s)
            else
                p = p + len(delim)
                p = p + shift_to_nonwhitespace(s(p:)) - 1 ! Extra-spaces make to_num_from_stream fail
                if (p > len(s)) p = len(s)
            end if
        end if

    end function shift_to_nondelim

    elemental function shift_to_nonwhitespace(s) result(p)
    !! move string to position of the next non white space character
        character(len=*), intent(in) :: s !! character chain
        integer :: p !! position
        !----------------------------------------------
        ! p = verify(s, blanks//nl)
        p = verify(s, blanks)
        if (p == 0) p = len(s)
    end function shift_to_nonwhitespace

    elemental function shift_to_whitespace(s) result(p)
    !! move string to position of the next white space character
        character(len=*), intent(in) :: s !! character chain
        integer :: p !! position
        !----------------------------------------------
        p = scan(s, blanks)
        if (p == 0) p = len(s)
    end function shift_to_whitespace

end module mod_loadtxt

