!> test_loadtxt
program test_loadtxt

    use mod_loadtxt, only: loadtxt
    use stdlib_kinds, only: dp
    USE stdlib_strings, only: to_string

    implicit none

    character(len=*), parameter :: ddir = "../data/"
    real(dp), allocatable :: d(:, :), d1(:, :)
    integer :: i, nrows, ncols
    character(len=12) :: fname

    ! Test several possible data files with blanks
    do i = 1, 5
        fname = "example"//to_string(i, 'i1')//".dat"
        print *, '-----------------------------------'
        print *, "Testing loadtxt with "//fname
        if (allocated(d)) deallocate (d)
        call loadtxt(ddir//fname, d)
        nrows = size(d, 1)
        ncols = size(d, 2)
        if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
        if (nrows /= 10) error stop 'unexpected number of rows'
        if (ncols /= 3) error stop 'unexpected number of columns'
        print *, "loadtxt with "//fname//" test passed"
    end do

    ! Test several possible data files with delimiters
    do i = 1, 2
        fname = "example"//to_string(i, 'i1')//".txt"
        print *, '-----------------------------------'
        print *, "Testing loadtxt with "//fname
        if (allocated(d)) deallocate (d)
        call loadtxt(ddir//fname, d, delimiter=',')
        nrows = size(d, 1)
        ncols = size(d, 2)
        if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
        if (nrows /= 10) error stop 'unexpected number of rows'
        if (ncols /= 3) error stop 'unexpected number of columns'
        print *, "loadtxt with "//fname//" test passed"
    end do

    ! Test data file with overlapping comments (,,) and delimiters (,)
    fname = "example3.txt"
    print *, '-----------------------------------'
    print *, "Testing loadtxt with "//fname
    if (allocated(d)) deallocate (d)
    call loadtxt(ddir//fname, d, delimiter=',', comments=',,')
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 3) error stop 'unexpected number of columns'
    print *, "loadtxt with "//fname//" test passed"

    ! Test max_rows option
    fname = "example1.dat"
    print *, '-----------------------------------'
    print *, "Testing loadtxt with "//fname
    if (allocated(d)) deallocate (d)
    call loadtxt(ddir//fname, d1)
    call loadtxt(ddir//fname, d, max_rows=3)
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 3) error stop 'unexpected number of rows'
    if (ncols /= 3) error stop 'unexpected number of columns'
    if (any(d(:, :) /= d1(1:3, :))) error stop 'reading with max_rows'

    print *, "loadtxt with max_rows option test passed"

    ! Test use_cols option
    fname = "example1.dat"
    print *, '-----------------------------------'
    print *, "Testing loadtxt with "//fname
    if (allocated(d)) deallocate (d)
    call loadtxt(ddir//fname, d1)
    call loadtxt(ddir//fname, d, usecols=[3, 1])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 2) error stop 'unexpected number of columns'
    if (any(d(:, 1) /= d1(:, 3)) .or. any(d(:, 2) /= d1(:, 1))) error stop 'reading with usecols'

    print *, "loadtxt with usecols option test passed"

end program test_loadtxt
