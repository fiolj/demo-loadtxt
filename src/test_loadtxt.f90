!> test_loadtxt
program test_loadtxt

    use mod_loadtxt, only: loadtxt
    use stdlib_kinds, only: dp
    USE stdlib_strings, only: to_string

    implicit none

    character(len=*), parameter :: ddir = "../data/"
    real(dp), allocatable :: d(:, :)
    real(dp), allocatable :: d1(:, :)
    integer :: i
    integer :: nrows, ncols
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
    print *, "Testing "//fname//" loadtxt with overlapping comments (,,) and delimiters (,)"
    if (allocated(d)) deallocate (d)
    call loadtxt(ddir//fname, d, delimiter=',', comments=',,')
    nrows = size(d, 1)
    ncols = size(d, 2)

    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 3) error stop 'unexpected number of columns'
    print *, "loadtxt  with overlapping comments (,,) and delimiters (,) test passed"

    ! Test max_rows option
    fname = "example1.dat"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with max_rows option"
    if (allocated(d)) deallocate (d)
    if (allocated(d1)) deallocate (d1)
    call loadtxt(ddir//fname, d1)
    call loadtxt(ddir//fname, d, max_rows=3)
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 3) error stop 'unexpected number of rows'
    if (ncols /= 3) error stop 'unexpected number of columns'
    if (any(d(:, :) /= d1(1:3, :))) error stop 'reading with max_rows'

    print *, "loadtxt with max_rows option test passed"

    ! Test use_cols option with default delimiter
    fname = "example1.dat"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with usecols with whitespace delimiter"
    if (allocated(d)) deallocate (d)
    if (allocated(d1)) deallocate (d1)
    call loadtxt(ddir//fname, d1)
    call loadtxt(ddir//fname, d, usecols=[3, 1])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 2) error stop 'unexpected number of columns'
    ! Check that effectively we are getting the right columns
    if (any(abs(d(:, 1) - d1(:, 3)) > 1.e-7) .or. any(abs(d(:, 2) - d1(:, 1)) > 1.e-7)) error stop 'reading wrong columns'
    print *, "loadtxt with usecols option test passed"
    if (allocated(d1)) deallocate (d1)
    if (allocated(d)) deallocate (d)

    ! Test data file with usecols and missing columns
    fname = "example4.txt"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with usecols and missing values"
    call loadtxt(ddir//fname, d1, delimiter=',')
    call loadtxt(ddir//fname, d, delimiter=',', usecols=[3, 1])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 2) error stop 'unexpected number of columns'
    ! Check that effectively we are getting the right columns
    if (any(abs(d(:, 1) - d1(:, 3)) > 1.e-7) .or. any(abs(d(:, 2) - d1(:, 1)) > 1.e-7)) error stop 'reading wrong columns'
    print *, "loadtxt test with usecols and missing cols passed"
    if (allocated(d1)) deallocate (d1)
    if (allocated(d)) deallocate (d)

    ! Test data file with usecols and missing columns for default delimiter
    fname = "example6.dat"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with usecols and missing values with whitespace delimiter"
    call loadtxt(ddir//"example1.dat", d1)
    call loadtxt(ddir//fname, d, usecols=[2, 1])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 2) error stop 'unexpected number of columns'
    ! Check that effectively we are getting the right columns
    if (any(abs(d(:, 1) - d1(:, 2)) > 1.e-7) .or. any(abs(d(:, 2) - d1(:, 1)) > 1.e-7)) error stop 'reading wrong columns'
    print *, "loadtxt test with usecols and missing values passed"

    ! do i = 1, 10
    !     print *, d(i, :)
    ! end do
    if (allocated(d1)) deallocate (d1)
    if (allocated(d)) deallocate (d)
end program test_loadtxt
