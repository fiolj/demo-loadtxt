!> test_loadtxt
program test_loadtxt

    use mod_loadtxt, only: loadtxt
    use stdlib_kinds, only: dp
    USE stdlib_strings, only: to_string

    implicit none

    character(len=*), parameter :: ddir = "../data/"
    real(dp), allocatable :: d(:, :)
    integer :: i, j
    integer :: nrows, ncols
    character(len=12) :: fname
    real(dp), parameter :: eps = 1.e-5_dp
    real(dp) :: d1(10, 3)
    d1 = reshape([5.0000000000000000e-1_dp, 1.0000000000000000e+0_dp, 5.0000000000000000e-2_dp, &
      & 1.5555555555555556e+0_dp, 3.1111111111111112e+0_dp, 4.8395061728395061e-1_dp,&
      & 2.6111111111111112e+0_dp, 5.2222222222222223e+0_dp, 1.3635802469135803e+0_dp,&
      & 3.6666666666666670e+0_dp, 7.3333333333333339e+0_dp, 2.6888888888888891e+0_dp,&
      & 4.7222222222222223e+0_dp, 9.4444444444444446e+0_dp, 4.4598765432098766e+0_dp,&
      & 5.7777777777777777e+0_dp, 1.1555555555555555e+1_dp, 6.6765432098765434e+0_dp,&
      & 6.8333333333333339e+0_dp, 1.3666666666666668e+1_dp, 9.3388888888888903e+0_dp,&
      & 7.8888888888888893e+0_dp, 1.5777777777777779e+1_dp, 1.2446913580246916e+1_dp,&
      & 8.9444444444444446e+0_dp, 1.7888888888888889e+1_dp, 1.6000617283950618e+1_dp,&
      & 1.0000000000000000e+1_dp, 2.0000000000000000e+1_dp, 2.0000000000000000e+1_dp], shape(d1), order=[2, 1])

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
        if (any(abs(d - d1) > eps)) then
            do j = 1, nrows
                print *, d(j, :)
            end do
            error stop 'reading table'
        end if
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
        if (any(abs(d - d1) > eps)) then
            do j = 1, nrows
                print *, d(j, :)
            end do
            error stop 'reading table'
        end if
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
    if (any(abs(d - d1) > eps)) then
        do j = 1, nrows
            print *, d(j, :)
        end do
        error stop 'reading table'
    end if
    print *, "loadtxt  with overlapping comments (,,) and delimiters (,) test passed"

    ! Test max_rows option
    fname = "example1.dat"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with max_rows option"
    if (allocated(d)) deallocate (d)
    call loadtxt(ddir//fname, d, max_rows=3)
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 3) error stop 'unexpected number of rows'
    if (ncols /= 3) error stop 'unexpected number of columns'
    if (any(abs(d(:, :) - d1(1:3, :)) > eps)) then
        do j = 1, nrows
            print *, d(j, :)
        end do
        error stop 'reading table with option max_rows'
    end if
    print *, "loadtxt with max_rows option test passed"

    ! Test use_cols option with default delimiter
    fname = "example1.dat"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with usecols with whitespace delimiter"
    call loadtxt(ddir//fname, d, usecols=[3, 1, 2, 3])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 4) error stop 'unexpected number of columns'
    if (any(abs(d(:, 1) - d1(:, 3)) > eps) .or. any(abs(d(:, 2) - d1(:, 1)) > eps) &
        & .or. any(abs(d(:, 3) - d1(:, 2)) > eps) .or. any(abs(d(:, 4) - d1(:, 3)) > eps)) then
        do j = 1, nrows
            print *, d(j, :)
        end do
        error stop 'reading table with option usecols and whitespace delimiter'
    end if
    print *, "loadtxt with usecols option test passed"
    if (allocated(d)) deallocate (d)

    ! Test data file with usecols and missing columns
    fname = "example4.txt"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with usecols and missing values"
    call loadtxt(ddir//fname, d, delimiter=',', usecols=[3, 1])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 2) error stop 'unexpected number of columns'
    if (any(abs(d(:, 1) - d1(:, 3)) > eps) .or. any(abs(d(:, 2) - d1(:, 1)) > eps)) then
        do j = 1, nrows
            print *, d(j, :)
        end do
        error stop 'reading table with option usecols and whitespace delimiter'
    end if
    print *, "loadtxt test with usecols and missing cols passed"
    if (allocated(d)) deallocate (d)

    ! Test data file with usecols and missing columns for default delimiter
    fname = "example6.dat"
    print *, '-----------------------------------'
    print *, "Testing "//fname//" loadtxt with usecols and missing values with whitespace delimiter"
    call loadtxt(ddir//fname, d, usecols=[2, 1])
    nrows = size(d, 1)
    ncols = size(d, 2)
    if (ncols == 0 .or. nrows == 0) error stop 'loadtxt did not read data'
    if (nrows /= 10) error stop 'unexpected number of rows'
    if (ncols /= 2) error stop 'unexpected number of columns'
    ! Check that effectively we are getting the right columns
    if (any(abs(d(:, 1) - d1(:, 2)) > eps) .or. any(abs(d(:, 2) - d1(:, 1)) > eps)) then
        do j = 1, nrows
            print *, d(j, :)
        end do
        error stop 'reading table with option usecols and whitespace delimiter'
    end if
    print *, "loadtxt test with usecols and missing cols passed"
    if (allocated(d)) deallocate (d)
end program test_loadtxt
