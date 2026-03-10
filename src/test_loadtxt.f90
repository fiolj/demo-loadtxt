!> test_loadtxt
program test_loadtxt

    use mod_loadtxt, only: loadtxt
    use stdlib_kinds, only: dp
    USE stdlib_strings, only: to_string

    implicit none

    character(len=*), parameter :: ddir = "../data/"
    real(dp), allocatable :: d(:, :)
    integer :: i
    character(len=12) :: fname

    ! Test several possible data files with blanks
    do i = 1, 5
        fname = "example"//to_string(i, 'i1')//".dat"
        print *, '-----------------------------------'
        print *, "Testing loadtxt with "//fname
        if (allocated(d)) deallocate (d)
        call loadtxt(ddir//fname, d)
        if (.not. allocated(d)) error stop 'loadtxt did not allocate output'
        if (size(d, 1) /= 10) error stop 'unexpected number of rows'
        if (size(d, 2) /= 3) error stop 'unexpected number of columns'
    end do

    ! Test several possible data files with delimiters
    do i = 1, 2
        fname = "example"//to_string(i, 'i1')//".txt"
        print *, '-----------------------------------'
        print *, "Testing loadtxt with "//fname
        if (allocated(d)) deallocate (d)
        call loadtxt(ddir//fname, d, delimiter=',')
        if (.not. allocated(d)) error stop 'loadtxt did not allocate output'
        if (size(d, 1) /= 10) error stop 'unexpected number of rows'
        if (size(d, 2) /= 3) error stop 'unexpected number of columns'
    end do

    ! Test data file with overlapping comments (,,) and delimiters (,)
    fname = "example3.txt"
    print *, '-----------------------------------'
    print *, "Testing loadtxt with "//fname
    if (allocated(d)) deallocate (d)
    call loadtxt(ddir//fname, d, delimiter=',')
    if (.not. allocated(d)) error stop 'loadtxt did not allocate output'
    if (size(d, 1) /= 10) error stop 'unexpected number of rows'
    if (size(d, 2) /= 3) error stop 'unexpected number of columns'

end program test_loadtxt
