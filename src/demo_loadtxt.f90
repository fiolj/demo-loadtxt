!> Demo use of loadtxt for fortran_stdlib

program demo_loadtxt
    USE stdlib_kinds, only: dp
    USE stdlib_io, only: loadtxt, savetxt
    USE stdlib_strings, only: to_string
    USE stdlib_math, only: linspace
    USE mod_loadtxt, only: loadtxt1 => loadtxt
    implicit none
    !
    character(len=*), parameter :: ddir = "../data/"
    character(len=:), allocatable :: fname
    !
    ! ------------------------------------------------------------
    call create_data(10, 20000)
    call load_data(10)

contains

    subroutine load_data(Nloop)
        implicit none
        integer, intent(in) :: Nloop
        real(dp), allocatable :: x(:, :)
        integer :: i
        real(dp) :: ti, tf, t1, t2
        !
        ! ------------------------------------------------------------
        print '(A)', "For "//to_string(Nloop, '(I2.2)')//" files:"
        call cpu_time(ti)
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            call loadtxt(fname, x, skiprows=1)
        end do
        call cpu_time(tf)
        t1 = tf - ti
        print '(a)', 'Original loadtxt took '//to_string(t1, '(f7.5)')//' sec'

        call cpu_time(ti)
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            call loadtxt1(fname, x)
        end do
        call cpu_time(tf)
        t2 = tf - ti
        print '(a)', 'loadtxt - stream took '//to_string(t2, '(f7.5)')//' sec'
        print '(a)', "Speed-up factor = "//to_string(t1 / t2, '(g0.4)')
        print "(A)", "Each file has "//to_string(size(x))//" values"
        ! Let's save the last file
        ! fname = ddir//'example'//to_string(i, "(i2.2)")//'.dat'
        ! print *, fname
        ! call savetxt(fname, x, fmt='es23.16e3')

    end subroutine load_data

    subroutine create_data(Nloop, Ndat)
        implicit none

        integer, intent(in) :: Nloop
        integer, intent(in) :: Ndat
        real(dp) :: x(Ndat, 3)
        real(dp) :: slim = 10._dp             ! upper limit
        character(len=*), parameter :: header = "x(arb. units)     double (a.u.)           x²" !< header
        real(dp), parameter :: ilim = 0.5_dp  ! lower limit
        integer :: i
        !
        ! ------------------------------------------------------------
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            x(:, 1) = linspace(i * ilim, i * slim, Ndat)
            x(:, 2) = x(:, 1) * 2
            x(:, 3) = x(:, 1)**2 / 2
            ! ------------------------------------------------------------
            ! Save the data
            call savetxt(fname, x, header=header, fmt='es23.16e3')
        end do

    end subroutine create_data

end program demo_loadtxt

