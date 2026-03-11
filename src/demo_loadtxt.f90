!>\mainpage Sketchs of loadtxt for fortran_libstd

program demo_loadtxt
    use, intrinsic :: iso_fortran_env, only: output_unit
    USE stdlib_kinds, only: dp
    USE stdlib_io, only: loadtxt, savetxt
    USE stdlib_strings, only: to_string
    USE stdlib_math, only: linspace
    USE new_io, only: loadtxt2
    USE mod_loadtxt, only: loadtxt1 => loadtxt
    implicit none

    character(len=*), parameter :: ddir = "../data/"
    character(len=:), allocatable :: fname

    !> Format strings with edit descriptors for each type and kind
    !> ([Specification](../page/specs/stdlib_io.html))
    character(*), parameter :: &
        !> Format string for integers
        FMT_INT = '(i0)', &
        !> Format string for single precision real numbers
        FMT_REAL_SP = '(es15.8e2)', &
        !> Format string for souble precision real numbers
        FMT_REAL_DP = '(es24.16e3)', &
        !> Format string for extended double precision real numbers
        FMT_REAL_XDP = '(es26.18e3)', &
        !> Format string for quadruple precision real numbers
        FMT_REAL_QP = '(es44.35e4)', &
        !> Format string for single precision complex numbers
        FMT_COMPLEX_SP = '(es15.08e2,1x,es15.08e2)', &
        !> Format string for double precision complex numbers
        FMT_COMPLEX_DP = '(es24.16e3,1x,es24.16e3)', &
        !> Format string for extended double precision complex numbers
        FMT_COMPLEX_XDP = '(es26.18e3,1x,es26.18e3)', &
        !> Format string for quadruple precision complex numbers
        FMT_COMPLEX_QP = '(es44.35e4,1x,es44.35e4)'
    character(len=1), parameter :: delimiter_default = " "
    character(len=1), parameter :: comment_default = "#"

    !
    ! ------------------------------------------------------------
    call create_data(10, 20000)
    call load_data(10)
    ! call read_data()

contains

    !> create_data
  !!
    subroutine load_data(Nloop)
        implicit none

    !! Examples:
    !!
        integer, intent(in) :: Nloop
        real(dp), allocatable :: x(:, :)
        integer :: i
        real(dp) :: ti, tf
        !
        ! ------------------------------------------------------------
        print '(A)', "For "//to_string(Nloop, '(I2.2)')//" files:"
        call cpu_time(ti)
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            call loadtxt(fname, x, skiprows=1)
        end do
        call cpu_time(tf)
        print *, 'loadtxt took '//to_string(tf - ti, '(g0.5)')//' sec'

        call cpu_time(ti)
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            call loadtxt2(fname, x)
        end do
        call cpu_time(tf)
        print *, 'loadtxt2-fast took '//to_string(tf - ti, '(f7.5)')//' sec'

        ! call cpu_time(ti)
        ! do i = 1, Nloop
        !     fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
        !     call loadtxt2(fname, x, fast=.False.)
        ! end do
        ! call cpu_time(tf)
        ! print *, 'loadtxt2-versatile took '//to_string(tf - ti, '(g0.5)')//' sec'

        call cpu_time(ti)
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            call loadtxt1(fname, x)
        end do
        call cpu_time(tf)
        print *, 'loadtxt1 took '//to_string(tf - ti, '(g0.5)')//' sec'
        print "(A)", "Each file has "//to_string(size(x))//" values"
    end subroutine load_data

    !> create_data
  !!
    subroutine create_data(Nloop, Ndat)
        implicit none

    !! Examples:
    !!
        integer, intent(in) :: Nloop
        integer, intent(in) :: Ndat
        real(dp) :: x(Ndat, 3)
        real(dp) :: slim = 10._dp             !límite superior
        character(len=*), parameter :: header = "x(arb. units)     double (a.u.)           x²" !< Encabezado de los datos
        real(dp), parameter :: ilim = 0.5_dp !límite inferior
        integer :: i
        !
        ! ------------------------------------------------------------
        do i = 1, Nloop
            fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
            x(:, 1) = linspace(i * ilim, i * slim, Ndat)
            x(:, 2) = x(:, 1) * 2
            x(:, 3) = x(:, 1)**2 / 5
            ! ------------------------------------------------------------
            ! Guardamos los datos
            call savetxt(fname, x, header=header, fmt='es23.16e3')
        end do

    end subroutine create_data

    !   !> read_data
    ! !!
    !   subroutine read_data()
    !       implicit none
    !       ! Cambiar acá para los valores que nos gusten
    !       character(len=:), allocatable :: fmt
    !       integer :: i, ndata(2)
    !       real(dp), allocatable :: y(:, :)

    !   !! Examples:
    !   !!
    !       fmt = "(3("//FMT_REAL_DP//",1x))"

    !       fname = ddir//'example.txt'
    !       call loadtxt2(fname, y)
    !       ndata = shape(y)
    !       ! print *, ndata
    !       do i = 1, ndata(1)
    !           print fmt, y(i, :)
    !       end do
    !   end subroutine read_data

end program demo_loadtxt

