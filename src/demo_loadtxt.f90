!>\mainpage Sketchs of loadtxt for fortran_libstd

program demo_loadtxt
  USE stdlib_kinds, only: dp
  USE stdlib_io, only: loadtxt, savetxt
  USE stdlib_strings, only: to_string
  USE stdlib_math, only: linspace
  USE new_io, only: loadtxt2
  implicit none

  character(len=1), parameter :: delimiter_default = " "
  character(len=1), parameter :: comment_default = "#"

  !
  character(len=*), parameter :: ddir = "../data/"
  character(len=:), allocatable :: fname
  !
  ! ------------------------------------------------------------
  call create_data(10, 10000)
  call load_data(10)

contains

  !> load_data
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

    call cpu_time(ti)
    do i = 1, Nloop
      fname = ddir//'example'//to_string(i, "(i2.2)")//'.txt'
      call loadtxt2(fname, x, fast=.False.)
    end do
    call cpu_time(tf)
    print *, 'loadtxt2-versatile took '//to_string(tf - ti, '(g0.5)')//' sec'
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
    character(len=*), parameter :: header = "x(arb. units)     double (a.u.)           x²"
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
      !
      ! If using branch savetxt-unit use second line here
      call savetxt(fname, x)
      ! call savetxt(fname, x, header=header)
    end do

  end subroutine create_data

end program demo_loadtxt

