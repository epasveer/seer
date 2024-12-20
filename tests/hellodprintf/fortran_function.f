      subroutine FORTRANFUNCTION(count)

      implicit none
      integer :: count
      integer :: nfact = 1
      integer :: n

      ! compute factorials
      print*,  "Fortran loop"
      do n = 1, count
          nfact = nfact * n
          ! printing the value of n and its factorial
          print*,  n, " ", nfact
      end do

      return
      end
