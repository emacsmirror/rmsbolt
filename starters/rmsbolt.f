c     FORTRAN 77 rmsbolt starter file
c
c     Local Variables:
c     rmsbolt-command: "gfortran -O0"
c     rmsbolt-disassemble: nil
c     End:

      function isrms( a )

          implicit none

          character :: a
          logical   :: isrms

          if( a .eq. 'R' ) then
              isrms = .true.
          else if( a .eq. 'M' ) then
              isrms = .true.
          else if ( a .eq. 'S' ) then
              isrms = .true.
          else
              isrms = .false.
          end if

      end function isrms

      program main

          implicit none

          character :: a

          logical :: isrms

          a = 'A'

          if( isrms( a ) ) then
              write( *, '(A)' ) a
          end if

      end program main
