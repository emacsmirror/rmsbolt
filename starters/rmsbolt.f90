! Fortran 90/95/2003/2008/2018 starter file
!
! Local Variables:
! rmsbolt-command: "gfortran -O0"
! rmsbolt-disassemble: nil
! End:

program main

    implicit none

    character :: a

    a = 'A'

    if( isrms( a ) ) then
        write( *, '(A)' ) a
    end if

contains

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

end program main
