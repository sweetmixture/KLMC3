subroutine fsub() bind(C,name='ffoo')

        use iso_c_binding

        integer(c_int) :: number
        write (*,'(a)') "this is fortran subroutine"

end subroutine
