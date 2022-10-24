! Created by  on 20.10.22.

module MyGranule_type
    use, intrinsic :: iso_c_binding, only: c_int, c_double
    use constants, only: mod_my_granule
    implicit none
    private
    ! allocatable stuff

    ! this is module private, not accessible for outside
    integer(c_int) :: module_id
    type, public :: MyGranule
        integer(c_int), private::my_id
        integer(c_int), private::count
        real(c_double), private, dimension(:,:), pointer::field_mask
        contains
        procedure :: init => init_granule
        procedure :: run => run_mod
        procedure :: close => teardown
    end type MyGranule


    contains
    subroutine run_mod(this, a, b, result)
        class(MyGranule), intent(inout)::this
        real(c_double), dimension(:,:), intent(in) :: a, b
        real(c_double), dimension(:,:), intent(out)::result
        this%count = this%count + 1
        call add_masked(this, a, b, result)
    end subroutine run_mod

    subroutine init_granule(this, mask)
        ! initialize type/granule local stuff:
        !    - allocate fields used only inside granule
        !    - set constant fields that are constant within runs
        class(MyGranule), intent(inout) ::this
        real(c_double), dimension(:,:), target, intent(in)::mask
        ! is this pointer stuff actually any useful? It is definitely unsafe ...
        !        - since modification to the field from the outside of the granule will have immediate effect
        !
        this%field_mask => mask
        this%count = 0
        this%my_id = mod_my_granule
        print*, "module id ", this%my_id, ":setup"
    end subroutine init_granule

    subroutine teardown(this)
        ! deallocate local stuff, nullify pointers, do all kind of cleanup
        class(MyGranule), intent(inout):: this
        print *, "teardown module id ", this%my_id, ": run ", this%count, " times"
        nullify(this%field_mask)
        end subroutine teardown

    subroutine add_masked(this, a, b, result)
        class(MyGranule), intent(in)::this
        real(c_double), dimension(:,:), intent(in) :: a,b
        real(c_double), dimension(:,:), intent(out)::result
        result = a + b* this%field_mask
    end subroutine add_masked

    subroutine mean_square(a, b, result)
        real(c_double), dimension(:,:), intent(in) :: a,b
        real(c_double), dimension(:), intent(out)::result
        result = sum(abs(a-b)**2, 1)
        end subroutine mean_square

end module MyGranule_type