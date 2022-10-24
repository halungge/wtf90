! Created by  on 21.10.22.

module MyGranule_facade
    use, intrinsic :: iso_c_binding, only: c_double, c_int
    use MyGranule_type, only: MyGranule
    implicit none

    type(MyGranule), private::my_granule

    contains
    subroutine setup_mygranule(mask)
        real(c_double), target, dimension(:,:), intent(in)::mask
        call my_granule%init(mask)
    end subroutine setup_mygranule

    subroutine run_mygranule(in1, in2, result)
        real(c_double), dimension(:,:), intent(in) :: in1, in2
        real(c_double), dimension(:,:), intent(out) :: result
        call my_granule%run(in1, in2, result)
    end subroutine run_mygranule

    subroutine teardown_mygranule()
        call my_granule%close()
    end subroutine teardown_mygranule

end module MyGranule_facade