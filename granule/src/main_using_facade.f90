! Created by  on 21.10.22.

program main_using_facade
     use, intrinsic :: iso_c_binding, only: c_int, c_double
     use constants, only: idim, jdim
     ! use only the facade module
     use MyGranule_facade

     implicit none

     ! this you can do because the type MyGranule is made globally available through the
     ! use MyGranule_facade statement, event though you have no access to the MyGranule instance
     ! used in that module (it's declared private)
     type(MyGranule) gr
     ! this does not work, even though the value from constants.f90 is USE'd in MyGranule_type which  is USE'd from MyGranule_facade
     ! it is not in the type(MyGranule) and that is the only publich thing in MyGranule_type
     !mod_my_granule = 3


     integer:: num_steps, step, foo
     real(c_double), allocatable, dimension(:,:) :: input1, input2, result
     real(c_double), allocatable, target, dimension(:,:) :: mask

     num_steps = 100
     print *, "allocation fields with dims ", idim, "x", jdim
     allocate(input1(idim, jdim), input2(idim, jdim), result(idim, jdim), mask(idim, jdim))
     call random_number(input1)
     call random_number(input2)
     result = 0.0
     mask = 0.0
     where(input1 < 0.5) mask = 1.0

    !  1. setup
    call setup_mygranule(mask)
    !  2. run the granule(s), possible several times within a loop (-> model time loop)
     do step = 1, num_steps
        call run_mygranule(input1, input2, result)
    end do
    !  3. clean up phase
    call teardown_mygranule()


    print *, "result: ", result
    deallocate(input1, input2, result, mask)

end program main_using_facade