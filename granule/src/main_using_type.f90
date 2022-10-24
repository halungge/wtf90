! Created by  on 20.10.22.

program main_using_type
     use, intrinsic :: iso_c_binding, only: c_int, c_double
     !us the MyGranule derived type
     use MyGranule_type, only: MyGranule
     !import global constants
     !use constants, only: zero, idim, jdim
     use add_to_constants
     implicit none

     integer:: num_steps, step, foo
     real(c_double), allocatable, dimension(:,:) :: input1, input2, result
     real(c_double), allocatable, target, dimension(:,:) :: mask
     real(c_double), allocatable, dimension(:) ::error
     type(MyGranule) :: gr
     ! this is locally defined here
     foo = 100
     ! idim is loaded from constants.f90 through add_index_fields.f90
     ! the use statement is transitive
     jdim = 12

     num_steps = 100
     print *, "allocation fields with dims ", idim, "x", jdim
     allocate(input1(idim, jdim), input2(idim, jdim), result(idim, jdim), mask(idim, jdim), error(jdim))
     call random_number(input1)
     call random_number(input2)
     result = zero
     error = zero
     mask = zero
     where(input1 < 0.5) mask = 1.0

     !use subroutine from module directly: only possible if it is set to public in module
     !call mean_square(input1, input2, error)
     ! print *, error

    !!this uses the derived type for the MyGranule and calls its interface in a
    ! 3 step way:
    !  1. setup
    call gr%init(mask)
    !  2. run the granule(s), possible several times within a loop (-> model time loop)
     do step = 1, num_steps
        call gr%run(input1, input2, result)
    end do
    !  3. clean up phase
    call gr%close()
    print *, "result: ", result
    deallocate(input1, input2, result, mask, error)

end program main_using_type