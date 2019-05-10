module modabort
implicit none
contains
    subroutine abort(reason)
        character(len=*) :: reason
        print *, reason
        stop
    end subroutine abort
end module modabort

module actions
use modabort
USE, INTRINSIC :: IEEE_ARITHMETIC, ONLY: IEEE_IS_FINITE
implicit none
contains
    subroutine add(a, b)
        real(kind=16), intent(inout) :: a
        real(kind=16), intent(in)    :: b
        real(kind=16)                :: res
        res = a + b
        a = res
    end subroutine add

    subroutine sub(a, b)
        real(kind=16), intent(inout) :: a
        real(kind=16), intent(in)    :: b
        real(kind=16)                :: res
        res = a - b
        a = res
    end subroutine sub

    subroutine mul(a, b)
        real(kind=16), intent(inout) :: a
        real(kind=16), intent(in)    :: b
        real(kind=16)                :: res
        res = a * b
        a = res
    end subroutine mul

    subroutine div(a, b)
        real(kind=16), intent(inout) :: a
        real(kind=16), intent(in)    :: b
        real(kind=16)                :: res
        if(b .eq. 0.0) then
            call abort("Division by zero")
        end if
        res = a / b
        a = res
    end subroutine div

    subroutine pow(a, b)
        real(kind=16), intent(inout) :: a
        real(kind=16), intent(in)    :: b
        real(kind=16)                :: res
        !for errors
        character(len=100)           :: atext, btext
        if(b .lt. 0.0) then
            call abort("The power is less that zero")
        end if
        res = a**b
        if(.not.IEEE_IS_FINITE(res)) then
            write(atext, "(E15.7)") a
            write(btext, "(E15.7)") b
            call abort("Problems with exponentiation: " // trim(atext) // " in " // trim(btext))
        end if
        a = res
    end subroutine pow
end module actions

module calc
implicit none
    interface
        subroutine func(a, b)
            implicit none
            real(kind=16), intent(inout) :: a
            real(kind=16), intent(in)    :: b
        end subroutine func
    end interface
    type action
        procedure(func), pointer, nopass :: action
    end type action

contains
    subroutine docalc(actions, numbers, res)
        type(action) , dimension(:), intent(in)    :: actions
        real(kind=16), dimension(:), intent(in)    :: numbers
        real(kind=16),               intent(inout) :: res
        integer                                    :: i
        res = numbers(1)
        print *, res
        do i = 1, size(actions)
            call calcaction(actions(i)%action, res, numbers(i+1))
        end do
    end subroutine docalc

    subroutine calcaction(action, a, b)
        real(kind=16), intent(inout) :: a
        real(kind=16), intent(in)    :: b
        abstract interface
            subroutine func(a, b)
                implicit none
                real(kind=16), intent(inout) :: a
                real(kind=16), intent(in)    :: b
            end subroutine func
        end interface
        procedure(func) :: action
        call action(a, b)
    end subroutine calcaction
end module calc

module parser
use calc
use actions
use modabort
implicit none
contains
    subroutine parse(string, res)
        character(len=:), allocatable, intent(in)  :: string
        real(kind=16),                 intent(out) :: res

        character(len=:), allocatable              :: chars
        type(action),     allocatable              :: actions(:), tmp(:)
        real(kind=16),    allocatable              :: numbers(:)

        integer                                    :: i
        type(action)                               :: act
        real(kind=16)                              :: a, b

        chars = '+-*/^ '
        numbers = (/2.0, 3.0, 1.0, 2.0, -0.4, 2.2/)
        
        allocate(actions(0))
        do i = 1, len(chars)
            select case (chars(i:i))
                case ('+')
                    allocate(tmp(size(actions)+1))
                    tmp(1:size(actions)) = actions(1:size(actions))
                    call move_alloc(tmp, actions)
                    actions(i)%action => add
                case ('-')
                    allocate(tmp(size(actions)+1))
                    tmp(1:size(actions)) = actions(1:size(actions))
                    call move_alloc(tmp, actions)
                    actions(i)%action => sub
                case ('*')
                    allocate(tmp(size(actions)+1))
                    tmp(1:size(actions)) = actions(1:size(actions))
                    call move_alloc(tmp, actions)
                    actions(i)%action => mul
                case ('/')
                    allocate(tmp(size(actions)+1))
                    tmp(1:size(actions)) = actions(1:size(actions))
                    call move_alloc(tmp, actions)
                    actions(i)%action => div
                case ('^')
                    allocate(tmp(size(actions)+1))
                    tmp(1:size(actions)) = actions(1:size(actions))
                    call move_alloc(tmp, actions)
                    actions(i)%action => pow
                case (' ')
                case default
                    call abort("Char " // chars(i:i) // " is not valid!")
            end select
        end do
        if(size(actions) + 1 .ne. size(numbers)) then
            call abort("Noncompatibility count of actions and numbers")
        end if
        call docalc(actions, numbers, res)
    end subroutine parse

    subroutine get_line(lun, line, iostat)
        integer, intent(in)           :: lun
        character(len=:), intent(out), allocatable :: line
        integer, intent(out)          :: iostat

        integer, parameter            :: buffer_len = 10
        character(len=buffer_len)     :: buffer
        integer                       :: size_read

        line = ''
        do
            read(lun, '(A)', iostat = iostat, advance = 'no', size = size_read) buffer
            if (is_iostat_eor(iostat)) then
                line = line // buffer(:size_read)
                iostat = 0
                exit
            else if (iostat == 0) then
                line = line // buffer
            else
                exit
            end if
        end do
    end subroutine get_line
end module parser

program calcExpression
    use parser
    use actions
    implicit none
    real(kind=16) :: res
    real(kind=16), allocatable :: numbers(:)
    character(len=:), allocatable :: string
    character(len=20)             :: tmp
    integer                       :: iostat, i
    string = ""
!     call get_line(5, string, iostat)
!     print *, string
    call parse(string, res)
    print *, res
end
