module lexicon
    ! Module Lexicon which contains various subroutines
    implicit none
    character(len=32), dimension(250000) :: dictionary
    character(len=32), dimension(250000) :: sortedDictionary
    integer :: count 
    logical :: found
    contains   
        subroutine buildLexicon(dictionary, count)   
            implicit none
            character(len=32), dimension(count), intent(inout) :: dictionary
            integer :: i
            integer, intent(inout) :: count
            open(unit=10, file='/usr/share/dict/words', status='old', action='read')
            count = 0
            do
                ! counting words in the dictionary
                count = count + 1
                ! Add values to the dictionary array and convert to lower case
                read(10, '(a)', iostat=i) dictionary(count)
                call toLowerCase(dictionary(count))
                if (i /= 0) exit
            end do
        
            close(10)
        end subroutine buildLexicon

        ! Takes in a word and searches through the dictionary, returns True if found
        subroutine findlex(dictionary, count, word, result)
            implicit none
            integer, intent(in) :: count 
            integer :: i
            logical, intent(out) :: result
            character(len=32) :: word
            character(len=32), dimension(count), intent(inout) :: dictionary
            
            ! Iterate through the dictionary, returns True if found
            do i = 1, count
                if (word == dictionary(i)) then
                    write(*,*) word, dictionary(i)
                    result = .true.
                end if 
            end do 
        end subroutine findlex

        ! Takes in a string and converts it to lower case
        subroutine toLowerCase(str)
            character(len=*), intent(inout) :: str
            integer :: i
            do i = 1, len(str)
                ! compares values and converts to lower case
                if (str(i:i) .ge. 'A' .and. str(i:i) .le. 'Z') then
                str(i:i) = char(ichar(str(i:i)) + 32)
            end if
        end do
        end subroutine toLowerCase
end module lexicon