program solvejumble
    ! Import module lexicon
    use lexicon
    implicit none
    character(len=32), dimension(50) :: words ! holds the number of input words
    character(len=32), dimension(50) :: anagrams ! holds the anagrams
    integer :: numWords

    ! user prompt to play the game
    write(*,*) 'Welcome to Word Jumble!'
    write(*,*) 'How many jumbled words? Please enter a positive integer'
    read(*,'(i10)') numWords

    ! Generates dictionary from "dict2.txt" provided in the directory in addition
    ! to counting the number of lines in the text file that will be used to build the dictionary
    call buildLexicon(dictionary, count)

    ! retrieves user input
    call inputJumble(words, numWords)

    ! generates the sorted alphabetical dictionary which contains anagrams
    call generateAnagrams(words, numWords, anagrams, count, dictionary, sortedDictionary)

    ! compares the anagrams and searches in the dictionary, then prints the result
    call findAnagram(words, numWords, anagrams, dictionary, sortedDictionary, count)

end program solvejumble

! inputJumble retrieves the user input for the jumbled words
subroutine inputJumble(words, numWords)
    use lexicon
    implicit none
    integer, intent(in) :: numWords
    integer :: i
    character (len=32), dimension(50), intent(inout) :: words
    write (*, '(A,I1,A)') 'Enter the ', numWords, ' jumbled words, each word is on a new line'
    do i = 1, numWords
        read(*,'(a)') words(i)
        call toLowerCase(words(i))
        ! write(*,*) 'Word Entered:', words(i)
    end do
end subroutine inputJumble

! subroutine takes in the jumbled words, the number of jumbled words
! and returns an array of words that are in alphabetical order that will be used
! to generate anagrams
subroutine generateAnagrams(words, numWords, anagrams, count, dictionary, sortedDictionary)
    implicit none 
    character (len=32), dimension(50), intent(inout) :: words
    character (len=32), dimension(50), intent(out) :: anagrams
    character (len=32), dimension(count), intent(inout) :: dictionary
    character (len=32), dimension(count), intent(inout) :: sortedDictionary
    integer :: i
    integer, intent(in) :: numWords, count
    
    do i = 1, numWords
        anagrams(i) = words(i)
    end do 

    ! sort the jumbled words in alphabetical order
    do i = 1, numWords 
        call sortCharacters(anagrams, numWords)
    end do

    ! Create another dictionary to store words that are sorted
    ! in alphabetical order, will be used later on to search for anagrams
    call copyValues(dictionary, sortedDictionary, count)

    ! Sort all the characters in alphabetical order in the new dictionary
    call sortCharacters(sortedDictionary, count)

end subroutine generateAnagrams

! subroutine takes in an array of alphabetical words and a dictionary
! will search through the dictionary for anagrams and output to the user
subroutine findAnagram(words, numWords, anagrams, dictionary, sortedDictionary, count)
    implicit none
    integer :: i, j, k
    character (len=1) :: answer
    integer, intent(in) :: numWords, count
    character (len=32) :: tempChar
    character (len=32), dimension(50), intent(inout) :: words, anagrams
    character (len=32), dimension(50) :: tempWords, newJumble
    character (len=32), dimension(count), intent(inout) :: dictionary, sortedDictionary

    ! check if the sorted anagram is in the sortedDictionary
    ! if it is, search the Dictionary for the index (line number) and output
    ! both the jumbled word and valid anagram side by side

    write(*,*) 'The following jumbles have been solved'

    k = 1
    do i = 1, numWords
        do j = 1, count
            ! searches in the dictionary for the words
            if (anagrams(i) == sortedDictionary(j)) then
                ! write the words that are in the dictionary if found
                write(*,*) words(i), dictionary(j)
                tempWords(k) = dictionary(j)
                k = k+1
                exit
            end if 
        end do
    end do

    ! if yes, output the next part of the game, otherwise exit the program
    write(*,*) 'Solve word jumble? - Y for yes, otherwise enter anything to exit'
    read(*,*) answer

    if (answer == "y" .or. answer == "Y") then
        ! allocate(userInput(numLetters))
        write(*,*) 'Select circled letters from word puzzle, the letters must be in the word with no spaces inbetween'
        write(*,*) 'Example: for the word "Popular" the user may input "oar" '
        ! ask the user to enter the circle positions for each word
        do i = 1, numWords
            write(*,*) tempWords(i)
            read(*,'(a)') tempChar
            newJumble(i) = tempChar
        end do 
    else
        ! end program if user does not want to solve word jumble
        write(*,*) 'Exiting Program!'
        stop
    end if 

    tempChar = ""

    ! Trims and appends the jumbled word
    call formatString(newJumble, numWords, tempChar)

    ! Prints the scrambled jumbled word
    write(*,*) 'Jumbled Word: ', trim(tempChar)

    ! sort the jumbled word jumble
    call sortCharacters(tempChar, 1)

    ! write (*,*) tempChar
    do j = 1, count
        ! searches in the dictionary for the words
        if (tempChar == sortedDictionary(j)) then
            write(*,*) 'Solved Jumble: ', dictionary(j)
            stop
        end if 
    end do

    write(*,*) 'Cannot solve word jumble! Exiting program' 
    
end subroutine findAnagram

! subroutine that takes in an array of strings and concatenates + trims them
subroutine formatString(strings, numWords, newString)
    implicit none
    integer :: i
    character(len=32), dimension(numWords), intent(inout) :: strings
    integer, intent(in) :: numWords
    character(len=32), intent(out) :: newString

    ! concatenates the string values 
    do i = 1, numWords
        newString = trim(newString) // trim(strings(i))
    end do
end subroutine formatString

! subroutine sorts the characters in alphabetical order by comparing and swapping one at a time
subroutine sortCharacters(words, n)
    implicit none
    character(len=*), dimension(n), intent(inout) :: words
    integer :: i, j, k, n
    character(len=1) :: temp
    do i = 1, n
        do j = 1, len(words(i)) - 1
            do k = j + 1, len(words(i))
                ! sort the indices
                if (words(i)(j:j) > words(i)(k:k)) then
                    ! perform swap
                    temp = words(i)(j:j)
                    words(i)(j:j) = words(i)(k:k)
                    words(i)(k:k) = temp
                end if
            end do
        end do
    end do
end subroutine sortCharacters

! subroutine copys values from one array to another
subroutine copyValues(from, to, n)
    implicit none
    character(len=*), dimension(n), intent(in) :: from
    character(len=*), dimension(n), intent(out) :: to
    integer :: i, n
    do i = 1, n
        to(i) = from(i)
    end do
end subroutine copyValues
