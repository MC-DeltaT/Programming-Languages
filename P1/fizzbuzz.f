c Fortran 77 fizz buzz implementation for numbers up to 100.

      program fizzbuzz

      integer i

c     For loop from 1 to 100, inclusive
      do 10 i = 1, 100
          if (mod(i, 15) .EQ. 0) then
              write(*, '(A)') 'Fizz buzz'
          elseif (mod(i, 3) .EQ. 0) then
              write(*, '(A)') 'Fizz'
          elseif (mod(i, 5) .EQ. 0) then
              write(*, '(A)') 'Buzz'
          else
              write(*, '(I0)') i
          endif
10    continue

      stop
      end
