c Fortran 77 fizz buzz implementation for numbers up to 100.

      program fizzbuzz

      integer i
      logical div3, div5

c     For loop from 1 to 100, inclusive
      do 10 i = 1, 100
          div3 = mod(i, 3) .EQ. 0
          div5 = mod(i, 5) .EQ. 0
          if (div3 .AND. div5) then
              write(*, *) 'Fizz buzz'
          elseif (div3) then
              write(*, *) 'Fizz'
          elseif (div5) then
              write(*, *) 'Buzz'
          else
              write(*, *) i
          endif
10    continue

      stop
      end
