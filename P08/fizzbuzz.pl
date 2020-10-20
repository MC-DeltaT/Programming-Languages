fizz(X) :- (X mod 3) =:= 0.
buzz(X) :- (X mod 5) =:= 0.

fizzbuzz_print(X) :- fizz(X), buzz(X), write('FizzBuzz'), nl, !.
fizzbuzz_print(X) :- fizz(X), write('Fizz'), nl, !.
fizzbuzz_print(X) :- buzz(X), write('Buzz'), nl, !.
fizzbuzz_print(X) :- write(X), nl, !.

fizzbuzz(0).
fizzbuzz(N) :- N2 is N - 1, fizzbuzz(N2), fizzbuzz_print(N), !.
