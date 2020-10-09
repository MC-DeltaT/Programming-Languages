1 to: 100 do: [:x |
    (x \\ 15 == 0) ifTrue: ['FizzBuzz' printNl].
    (x \\ 3 == 0) ifTrue: ['Fizz' printNl].
    (x \\ 5 == 0) ifTrue: ['Buzz' printNl].
    x printNl.
]
