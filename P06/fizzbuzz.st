1 to: 100 do: [:x |
    (x \\ 15 == 0) ifTrue: ['FizzBuzz' displayNl]
    ifFalse: [
        (x \\ 3 == 0) ifTrue: ['Fizz' displayNl]
        ifFalse: [
            (x \\ 5 == 0) ifTrue: ['Buzz' displayNl]
            ifFalse: [x displayNl]
        ]
    ]
]
