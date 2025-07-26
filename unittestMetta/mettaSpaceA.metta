
(= (exampleFunctionA $X)
    (sequence
        (addatom2 self2 (A 5))

        (asserteq2 (match self2 (A 5) 7)  7)
    )
)