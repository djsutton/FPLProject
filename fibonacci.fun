/* DJ sutton */
/* Recursive Fibonacci number algorighm in Reynolds' defined language*/
(
    lambda n .
    (
        letrec add =
            lambda a .
            (
                lambda b .
                (
                    if (equal b 0)
                    then a
                    else
                    (
                        add (succ a) (pred b)
                    )
                )
            )
        in
        (
            letrec fibonacci = lambda x .
                if (equal x 0)
                then 0
                else
                (
                    if (equal x 1)
                    then 1
                    else
                    (
                        add (fibonacci (pred x)) (fibonacci (pred (pred x)))
                    )
                )
            in 
            (
                fibonacci n
            )
        )
    )
)
27
