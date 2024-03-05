// Samuel Nerayo
// c3400 - hw5.fsx
// F# function hat simplifies an algebraic expression


type Expression =
| X
| Y
| Const of float
| Neg of Expression
| Add of Expression * Expression
| Sub of Expression * Expression
| Mul of Expression * Expression

let rec simplify expr =
    match expr with
    // If the expression is a variable or a constant, return as is
    | X | Y | Const _ -> expr

    // Handle negation
    | Neg e ->
        match simplify e with
        // If the negated expression is a constant, negate the constant
        | Const c -> Const -c
        // Double negation: Neg (Neg e) simplifies to e
        | Neg e' -> e'
        // Return the negation of the simplified expression
        | e' -> Neg e'

    // Handle addition
    | Add (e1, e2) ->
        match simplify e1, simplify e2 with
        // If both expressions are constants, add the constants
        | Const c1, Const c2 -> Const (c1 + c2)
        // Adding zero does not change the other operand
        | e', Const 0.0 | Const 0.0, e' -> e'
        // If no simplification could be done, return the original add expression
        | e1', e2' -> Add (e1', e2')

    // Handle subtraction
    | Sub (e1, e2) ->
        match simplify e1, simplify e2 with
        // If both expressions are constants, subtract the constants
        | Const c1, Const c2 -> Const (c1 - c2)
        // Subtracting zero does not change the other operand
        | e', Const 0.0 -> e'
        // Subtracting from zero gives the negated other operand
        | Const 0.0, e' -> Neg e'
        // Subtracting an expression from itself results in zero
        | e1', e2' when e1' = e2' -> Const 0.0
        // If no simplification could be done, return the original sub expression
        | e1', e2' -> Sub (e1', e2')

    // Handle multiplication
    | Mul (e1, e2) ->
        match simplify e1, simplify e2 with
        // If both expressions are constants, multiply the constants
        | Const c1, Const c2 -> Const (c1 * c2)
        // Multiplying by zero results in zero
        | _, Const 0.0 | Const 0.0, _ -> Const 0.0
        // Multiplying by one does not change the other operand
        | e', Const 1.0 | Const 1.0, e' -> e'
        // If no simplification could be done, return the original mul expression
        | e1', e2' -> Mul (e1', e2')


let testSimplify() =
    let expr1 = Add (Const 9.0, Const 4.0)
    printfn "%A -> %A" expr1 (simplify expr1)

    let expr2 = Sub (Const 10.0, Const 3.5)
    printfn "%A -> %A" expr2 (simplify expr2)

    let expr3 = Mul (Const 6.0, Const 7.0)
    printfn "%A -> %A" expr3 (simplify expr3)

    let expr4 = Neg (Const 0.3)
    printfn "%A -> %A" expr4 (simplify expr4)

    let expr5 = Add (X, Const 0.0)
    printfn "%A -> %A" expr5 (simplify expr5)

    let expr6 = Sub (X, Const 0.0)
    printfn "%A -> %A" expr6 (simplify expr6)

    let expr7 = Mul (X, Const 0.0)
    printfn "%A -> %A" expr7 (simplify expr7)

    let expr8 = Mul (X, Const 1.0)
    printfn "%A -> %A" expr8 (simplify expr8)

    let expr9 = Neg (Neg X)
    printfn "%A -> %A" expr9 (simplify expr9)

    

testSimplify()
