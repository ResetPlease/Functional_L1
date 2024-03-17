// Yunusov R.
// v.23
// function to compute: arctg(x)
let f = System.Math.Atan
let EPS = 1e-6
let a = 0.0
let b = 0.5
let n = 10

let rec pow a b =
    match b with
        | 0.0 -> 1.0
        | _ -> a*(pow a (b-1.0))

let general x n = (pow -1.0 n)*(pow x (2.0*n+1.0))/(2.0*n+1.0)
 
let rec accumulator i f =
    let mem = f i
    if abs(mem) < EPS then (i, mem)
    else 
        let a, b = accumulator (i+1.0) f
        (a, b + mem)
let rec taylor_naive x =
    accumulator 0 (general x)

let addition x i = (-1.0)*(pow x 2.0)*(1.0-1.0/(i+0.5))

let rec smart_accumulator i e f =
    let mem = e*(f i)
    if abs(mem) < EPS then (i, mem)
    else 
        let a , b = smart_accumulator (i+1.0) (mem) f
        (a, mem + b)

let taylor x = 
    let iteration, result = smart_accumulator 1 x (addition x)
    (iteration, result+x)

let main =
   printfn "%10s %10s %10s %10s %10s %10s" "X" "Builtin" "Smart" "#terms" "Dumb" "#terms"
   for i=0 to n do
     let x = a+(float i)/(float n)*(b-a)
     let naive_iteration, naive_value = taylor_naive x
     let smart_taylor_iteration, smart_taylor_value = taylor x
     printfn "%10f %10f %10f %10.0f %10f %10.0f" 
        x (f x) 
        smart_taylor_value smart_taylor_iteration
        naive_value naive_iteration

main
