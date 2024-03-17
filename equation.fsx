// Yunusov R.
// v.23
let EPS = 1e-6

let rec dichotomy f a b = 
    let c = (a+b)/2.0
    if abs(f(c)) < EPS then c
    elif f(c)*f(a) < 0.0 then  dichotomy f a c
    else dichotomy f c b

let rec iterations phi x0 =
    if abs(x0 - (phi x0)) <= EPS then phi x0
    else iterations phi (phi x0)

let newthon f f' x0 = 
    iterations (fun x-> x-(f x)/(f' x)) (x0-EPS)


//23::f1::root=3.23 [2,4]
let f1 x = 3.0*x - 4.0*System.Math.Log(x) - 5.0

//24::f2::root=1.8756 [1,2]
let f2 x = System.Math.Cos(2.0/x) - 2.0*System.Math.Sin(1.0/x) + 1.0/x

//25::f3::root=0.7627 [0,1]
let f3 x = System.Math.Sqrt(1.0-0.4*x*x) - System.Math.Asin(x)


let f1' x = 3.0 - 4.0/x
let f2' x = 2.0*(1.0/(x*x))*System.Math.Sin(2.0/x) 
                    + 2.0*(1.0/(x*x))*System.Math.Cos(1.0/x) - 1.0/(x*x) 
let f3' x = -0.4*x/System.Math.Sqrt(1.0-0.4*x*x) - 1.0/System.Math.Sqrt(1.0-x*x)


let phi1 x = (4.0*System.Math.Log(x)+5.0)/3.0
let phi2 x = 2.0/System.Math.Acos( System.Math.Sin(1.0/x)*2.0 - 1.0/x )
let phi3 x = System.Math.Sin( System.Math.Sqrt(1.0-0.4*x*x) )

let main = 
    printfn "   %10s  %10s  %10s" "Dichotomy" "Iterations" "Newthon"
    printfn "F1 %10.5f  %10.5f  %10.5f" (dichotomy f1 2. 4.) (iterations phi1 2.) (newthon f1 f1' 4.)
    printfn "F2 %10.5f  %10.5f  %10.5f" (dichotomy f2 1. 2.) (iterations phi2 1.) (newthon f2 f2' 2.)
    printfn "F3 %10.5f  %10.5f  %10.5f" (dichotomy f3 0. 1.) (iterations phi3 0.) (newthon f3 f3' 1.)

 