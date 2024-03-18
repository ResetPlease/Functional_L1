# Functional_L1
## Лабораторная работа по функциональному программированию. Вариант 23
### 1. Разложение функции в ряд Тейлора
Функция $f(x) = arctg(x)$ раскладывается в ряд
$x - \frac{x^3}{3} + ... + (-1)^n\frac{x^{2x+1}}{2n+1}$
<br> Была реализована функция pow - простейшее возведения числа в степень
```F#
   let rec pow a b =
    match b with
        | 0.0 -> 1.0
        | _ -> a*(pow a (b-1.0)) 
```
Так же реализованы две функции разложения 
* Наивный метод
    ```F#
        let general x n = 
            (pow -1.0 n)*(pow x (2.0*n+1.0))/ (2.0*n+1.0)
    
        let rec accumulator i f =
            let mem = f i
            if abs(mem) < EPS then (i, mem)
            else 
                let a, b = accumulator (i+1.0) f
                (a, b + mem)

        let rec taylor_naive x =
            accumulator 0 (general x)
    ```
* Умный метод
    ```F#
    let addition x i = 
        (-1.0)*(pow x 2.0)*(1.0-1.0/(i+0.5))

    let rec smart_accumulator i e f =
        let mem = e*(f i)
        if abs(mem) < EPS then (i, mem)
        else 
            let a , b = smart_accumulator (i+1.0) (mem) f
            (a, mem + b)

    let taylor x = 
        let iteration, result = smart_accumulator 1 x (addition x)
        (iteration, result+x)
    ```

**Вывод программы:**
```bash
        X    Builtin      Smart     #terms       Dumb     #terms
  0.000000   0.000000   0.000000          1   0.000000          0
  0.050000   0.049958   0.049958          2   0.049958          2
  0.100000   0.099669   0.099669          3   0.099669          3
  0.150000   0.148890   0.148890          3   0.148890          3
  0.200000   0.197396   0.197396          4   0.197396          4
  0.250000   0.244979   0.244979          4   0.244979          4
  0.300000   0.291457   0.291457          5   0.291457          5
  0.350000   0.336675   0.336675          5   0.336675          5
  0.400000   0.380506   0.380506          6   0.380506          6
  0.450000   0.422854   0.422854          7   0.422854          7
  0.500000   0.463648   0.463648          8   0.463648          8
```

### Решение уравнений методами: дихотомии, итераций, Ньютона