module NLDynamics

open DiffSharp
open DiffSharp.AD
open DiffSharp.AD.Float64

let rungeKutta (fn :D->D) (x :D) (dt :D) = //fn x -> dx/dt
    let k1 = fn(x)*dt
    let k2 = fn(x + (D 0.5)*k1)*dt
    let k3 = fn(x + (D 0.5)*k2)*dt
    let k4 = fn(x + k3)*dt
    x + (1./6.)*(k1 + (D 2.)*k2 + (D 2.)*k3 + k4)

let wave (amp :D) (w :D) (phase :D) (t :D) =
    amp * sin(w*t + phase)

//mode locking equations from Fletcher 1999

let adot (g :(D->D)->D) (a :D) (amp :D) (w :D) (phase :D) (yn :D->D) (t :D) =
    g(yn) * cos(w*t + phase) / w - a*amp/2

let fdot (g :(D->D)->D) (amp :D) (w :D) (phase :D) (yn :D->D) (t :D) =
    - g(yn) * sin(w + phase) / (w*amp)

let makeg (c1 :D) (c2 :D) (p :D) (q :D) = //linear case but summation doesn't make sense  
    fun (yfn :D->D) ->
         let y fn = List.sumBy (fun x -> fn(D x)) [0.0 .. 0.01 .. 6.28]
         c1*(y yfn) + c2*(y (diff yfn))



module Tests =
    open FSharp.Charting
    let test1() =
        let fn (x :D) = x*(1-x)
        let dt = 0.1
        let chart = 
            List.mapFold (fun (x :D) t -> 
                let xn = rungeKutta fn x (D dt)
                ((t, float xn),xn)) (D 1.5) [for t in 0. .. dt .. 10. -> t]
        printfn "%O" (fst chart)        
        Chart.Line(chart |> fst).ShowChart()
    test1() |> ignore