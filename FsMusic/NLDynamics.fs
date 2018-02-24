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