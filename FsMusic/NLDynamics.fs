﻿module NLDynamics

open DiffSharp
open DiffSharp.AD
open DiffSharp.AD.Float64

let rungeKutta (fn :D->D) (x :D) (dt :D) = //fn x -> dx/dt
    let k1 = fn(x)*dt
    let k2 = fn(x + (D 0.5)*k1)*dt
    let k3 = fn(x + (D 0.5)*k2)*dt
    let k4 = fn(x + k3)*dt
    x + (1./6.)*(k1 + (D 2.)*k2 + (D 2.)*k3 + k4)

let secondOrder_RK (fn1 :D*D->D) (fn2 :D*D->D) (x1 :D) (x2 :D) (dt :D) =
    let k1 = fn1(x1,x2)*dt
    let l1 = fn2(x1,x2)*dt
    let k2 = fn1(x1 + (D 0.5)*k1,x2 + (D 0.5)*l1)*dt
    let l2 = fn2(x1 + (D 0.5)*k1,x2 + (D 0.5)*l1)*dt
    let k3 = fn1(x1 + (D 0.5)*k2,x2 + (D 0.5)*l2)*dt
    let l3 = fn2(x1 + (D 0.5)*k2,x2 + (D 0.5)*l2)*dt
    let k4 = fn1(x1 + k3,x2 + l3)*dt
    let l4 = fn2(x1 + k3,x2 + l3)*dt
    let xdot1 = x1 + (1./6.)*(k1 + (D 2.)*k2 + (D 2.)*k3 + k4)
    let xdot2 = x2 + (1./6.)*(l1 + (D 2.)*l2 + (D 2.)*l3 + l4)
    xdot1,xdot2
let wave (amp :D) (w :D) (phase :D) (t :D) =
    amp * sin(w*t + phase)

let modu (x :D) (modu :D) =
    let z = x/modu
    (z - (D.Round z))*modu


let ramp (amp :D) (w :D) (phase :D) (t :D) =
    (amp*(w*(modu (t + phase) (D 6.28)))/(D 6.28))

//mode locking equations from Fletcher 1999

let adot (g :(D->D)->D) (a :D) (amp :D) (w :D) (phase :D) (yn :D->D->D->D->D) (t :D) =
    g(yn amp w phase) * cos(w*t + phase) / w - a*amp/2

let fdot (g :(D->D)->D) (amp :D) (w :D) (phase :D) (yn :D->D->D->D->D) =
    - g(yn amp w phase) * sin(w + phase) / (w*amp)

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

    let test2() = //single mode
        let c1 = D 0.5
        let c2 = D 0.1
        let dt = 0.1
        let g = makeg c1 c2 (D 0.0) (D 0.0)
        let f = (fun (x1,x2) -> fdot g x1 x2 (D 0.) ramp)
        let a = (fun t (x1,x2) -> adot g (D 1.0) x1 x2 (D 0.) ramp t)

        let chart = 
            List.mapFold (fun ((x1,x2) :D*D) t -> 
                let x1,x2 = secondOrder_RK f (a (D t)) x1 x2 (D dt)
                ((t,float x1),(x1,x2))) ((D 120.),(D 1.)) [for t in 0. ..dt ..10. -> t]
        printfn "%O" (fst chart)        
        Chart.Line(chart |> fst).ShowChart()