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

let nOrder_RK (fns :(D [] -> D) []) (xs :D []) (dt :D) =
    let k1 = Array.map (fun fn -> (fn xs)*dt) fns    
    let k2 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + (D 0.5)*k) xs k1))*dt) fns
    let k3 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + (D 0.5)*k) xs k2))*dt) fns
    let k4 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + k) xs k3))*dt) fns
    let primes = Array.zip (Array.zip3 xs k1 k2) (Array.zip k3 k4)
    Array.map (fun ((x,k1,k2),(k3,k4)) -> x + (1./6.)*(k1 + (D 2.)*k2 + (D 2.)*k3 + k4)) primes

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
         let y fn = List.sumBy (fun x -> fn(D x)*(D 0.01)) [0.0 .. 0.01 .. 6.28]
         c1*(y yfn) + c2*(y (diff yfn))

//mode locking fletcher 1978
type ModeParam =
    { 
        Mode : int
        Freq : float
        Driver : float
        Amp : float
        C : float []
    }

type ModeActives =
    {
        Freq :float
        Amp :float
        Delta :float
        Phi :float
    }    
let make_fdots_2 modei modej c1 c2 =
    let p = float modej.Mode
    let q = float modei.Mode
    let C1 =
        let s = q - 1.
        (p + s)/(p*s*(2.**(p+s)))*c1
    let C2 =
        let r = p - 1.
        (r + q)/(r*q*(2.**(r+q)))*c1
    let fj xj t deltai thetai deltaj thetaj ampi ampj =
        (modej.Driver/(modej.Freq*xj))
        * ((modej.C.[0]*ampj*modej.Freq + 0.25*modej.C.[2]*(ampj**3.0)*(modej.Freq**3.0))*sin(deltaj)
        + C1*sin((p*modei.Freq - q*modej.Freq)*t + p*(thetai+deltai) - q*(thetaj+deltaj) + deltaj))
    let fi xi t deltai thetai deltaj thetaj ampi ampj =
        (modei.Driver/(modei.Freq*xi))
        * ((modei.C.[0]*ampi*modei.Freq + 0.25*modei.C.[2]*(ampi**3.0)*(modei.Freq**3.0))*sin(deltai)
        + C2*sin((p*modei.Freq - q*modej.Freq)*t + p*(thetai+deltai) - q*(thetaj+deltaj) + deltai))
    (fi,fj)

let makec1 a n (C : float []) =
    List.fold (fun acc i -> 
        let k = float (i*2 + 1)
        acc + C.[i]*(0.5**(float (i*2))*(a**k)*(n**k))) 0. [0 .. (C.Length - 1)]

let factorial x = 
    let rec util(value, acc) = 
        match value with
        |0 | 1 -> acc
        | _    -> util(value - 1, acc * float value)
    util(x,1.)

let makec2 mode (modes :ModeParam []) (C :float []) =
    let len = modes.Length
    let ary = [1 .. 1 .. len]
    let sum = List.sum ary - 1
    let top = factorial sum
    let bottom = List.fold (fun acc x -> if (len - mode) = x then acc*(factorial (x - 1)) else acc*(factorial x)) 0.0 ary
    let c = (top*(C.[mode-1]**(float sum)))/(bottom*(2.**(float sum)))
    let revary = List.rev ary 
    let pre = Array.fold (fun acc m ->
            let i = float revary.[m.Mode - 1]
            if mode = m.Mode 
            then acc*(m.Freq**(i-1.0))*(m.Amp**i)
            else acc*(m.Freq**i)*(m.Amp**i)) 0.0 modes
    pre*c

let make_fdots_n (modes :ModeParam []) ary =
    Array.map (fun m ->
    (fun t (a :ModeActives) ->
        (m.Driver/(a.Freq*a.Amp))*(makec1 a.Amp a.Freq m.C)*(sin a.Delta)
        + (makec2 m.Mode modes ary)*(sin())
        )) modes


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

//let test2() = //single mode
let c1 = D 0.1
let c2 = D 0.0
let alpha = D 0.2
let dt = 0.01
let freq = D 120.0
let amp = D 1.0

let make_chart c1 c2 freq amp alpha dt (detuple :D*D -> float) =
    let g = makeg c1 c2 (D 0.0) (D 0.0)
    let f = (fun (x1,x2) -> fdot g x1 x2 (D 0.) wave)
    let a = (fun t (x1,x2) -> adot g alpha x1 x2 (D 0.) wave t)
    let chart = 
        List.mapFold (fun ((x1,x2) :D*D) t -> 
            let y = secondOrder_RK f (a (D t)) x1 x2 (D dt)
            ((t,detuple y),y)) (freq,amp) [for t in 0. ..dt ..10. -> t]
    chart |> fst

let charts1 = 
    List.map (fun x -> make_chart c1 c2 freq amp (D x) dt (fst >> float) |> Chart.Line) [0.0 .. 0.1 .. 1.]

let charts2 = 
    List.map (fun x -> make_chart c1 c2 freq (D x) alpha dt (snd >> float) |> Chart.Line) [0.0 .. 0.1 .. 1.]
        
let chart1 = (make_chart (D 0.1) (D 0.01) (D 120.) (D 0.5) (D 0.1) dt (snd >> float) |> Chart.Line).ShowChart()
let chart2 = (make_chart (D 0.1) (D 0.01) (D 120.) (D 0.5) (D 0.1) dt (fst >> float) |> Chart.Line).WithYAxis(Max = 121., Min = 119.).ShowChart()

Chart.Combine(charts1).ShowChart()
Chart.Combine(charts2).ShowChart()