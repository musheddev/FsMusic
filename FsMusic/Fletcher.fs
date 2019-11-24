module Fletcher

let nOrder_RK (fns : (float [] -> float) []) (xs : float []) (dt : float) : float[] =
  let k1 = Array.map (fun fn -> (fn xs)*dt) fns    
  let k2 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + 0.5*k) xs k1))*dt) fns
  let k3 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + 0.5*k) xs k2))*dt) fns
  let k4 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + k) xs k3))*dt) fns
  let primes = Array.zip (Array.zip3 xs k1 k2) (Array.zip k3 k4)
  Array.map (fun ((x,k1,k2),(k3,k4)) -> x + (1./6.)*(k1 + 2.0*k2 + 2.0*k3 + k4)) primes


let secondOrder_RK (fn1 :float*float->float) (fn2 :float*float->float) ((x1,x2) :float*float) (dt :float) =
  let k1 = fn1(x1,x2)*dt
  let l1 = fn2(x1,x2)*dt
  let k2 = fn1(x1 + 0.5*k1,x2 + 0.5*l1)*dt
  let l2 = fn2(x1 + 0.5*k1,x2 + 0.5*l1)*dt
  let k3 = fn1(x1 + 0.5*k2,x2 + 0.5*l2)*dt
  let l3 = fn2(x1 + 0.5*k2,x2 + 0.5*l2)*dt
  let k4 = fn1(x1 + k3,x2 + l3)*dt
  let l4 = fn2(x1 + k3,x2 + l3)*dt
  let xdot1 = x1 + (1./6.)*(k1 + 2.0*k2 + 2.0*k3 + k4)
  let xdot2 = x2 + (1./6.)*(l1 + 2.0*l2 + 2.0*l3 + l4)
  xdot1,xdot2

type ModeParam =
  { 
      Mode : int
      DampingCoef : float
      Freq : float
      Amp : float
  }

type Vars =
  {
      ω : float
      Y : float
      ``Y'`` : float
      a : float
  }
  
type SimState =
  {
      Mode : int
      ω : float
      Y : float
      ``Y'`` : float
      a : float
      ϕ : float
      t : float


  }

type SimProps =
  {
      dt : float
      start_t : float
      end_t : float
  }

type G = Vars[] -> float

let ``a'`` (p : ModeParam) (g : G) (vars_T : Vars[]) (t : float) (vars_t : float []) : float =
  let [|a;ϕ|] = vars_t
  let var = vars_T.[p.Mode - 1]
  (g(vars_T) * cos(var.ω*t + ϕ) / var.ω) - (p.DampingCoef*a / 2.0)

let ``ϕ'`` (p : ModeParam) (g : G) (vars_T : Vars[]) (t : float) (vars_t : float []) : float =
  let [|a;ϕ|] = vars_t
  let var = vars_T.[p.Mode - 1]
  -g(vars_T) * sin(var.ω*t + ϕ) / (var.ω * a)


let simulate (p : ModeParam[]) (g : G) (s : SimProps) : SimState[][] =
  let init = p |> Array.map (fun x -> {
      Mode = x.Mode
      Y = 1.0
      Y' = 0.0
      ω = x.Freq
      a = x.Amp
      ϕ = 0.0
      t = s.start_t })

  //let make_fns mode vars_T t = 
  //  let mp = p |> Array.find (fun x -> x.Mode = mode)
  //  [|
  //    Array.chunkBySize 2 >> a' mp g vars_T t
  //    Array.chunkBySize 2 >> ϕ' mp g vars_T t
  //  |]

  let history = init |> Array.unfold (fun (st : SimState[]) -> 
    if st.[0].t >= s.end_t then None
    else
      let vars_T = st |> Array.map (fun x -> {ω = x.ω; Y = x.Y; Y' = x.Y'; a = x.a})
      //let vars_t = st |> Array.map (fun x -> [|x.a; x.ϕ|])
      //let all_fns = st |> Array.map (fun x -> make_fns x.Mode vars_T x.t) |> Array.concat
      let result = st |> Array.map (fun x -> 
        let mp = p |> Array.find (fun y-> y.Mode = x.Mode)
        let fn1 = (fun (a,b) -> a' mp g vars_T x.t [|a;b|])
        let fn2 = (fun (a,b) -> ϕ' mp g vars_T x.t [|a;b|])
        let result = secondOrder_RK fn1 fn2 (x.a,x.ϕ) s.dt
        result) 

      let newst =
        result
        |> Array.zip st
        |> Array.mapi (fun i (state,(a', ϕ'))  -> 
          let t = state.t + s.dt
          let a = state.a + a'*s.dt
          let ω = state.ω
          let ϕ = state.ϕ + ϕ'*s.dt
          let Y = a * sin(ω*t + ϕ)
          let Y' = a * ω * cos(ω*t + ϕ)
          { Mode = state.Mode; ω = ω; a = a; Y = Y; ϕ = ϕ; t = t; Y' = Y'})

      Some(st,newst)
    ) 


  history


let example () = 
  let modeparams = [|
    { Mode = 1
      DampingCoef = 0.1
      Freq = 440.0
      Amp = 0.5 }
    { Mode = 2
      DampingCoef = 0.1
      Freq = 890.0
      Amp = 0.5 }
  |]

  let g (vars : Vars[]) = vars.[0].Y*0.4 + vars.[1].Y*0.4 

  let trace = simulate modeparams g {dt = 0.0001; start_t = 0.0; end_t = 0.1} 

  trace