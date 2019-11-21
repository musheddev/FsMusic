module Fletcher

let nOrder_RK (fns : (float [] -> float) []) (xs : float []) (dt : float) : float[] =
  let k1 = Array.map (fun fn -> (fn xs)*dt) fns    
  let k2 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + 0.5*k) xs k1))*dt) fns
  let k3 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + 0.5*k) xs k2))*dt) fns
  let k4 = Array.map (fun fn -> (fn (Array.map2 (fun x k -> x + k) xs k3))*dt) fns
  let primes = Array.zip (Array.zip3 xs k1 k2) (Array.zip k3 k4)
  Array.map (fun ((x,k1,k2),(k3,k4)) -> x + (1./6.)*(k1 + 2.0*k2 + 2.0*k3 + k4)) primes

type ModeParam =
  { 
      Mode : int
      DampingCoef : float
      Freq : float
      Amp : float
  }

type ModeActives =
  {
      Mode : int
      ``ω`` : float
      ``a`` : float
      Y : float
      ``ϕ`` : float
      t : float
  }
  
//type ModeFns =
//  {
//      Mode : int
//      ``ω`` : float [] -> float
//      ``a`` : float [] -> float
//      Y : float [] -> float
//      ``ϕ`` : float [] -> float
//      t : float [] -> float
//  }    


type SimProps =
  {
      dt : float
      start_t : float
      end_t : float
  }

type G = ModeActives [] -> float

let ``a'`` (p : ModeParam) (g : G) (vars : ModeActives []) : float =
  let var = vars |> Array.find (fun x -> p.Mode = x.Mode)
  (g(vars) * cos(var.ω*var.t + var.ϕ) / var.ω) - (p.DampingCoef*var.a / 2.0)

let ``ϕ'`` (p : ModeParam) (g : G) (vars : ModeActives []) : float =
  let var = vars |> Array.find (fun x -> p.Mode = x.Mode)
  -g(vars) * sin(var.ω*var.t + var.ϕ) / (var.ω * var.a)

let y (p : ModeParam) (vars : ModeActives []) : float =
  let var = vars |> Array.find (fun x -> p.Mode = x.Mode)
  var.a * sin(var.ω*var.t + var.ϕ)

let ``ω`` (p : ModeParam) (g : G) (vars : ModeActives []) : float =
   let var = vars |> Array.find (fun x -> p.Mode = x.Mode)
   var.ω + (ϕ' p g vars)

let toArray (vars : ModeActives[]) = vars |> Array.map (fun vars -> [|vars.ω; vars.a; vars.Y; vars.ϕ; vars.t|]) |> Array.concat

let fromArray (vars : float[]) =
    vars
    |> Array.chunkBySize 5
    |> Array.mapi (fun i x -> 
      match x with
      | [| w; a; Y; p; t|] -> { Mode = i+1; ω = w; a = a; Y = Y; ϕ = p; t = t})

//sim modeparam and G and sim props 
// then setup inital actives 
// setup functions 

let simulate (p : ModeParam[]) (g : G) (s : SimProps) : ModeActives[][] =
  let init = p |> Array.map (fun x -> {
      Mode = x.Mode
      Y = 1.0
      ω = x.Freq
      a = x.Amp
      ϕ = 0.0
      t = s.start_t })

  let make_fns mode = 
    let mp = p |> Array.find (fun x -> x.Mode = mode)
    [|
    fromArray >> ω mp g
    fromArray >> (y mp)
    fromArray >> ϕ' mp g
    fromArray >> a' mp g
    fromArray >> (fun vars -> vars.[0].t + s.dt)
    |]




  let history = init |> Array.unfold (fun (st : ModeActives[]) -> 
    if st.[0].t >= s.end_t then None
    else
      let xs = st |> toArray
      let all_fns = st |> Array.map (fun x -> make_fns x.Mode) |> Array.concat
      let result = nOrder_RK all_fns xs s.dt

      let newst =
        result
        |> Array.chunkBySize 5
        |> Array.mapi (fun i x -> 
          match x with
          | [| w; a; Y; p; t|] -> { Mode = i+1; ω = w; a = a; Y = Y; ϕ = p; t = t})

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

  let g (vars : ModeActives[]) = vars.[0].Y*0.9 + vars.[1].Y*0.5 

  let trace = simulate modeparams g {dt = 0.001; start_t = 0.0; end_t = 0.1} 

  trace