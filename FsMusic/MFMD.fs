module MFMD
open System

//let pi = Math.PI

//module FourierTransform =
open System
open System.Numerics

let maxSize       = 4096
let pi            = Math.PI
let tau           = 2.*pi

 // module internal Details =
let isPowerOf2 n  = (n &&& n - 1) = 0
let ilog2 n       =
  if n < 2              then failwith "n must be greater than 1"
  if not (isPowerOf2 n) then failwith "n must be a power of 2"
  let rec loop n c s =
    let t = 1 <<< c
    if t = n then
      c
    elif t > n then
      loop n (c - s) (s >>> 1)
    else
      loop n (c + s) (s >>> 1)
  loop n 16 8
let twiddle a     = Complex.FromPolarCoordinates(1., a)

let twiddles      =
  let unfolder c =
    if c < 2*maxSize then
      let vs = Array.init (c / 2) (fun i -> twiddle (-tau * float i / float c))
      Some (vs, c*2)
    else
      None
  Array.unfold unfolder 1

let rec loop n2 ln s c f t =
  if c > 2 then
    let c2 = c >>> 1
    let struct (t, f) = loop n2 (ln - 1) (s <<< 1) c2 f t

    let twiddles = twiddles.[ln]

    if s > 1 then
      for j = 0 to c2 - 1 do
        let w   = twiddles.[j]
        let off = s*j
        let off2= off <<< 1;
        for i = 0 to s - 1 do
          let e = Array.get f (i + off2 + 0)
          let o = Array.get f (i + off2 + s)
          let a = w*o
          Array.set t (i + off + 0)   (e + a)
          Array.set t (i + off + n2)  (e - a)
    else
      for j = 0 to c2 - 1 do
        let w = twiddles.[j]
        let e = Array.get f (2*j + 0)
        let o = Array.get f (2*j + s)
        let a = w*o
        Array.set t (j + 0)   (e + a)
        Array.set t (j + n2)  (e - a)

    struct (f, t)
  elif c = 2 then
    for i = 0 to s - 1 do
      let e = Array.get f (i + 0)
      let o = Array.get f (i + s)
      let a = o
      Array.set t (i + 0)   (e + a)
      Array.set t (i + n2)  (e - a)

    struct (f, t)
  else
    struct (t, f)

 // open Details
  
let dft (vs : Complex []) =
    let l   = vs.Length
    let am  = tau / float l
    let rec loop s j i =
      if j < l then
        let v = vs.[j]
        let n = v*twiddle (-float i * float j * am)
        loop (s + n) (j + 1) i
      else 
        s
    Array.init l (loop Complex.Zero 0)

let fft (vs : Complex []) : Complex [] =
    let n   = vs.Length
    let ln  = ilog2 n

    let vs0 = Array.copy vs
    let vs1 = Array.zeroCreate n

    let struct (_, t) = loop (n >>> 1) ln 1 n vs0 vs1

    t

open System.Numerics


let FFT (signal : float []) =
    signal |> Array.map (fun x -> Complex(x,0.0))

let rec NormPhase (v) =
    if v > pi then NormPhase(v - pi) 
    else if v < -pi then NormPhase(v + pi) 
    else v

let unwrapPhase (u : float []) =
    let mutable k=0 // initialize k to 0
    let i=1 //% initialize the counter to 1
    let alpha=pi //% set alpha to the desired Tolerance. In this case, pi

    for i in 1 .. (u.Length - 2) do
        u.[i] <- u.[i]+(2.0*pi*(float k)) //% add 2*pi*k to ui
        if ((abs(u.[i+1]-u.[i]))>(abs(alpha))) then //%if diff is greater than alpha, increment or decrement k
                   
            if u.[i+1] < u.[i] then //   % if the phase jump is negative, increment k
                k <- k+1;
            else            // % if the phase jump is positive, decrement k
                k <- k-1;     
    //yout((i+1),:)=u(i+1)+(2*pi*k); //% add 2*pi*k to the last element of the input
    u.[u.Length - 1] <- u.[u.Length - 1]+(2.0*pi*(float k))
    u

let LTH_FS (signal : float []) =
    let N_length = signal.Length
    let j = 1.0

    let X = FFT signal
    let N_middle = if N_length % 2 = 0 then N_length/2 - 1 else (N_length - 1)/2

    let mutable results = []
    let mutable i = 0
    let mutable N = Array.zeroCreate N_length
    let mutable k = 0
    printfn "Begin length %i" N_middle
    while k < N_middle do
        i <- i + 1
        let mutable tmp : Complex [] = Array.zeroCreate N_length
        for kk in [|(N.[i-1] + 1) .. N_middle |] do
            k <- kk
            printfn "LOOP k=%i " k
            tmp <- tmp |> Array.mapi (fun n v -> v + X.[kk] * Complex.Exp(Complex(0.0,2.0*pi*(float kk)*((float n) / (float N_length)))))
            let tmpTheta = tmp |> Array.map (fun c -> c.Real) |>  unwrapPhase 
            let tmpW = 
                seq { 
                    for n in 1 .. tmpTheta.Length - 2 do
                        yield (tmpTheta.[n+1] - tmpTheta.[n-1]) / 2.0
                } |> Seq.toArray
            //tmpTheta |> Array.mapi (fun n v -> (tmpTheta.[n+1] - tmpTheta.[n-1]) / 2.0) //bounds issues
            if (Array.forall (fun v -> v > 0.0) tmpW) then
                printfn "found results at %i" kk
                results <- (tmp |> Array.toList, tmpW |> Array.toList) :: results
                N.[i] <- kk

    results

let wave1 t = 0.5 * Math.Sin(t*600.0) //60hz
let wave2 t = 0.3 * Math.Sin(t*1333.0) //133Hz
let wave t = wave1 t + wave2 t 

let time = 0.5 //sec
let resolution = 48000.0 //48khz
let timestep = 1.0 / resolution

let samples = [| 0.0 .. timestep .. 0.01 |]
let signal = Array.map wave samples

//sample 
let results = LTH_FS signal



let LTH_FDM_Test () =
    
    let wave1 t = 0.5 * Math.Sin(t*600.0) //60hz
    let wave2 t = 0.3 * Math.Sin(t*1330.0) //133Hz
    let wave t = wave1 t + wave2 t 
    
    let time = 0.5 //sec
    let resolution = 48000.0 //48khz
    let timestep = 1.0 / resolution

    let samples = [| 0.0 .. timestep .. 0.005 |]
    let signal = Array.map wave samples

    //sample 
    let results = LTH_FS signal

    

    ()