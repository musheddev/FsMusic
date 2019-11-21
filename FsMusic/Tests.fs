module Tests

open Expecto
open XPlot.Plotly
open NLDynamics
open DiffSharp.AD.Float64





let make_chart c1 c2 freq amp alpha dt (detuple :D*D -> float) =
    let g = makeg c1 c2 (D 0.0) (D 0.0)
    let f = (fun (x1,x2) -> fdot g x1 x2 (D 0.) wave)
    let a = (fun t (x1,x2) -> adot g alpha x1 x2 (D 0.) wave t)
    let chart = 
        List.mapFold (fun ((x1,x2) :D*D) t -> 
            let y = secondOrder_RK f (a (D t)) x1 x2 (D dt)
            ((t,detuple y),y)) (freq,amp) [for t in 0. ..dt ..10. -> t]
    chart |> fst


let rungekuttaTest = test "RungeKutta Test" {
    let fn (x :D) = x*(1-x)
    let dt = 0.1
    let chart = 
        List.mapFold (fun (x :D) t -> 
            let xn = rungeKutta fn x (D dt)
            ((t, float xn),xn)) (D 1.5) [for t in 0. .. dt .. 10. -> t]
    printfn "%O" (fst chart)        
    Chart.Line(chart |> fst).Show()
    Expect.isTrue true ""
}

let someTest = testCase "fletcher" <| fun _ ->
  let c1 = D 0.1
  let c2 = D 0.0
  let alpha = D 0.2
  let dt = 0.01
  let freq = D 120.0
  let amp = D 1.0

  let charts1 = 
    List.map (fun x -> 
      let chrt = make_chart c1 c2 freq amp (D x) dt (fst >> float)
      Scatter(
        x = List.map fst chrt,
        y = List.map snd chrt,
        mode = "lines+markers")) [0.0 .. 0.1 .. 1.]

  let charts2 = 
    List.map (fun x -> 
    let chrt = make_chart c1 c2 freq (D x) alpha dt (snd >> float) 
    Scatter(
      x = List.map fst chrt,
      y = List.map snd chrt,
      mode = "lines+markers")
    ) [0.0 .. 0.1 .. 1.]

  charts1 |> Chart.Plot |> Chart.Show |> ignore
  charts2 |> Chart.Plot |> Chart.Show |> ignore


  Expect.isTrue true ""


let fletcher1999 = testCase "flecther1999" <| fun _ ->
    let results = Fletcher.example()
    let x_axis = results |> Array.map (fun x -> x.[0].t) 

    let mode m = results |> Array.map (fun x -> x |> Array.find (fun y -> y.Mode = m))

    let charts m =
      let modeResult = mode m

      let chartY = Scatter( 
        x = x_axis,
        y = Array.map (fun (x : Fletcher.ModeActives) -> x.Y) modeResult,
        name = sprintf "Y mode %i" m,
        mode = "lines+markers")

      let chartA = Scatter(
        x = x_axis,
        y = Array.map (fun (x : Fletcher.ModeActives) -> x.a) modeResult,
        name = sprintf "a mode %i" m,
        mode = "lines+markers")
      
      let chartPhase = Scatter( 
        x = x_axis,
        y = Array.map (fun (x : Fletcher.ModeActives) -> x.ϕ) modeResult,
        name = sprintf "ϕ mode %i" m,
        mode = "lines+markers")

      let chartFreq = Scatter( 
        x = x_axis,
        y = Array.map (fun (x : Fletcher.ModeActives) -> x.ω) modeResult,
        name = sprintf "ω mode %i" m,
        mode = "lines+markers")

      [chartY; chartA; chartPhase; chartFreq ]

    List.zip (charts 1) (charts 2)
    |> List.iter (fun (ch1,ch2) -> 
      [ch1;ch2] |> Chart.Plot |> Chart.Show
    )

    Expect.isTrue true ""

let tests = testList "tests" [ rungekuttaTest; fletcher1999]