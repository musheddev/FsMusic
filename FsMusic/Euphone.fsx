module Test 

[<Measure>] type sec
[<Measure>] type hz = 1/sec
[<Measure>] type m
[<Measure>] type cm
[<Measure>] type GPa
[<Measure>] type kg
[<Measure>] type lbs
[<Measure>] type In
[<Measure>] type kgf


let GPa_to_Lbsperinsq = 145038.0<(lbs/In^2) / GPa>
let kgpermcub_to_lbsperincub = 0.0000361

let kgf_to_kgmpersec2 = 9.80665<(kg*m/(sec^2)) / kgf>
let GPa_to_kgfperm2 = 101971621.29779<(kgf/m^2) / GPa>

let pi = 3.1415926
let g = 386.4<In/sec^2>

let target_freq = 220<hz>
let E_alum = 69.0<GPa>
let E_steel = 203.0<GPa>
let E_steel_english = 3.0e7<lbs/(In^2)>
let p_alum = 2720<kg/m^3>
let p_steel = 7850.0<kg/m^3>
let p_steel_english = 7.33e-4<lbs*sec^2/In^4>
let p_steel_english_2 = p_steel_english * g
let poisson_alum = 0.35
let poisson_steel = 0.287

//0.11811 in = 3mm

let steel_english = E_steel_english,p_steel_english,poisson_steel

let wn_steel (thickness : float<In>,diameter : float<In>) = 
    let E = E_steel_english
    let p = p_steel_english
    let q : float<lbs> = E * thickness * thickness
    let K = 1.0 - poisson_steel * poisson_steel
    1.01 * sqrt (q / (p * diameter * diameter * diameter * diameter * K)) 

let test_1 = wn_steel(0.11811<In>,8.0<In>)
printfn "test1 square %f" test_1





let wn_steel_2 (thickness : float<In>, length : float<In>, width : float<In>) = 
    let E = E_steel_english
    let p = p_steel_english
    let q : float<lbs> = E * thickness * thickness
    let K = 1.0 - poisson_steel * poisson_steel
    1.01 * sqrt (q / (p * length * length * width * width * K)) 

let test_2 = wn_steel_2(0.11811<In>,8.0<In>,4.0<In>)
printfn "%f" test_2

let wn_steel_rod (thickness : float<m>, length : float<m>, width : float<m>) = 
    let E = E_steel * GPa_to_kgfperm2 * kgf_to_kgmpersec2
    let CB = sqrt (E / p_steel)
    let K = thickness
    (pi / 8.0) * CB * (K / (length * length)) * (1.1937*1.1937) 

let test_3 = wn_steel_rod(0.003<m>,0.12<m>,4.0<m>)
printfn "%f" test_3


let test4_props = 2.8e6<lbs/In^2>,0.1<lbs/In^3>,0.2
let test4_steel = E_steel_english, p_steel_english_2, poisson_steel


// let c_n (h : float<In>, length : float<In>, width : float<In>) =
//     let y1 = p_steel_english
//     let u = poisson_steel 
//     let E = E_steel_english
//     let Q = sqrt (12.0 * y1 * (1.0 - u * u) / (E * h *h))
//     length * width * Q

let ``D/p`` (E : float<lbs/In^2>,p : float<lbs/In^3>,u) (h : float<In>) =
    (E * h * h * g) / (12.0 * p * (1.0 - u * u))
    


let wn_test_4 (E,p,u) (thickness : float<In>, length : float<In>, width : float<In>) = 
    let c4 = 0.637
    let c1 = 0.314
    let a = length
    let b = width
    let Q = ``D/p`` (E,p,u) (thickness)
    c4 * (pi / 2.0) * sqrt ( Q * (c1 / (a * a * a * a)) )

let test4 = wn_test_4 test4_props (0.06<In>,6.0<In>,3.0<In>)
printfn "test4 D/p = %f should equal 3374569" (``D/p`` test4_props 0.06<In>)
printfn "test4 D/p = %f should equal 3132586" (``D/p`` (2e6<lbs/In^2>,0.208<lbs/In^3>,0.12) 0.1<In>)
printfn "test4 %f should equal 28.6" test4

let test4_square = wn_test_4 test4_steel (0.11811<In>,8.0<In>,4.0<In>)
printfn "test4 rect %f" test4_square



let len_from_hz (E,p,u) (thickness :float<In>) (width : float<In>) (freq : float</sec>) =
    let c4 = 0.637
    let c1 = 0.314
    let Q = ``D/p`` (E,p,u) (thickness)
    let inner = (Q * c1 * pi * pi * c4 * c4) / (4.0 * freq * freq)
    sqrt (sqrt inner)


let result = len_from_hz test4_steel 0.11811<In> 4.0<In> 220.0</sec>
printfn "ouput: %f" result

let len_from_hz_d_clamp (E,p,u) (thickness :float<In>) (width : float<In>) (freq : float</sec>) =
    let c4 = 0.637
    let c1 = 12.603
    let Q = ``D/p`` (E,p,u) (thickness)
    let inner = (Q * c1 * pi * pi * c4 * c4) / (4.0 * freq * freq)
    sqrt (sqrt inner)

let result2 = len_from_hz_d_clamp test4_steel 0.11811<In> 4.0<In> 220.0</sec>
printfn "ouput2: %f" result2

let result3 = len_from_hz test4_steel 0.19<In> 0.0<In> 220.0</sec>
printfn "ouput3: %f" result3

let result4 = len_from_hz_d_clamp test4_steel 0.11811<In> 0.0<In> 160.0</sec>
printfn "ouput4: %f" result4

//8 3/16 in

let wn (c1,c2,c3,c4) (E,p,u) (thickness :float<In>) (width : float<In>) (length : float<In>) =
    let a = length
    let b = width
    let Q = ``D/p`` (E,p,u) (thickness)
    c4 * (pi / 2.0) * sqrt ( Q *( (c1 / (a*a*a*a)) + c2/(a*a*b*b) + c3/(b*b*b*b) ))

let free = (0.0,2.08,0.0,1.0)
let clamp_b = (0.314,0.0,0.0,0.637)
let clamp_b_dual = (12.603,0.0,0.0,0.637)
let support_b_dual = (1.0,0.0,0.0,1.0)


let freq_free_1 = wn free test4_steel 0.11811<In> 3.0<In> 8.0<In>
printfn "open: %f" freq_free_1

let alum = E_alum,p_alum,poisson_alum
let freq_free_2 = wn free alum 0.11811<In> 3.0<In> 11.0<In>

//358mm
//179mm
//anti nodes 80.2mm from end

//160hz result in A

let cofactor_A = 160.0*358.0*358.0

let len_220_A = sqrt(cofactor_A/220.0)
//305mm
//anti-nodes 68.3
//282 result in A