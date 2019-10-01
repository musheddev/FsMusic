namespace FMusic

[<AutoOpen>][<RequireQualifiedAccess>]
module Sound =

    open FMusic.Types
    open FMusic.Music
    open System

    let sine_wave = 
        (fun tone time -> 
            let t = tone.time time
            let f = tone.freq time
            let v = tone.vel time
            let amp = (freq f t) |> D.Sin
            amp*v)
        

    let evelope (attack :float) (sustain :float) (delay :float) (release :float)  =
        (fun tone -> {tone with vel=(fun x -> D.One - x/(D attack))} ) //not done
                        

    let set_vel (vol:float) :Tonefunc =  (fun x -> {x with vel=(fun y ->vol)} )

    let adjust_vel (vol:float) :Tonefunc = (fun x -> {x with vel=(fun y -> y * vol) >> x.vel } )



    let shift_time (len :float) :Sectionfunc = (fun x -> { x with start=x.start+len} )
    
    let additive_mixer :Mixer= 
        (fun sections -> 
            (fun time -> seq {for sec in sections do
                                 if time >= sec.start && sec.fin >= time then
                                     for seg in sec.segments do 
                                        if time >= seg.start && seg.fin >= time then
                                            yield seg.tone.wave seg.tone time 
                             } |> Seq.sum) )



    let render (mixer :Mixer) (sections :Section list) (samplerate :Samplerate) (start :float option) (length :float option)= 
        let fn = mixer sections
        let strt = match start with | Some(x) -> x | None -> 0.0
        let len = match length with | Some(x) -> x | None -> sections |> List.fold (fun max x -> if float x.fin > max then float x.fin else max) 0.0
        let tic = 1.0/(float samplerate)
        
        let mutable smp = strt
        let mutable ls = []
        while smp < strt + len  do
             ls <- (fn (D smp)) :: ls
             smp <- smp + tic
        ls |> List.toArray |> Array.rev |> DV.ofArray 