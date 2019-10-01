// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module execute

open FMusic
open FMusic.Types
open FMusic.Music


let instrement f decay =
    let wavey = (fun s t -> Sound.sine_wave s t |> D.Sqrt )
    {time=(fun y -> y);freq=(fun _ -> f); vel=decay; wave=wavey }

let test_one name =
    let chromatic_scale = [|0 .. 12|] |> Array.map (fun x -> 
        let freq = D (EDO12 (0,x))
        let dx = x |> float |> D
        let map_local (t :D) = t - float (int (float t))
        let decay t = D.One - t 
        let ins = instrement freq (map_local >> decay)
        {start=D.Zero+dx; fin=D.One+dx; tone=ins}) |> Array.toList
    let c_sect = [{start=D.Zero; segments=chromatic_scale; fin=(D 12.0)}]

    let render = Sound.render Sound.additive_mixer c_sect (Samplerate.k44) None None
    let audio = Mono(render)
    IO.audio_to_wave audio Bitrate.B16bit name (Samplerate.k44 |> int)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    test_one "test.wav"
    0 // return an integer exit code
