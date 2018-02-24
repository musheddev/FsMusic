namespace FMusic

module Types =

    open System.IO
    open System.Collections.Generic
    open System
    open FSharp.Core
    open DiffSharp
    open DiffSharp.Numerical.Float64
    open DiffSharp.AD
    open DiffSharp.AD.Float64
  
    //sample is float bound by -1.0 to 1.0 

    type Bitrate =
    | B24bit = 24
    | B32bit = 32
    | B16bit = 16

    type Samplerate =
    | k192 = 192000
    | k44 = 44000
    | k96 = 96000
    | k48 = 48000

    let freq f = (fun (time :D) -> f*time*(D 2.0*Math.PI))  //time to rad

    type Tone = {  //local time continous
                        time :D->D //sampler remap leave alone
                        freq :D->D  //time to freq
                        vel :D->D //time to vel
                        wave :Tone -> D ->D //rad to amp
                      }

    type Segment = { //global time discrate
                        start :D
                        tone :Tone
                        fin :D}

    type Section = { start :D 
                     segments: Segment list
                     fin :D}

    type Sectionfunc = Section -> Section
    
    type Tonefunc = (Tone -> Tone)


    type Mixer = Section list -> (D -> D) 

    type Render = Section list * Mixer * Samplerate -> DV

    type Filter = DV -> DV

    type Sound = DV

    type Audio = 
        | Mono of Sound
        | Stereo of Sound * Sound
        | Multi of Sound list


module Music =
    open System

    type Note = int * int

    [<Literal>]
    let trootoftwo = 1.0594630943592952645618252949463
    let private resolve (n :Note) (b :int) = (fst n)*b + (snd n)

    let EDO12 (note :Note) = 440.0*Math.Pow(trootoftwo,float (resolve note 12))

    let EDO53 (note :Note) = 
        let A0 = 440.0*float(fst note + (53/(snd note)) )  
        let range = match (53 % (snd note)) with
                    | 0 -> [1.0]
                    | 1 -> [81.0 / 80.0; 64.0/63.0]
                    | 2 -> [128.0 / 125.0]
                    | 3 -> [26.0/25.0;25.0 / 24.0]
                    | 4 -> [256.0 / 243.0; 135.0 / 128.0; 21.0/20.0]
                    | 5 -> [2187.0 / 2048.0; 16.0 / 15.0; 15.0/14.0 ]
                    | 6 -> [14.0/13.0;13.0/12.0;27.0 / 25.0]
                    | 7 -> [12.0 / 11.0; 800.0 / 729.0; 11.0/10.0]
                    | 8 -> [10.0/9.0]
                    | 9 -> [9.0/8.0]
                    | 10 -> [256.0/225.0; 8.0/7.0]
                    | 11 -> [15.0/13.0;144.0/125.0]
                    | 12 -> [7.0/6.0; 75.0/64.0]
                    | 13 -> [13.0/11.0;32.0/27.0]
                    | 14 -> [6.0/5.0]
                    | 15 -> [243.0/200.0; 11.0/9.0]
                    | 16 -> [16.0/13.0;100.0/81.0]
                    | 17 -> [5.0/4.0]
                    | 18 -> [81.0/64.0]
                    | 19 -> [32.0/25.0;9.0/7.0]
                    | 20 -> [125.0/96.0; 9.0/7.0]
                    | 21 -> [21.0/16.0; 320.0/243.0]
                    | 22 -> [4.0/3.0]
                    | 23 -> [27.0/20.0]
                    | 24 -> [15.0/11.0;512.0/375.0;11.0/8.0]
                    | 25 -> [25.0/18.0;7.0/5.0]
                    | 26 -> [45.0/32.0;]
                    | 27 -> [10.0/7.0;64.0/45.0]
                    | 28 -> [13.0/9.0]
                    | 29 -> [16.0/11.0;22.0/15.0]
                    | 30 -> [40.0/27.0]
                    | 31 -> [3.0/2.0]
                    | 32 -> [32.0/21.0;243.0/160.0;1024.0/675.0]
                    | 33 -> [20.0/13.0;192.0/125.0]
                    | 34 -> [14.0/9.0; 25.0/16.0]
                    | 35 -> [128.0/81.0]
                    | 36 -> [8.0/5.0]
                    | 37 -> [13.0/8.0;81.0/25.0]
                    | 38 -> [18.0/11.0;400.0/243.0]
                    | 39 -> [5.0/3.0]
                    | 40 -> [22.0/13.0;27.0/16.0]
                    | 41 -> [12.0/7.0]
                    | 42 -> [26.0/15.0;125.0/72.0]
                    | 43 -> [7.0/4.0]
                    | 44 -> [16.0/9.0]
                    | 45 -> [9.0/5.0]
                    | 46 -> [11.0/6.0;20.0/11.0;729.0/400.0]
                    | 47 -> [13.0/7.0;24.0/13.0;50.0/27.0] 
                    | 48 -> [15.0/8.0]
                    | 49 -> [40.0/21.0;243.0/128.0]
                    | 50 -> [48.0/25.0;27.0/14.0]
                    | 51 -> [125.0/64.0]
                    | 52 -> [160.0/81.0]
                    | _ -> []
        List.head range
