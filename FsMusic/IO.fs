namespace FMusic

[<AutoOpen>][<RequireQualifiedAccess>]
module IO =

    open System.IO
    open System.Collections.Generic
    open System
    open FSharp.Core
    open Types

    let private float_to_bytes (b :BinaryWriter) (cnt :int ref) (w :Bitrate) (sample :float) = 
        match (int w) with
        | 32 -> b.Write (int32 (sample * (float Int32.MaxValue)) )
                cnt := !cnt + 4
        | 16 -> let x = (int16 (sample * (float Int16.MaxValue)) )
                b.Write x
                cnt := !cnt + 2
        | 24 -> b.Write (Array.map (fun x -> (byte (int64 (sample * float Int64.MaxValue)  >>> (64 - x*8))))  [|1;2;3|] ) // take most significant bits THIS????
                cnt := !cnt + 3


    let private write (a :Audio) (w :Bitrate) (b :BinaryWriter) =
        let byte_cnt = ref 0
        let toarray v=  (* (((DV.Normalize v) - 0.5) * 2.0 ) |>*) DV.toArray v
        match a with
          | Mono sound -> sound |> toarray |> Seq.iter (fun v -> v |> float |> float_to_bytes b byte_cnt w )
          | Stereo(left, right) -> (Array.zip (toarray left) (toarray right)) |> Seq.iter (fun (l,r) -> l |> float |> float_to_bytes b byte_cnt w
                                                                                                        r |> float |> float_to_bytes b byte_cnt w )
          | Multi channels -> let chnls = List.map toarray channels
                              let length = channels |> List.fold (fun st x -> min st x.Length ) Int32.MaxValue
                              for l in [|0..(length-1)|] do
                                for ch in [|0..(channels.Length-1)|] do
                                    chnls.Item(ch) |> (Array.item l) |> float |> float_to_bytes b byte_cnt w
        !byte_cnt
          
    let private writeWavHeader samplingRate numChannels bitrate numBytes (writer : System.IO.BinaryWriter) = 
        let chunkID = "RIFF"B
        let chunkSize = 36 + numBytes // * seek and update after numBytes is known
        let format = "WAVE"B
        let subChunk1ID = "fmt "B
        let subChunk1Size = 16
        let audioFormat = 1s // only support PCM at the moment
        let nc = int16 numChannels
        let bitsPerSample = int16 (int bitrate)
        let blockAlign = int16 (numChannels * (int bitrate / 8)) 
        let byteRate = samplingRate * numChannels * (int bitrate /8) 
        let subChunk2Id = "data"B
        let subChunk2Size = numBytes // * seek and update after numBytes is known
        writer.Write(chunkID) // 0
        writer.Write(chunkSize) // 4 (*)
        writer.Write(format) // 8
        writer.Write(subChunk1ID) // 12
        writer.Write(subChunk1Size) // 16
        writer.Write(audioFormat) // 20
        writer.Write(nc) // 22
        writer.Write(samplingRate : int) // 24
        writer.Write(byteRate) // 28
        writer.Write(blockAlign) // 32
        writer.Write(bitsPerSample) // 34
        writer.Write(subChunk2Id) // 36
        writer.Write(subChunk2Size) // 40 (*)

    let audio_to_wave (a :Audio) (w :Bitrate) (path :string) (samplerate :int) =
        use fileStream = new System.IO.FileStream(path, System.IO.FileMode.Create)
        use writer = new System.IO.BinaryWriter(fileStream)
        let chnl_cnt = match a with
                        | Mono(_) -> 1
                        | Stereo(_,_) -> 2
                        | Multi(x) -> x.Length
       
        // write header
        writeWavHeader samplerate chnl_cnt w 0 writer
        // pack and write the stream
        let byte_cnt = write a w writer
        // now we should know the number of bytes
        fileStream.Seek(4L, SeekOrigin.Begin) |> ignore
        writer.Write(36 + byte_cnt)
        fileStream.Seek(32L, SeekOrigin.Current) |> ignore
        writer.Write(byte_cnt)