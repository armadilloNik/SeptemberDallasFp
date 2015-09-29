// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System;
open System.IO;

[<EntryPoint>]
let main argv = 
   
    let print something = printfn "%A" something 

    let padLeft  (count : int) (pad : char) (input : string)  = input.PadLeft(input.Length + count, pad)
    let padRight  (count : int) (pad : char)(input : string)  = input.PadRight(input.Length + count, pad)

    let padLeftUntil (count : int) (pad : char) (input : string) = input.PadLeft(count, pad)
    let padRightUntil (count : int) (pad : char) (input : string) = input.PadRight(count, pad)

    let rec slideGroup n sq =
        match Seq.length sq < n with
            | true -> Seq.empty  
            | false -> seq { 
                            yield Seq.take n sq 
                            yield! slideGroup n (Seq.skip 1 sq)   
                           }

    
    let neighborhoods (input : string) = 
        input |> slideGroup 3 
              |> Seq.map (String.Concat) 
 
    let int2Bin (x : int) = System.Convert.ToString(x, 2) |> padLeftUntil 8 '0'

    let bin2Display (bin : string) (n : int) = 
        if bin.Chars(n) = '1' 
        then "1" 
        else "_"

    let ruleFactory ruleNumber x =
        let bin = int2Bin ruleNumber

        match x with
        | "111" -> bin2Display bin 0
        | "11_" -> bin2Display bin 1
        | "1_1" -> bin2Display bin 2
        | "1__" -> bin2Display bin 3
        | "_11" -> bin2Display bin 4
        | "_1_" -> bin2Display bin 5
        | "__1" -> bin2Display bin 6
        | "___" -> bin2Display bin 7
        | _ -> "E" 

    let generationFactory ruleNumber input =
        let rule = ruleFactory ruleNumber
        input |> neighborhoods
              |> Seq.map (String.Concat >> rule)
              |> String.Concat
              |> padLeft 1 '_'
              |> padRight 1 '_'
 
    let rec giveLife ruleNumber input =
        let createGeneration = generationFactory ruleNumber
        let thisGeneration = createGeneration input

        seq { 
                yield thisGeneration
                yield! giveLife ruleNumber thisGeneration   
            }


    let showGenerations ruleNumber numberOfGenerations input =
        print input
        input |> giveLife ruleNumber
              |> Seq.take numberOfGenerations
              |> Seq.toArray
              |> Seq.iter print
   
    let generation0 = "1" |> padLeft 25 '_' |> padRight 25 '_'

    showGenerations 30 25 generation0
                             
    0 

   

