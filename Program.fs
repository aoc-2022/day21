open System.IO

let parse (s:string) =
    match s.Split [|' '|] with
    | s -> s
let input = File.ReadAllLines "/tmp/aoc/input.t" |> Seq.map parse |> Seq.toList

input |> List.map (printfn "%A")