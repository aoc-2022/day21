open System.IO
open System

type Oper =
    | Number of int64
    | Div of string * string
    | Mul of string * string
    | Add of string * string
    | Sub of string * string

type Monkey(name: string, oper: Oper) =
    member this.Name = name
    member this.Oper = oper
    override this.ToString() = $"Monkey({name} {oper}"

let (|Int|_|) (s: string) =
    match Int64.TryParse s with
    | true, value -> Some(value)
    | false, _ -> None

let parse (s: string) =
    printfn $"{s}"

    match s.Split [| ' '; ':' |] |> Array.toList with
    | [ name; _; Int n ] -> Monkey(name, Number n)
    | [ name; _; m1; "*"; m2 ] -> Monkey(name, Mul(m1, m2))
    | [ name; _; m1; "+"; m2 ] -> Monkey(name, Add(m1, m2))
    | [ name; _; m1; "-"; m2 ] -> Monkey(name, Sub(m1, m2))
    | [ name; _; m1; "/"; m2 ] -> Monkey(name, Div(m1, m2))

let input = File.ReadAllLines "/tmp/aoc/input" |> Seq.map parse |> Seq.toList

input |> List.map (printfn "%A")

type State(cache: Map<string, int64>, monkeys: Monkey list) =
    member this.Cache = cache

    member this.IsDone() = cache.ContainsKey "root"

    member this.Step() =
        let toAdd (monkey: Monkey) =
            match monkey.Oper with
            | Div (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] / cache[m2])
            | Sub (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] - cache[m2])
            | Add (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] + cache[m2])
            | Mul (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] * cache[m2])
            | _ -> None

        let adds =
            monkeys |> List.map toAdd |> List.filter Option.isSome |> List.map Option.get

        let rest = monkeys |> List.filter (fun m -> toAdd m |> Option.isNone)
        let full = [ cache |> Map.toSeq |> Seq.toList; adds ] |> List.concat |> Map.ofList
        State(full, rest)
    member this.Root () =
        cache.TryFind "root"

    static member init(monkeys: Monkey list) =
        let toNum (m: Monkey) : Option<string * int64> =
            match m.Oper with
            | Number num -> Some(m.Name, num)
            | _ -> None

        let toNonNum (m: Monkey) : Option<Monkey> =
            match m.Oper with
            | Number _ -> None
            | _ -> Some(m)

        let cache =
            monkeys
            |> List.map toNum
            |> List.filter Option.isSome
            |> List.map Option.get
            |> Map.ofList

        let rest =
            monkeys |> List.map toNonNum |> List.filter Option.isSome |> List.map Option.get

        State(cache, rest)

    override this.ToString() =
        $"Cache({cache.Count} {monkeys.Length} done={this.IsDone()})"

let state = State.init input
let state2 = state.Step()
let state3 = state2.Step()

printfn $"{state}"
printfn $"{state2}"
printfn $"{state3}"

let rec stepAll (state:State) =
    if state.IsDone () then state
    else stepAll (state.Step ())

let endState = stepAll state

printfn $"{endState} {endState.Root ()}"