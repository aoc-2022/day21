open System.IO
open System

type Oper =
    | Number of int64
    | Div of string * string
    | Mul of string * string
    | Add of string * string
    | Sub of string * string
    | MulYou of string
    | DivByYou of string
    | DivYouBy of string
    | AddYou of string
    | MinusYou of string
    | YouMinus of string
    | MulYouC of int64
    | DivCByYou of int64
    | DivYouByC of int64
    | AddYouC of int64
    | CMinusYou of int64
    | YouMinusC of int64
    | MulC of string * int64
    | AddC of string * int64
    | CMinus of int64 * string
    | MinusC of string * int64
    | DivCBy of int64 * string
    | DivByC of string * int64
    | You

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
    member this.Monkeys = monkeys

    member this.IsDone() = cache.ContainsKey "root"

    member this.Step() =
        let toAdd (monkey: Monkey) =
            match monkey.Oper with
            | Div (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] / cache[m2])
            | Sub (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] - cache[m2])
            | Add (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] + cache[m2])
            | Mul (m1, m2) when cache.ContainsKey m1 && cache.ContainsKey m2 -> Some(monkey.Name, cache[m1] * cache[m2])
            | AddC (m1, i) when cache.ContainsKey m1 -> Some(monkey.Name, cache[m1] + i)
            | MulC (m1, i) when cache.ContainsKey m1 -> Some(monkey.Name, cache[m1] * i)
            | MinusC (m1, i) when cache.ContainsKey m1 -> Some(monkey.Name, cache[m1] - i)
            | CMinus (i, m1) when cache.ContainsKey m1 -> Some(monkey.Name, i - cache[m1])
            | DivByC (m1, i) when cache.ContainsKey m1 -> Some(monkey.Name, cache[m1] / i)
            | DivCBy (i, m1) when cache.ContainsKey m1 -> Some(monkey.Name, i / cache[m1])
            | _ -> None

        let adds =
            monkeys |> List.map toAdd |> List.filter Option.isSome |> List.map Option.get

        let rest = monkeys |> List.filter (fun m -> toAdd m |> Option.isNone)
        let full = [ cache |> Map.toSeq |> Seq.toList; adds ] |> List.concat |> Map.ofList
        State(full, rest)

    member this.EmbedYou() =
        let embed (monkey: Monkey) =
            match monkey.Oper with
            | Mul (m1, "humn") -> Monkey(monkey.Name, MulYou m1)
            | Mul ("humn", m1) -> Monkey(monkey.Name, MulYou m1)
            | Div (m1, "humn") -> Monkey(monkey.Name, DivByYou m1)
            | Div ("humn", m1) -> Monkey(monkey.Name, DivYouBy m1)
            | Add (m1, "humn") -> Monkey(monkey.Name, AddYou m1)
            | Add ("humn", m1) -> Monkey(monkey.Name, AddYou m1)
            | Sub (m1, "humn") -> Monkey(monkey.Name, MinusYou m1)
            | Sub ("humn", m1) -> Monkey(monkey.Name, YouMinus m1)
            | MulYou m1 when cache.ContainsKey m1 -> Monkey(monkey.Name, MulYouC cache[m1])
            | DivByYou m1 when cache.ContainsKey m1 -> Monkey(monkey.Name, DivCByYou cache[m1])
            | DivYouBy m1 when cache.ContainsKey m1 -> Monkey(monkey.Name, DivYouByC cache[m1])
            | AddYou m1 when cache.ContainsKey m1 -> Monkey(monkey.Name, AddYouC cache[m1])
            | MinusYou m1 when cache.ContainsKey m1 -> Monkey(monkey.Name, CMinusYou cache[m1])
            | YouMinus m1 when cache.ContainsKey m1 -> Monkey(monkey.Name, YouMinusC cache[m1])
            | Add (m1, m2) when cache.ContainsKey m1 -> Monkey(monkey.Name, AddC(m2, cache[m1]))
            | Add (m1, m2) when cache.ContainsKey m2 -> Monkey(monkey.Name, AddC(m1, cache[m2]))
            | Mul (m1, m2) when cache.ContainsKey m1 -> Monkey(monkey.Name, MulC(m2, cache[m1]))
            | Mul (m1, m2) when cache.ContainsKey m2 -> Monkey(monkey.Name, MulC(m1, cache[m2]))
            | Sub (m1, m2) when cache.ContainsKey m1 -> Monkey(monkey.Name, CMinus(cache[m1], m2))
            | Sub (m1, m2) when cache.ContainsKey m2 -> Monkey(monkey.Name, MinusC(m1, cache[m2]))
            | Div (m1, m2) when cache.ContainsKey m1 -> Monkey(monkey.Name, DivCBy(cache[m1], m2))
            | Div (m1, m2) when cache.ContainsKey m2 -> Monkey(monkey.Name, DivByC(m1, cache[m2]))
            | _ -> monkey

        let monkeys = monkeys |> List.map embed
        State(cache, monkeys)

    member this.SetValuesFromRoot() =
        let root = monkeys |> List.find (fun monkey -> monkey.Name = "root")

        match root.Oper with
        | AddC (m1, c) ->
            printfn $"* Setting {m1} to {c}"
            let cache = cache.Add(m1, c)
            // let monkeys = monkeys |> List.filter (fun m -> m.Name <> m1)
            State(cache, monkeys)
        | _ ->
            printfn $"### Can't set values from {root}"
            this

    member this.RevStep() =
        let revcache (monkey: Monkey) : Option<string * int64> =
            if cache.ContainsKey monkey.Name then
                printfn $"Cache has {monkey.Name}"
                let value = cache[monkey.Name]

                match monkey.Oper with
                | AddC (m1, i) -> Some(m1, value - i)
                | MulC (m1, i) when value <> 0 -> Some(m1, value / i)
                | MinusC (m1, i) -> Some(m1, i + value)
                | CMinus (i, m1) -> Some(m1, i - value)
                | DivByC (m1, c) -> Some(m1, value * c)
                | DivCBy (c, m1) when value <> 0 -> Some(m1, c / value)
                | AddYouC c -> Some("YOU", value - c)
                | YouMinusC c -> Some("YOU", value + c)
                | CMinusYou c -> Some("YOU", c - value)
                | MulYouC c when value <> 0 -> Some("YOU", value/c)
                | DivYouByC c -> Some("YOU",value*c)
                | DivCByYou c -> Some("YOU", c * value) 
                | _ -> None
            else
                // printfn $"Cache is missing: {monkey.Name}"
                None

        let updates =
            monkeys |> List.map revcache |> List.filter Option.isSome |> List.map Option.get

        let cache =
            [ cache |> Map.toSeq |> Seq.toList; updates ] |> List.concat |> Map.ofList
        // printfn $"updates: {updates}"
        State(cache, monkeys)
    member this.You = cache.TryFind "YOU"

    member this.Root() = cache.TryFind "root"

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

let state = input |> State.init

let rec stepAll (state: State) =
    if state.IsDone() then state else stepAll (state.Step())

let endState = stepAll state

printfn $"{endState} {endState.Root()}"

let replaceWithYou (monkey: Monkey) =
    if monkey.Name = "humn" then
        Monkey(monkey.Name, You)
    else
        monkey

let input2 = input |> List.map replaceWithYou

let rec stepMax (state: State) =
    let state2 = state.Step()

    if state2.ToString() = state.ToString() then
        state
    else
        stepMax state2

let state2 = State.init input2 |> stepMax

printfn $"state2 = {state2}"

let rec embed (state: State) : State =
    // state.Cache |> Map.toSeq |> Seq.toList |> List.map (printfn "%A")
    // state.Monkeys |> List.map (printfn "%A")
    printfn "STEP2: "
    let state = state.EmbedYou()
    let state = state.EmbedYou()
    // state.Monkeys |> List.map (printfn "%A")
    state

let state3 = embed state2
let state4 = state3.SetValuesFromRoot()
printfn $"{state4}"

let state5 = state4.RevStep()
let state6 = state5.RevStep()

printfn $"{state5} {state6}"

let rec revSteps (state: State) =
    let state2 = state.RevStep()

    if state2.ToString() = state.ToString() then
        state
    else
        revSteps state2

let stateT2 = revSteps state4

printfn $"FINAL {stateT2} {stateT2.You}"
