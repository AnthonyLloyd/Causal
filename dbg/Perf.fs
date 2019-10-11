namespace global

open System
open System.Threading
open System.Diagnostics

module Perf =
    [<Struct>]
    type Region = Region of string * int64 * totalDelay: int64 * onSince: int64
    [<Struct>]
    type private PerfRun = Nothing | CollectTimes | Delay
    let mutable private perfRun = Nothing
    let mutable private delayName = null
    let mutable private delayTime = 0
    let mutable private lock = SpinLock false
    let mutable private times = ListSlim()
    let mutable private delayCount = 0
    let mutable private onSince = 0L
    let mutable private totalDelay = 0L

    let regionStart (name:string) =
        if perfRun = Nothing then Region(null, 0L, 0L, 0L)
        else
            let now = Stopwatch.GetTimestamp()
            let mutable lockTaken = false
            lock.Enter &lockTaken
            if delayName=name && delayTime<0 then
                if delayCount = 0 then
                    if onSince <> 0L then failwithf "um %i" onSince
                    onSince <- now
                delayCount <- delayCount + 1
            let pr = Region (name, now, totalDelay, onSince)
            if lockTaken then lock.Exit false
            pr

    let regionEnd (Region (name,start,startTotalDelay,startOnSince)) =
        if perfRun = Nothing then ()
        else
            let now = Stopwatch.GetTimestamp()
            let mutable lockTaken = false
            lock.Enter &lockTaken
            if perfRun = CollectTimes then
                times.Add struct (name,int(now-start)) |> ignore
                if lockTaken then lock.Exit false
            else
                if delayTime<0 then
                    if name=delayName then
                        delayCount <- delayCount - 1
                        if delayCount = 0 then
                            totalDelay <- totalDelay
                                + (now-onSince) * int64 delayTime / -100L
                            onSince <- 0L
                        if lockTaken then lock.Exit false
                    else
                        let wait =
                            now + totalDelay - startTotalDelay +
                            ((if onSince=0L then 0L else now-onSince)
                             +  if startOnSince=0L then 0L
                                else startOnSince-start
                            ) * int64 delayTime / -100L
                        if lockTaken then lock.Exit false
                        while Stopwatch.GetTimestamp() < wait do ()
                elif delayTime>0 && name=delayName then
                    if lockTaken then lock.Exit false
                    let wait = now + (now-start) * int64 delayTime / 100L
                    while Stopwatch.GetTimestamp() < wait do ()
                else
                    if lockTaken then lock.Exit false

    let causalProfiling n (f:unit->unit) =
        printfn "Causal profiling..."
        let run (d:PerfRun,dName:string,dTime:int) =
            perfRun <- d
            delayName <- dName
            delayTime <- dTime
            onSince <- 0L
            delayCount <- 0
            totalDelay <- 0L
            let start = Stopwatch.GetTimestamp()
            f()
            int(Stopwatch.GetTimestamp() - start - totalDelay)
        let clear() = times <- ListSlim times.Count
        run (CollectTimes,null,0) |> ignore
        clear()
        let summary =
            let totalTimePct =
                float(run (CollectTimes,null,0) * Environment.ProcessorCount)
                * 0.01
            times.ToSeq()
            |> Seq.groupBy (fun struct (n,_) -> n)
            |> Seq.map (fun (r,s) ->
                r,  {|
                        Region = r
                        Count = Seq.length s
                        Time = float(Seq.sumBy (fun struct (_,t) -> t) s)
                               / totalTimePct
                    |} )
            |> dict

        clear()

        let delays = [|
            Delay,null,0
            for i in [|5;-5;10;-10;-15;-20|] do
                for n in summary.Keys do
                    Delay,n,i
        |]
        let results = Seq.map (fun i -> i, ListSlim()) delays |> dict
        let median d = Statistics.estimate results.[Delay,fst d,snd d]

        let createReport() =
            let totalPct = median(null,0) * 0.01
            "| Region         |  Count  |  Time%  |     +10%     \
             |      +5%     |      -5%     |     -10%     |     -15%     \
             |     -20%     |\n|:---------------|--------:|--------:\
             |-------------:|-------------:|-------------:|-------------:\
             |-------------:|-------------:|      \n"
            + (summary |> Seq.map (fun (KeyValue(name,s)) ->
                "| " + name.PadRight 14 +
                " | " + s.Count.ToString().PadLeft 7 +
                " | " + s.Time.ToString("0.0").PadLeft 7 +
                " | " + string(100.0 - median(name, 10)/totalPct) +
                "  | " + string(100.0 - median(name,  5)/totalPct) +
                "  | " + string(100.0 - median(name, -5)/totalPct) +
                "  | " + string(100.0 - median(name,-10)/totalPct) +
                "  | " + string(100.0 - median(name,-15)/totalPct) +
                "  | " + string(100.0 - median(name,-20)/totalPct) +
                "  |\n"
              ) |> System.String.Concat)

        for i = 1 to n do
            stdout.Write " iteration progress...  0% "
            for j = 0 to delays.Length-1 do
                let d = delays.[j]
                run d |> results.[d].AddSort
                "\u001b[29D iteration progress..." +
                string(4*(j+1)).PadLeft 3 + "% " |> stdout.Write
            [|
                if i>1 then "\u001b["; string(summary.Count+3); "F"
                else "\u001b[29D"
                "Iterations: "; string i; "                 \n"
                createReport()
            |] |> System.String.Concat |> stdout.Write
            stdout.Write "                             \u001b[29D"

        perfRun <- Nothing