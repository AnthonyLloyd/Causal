namespace global

open System
open System.Threading
open System.Diagnostics

module PerfSimple =
    [<Struct>]
    type Region = Region of string * int64
    [<Struct>]
    type private PerfRun = Nothing | CollectTimes | Delay
    let mutable private perfRun = Nothing
    let mutable private delayName = null
    let mutable private delayTime = 0
    let mutable private lock = SpinLock false
    let mutable private times = ListSlim()

    let regionStart (name:string) =
        if perfRun = Nothing then Region(null, 0L)
        else Region (name, Stopwatch.GetTimestamp())

    let regionEnd (Region (name,start)) =
        if perfRun = Nothing then ()
        else
            let now = Stopwatch.GetTimestamp()
            if perfRun = CollectTimes then
                let mutable lockTaken = false
                lock.Enter &lockTaken
                times.Add struct (name,int(now-start)) |> ignore
                if lockTaken then lock.Exit false
            elif delayTime<>0 && ((delayTime < 0) <> (delayName = name)) then
                let wait = now + (now-start) * int64(abs delayTime) / 100L
                while Stopwatch.GetTimestamp() < wait do ()

    let causalProfiling n (f:unit->unit) =
        printfn "Causal profiling..."
        let run (d:PerfRun,dName:string,dTime:int) =
            perfRun <- d
            delayName <- dName
            delayTime <- dTime
            let start = Stopwatch.GetTimestamp()
            f()
            int(Stopwatch.GetTimestamp() - start)
        let clear() = times <- ListSlim times.Count
        run (CollectTimes,null,0) |> ignore
        clear()

        let times() =
            let totalTimePct =
                float(run (CollectTimes,null,0) * Environment.ProcessorCount)
                * 0.01
            let summaries =
                times.ToSeq()
                |> Seq.groupBy (fun struct (n,_) -> n)
                |> Seq.map (fun (r,s) ->
                    {|
                        Region = r
                        Count = Seq.length s
                        Time = float(Seq.sumBy (fun struct (_,t) -> t) s)
                                / totalTimePct
                    |} )
                |> Seq.toList
            clear()
            summaries

        let summary =
            Seq.init 6 (ignore >> times)
            |> Seq.concat
            |> Seq.groupBy (fun i -> i.Region)
            |> Seq.map (fun (r,s) ->
              r,{|
                  Region = r
                  Count = (s |> Seq.sumBy (fun i -> i.Count)) / Seq.length s
                  Time = s |> Seq.map (fun i -> i.Time) |> Seq.medianNoOutliers
                |} )
            |> dict

        let delays = [|
            Delay,null,0
            Delay,null, -5
            Delay,null,-10
            Delay,null,-15
            Delay,null,-20
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
               " | "  + string(100.0 - median(name, 10)/totalPct) +
               "  | " + string(100.0 - median(name,  5)/totalPct) +
               "  | " + string((median(null, -5)-median(name, -5))/totalPct) +
               "  | " + string((median(null,-10)-median(name,-10))/totalPct) +
               "  | " + string((median(null,-15)-median(name,-15))/totalPct) +
               "  | " + string((median(null,-20)-median(name,-20))/totalPct) +
               "  |\n"
              ) |> System.String.Concat)

        for i = 1 to n do
            stdout.Write " iteration progress...  0% "
            for j = 0 to delays.Length-1 do
                let d = delays.[j]
                run d |> results.[d].AddSort
                "\u001b[29D iteration progress..." +
                string(100*(j+1)/29).PadLeft 3 + "% " |> stdout.Write
            [|
                if i>1 then "\u001b["; string(summary.Count+3); "F"
                else "\u001b[29D"
                "Iterations: "; string i; "                 \n"
                createReport()
            |] |> System.String.Concat |> stdout.Write
            stdout.Write "                             \u001b[29D"

        perfRun <- Nothing