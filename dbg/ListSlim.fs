namespace global

open System

type internal ListSlim<'k> =
    val mutable private count : int
    val mutable private entries : 'k[]
    new() = {count=0; entries=Array.empty}
    new(capacity:int) = {count = 0; entries = Array.zeroCreate capacity}

    member m.Count = m.count

    member m.Item
        with get i = m.entries.[i]
        and set i v = m.entries.[i] <- v

    member m.Add(key:'k) =
        let i = m.count
        if i = m.entries.Length then
            if i = 0 then
                m.entries <- Array.zeroCreate 4
            else
                let newEntries = i * 2 |> Array.zeroCreate
                Array.Copy(m.entries, 0, newEntries, 0, i)
                m.entries <- newEntries
        m.entries.[i] <- key
        m.count <- i+1
        i

    member m.ToArray() =
        Array.init m.count (Array.get m.entries)

    member m.ToList() =
        List.init m.count (Array.get m.entries)

    member m.ToSeq() =
        Seq.init m.count (Array.get m.entries)

    member m.AddSort(key:'k) =
        let i = m.count
        if i = m.entries.Length then
            if i = 0 then
                m.entries <- Array.zeroCreate 4
            else
                let newEntries = i * 2 |> Array.zeroCreate
                Array.Copy(m.entries, 0, newEntries, 0, i)
                m.entries <- newEntries
        let j = abs(1 + Array.BinarySearch(m.entries, 0, i, key))
        Array.Copy(m.entries, j, m.entries, j+1, i-j)
        m.entries.[j] <- key
        m.count <- i+1