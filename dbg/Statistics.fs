namespace global

open System

type Estimate = Estimate of median:float * error:float
with
    static member (-)(a:float,Estimate(m,e)) = Estimate(a-m, e)
    static member (-)(Estimate(m,e),a:float) = Estimate(m-a, e)
    static member (-)(Estimate(m1,e1),Estimate(m2,e2)) =
        Estimate(m1-m2, sqrt(sqr e1+sqr e2))
    static member (*)(Estimate(m,e),a:float) = Estimate(m*a, e*a)
    static member (/)(Estimate(m1,e1),Estimate(m2,e2)) =
        Estimate(m1/m2, sqrt(sqr(e1/m1)+sqr(e2/m2))*abs(m1/m2))
    override e.ToString() =
        let (Estimate(m,e)) = e
        (max m -99.9 |> min 99.9).ToString("0.0").PadLeft 5 + " ±" +
        (min e 99.9).ToString("0.0").PadLeft 4

module internal Statistics =

    let inline median (l:ListSlim<_>) (i:int) (j:int) =
        if i+j % 2 = 0 then float l.[(i+j)/2]
        else float(l.[(i+j)/2] + l.[(i+j)/2+1]) * 0.5

    let error (list:ListSlim<int>) (i:int) (j:int) (median:float) =
        let mutable s2 = 0.0
        for k = i to j do
            s2 <- s2 + sqr(float list.[k]-median)
        sqrt(s2/float((j-i+1)*(j-i))) * 1.253

    let mad (list:ListSlim<int>) (m:float) =
        let deviations = Array.zeroCreate list.Count
        for i = 0 to deviations.Length-1 do
            deviations.[i] <- abs(float list.[i]-m)
        Array.sortInPlace deviations
        let l = deviations.Length
        if l % 2 = 0 then (deviations.[l/2] + deviations.[l/2-1]) * 0.5
        else deviations.[l/2]

    let estimate (list:ListSlim<int>) =
        let count = list.Count
        let m = median list 0 (count-1)
        let mad = mad list m

        let lowerBound = m - 3.0 * mad
        let mutable i = 0
        while float list.[i] < lowerBound do i <- i + 1

        let upperBound = m + 3.0 * mad
        let mutable j = count - 1
        while float list.[j] > upperBound do j <- j - 1

        let median = median list i j
        Estimate(median, error list i j median)