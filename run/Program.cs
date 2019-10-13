using Microsoft.FSharp.Core;

class Program
{
    static void Main(string[] _)
    {
        var n = 200;

        Dbg.Write("Starting Fasta Profiling Simple");

        PerfSimple.causalProfiling(n, 
            FuncConvert.FromAction(() => Fasta.Run(new[] { "25000000" })));

        Dbg.Write("Starting Fasta Profiling Full");

        Perf.causalProfiling(n,
            FuncConvert.FromAction(() => Fasta.Run(new[] { "25000000" })));

        Dbg.Write("Finished Fasta Profiling");
    }
}
