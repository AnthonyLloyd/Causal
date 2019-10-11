using Microsoft.FSharp.Core;

class Program
{
    static void Main(string[] _)
    {
        Dbg.Write("Starting Fasta Profiling");

        Perf.causalProfiling(300, 
            FuncConvert.FromAction(() => Fasta.Run(new[] { "25000000" })));

        Dbg.Write("Finished Fasta Profiling");
    }
}
