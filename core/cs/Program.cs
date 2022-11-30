using System.Text;
using AoC22.Utility;

namespace AoC22.Core;

internal static partial class Program
{
    private static void Main(string[] args)
    {
        if (args.Length < 2)
        {
            Console.WriteLine("""
            Usage: <day> <input>

            Arguments:
            <day>       The day to run the solver for.
            <input>     The input to the solver,
                        or the file path to a file
                        containing the input.
            """);
			return;
        }

        if (!int.TryParse(args[0], out var day) ||
			!Days.Solutions.TryGetValue(day, out var solverFactory))
        {
            Console.WriteLine($"Unknown day {args[0]}");
            return;
        }

		string input = args[1];
		if (File.Exists(input))
		{
			input = File.ReadAllText(input);
		}
        
		var solver = solverFactory();
        var result = solver.Solve(input);

        Console.WriteLine(GetResultString(result));
    }

    private static string GetResultString(CombinedSolutionResult result)
    {
        StringBuilder builder = new();

        if (result.Part1.Kind != SolutionKind.None)
        {
            builder.AppendLine(GetResultString(result.Part1));
        }

        if (result.Part2.Kind != SolutionKind.None)
        {
            builder.AppendLine(GetResultString(result.Part2));
        }

        return builder.ToString();
    }

    private static string GetResultString(SolutionResult result) => result.Kind switch
    {
        SolutionKind.Characters => new string(result.Characters),
        SolutionKind.Int => result.Int.ToString(),
        SolutionKind.ULong => result.ULong.ToString(),
        SolutionKind.BigInteger => result.BigInteger.ToString(),
        
        _ => ""
    };
}
