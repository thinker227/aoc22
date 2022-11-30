using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
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

        if (!int.TryParse(args[0], out var day))
        {
            Console.WriteLine($"Could not parse '{args[0]}' to a day.");
            return;
        }
        
        var days = GetDays();
        if (!days.TryGetValue(day, out var solverType))
        {
            Console.WriteLine($"Could not find a solver for day {day}.");
            return;
        }

		string input = args[1];
		if (File.Exists(input))
		{
			input = File.ReadAllText(input);
		}

        var solver = (CombinedSolution)Activator.CreateInstance(solverType)!;
        
        var result = solver.Solve(input);

        Console.WriteLine(GetResultString(result));
    }

	private static IReadOnlyDictionary<int, Type> GetDays() =>
        Assembly.GetExecutingAssembly()
			.GetTypes()
			.Where(type => type.Namespace == "AoC22.Solutions")
			.Where(type => type.IsAssignableTo(typeof(CombinedSolution)))
			.ToDictionary(type =>
			{
				var match = MyRegex().Match(type.Name);
				return int.Parse(match.Value);
			});

	[GeneratedRegex("([0-9]+)$")]
	private static partial Regex MyRegex();

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
