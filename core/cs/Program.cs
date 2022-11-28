using System.Reflection;
using System.Text.RegularExpressions;

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

    private static string GetResultString(CombinedSolutionResult result) =>
		(result.Part1.Kind, result.Part2.Kind) switch
		{
			(SolutionKind.None, SolutionKind.None) => "",
			(_, SolutionKind.None) => GetResultString(result.Part1),
			(SolutionKind.None, _) => "\n" + GetResultString(result.Part2),
			_ => $"{GetResultString(result.Part1)}\n{GetResultString(result.Part2)}"
		};

    private static string GetResultString(SolutionResult result) => result.Kind switch
    {
        SolutionKind.Characters => new string(result.Characters),
        SolutionKind.Int => result.Int.ToString(),
        SolutionKind.ULong => result.ULong.ToString(),
        SolutionKind.BigInteger => result.BigInteger.ToString(),
        
        _ => ""
    };
}
