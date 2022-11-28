using System.Numerics;

namespace AoC22.Core;

/// <summary>
/// The result of an individual part of a solution.
/// </summary>
public readonly ref struct SolutionResult
{
	public required SolutionKind Kind { get; init; }
	public ReadOnlySpan<char> Characters { get; init; }
	public int Int { get; init; }
	public ulong ULong { get; init; }
	public BigInteger BigInteger { get; init; }

	public static implicit operator SolutionResult(ReadOnlySpan<char> characters) => new()
	{
		Kind = SolutionKind.Characters,
		Characters = characters
	};

	public static implicit operator SolutionResult(string @string) =>
		(SolutionResult)@string.AsSpan();

	public static implicit operator SolutionResult(int @int) => new()
	{
		Kind = SolutionKind.Characters,
		Int = @int
	};

	public static implicit operator SolutionResult(ulong @ulong) => new()
	{
		Kind = SolutionKind.Characters,
		ULong = @ulong
	};

	public static implicit operator SolutionResult(BigInteger bigInteger) => new()
	{
		Kind = SolutionKind.Characters,
		BigInteger = bigInteger
	};
}

/// <summary>
/// The combined result of both parts of a solution.
/// </summary>
public readonly ref struct CombinedSolutionResult
{
	public readonly SolutionResult Part1;
	public readonly SolutionResult Part2;

	public CombinedSolutionResult(SolutionResult part1, SolutionResult part2)
	{
		Part1 = part1;
		Part2 = part2;
	}
}

/// <summary>
/// The kind of a solution.
/// </summary>
public enum SolutionKind
{
	None,
	Characters,
	Int,
	ULong,
	BigInteger
}

/// <summary>
/// A solution which solves both parts in a single method.
/// </summary>
public abstract class CombinedSolution
{
	/// <summary>
	/// Solves both parts.
	/// </summary>
	/// <param name="input">The input to the solution.</param>
	public abstract CombinedSolutionResult Solve(string input);
}

/// <summary>
/// A solution which solves both parts individually.
/// </summary>
public abstract class SeparateSolution : CombinedSolution
{
	public override CombinedSolutionResult Solve(string input)
	{
		SolutionResult part1;
		try
		{
			part1 = Part1(input);
		}
		catch (NotImplementedException)
		{
			return new();
		}

		SolutionResult part2;
		try
		{
			part2 = Part2(input);
		}
		catch (NotImplementedException)
		{
			return new(part1, default);
		}

		return new(part1, part2);
	}

	/// <summary>
	/// Solves part 1.
	/// </summary>
	/// <param name="input">The input to the solution.</param>
	public abstract SolutionResult Part1(string input);

	/// <summary>
	/// Solves part 2.
	/// </summary>
	/// <param name="input">The input to the solution.</param>
	public abstract SolutionResult Part2(string input);
}
