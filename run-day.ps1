param (
	[Int32] $dayNum,
	[String] $aocInput
)

if (($dayNum -gt 0) -and ($dayNum -lt 10)) {
	$day = "0$dayNum"
} else {
	$day = "$dayNum"
}

$pathAndFileName = "./solutions/Day$day"

if (Test-Path "$pathAndFileName.cs") {
	Write-Output "Day $day`: C#"
	dotnet run --project ./core/cs -- "$dayNum" "$aocInput"
}

if (Test-Path "$pathAndFileName.hs") {
	Write-Output "Day $day`: Haskell"
	cabal run aoc22 -- "$dayNum" "$aocInput"
}
