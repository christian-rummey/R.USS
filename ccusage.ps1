# ccusage.ps1
$out = "ccusage.log"
$data = npx ccusage@latest --json | ConvertFrom-Json

# Write header
"Date`tTokens`tCost" | Out-File $out -Encoding utf8

# Write rows
$data.daily | ForEach-Object {
    "{0}`t{1}`t{2:N2}" -f $_.date, $_.totalTokens, $_.totalCost
} | Out-File $out -Append -Encoding utf8
