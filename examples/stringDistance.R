# equal?
damerauLevenshteinDistance(1340, 1340)

# deletion, should be 1
damerauLevenshteinDistance(1340, 140)

# insertion, should be 1
damerauLevenshteinDistance(1340, 10340)

# substitution, should be 1
damerauLevenshteinDistance(1340, 1360)

# transposition, should be 1
damerauLevenshteinDistance(1340, 1430)

# transposition + insertion, should be 2
damerauLevenshteinDistance(1340, 14320)
