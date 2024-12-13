# Advent of Code 2024, Day 13 - Claw Contraption
# by Dan Higdon, adapted from an idea by Jim Gage

# Solve this system of equations using Cramer's Rule (thank, Jim)
# [ ax bx | px ]
# [ ay by | py ]
function solve(   a,b,d,res ) {
   d = ax*by - ay*bx # DET ((ax bx)(ay by))
   a = px*by - py*bx # DET ((px bx)(py by))
   if (a%d != 0) return 0
   b = ax*py - ay*px # DET ((ax px)(ay py))
   if (b%d != 0) return 0
   return 3*int(a/d)+int(b/d)
}

BEGIN { FS="[:+=, ]" }
/^Button A/ { ax = $5; ay = $8 }
/^Button B/ { bx = $5; by = $8 }
/^Prize/    { px = $4; py = $7 }
/^$/        {
   part1 += solve()
   px += 10000000000000
   py += 10000000000000
   part2 += solve()
}

END { print "part1 =",part1,"part2 =",part2 }
