# Advent of Code 2024, Day 2 - Red Nosed Reports
# by Dan Higdon

function safe( skipindex,   idx,last,diff,dir ) {
   # Startup is a bit tricky with skipindex
   if( skipindex == 1 ) { idx=3; last=$2 }
   else if (skipindex == 2 ) { idx=3; last=$1 }
   else { idx=2; last=$1 }

   # Figure the direction this "run" goes in
   if( last < $idx ) dir = -1
   else if( last > $idx ) dir = 1
   else return 0;

   for( ; idx <= NF; ++idx ) {
      if( idx == skipindex ) continue
      diff = dir * (last - $idx)
      if( diff<1 || diff>3 ) break
      last = $idx
   }
   return (idx == NF+1)
}

function dampensafe(    dampen ) {
   for( dampen = 0; dampen <= NF; ++dampen )
      if( safe( dampen ) ) return dampen
   return -1
}

BEGIN { part1=0; part2=0 }

safe(0)           { ++part1 }
dampensafe() >= 0 { ++part2 }

END { print part1, part2 }
