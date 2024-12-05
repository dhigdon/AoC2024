BEGIN { FS="|";edges = 1 }
/^$/ { edges=0; FS=","; print ":data"; next }
edges { print "(",$1,".", $2, ")" }
!edges {
   line = "";
   for( i = 1; i <= NF; ++i )
      line = line " " $i
   print "(",line,")"
}
