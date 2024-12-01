{ pres[i++]=$1; sim[$2]++ }
END {
   for (p in pres) {
      v = pres[p]
      tot += v * sim[v]
   }
   print tot
}
