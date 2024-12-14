# Advent of Code 2024, Day 14 - Restroom Redoubt

function modulo(a,b) {
   a=a%b
   if(a<0) return a+b;
   return a;
}

BEGIN {
   FS="[ ,=]";
   if(ARGC>1) {
      iter=ARGV[2]
      --ARGC
      print iter
   }
}

/^map/ {
   width=$2
   height=$3
   xaxis=int(width/2)
   yaxis=int(height/2)
   q1=0;q2=0;q3=0;q4=0
}

/^p=/ {
   px=$2; py=$3; vx=$5; vy=$6

   dx=modulo(iter*vx + px, width)
   dy=modulo(iter*vy + py, height)
   if(dx<xaxis) {
      if(dy<yaxis) q1++;
      if(dy>yaxis) q3++;
   }
   if(dx>xaxis){
      if(dy<yaxis) q2++;
      if(dy>yaxis) q4++;
   }
}

END { print iter, q1*q2*q3*q4 }
