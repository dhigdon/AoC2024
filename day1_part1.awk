/* Advent of Code 2024, Day 1 Part 1 */
{  y = $2 - $1;
   x += (y<0) ? -y : y }
END { print x }
