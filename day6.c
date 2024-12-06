// Advent of Code 2024, Day 6 - Guard Gallivant
// by Dan Higdon

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

char *pmap;
int width;

char idx( int x, int y )
{
   return pmap[x + y * width];
}

void set( int x, int y, char c )
{
   pmap[x + y * width] = c;
}

int in_map( int x, int y )
{
   return x >= 0 && x < width
      && y >= 0 && y < width;
}

int main( int argc, char **argv )
{
   FILE * pFile = fopen( argv[1], "ra" );
   int dx, dy, cx, cy, nx, ny, t, count;
   char buf[256];

   // The map is square, so use the first line to determine the size
   fgets( buf, sizeof( buf ), pFile );
   width = strlen( buf ) - 1;
   pmap = malloc( width * width );

   for( int i = 0; i < width; ++i )
   {
      char * pcursor = strchr( buf, '^');
      if ( pcursor )
      {
         cx = pcursor - buf;
         cy = i;
      }
      memcpy( pmap + i * width, buf, width );
      fgets( buf, sizeof(buf), pFile );
   }

   // Start going up
   dx = 0;
   dy = -1;
   count = 1; // Already in one space

   nx = cx + dx;
   ny = cy + dy;
   while( in_map( nx, ny ) )
   {
      switch( idx( nx, ny ) )
      {
      case 'X': // Traversing already marked space
      case '^':
         cx = nx;
         cy = ny;
         break;

      case '.': // Traversing new space
         cx = nx;
         cy = ny;
         set( nx, ny, 'X' );
         ++count;
         break;

      case '#': // Turn right, don't move
         t = dx;
         dx = -dy;
         dy = t;
         break;
      }
      nx = cx + dx;
      ny = cy + dy;
   }

   printf("part1 = %d\n", count);

   return 0;
}
