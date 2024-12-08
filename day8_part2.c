// Advent of Code 2024, Day 8 - Resonant Collinearity, Part 2
// by Dan Higdon

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Point_
{
   char id;
   int x, y;
} Point;
typedef Point const * const PCPoint;

// NOTE: test data is 50x50
Point antennae[50 * 50];
int n_antennae = 0;

Point nodes[50 * 50 * 2];
int n_nodes = 0;

// NOTE: map is square in the test data....
int map_size = 0;

void read_map( char const * const filename )
{
   FILE * pFile = fopen( filename, "ra" );
   Point pt = { '\0', 0, 0 };
   while( pFile ) 
   {
      int ch = fgetc( pFile );
      switch( ch )
      {
      case -1:
         // EOF - finish up
         fclose( pFile );
         pFile = NULL;
         break;

      case '\n':
         // all lines should be the same size in the data
         assert(map_size == 0 || map_size == pt.x );
         map_size = pt.x;
         pt.x = 0;
         ++pt.y;
         break;

      case '.':
         // Skip empty space
         ++pt.x;
         break;

      default:
         // Mark the antenna
         pt.id = ch;
         antennae[ n_antennae++ ] = pt;
         ++pt.x;
         break;
      }
   }
}

int in_bounds( PCPoint p )
{
   return p->x >= 0 && p->x < map_size
      &&  p->y >= 0 && p->y < map_size;
}

int match_pos( PCPoint a, PCPoint b )
{
   return a->x == b->x
      &&  a->y == b->y;
}

// 'p' is valid if there are no nodes present
int is_valid( PCPoint p )
{
   for( int i = 0; i < n_nodes; ++i )
      if( match_pos( &nodes[ i ], p ) ) return 0;

   return 1;
}

void calc_nodes( PCPoint a, PCPoint b )
{
   assert( a && b && a->id == b->id );
   assert( a->x != b->x || a->y != b->y );

   int const dx = b->x - a->x;
   int const dy = b->y - a->y;
   Point node;

   node = *a;
   while( in_bounds( &node ) )
   {
      if ( is_valid( &node ) ) nodes[ n_nodes++ ] = node;
      node.x -= dx;
      node.y -= dy;
   }

   node = *b;
   while( in_bounds( &node ) ) 
   {
      if ( is_valid( &node ) ) nodes[ n_nodes++ ] = node;
      node.x += dx;
      node.y += dy;
   }
}

void main( int argc, char const * const * const argv )
{
   read_map( argv[1] );

   n_nodes = 0;
   for( int cur = 0; cur < n_antennae; ++cur )
   {
      PCPoint pcur = &antennae[ cur ];
      for( int i = cur + 1; i < n_antennae; ++i )
      {
         PCPoint p = &antennae[ i ];
         if( p->id == pcur->id ) calc_nodes( p, pcur );
      }
   }

   printf("Part2 = %d\n", n_nodes );
}

