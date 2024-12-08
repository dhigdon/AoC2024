// Advent of Code 2024, Day 8 - Resonant Collinearity
// by Dan Higdon

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Point_
{
   char id;
   int x, y;
} Point;

// NOTE: test data is 50x50
Point antennae[50 * 50];
int n_antennae = 0;

Point nodes[50 * 50 * 2];
int n_nodes = 0;

int map_width = 0;
int map_height = 0;

void reset() { n_antennae = n_nodes = 0; }

void read( const char * filename )
{
   FILE * pFile = fopen( filename, "ra" );
   Point pt = { '\0', 0, 0 };
   while( pFile ) 
   {
      int ch = fgetc( pFile );
      switch( ch )
      {
      case -1:    // EOF
         map_height = map_width; // HACK
         fclose( pFile );
         pFile = NULL;
         break;

      case '\n':  // EOL
         map_width = pt.x;
         pt.x = 0;
         ++pt.y;
         break;

      case '.':   // Empty space
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

int in_bounds( const Point * p )
{
   return p->x >= 0 && p->x < map_width
      &&  p->y >= 0 && p->y < map_height;
}

// See if 'p' already has a node
int is_valid( const Point * p )
{
   // Gotta be in bounds
   if( !in_bounds( p ) ) return 0;

   // Can't already have a node here
   for( int i = 0; i < n_nodes; ++i )
   {
      const Point * pn = &nodes[ i ];
      if( pn->x == p->x && pn->y == p->y ) return 0;
   }

   // All clear
   return 1;
}

void calc_nodes( const Point * a, const Point * b )
{
   assert( a && b && a->id == b->id );
   assert( a->x != b->x || a->y != b->y );

   int dx = b->x - a->x;
   int dy = b->y - a->y;
   Point node = { a->id, 0, 0 };

   node.x = a->x - dx;
   node.y = a->y - dy;
   if( is_valid( &node ) ) nodes[ n_nodes++ ] = node;

   node.x = b->x + dx;
   node.y = b->y + dy;
   if( is_valid( &node ) ) nodes[ n_nodes++ ] = node;
}

void dump_antennae()
{
   printf("Map %d by %d\n", map_width, map_height );
   for( int i = 0; i < n_antennae; ++i )
   {
      printf( "Emit: '%c' %d %d, ", antennae[ i ].id, antennae[ i ].x, antennae[ i ].y);
   }
   printf("\n");
   for( int i = 0; i < n_nodes; ++i )
   {
      printf( "Node: '%c' %d %d, ", nodes[ i ].id, nodes[ i ].x, nodes[ i ].y);
   }
   printf("\nAntenna = %d, Nodes = %d\n", n_antennae, n_nodes);
}

void show_map()
{
   for( int row = 0; row < map_height; ++row )
   {
      for( int col = 0; col < map_width; ++col )
      {
         char c = '.';
         for( int i = 0; i < n_antennae; ++i )
            if( antennae[ i ].x == col && antennae[ i ].y == row )
               c = antennae[ i ].id;
         for( int i = 0; i < n_nodes; ++i )
            if( nodes[ i ].x == col && nodes[ i ].y == row )
               c = '#';
         printf("%c",c);
      }
      printf("\n");
   }
}

void part1()
{
   n_nodes = 0;
   for( int cur = 0; cur < n_antennae; ++cur )
   {
      const Point * pcur = &antennae[ cur ];
      for( int i = cur + 1; i < n_antennae; ++i )
      {
         const Point * p = &antennae[ i ];
         if( p->id == pcur->id )
            calc_nodes( p, pcur );
      }
   }
}

void main( int argc, char **argv )
{
   read( argv[1] );
   part1();
   // dump_antennae();
   // show_map();
   printf("Part1 = %d\n", n_nodes);
}
