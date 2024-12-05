// Advent of Code 2024, Day 4 - Ceres Search
// by Dan Higdon

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// The map is a square matrix of characters
char key[] = "XMAS";
char * pMap = NULL;
int mapSpan = 0;

int in_range( int v ) { return v >= 0 && v < mapSpan; }

char idx( int x, int y )
{
   if ( !in_range( x ) || !in_range( y ) )
      return '\0';
   return pMap[x + y * mapSpan];
}

int read_map( const char * fname )
{
   char buf[256]; // NOTE: map known to be smaller than this
   FILE * pFile = fopen( fname, "ra" );
   if( pFile == NULL ) return 0;

   fgets( buf, sizeof( buf ), pFile );
   mapSpan = strlen( buf ) - 1; // Trailing \n
   pMap = malloc( mapSpan * mapSpan );

   for( int i = 0; i < mapSpan; ++i)
   {
      memcpy( pMap + (mapSpan * i), buf, mapSpan );
      fgets( buf, sizeof( buf ), pFile );
      // The last fgets should not succeed, but I won't assert that
   }

   return mapSpan;
}

// A clever way to do this would be to make a stream that traverses
// the map in the various different directions.
// We however are going to do it the stupid way.

int count_horizontal()
{
   char shift[5];
   int result = 0;

   for( int row = 0; row < mapSpan; ++row )
   {
      // forwards
      memset(shift, 0, sizeof( shift ) );
      for( int col = 0; col < mapSpan; ++col )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( col, row );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
      // backwards
      memset(shift, 0, sizeof( shift ) );
      for( int col = mapSpan - 1; col >= 0; --col )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( col, row );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
   }

   return result;
}

int count_vertical()
{
   char shift[5];
   int result = 0;

   for( int row = 0; row < mapSpan; ++row )
   {
      // Down
      memset(shift, 0, sizeof( shift ) );
      for( int col = 0; col != mapSpan; ++col )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( row, col );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
      // Up
      memset(shift, 0, sizeof( shift ) );
      for( int col = mapSpan - 1; col >= 0; --col )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( row, col );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
   }

   return result;
}

int count_diagonal_left()
{
   char shift[5];
   int result = 0;

   for( int col = -mapSpan + 1; col < mapSpan; ++col )
   {
      // Down
      memset( shift, 0, sizeof( shift ) );
      for( int i = 0; i < mapSpan; ++i )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( col + i, i );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
      memset( shift, 0, sizeof( shift ) );
      for( int i = mapSpan-1; i >= 0; --i )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( col + i, i );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
      // Up
   }
   return result;
}

int count_diagonal_right()
{
   char shift[5];
   int result = 0;

   for( int col = 0; col < 2 * mapSpan; ++col )
   {
      // Down
      memset( shift, 0, sizeof( shift ) );
      for( int i = 0; i < mapSpan; ++i )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( col - i, i );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
      memset( shift, 0, sizeof( shift ) );
      for( int i = mapSpan-1; i >= 0; --i )
      {
         memmove( shift, shift+1, 3 );
         shift[3] = idx( col - i, i );
         if( strncmp( key, shift, 4 ) == 0 ) ++result;
      }
      // Up
   }
   return result;
}

int count_x()
{
   // Needs to find "crossed MAS", but in any orientation
   // M.S
   // .A.
   // M.S
   int result = 0;
   for( int row = 1; row < mapSpan-1; ++row )
   {
      for( int col = 1; col < mapSpan-1; ++col )
      {
         if( idx( col, row ) == 'A')
         {
            if( idx( col-1, row-1 ) == 'M')
            {
               if( idx( col+1, row+1 ) != 'S' ) continue;
            }
            else if( idx( col-1, row-1 ) == 'S')
            {
               if( idx( col+1, row+1 ) != 'M' ) continue;
            }
            else continue;

            if( idx( col+1, row-1 ) == 'M' )
            {
               if( idx( col-1, row+1 ) != 'S' ) continue;
            }
            else if( idx( col+1, row-1 ) == 'S')
            {
               if( idx( col-1, row+1 ) != 'M' ) continue;
            }
            else continue;

            ++result;
         }
      }
   }

   return result;
}

int main( int argc, char *argv[] )
{
   if( argc != 2 )
   {
      fprintf(stderr, "Usage: day4: <fname>\n");
      return 1;
   }

   if( !read_map( argv[1] ) )
   {
      fprintf(stderr, "Error reading %s\n", argv[1] );
      return 2;
   }

   int part1 = 0;
   part1 += count_horizontal();
   part1 += count_vertical();
   part1 += count_diagonal_left();
   part1 += count_diagonal_right();

   printf( "Part1 = %d\n", part1 );
   printf( "Part2 - %d\n", count_x() );

   return 0;
}
