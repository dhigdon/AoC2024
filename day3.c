// Advent of Code 2024, Day 3 - ???
// by Dan Higdon

#include <stdio.h>
#include <ctype.h>

// Stages of the state machine
// When the final ')' is found, bank the product and go back to scanning.
enum Stage
{
   STAGE_S,       // Scanning
   STAGE_M,       // M...
   STAGE_U,       // MU...
   STAGE_L,       // MUL..
   STAGE_LP,      // MUL(...
   STAGE_LV,      // MUL([0-9]*...
   STAGE_COMMA,   // MUL([0-9]*,...
   STAGE_RV,      // MUL([0-9]*,[0-9]*...
 
   // For part two, we need to recognize do() and don't() keywords
   STAGE_DO_D,    // d...
   STAGE_DO_DO,   // do...
   STAGE_DO_LP,   // do(...
   STAGE_DO_N,    // don...
   STAGE_DO_NO,   // don'...
   STAGE_DO_NOT,  // don't...
   STAGE_DO_NOT_LP, // don't(
};

int main( int argc, char **argv )
{
   if( argc != 2 )
   {
      fprintf(stderr, "Usage: day3 <datafile>");
      return 1;
   }

   FILE * pFile = fopen(argv[1], "r");
   if( pFile != NULL )
   {
      long lvalue, rvalue;
      long part1 = 0;
      long part2 = 0;
      int enable = 1;

      enum Stage stage = STAGE_S;
      int ch;

      while ((ch = fgetc( pFile )) != EOF)
      {
         switch (stage)
         {
         case STAGE_S:
            switch (ch)
            {
            case 'd': stage = STAGE_DO_D; break;
            case 'm': stage = STAGE_M; break;
            default: stage = STAGE_S;
            }
            break;

         case STAGE_DO_D: stage = (ch == 'o') ? STAGE_DO_DO : STAGE_S; break;

         case STAGE_DO_DO: // may be either do() or don't()
            switch (ch)
            {
            case '(': stage = STAGE_DO_LP; break;
            case 'n': stage = STAGE_DO_N; break;
            default: stage = STAGE_S; break;
            }
            break;

         case STAGE_DO_LP: // Finish do() call
            if( ch == ')' ) enable = 1;
            stage = STAGE_S;
            break;

         case STAGE_DO_N:   stage = (ch == '\'') ? STAGE_DO_NO     : STAGE_S; break;
         case STAGE_DO_NO:  stage = (ch == 't')  ? STAGE_DO_NOT    : STAGE_S; break;
         case STAGE_DO_NOT: stage = (ch == '(')  ? STAGE_DO_NOT_LP : STAGE_S; break;

         case STAGE_DO_NOT_LP:   // finish don't()
            if( ch == ')' ) enable = 0;
            stage = STAGE_S;
            break;

         case STAGE_M: stage = (ch == 'u') ? STAGE_U  : STAGE_S; break;
         case STAGE_U: stage = (ch == 'l') ? STAGE_L  : STAGE_S; break;
         case STAGE_L: stage = (ch == '(') ? STAGE_LP : STAGE_S; break;

         case STAGE_LP:
            if( isdigit( ch ) )
            {
               lvalue = ch - '0';
               stage = STAGE_LV;
            }
            else stage = STAGE_S;
            break;

         case STAGE_LV:
            if( isdigit( ch ) )
            {
               lvalue = lvalue * 10 + (ch - '0');
            }
            else if( ch == ',' )
            {
               stage = STAGE_COMMA;
            }
            else stage = STAGE_S;
            break;

         case STAGE_COMMA:
            if( isdigit( ch ) )
            {
               rvalue = ch - '0';
               stage = STAGE_RV;
            }
            else stage = STAGE_S;
            break;

         case STAGE_RV:
            if( isdigit( ch ) )
            {
               rvalue = rvalue * 10 + (ch - '0');
            }
            else
            {
               if( ch == ')' )
               {
                  part1 += lvalue * rvalue;
                  if( enable ) part2 += lvalue * rvalue;
               }
               stage = STAGE_S;
            }
            break;
         }
      }
      fclose( pFile );
      printf( "part1 = %ld\n", part1 );
      printf( "part2 = %ld\n", part2 );
   }
}
