/* Day 13 : Claw Contraption */

#include <stdlib.h>
#include <stdio.h>

/* Given :

  Button A: X+94, Y+34
  Button B: X+22, Y+67
  Prize: X=8400, Y=5400

  We want to solve :
  
  a*94 + b*22 = 8400
  a*34 + b*67 = 5400
  
  Which is

  ( 94 22) (a)   (8400)
  ( 34 67) (b) = (5400)

  For this, we just have to inverse the left matrix.
  The inverse of
  (ax bx)    (by  -bx)
  (ay by) is (-ay  ax) / (ax*by - ay*bx)
  
  
  I'll call the columns of the matrix ax ay | bx by, and the prize px py.
*/

/** Return the token cost of a system */
int64_t solve(int64_t ax, int64_t ay, int64_t bx, int64_t by, 
                    int64_t px, int64_t py) {
  int64_t det = ax*by - ay*bx;
  if (det == 0) return 0;

  int64_t numA = by * px - bx * py;
  int64_t numB = -ay * px + ax * py;
  
  // Don't forget to check that the only solution is integer !! */
  if (numA % det != 0 || numB % det != 0) return 0;
  
  return 3 * (numA / det) + (numB / det);
}


/** Reads each equation, and computes its cost (for both parts). */
int main(int argc, char* argv[]) {

  if (argc != 2) {
    fprintf(stderr, "Usage : %s input_file\n", argv[0]);
    return EXIT_FAILURE;
  }
  char* input_file = argv[1];
  
  int64_t ax, bx,  px,
          ay, by,  py;
  char const* pattern = " Button A: X+%ld, Y+%ld"
                        " Button B: X+%ld, Y+%ld"
                        " Prize: X=%ld, Y=%ld";
  int64_t answer_part1 = 0;
  int64_t answer_part2 = 0;

  FILE* f_input = fopen(input_file, "r");
  while ( fscanf(f_input, pattern, &ax, &ay, &bx, &by, &px, &py) != EOF ) {
    answer_part1 += solve(ax, ay, bx, by, px, py);
    px += 10000000000000;
    py += 10000000000000;
    answer_part2 += solve(ax, ay, bx, by, px, py);
  }
  fclose(f_input);

  printf("%lu\n%lu\n", answer_part1, answer_part2);
  return EXIT_SUCCESS;
}
