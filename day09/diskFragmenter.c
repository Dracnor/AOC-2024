/* Day 9 : disk fragmenter */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

/* Today is very array-like, hence C */

typedef enum {Empty, Not_Empty} status_t;

typedef struct {
  status_t status;
  int len;
  int id;
} cell_t;

#define MAX_NB_CELL 20000 // length of my input

/* part 1 */

/* Okay, SO : today I'm, regrattably, not lazy.
   I'll keep two indices : dst (on the left)
   and src (on the right).
   At each iteration, I copy as much as I can from the src
   cell to the dst cell.
   It would be easier to *build* the extended block notation
   at each time. But that would take longer, and I'm smart today.
   So I won't. I will have to corrupt the layout array as I go, 
   so it will be destructive.
*/

/** sums integer in [a;b[ */
uint64_t sum_range(uint64_t a, uint64_t b) {
  return ( (a+b-1) * (b-a) ) / 2 ;
}

/** Cf big comment. Corrupts layout content. */
uint64_t part1(cell_t layout[], int n) {
  int dst = 0;
  int src = n-1;
  uint64_t pos = 0; // pos in the extended block notation. Required to compute checksum.
  uint64_t checksum = 0;
  
  while (dst <= src) { 
    if (layout[dst].status != Empty) {
      checksum += layout[dst].id * sum_range(pos, pos+layout[dst].len);
      pos += layout[dst].len;
      dst++;
      continue;
    }
    else if (layout[src].status != Not_Empty) {
      src--;
      continue;
    }
    // Now, layou[src] is FREE and layout[dst] is FILE
    // I move as much as I can from the src into the dst,
    // and update the checksum
    
    if (layout[dst].len == layout[src].len) {
      // Move whole file, which entirely fills dst.
      checksum += layout[src].id * sum_range(pos, pos+layout[dst].len);
      pos += layout[dst].len;
      dst++;
      src--;
      continue;
    }
    if (layout[dst].len > layout[src].len ) {
      // Move whole file, which partially fills dst
      checksum += layout[src].id *sum_range(pos, pos+layout[src].len);
      layout[dst].len -= layout[src].len;
      pos += layout[src].len;
      src--;
      continue;
    }
    if (layout[dst].len < layout[src].len) {
      // Move only part of file, which entirely fills dst
      checksum += layout[src].id * sum_range(pos, pos+layout[dst].len);
      layout[src].len -= layout[dst].len;
      pos += layout[dst].len;
      dst++;
      continue;
    }
  }

  return checksum;
}



/* part 2 */

/* I'll basically do the naive idea : starting from the rightmost file,
   search the leftmost place where it can be.
   
   Computing pos on the fly is harder, so I'll have to store cell pos
   in the array positions
*/

/** Finds the first empty cell that can fit len_file */
int find_dst(cell_t layout[], int n, int len_file) {
  for (int dst = 1; dst < n; dst+=2) {
    assert(layout[dst].status == Empty);
    if (layout[dst].len >= len_file) {
     return dst; 
    }
  }
  return -1;
}


uint64_t part2(cell_t layout[], int n) {
  int src = n-1;
  int pos[MAX_NB_CELL] = {0};
  for (int i = 1; i < n; i++) {
    pos[i] = pos[i-1] + layout[i-1].len;
  }
  uint64_t checksum = 0;
  
  while (src > 0) {
    if (layout[src].status == Empty) {
      src--;
      continue;
    }
    
    int dst = find_dst(layout, n, layout[src].len);
    
    // If there's nowhere to move the file to
    if (dst == -1) {
      checksum += layout[src].id 
                  * sum_range(pos[src], pos[src] +layout[src].len);
      src--;
      continue;
    }
    
    // else : move the file
    else  {
      checksum += layout[src].id 
                  * sum_range(pos[dst], pos[dst] +layout[src].len);

      layout[dst].len -= layout[src].len;
      pos[dst] += layout[src].len; // the empty now starts more on the right
      
      src--;
      continue;
    }
  }
  
  return checksum;
}







int main(int argc, char* argv[]) {

  if (argc < 2) {
    fprintf(stderr, "Usage : ./%s name_inputfile\n", argv[0]);
    return EXIT_FAILURE;
  }
  
  char* input_file = argv[1];
  FILE* f_input = fopen(input_file, "r");
  cell_t layout[MAX_NB_CELL];
  char c;
  int i = 0;
  while ( (c = fgetc(f_input)) != EOF) {
    if (i % 2 == 0)
      layout[i] = (cell_t) {.status = Not_Empty, .len = c - '0', .id = i/2};
    else
      layout[i] = (cell_t) {.status = Empty, .len = c - '0'};
    i++;
  }
  fclose(f_input);
  
  cell_t layout_copy[MAX_NB_CELL];
  memcpy(layout_copy, layout, i * sizeof(cell_t));
  
  uint64_t answer_part1 = part1(layout_copy, i);
  uint64_t answer_part2 = part2(layout, i);
  printf("%lu\n%lu\n", answer_part1, answer_part2);
  
  return EXIT_SUCCESS;
}
