/* image_transform_sample.c`
 *
 * Matthew Yaspan, Casey Gowrie
 *
 * takes in a ppm file
 * rotates or flips the image based on command line args
 * either 0 degrees, 90 degrees, 180 degrees, or 270
 * 
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <assert.h>
#include <a2methods.h>
#include <a2plain.h>
#include <a2blocked.h>
#include <pnm.h>
#include <pnmrdr.h>
#include <seq.h>

#define HINT 10


void transposeImage(FILE *fp, int rotate, A2Methods_T methods, 
                    A2Methods_mapfun *map);

Pnm_ppm rotate(Pnm_ppm image, A2Methods_mapfun *map, A2Methods_T methods,
                                                               int rotation);
void movePix90(int i, int j, A2Methods_UArray2 array2, A2Methods_Object *ptr, 
               void *cl);
void movePix180(int i, int j, A2Methods_UArray2 array2, A2Methods_Object *ptr, 
               void *cl);
void movePix270(int i, int j, A2Methods_UArray2 array2, A2Methods_Object *ptr, 
               void *cl);



int main(int argc, char *argv[]) {

        /* provided to us in assignment */
        int rotation = 0;
        A2Methods_T methods = uarray2_methods_plain;
        /* default to UArray2 methods */
        assert(methods);
        A2Methods_mapfun *map = methods->map_default;
        /* default to best map */
        assert(map);
        #define SET_METHODS(METHODS, MAP, WHAT) do { \
                methods = (METHODS); \
                assert(methods); \
                map = methods->MAP; \
                if (!map) { \
                        fprintf(stderr, \
                                "%s does not support " WHAT "mapping\n", \
                                argv[0]); \
                        exit(1); \
                } \
        } while(0)

        int i;
        for (i = 1; i < argc; i++) {
                if (!strcmp(argv[i], "-row-major")) {
                        SET_METHODS(uarray2_methods_plain, map_row_major,
                                    "row-major");
                } else if (!strcmp(argv[i], "-col-major")) {
                        SET_METHODS(uarray2_methods_plain, map_col_major,
                                    "column-major");
                } else if (!strcmp(argv[i], "-block-major")) {
                        SET_METHODS(uarray2_methods_blocked, map_block_major,
                                    "block-major");
                } else if (!strcmp(argv[i], "-rotate")) {
                        assert(i + 1 < argc);
                        char *endptr;
                        rotation = strtol(argv[++i], &endptr, 10);
                        assert(*endptr == '\0'); /* parsed all correctly */
                        assert(rotation == 0   || rotation == 90
                               || rotation == 180 || rotation == 270);
                } else if (*argv[i] == '-') {
                        fprintf(stderr, "%s: unknown option '%s'\n", argv[0],
                               argv[i]);
                        exit(1);
                } else if (argc - i > 2) {
                        fprintf(stderr, "Usage: %s [-rotate <angle>] "
                                "[-{row,col,block}-major] [filename]\n",
                                argv[0]);
                        exit(1);
                } else {
                        break;
                }
        }
        
        /* now our code */
        int numFiles = 0;
        FILE *fp = NULL;
        for (i = 1; i < argc; i++) {
                fp = fopen(argv[i], "r"); /* opens any file on command line */
                if (fp != NULL) { /* counts number of files on command line */
                        numFiles++; 
                }
        }

        if (numFiles < 1) {
                fprintf(stderr, "Not enough files provided\n");
                exit(1);
        } else if (numFiles > 1) {
                fprintf(stderr, "Too many files\n");
                exit(1);
        } else { /* if correct number of files have been opened */
                transposeImage(fp, rotation, methods, map);
        }
        fclose(fp);
        

}

/* transposeImage()
 * 
 * Takes in a file handle, the number of degrees an image needs to be rotated,
 * the preferred methods, and the preferred map function
 * Returns the image altered to the specifications of the user at the command
 * line.
 */
void transposeImage(FILE *fp, int rotation, A2Methods_T methods, 
                    A2Methods_mapfun *map)
{
        Pnm_ppm image = Pnm_ppmread(fp, methods);
        Pnm_ppm newImg = NULL;

        newImg = rotate(image, map, methods, rotation);

        image->methods = methods;
        Pnm_ppmwrite(stdout, newImg);
        Pnm_ppmfree(&image);
        if (rotation != 0)
                Pnm_ppmfree(&newImg);

}


/* rotate()
 * Takes in original image, map function, methods suite, rotation number.
 * If rotation number is 0, returns original image.
 * Otherwise, allocates a new image, gives it the appropriate width, height,
 * size, and the methods suite, and calls the map function on the original
 * image's array of Pnm_rgb, the appropriate movePix function, each
 * corresponding to a rotation number, and the new image as a closure.
 * Returns new image.
 */
Pnm_ppm rotate(Pnm_ppm image, A2Methods_mapfun *map, A2Methods_T methods,
                                                             int rotation)
{
        if (rotation == 0){
                return image;
        }
        
        Pnm_ppm newImg = malloc(sizeof(*newImg));
        assert(newImg);
        newImg->denominator = image->denominator;
        newImg->methods = image->methods;
        Pnm_rgb sample = malloc(sizeof(Pnm_rgb));
        
        if (rotation == 90) {
                newImg->height = image->width;
                newImg->width = image-> height;
                newImg->pixels = methods->new(newImg->width, newImg->height,
                                                        sizeof(*sample));
                map(image->pixels, movePix90, newImg);
        } else if (rotation == 180) {
                newImg->height = image->height;
                newImg->width = image-> width;
                newImg->pixels = methods->new(newImg->width, newImg->height,
                                                        sizeof(*sample));
                map(image->pixels, movePix180, newImg);
        } else if (rotation == 270) {
                newImg->height = image->width;
                newImg->width = image->height;
                newImg->pixels = methods->new(newImg->width, newImg->height,
                                                        sizeof(*sample));
                map(image->pixels, movePix270, newImg);
        }
        
        free(sample);
                
        return newImg;
}

/* movePix90()
 *
 * Apply function given a column, row, A2Methods_Object (really a Pnm_rgb),
 * and the new Pnm_ppm as a closure. Takes element, which is from array location
 * (i, j) and places it in (height-j-1, i) and deep copies it to the new image
 * (height is width of new image).
 * 
*/
void movePix90(int i, int j, A2Methods_UArray2 array2, A2Methods_Object *ptr, 
               void *cl)
{
            assert(ptr);

            Pnm_ppm rImage = cl;
            (void) array2;
            /* rImage->width is the equivalent of height of old array */
            int newRow = rImage->width - j - 1;
            int newCol = i;

            Pnm_rgb elem = ptr;

            Pnm_rgb pix = rImage->methods->at(rImage->pixels, newRow, newCol);

            pix->red = elem->red;
            pix->green = elem->green;
            pix->blue = elem->blue;


}

/* movePix180()
 *
 * Apply function given a column, row, A2Methods_Object (really a Pnm_rgb),
 * and the new Pnm_ppm as a closure. Takes element, which is from array location
 * (i, j) and places it in (width-i-1, height-i-1) deep copies it to the new
 * image.
 * 
*/
void movePix180(int i, int j, A2Methods_UArray2 array2, A2Methods_Object *ptr, 
               void *cl)
{
            assert(ptr);
            Pnm_ppm rImage = cl;
            (void) array2;
            /* rImage->width is the equivalent of height of old array */
            int newRow = rImage->width - i - 1;
            int newCol = rImage->height - j - 1;

            Pnm_rgb elem = ptr;
            Pnm_rgb pix = rImage->methods->at(rImage->pixels, newRow, newCol);
            pix->red = elem->red;
            pix->green = elem->green;
            pix->blue = elem->blue;
}

/* movePix90()
 *
 * Apply function given a column, row, A2Methods_Object (really a Pnm_rgb),
 * and the new Pnm_ppm as a closure. Takes element, which is from array location
 * (i, j) and places it in (j, height-i-1) and deep copies it to the new image
 * (height is width of new image).
 * 
*/
void movePix270(int i, int j, A2Methods_UArray2 array2, A2Methods_Object *ptr, 
               void *cl)
{
            assert(ptr);
            Pnm_ppm rImage = cl;
            (void) array2;
            /* rImage->width is the equivalent of height of old array */
            int newCol = rImage->height - i - 1;
            int newRow = j;

            Pnm_rgb elem = ptr;
            Pnm_rgb pix = rImage->methods->at(rImage->pixels, newRow, newCol);
            pix->red = elem->red;
            pix->green = elem->green;
            pix->blue = elem->blue;
}



