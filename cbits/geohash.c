/*

Encoding using GeoHash.
----------------------

To be used as an FFI from Naqsha.Geometry.Coordinate.GeoHash


*/

#include <stdint.h>

const char b32[] = "0123456789bcdefghjkmnpqrstuvwxyz";


static inline uint64_t move(uint64_t u, int base5Digit, int bit, int pos)
{

    return (u >> (63 - (5 * base5Digit + bit)) & 1) << pos;
}

/* Computes the 2k-th  base32-digit */
static inline char high(uint64_t x, uint64_t y, int k)
{

    return b32[   move(x , k , 0 , 4)
		| move(y , k , 0 , 3)
		| move(x , k , 1 , 2)
		| move(y , k , 1 , 1)
		| move(x , k , 2 , 0)
	];
}

/* Computes the 2k+1st base32 digit */
static inline char low(uint64_t x, uint64_t y, int k)
{

    return b32[   move(y , k , 2 , 4 )
		| move(x , k , 3 , 3 )
		| move(y , k , 3 , 2 )
		| move(x , k , 4 , 1 )
		| move(y , k , 4 , 0 )
	];
}


void naqsha_geohash32(const uint64_t x, const uint64_t y, char *output)
{
    output[0] = high(x, y, 0);
    output[1] =  low(x, y, 0);

    output[2] = high(x, y, 1);
    output[3] =  low(x, y, 1);

    output[4] = high(x, y, 2);
    output[5] =  low(x, y, 2);

    output[6] = high(x, y, 3);
    output[7] =  low(x, y, 3);

    output[8] = high(x, y, 4);
    output[9] =  low(x, y, 4);

    output[10] = high(x, y, 5);
    output[11] =  low(x, y, 5);

    output[12] = high(x, y, 6);
    output[13] =  low(x, y, 6);

    output[14] = high(x, y, 7);
    output[15] =  low(x, y, 7);


    output[16] = high(x, y, 8);
    output[17] =  low(x, y, 8);


    output[18] = high(x, y, 9);
    output[19] =  low(x, y, 9);


    output[20] = high(x, y, 10);
    output[21] =  low(x, y, 10);


    output[22] = high(x, y, 11);
    output[23] =  low(x, y, 11);


}
