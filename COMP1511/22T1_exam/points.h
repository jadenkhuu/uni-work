// Points Library
// points.h
//
// This program was written by cs1511
//
// The .h file for the points library. Only used in q8.

#define DOES_INTERSECT 1
#define NO_INTERSECTION 0

// This struct represents a point in 2-dimensional space.
struct point {
    double x;
    double y;
};

// This function uses some maths (Tom had to really work hard)
// to find the intersection between two line-segments.
//
// If the segment between `start_1` and `end_1` intersects with
// the segment between `start_2` and `end_2`, this function returns
// `DOES_INTERSECT`, and puts the x and y values of the intercept
// in the memory pointed to by `intersection`.
//
// Otherwise, it returns `NO_INTERSECTION`.
int find_intersection(
    struct point start_1,
    struct point end_1,
    struct point start_2,
    struct point end_2,
    struct point *intersection
) ;

// This function returns `1` if a and b are equal,
// and `0` otherwise.
int points_equal(struct point a, struct point b);
