// Points Library
// points.c
//
// This program was written by cs1511
//
// The .c file for the points library. Only used in q8.
// YOU ARE NOT EXPECTED TO READ OR UNDERSTAND THIS FILE.

#include "points.h"
#include <float.h>
#include <math.h>

int points_equal(struct point a, struct point b) {
    return fabs(a.x - b.x) < FLT_EPSILON && fabs(a.y - b.y) < FLT_EPSILON;
}

int find_intersection(
    struct point start_1,
    struct point end_1,
    struct point start_2,
    struct point end_2,
    struct point *intersection
) {

    double t_numerator = (start_1.x-start_2.x) * (start_2.y-end_2.y)-(start_1.y-start_2.y) * (start_2.x-end_2.x);
    double t_denominator = (start_1.x-end_1.x) * (start_2.y-end_2.y)-(start_1.y-end_1.y) * (start_2.x-end_2.x);

    double u_numerator = (start_1.x-start_2.x) * (start_1.y-end_1.y)-(start_1.y-start_2.y) * (start_1.x-end_1.x);
    double u_denominator = (start_1.x-end_1.x) * (start_2.y-end_2.y)-(start_1.y-end_1.y) * (start_2.x-end_2.x);


    if (fabs(t_denominator) > FLT_EPSILON && fabs(u_denominator) > FLT_EPSILON) {
        double t = t_numerator / t_denominator;
        double u = u_numerator / u_denominator;

        if (t < 0 || t > 1 || u < 0 || u > 1) {
            return NO_INTERSECTION;
        }

        intersection->x = start_1.x + t * (end_1.x-start_1.x);
        intersection->y = start_1.y + t * (end_1.y-start_1.y);

        return DOES_INTERSECT;
    }

    return NO_INTERSECTION;

}
