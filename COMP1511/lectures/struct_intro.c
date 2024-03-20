// Introducing you to the user defined data structure - STRUCT
// It is basically a mixed bag of tricks that are related
// that you are in charge of

#include <stdio.h>

//Three main points to a struct
//1. Define the struct - what is inside this struct?

struct coordinate{
    double x;
    double y;
};

int main(void) {

//2. Declare the struct inside the main - declaring that I will use it inside my main
//3. Initialise the struct inside the main - give initial values
    //Declare a struct 
    struct coordinate point_one;
    struct coordinate point_two;
    double gradient;
    int scanf_return;
    
    printf("Please enter the x and y coordinates of point one: ");
    scanf_return = scanf("%lf %lf", &point_one.x, &point_one.y);
    
    if (scanf_return != 2) {
        printf("You have not entered a point. Program will now exit\n");
        return 1;
    }
    
    printf("Please enter the x and y coordinates of point two: ");
    scanf_return = scanf("%lf %lf", &point_two.x, &point_two.y);
    
    if (scanf_return != 2) {
        printf("You have not entered a point. Program will now exit\n");
        return 1;
    }
    
    gradient = ((point_one.y - point_two.y)/(point_one.x - point_two.x));

    printf("Point one (%.1lf, %.1lf)\nPoint two (%.1lf, %.1lf)\nGradient = %.2lf\n", point_one.x, point_one.y, point_two.x, point_two.y, gradient);

// Problem: Scan in the coordinate points of two points and want
// to compute the gradient of the line (formula)
// for gradient ((y1 - y2)/(x1 - x2))

    return 0;
}
