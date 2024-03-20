// Lab02 Exercise - Addition
// addition.c
// 
// This program was written by Jaden Khuu (z5416824)
// on 23/02/22
//
// A simple program to add together the number of students and tutors in a class 


#include <stdio.h>

int main(void) {

    int students;
    int tutors;
    int sum;

    printf("Please enter the number of students and tutors: ");
    scanf("%d %d", &students, &tutors);

    sum = students + tutors;
    
    printf("%d + %d = %d\n", students, tutors, sum);

    return 0;
}

