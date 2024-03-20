// COMP2521 22T2 Assignment 1

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "Set.h"
#include "SetStructs.h"

void testSetInsert(void);
void testSetSize(void);
void testSetContains(void);
void testSetShow(void);

void testSetUnion(void);
void testSetIntersection(void);
void testSetDifference(void);
void testSetEquals(void);
void testSetSubset(void);
void testSetFloor(void);
void testSetCeiling(void);

void testSetCursor(void);

int main(int argc, char *argv[]) {
	testSetInsert();	
/*
	testSetInsert();	
	testSetSize();
	testSetContains();
	testSetShow();
	
	testSetUnion();
	testSetIntersection();
	testSetDifference();
	testSetEquals();
	testSetSubset();
	testSetFloor();
	testSetCeiling();
	testSetCursor();
*/
	
}

void testSetInsert(void) {
	// NOTE: SetInsert can't be tested on its own unless we have direct
	//       access to the internal representation of the ADT, so you
	//       may want to #include "SetStructs.h" and then access
	//       s->tree
	Set s = SetNew();
	for (int i = 0; i < 10000000; i++) {
		SetInsert(s, i);
	}
	
	/*
	int num;
	while (scanf("%d", &num) == 1) {
		SetInsert(s, num);
	}
	for (int i = 0; i < 10000000; i++) {
		SetInsert(s, i);
	}
	*/

	SetFree(s);
}

void testSetSize(void) {
	Set s = SetNew();
	assert(SetSize(s) == 0);
	SetInsert(s, 5);
	SetInsert(s, 2);
	SetInsert(s, 8);
	SetInsert(s, 4);
	assert(SetSize(s) == 4);
	SetInsert(s, 7);
	SetInsert(s, 2);
	assert(SetSize(s) == 5);

	SetFree(s);

	printf("Success Size\n");
}

void testSetContains(void) {
	Set s = SetNew();
	SetInsert(s, 5);
	SetInsert(s, 2);
	SetInsert(s, 8);
	SetInsert(s, 4);
	assert(SetContains(s, 2));
	assert(SetContains(s, 4));
	assert(SetContains(s, 5));
	assert(SetContains(s, 8));
	assert(!SetContains(s, 3));

	SetFree(s);

	printf("Success Contains\n");
}

void testSetShow(void) {
	// SetShow can't be tested with assert, since it prints to stdout.
	// Therefore, it must be manually checked.

	Set s = SetNew();
	SetInsert(s, 5);
	SetInsert(s, 2);
	SetInsert(s, 8);
	SetInsert(s, 4);
	SetShow(s);
	printf("\n");

	SetFree(s);
	printf("Success Show\n");
}

void testSetUnion(void) {
	Set a = SetNew();
	SetInsert(a, 5);
	SetInsert(a, 2);
	SetInsert(a, 8);
	SetInsert(a, 4);

	Set b = SetNew();
	SetInsert(b, 3);
	SetInsert(b, 8);
	SetInsert(b, 7);
	SetInsert(b, 5);

	Set c = SetUnion(a, b);
	assert(SetContains(c, 2));
	assert(SetContains(c, 3));
	assert(SetContains(c, 4));
	assert(SetContains(c, 5));
	assert(SetContains(c, 7));
	assert(SetContains(c, 8));
	printf("Union: ");
	SetShow(c);
	printf("\n");
    assert(SetSize(c) == 6);

	SetFree(a);
	SetFree(b);
	SetFree(c);

	printf("Success Union\n");
}

void testSetIntersection(void) {
	Set a = SetNew();
	SetInsert(a, 5);
	SetInsert(a, 2);
	SetInsert(a, 8);
	SetInsert(a, 4);

	Set b = SetNew();
	SetInsert(b, 3);
	SetInsert(b, 8);
	SetInsert(b, 7);
	SetInsert(b, 5);

	Set c = SetIntersection(a, b);
	assert(SetContains(c, 5));
	assert(SetContains(c, 8));
	assert(!SetContains(c, 2));
	assert(!SetContains(c, 3));
	assert(!SetContains(c, 4));
	assert(!SetContains(c, 7));
	assert(SetSize(c) == 2);

	SetFree(a);
	SetFree(b);
	SetFree(c);
	printf("Success Intersection\n");
}

void testSetDifference(void) {
	Set a = SetNew();
	SetInsert(a, 5);
	SetInsert(a, 2);
	SetInsert(a, 8);
	SetInsert(a, 4);

	Set b = SetNew();
	SetInsert(b, 3);
	SetInsert(b, 8);
	SetInsert(b, 7);
	SetInsert(b, 5);

	Set c = SetDifference(a, b);
	assert(SetContains(c, 2));
	assert(SetContains(c, 4));
	assert(!SetContains(c, 3));
	assert(!SetContains(c, 5));
	assert(!SetContains(c, 7));
	assert(!SetContains(c, 8));
	assert(SetSize(c) == 2);

	SetFree(a);
	SetFree(b);
	SetFree(c);
	printf("Success Difference\n");
}

void testSetEquals(void) {
	Set a = SetNew();
	SetInsert(a, 4);
	SetInsert(a, 2);
	SetInsert(a, 7);
	SetInsert(a, 1);

	Set b = SetNew();
	SetInsert(b, 2);
	SetInsert(b, 4);
	SetInsert(b, 1);
	SetInsert(b, 7);

	Set c = SetNew();
	SetInsert(c, 9);
	SetInsert(c, 2);
	SetInsert(c, 4);
	SetInsert(c, 1);

	assert(SetEquals(a, b));
	assert(!SetEquals(a, c));

	SetFree(a);
	SetFree(b);
	SetFree(c);
	printf("Success Equal\n");
}

void testSetSubset(void) {
	Set a = SetNew();
	SetInsert(a, 5);
	SetInsert(a, 3);
	SetInsert(a, 6);

	Set b = SetNew();
	SetInsert(b, 3);
	SetInsert(b, 9);
	SetInsert(b, 5);
	SetInsert(b, 6);

	Set c = SetNew();
	SetInsert(c, 4);
	SetInsert(c, 6);
	SetInsert(c, 3);
	SetInsert(c, 7);

	assert(SetSubset(a, b));
	assert(!SetSubset(a, c));

	SetFree(a);
	SetFree(b);
	SetFree(c);
	printf("Success Subset\n");
}

void testSetFloor(void) {
	Set s = SetNew();
	SetInsert(s, 2);
	SetInsert(s, 4);
	SetInsert(s, 6);
	SetInsert(s, 8);

	assert(SetFloor(s, 20) == 8);
	assert(SetFloor(s, 4) == 4);
	assert(SetFloor(s, 1) == UNDEFINED);

	SetFree(s);
	printf("Success Floor\n");
}

void testSetCeiling(void) {
	Set s = SetNew();
	SetInsert(s, 5);
	SetInsert(s, 2);
	SetInsert(s, 8);
	SetInsert(s, 4);

	assert(SetCeiling(s, 2) == 2);
	assert(SetCeiling(s, 6) == 8);
	assert(SetCeiling(s, 9) == UNDEFINED);

	SetFree(s);
	printf("Success Ceiling\n");
}

void testSetCursor(void) {
	Set s = SetNew();
	SetInsert(s, 5);
	SetInsert(s, 2);
	SetInsert(s, 8);
	SetInsert(s, 4);
	
	SetCursor cur = SetCursorNew(s);
	assert(SetCursorNext(cur) == 2);
	assert(SetCursorNext(cur) == 4);
	assert(SetCursorNext(cur) == 5);
	assert(SetCursorNext(cur) == 8);
	assert(SetCursorNext(cur) == UNDEFINED);
	
	SetFree(s);
	SetCursorFree(cur);
}

