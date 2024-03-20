
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "list.h"

static void listAppend(List l, int val);

// Worst case time complexity of this solution: O(n + m)
List zipList(List l1, int x, List l2, int y) {
    List l = ListNew();

    Node curr1 = l1->first;
    Node curr2 = l2->first;
    while (curr1 != NULL && curr2 != NULL && x > 0 && y > 0) {
        int count1 = 0;
        while (curr1 != NULL && count1 < x) {
            listAppend(l, curr1->value);
            curr1 = curr1->next;
            count1++;
        }

        int count2 = 0;
        while (curr2 != NULL && count2 < y) {
            listAppend(l, curr2->value);
            curr2 = curr2->next;
            count2++;
        }
    }

    while (curr1 != NULL && x > 0) {
        listAppend(l, curr1->value);
        curr1 = curr1->next;
    }

    while (curr2 != NULL && y > 0) {
        listAppend(l, curr2->value);
        curr2 = curr2->next;
    }

    return l;
}

static void listAppend(List l, int val) {
    Node n = newNode(val);
    if (l->first == NULL) {
        l->first = n;
        l->last = n;
    } else {
        l->last->next = n;
        l->last = n;
    }
}
