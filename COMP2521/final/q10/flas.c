
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "list.h"

List flas(List l) {
    // TODO
    if (l == NULL) {
        return NULL;
    }

    int current_length = 1;
    int max_length = 1;
    int total = 1;
    int flas_index = 0;

    for (List current = l; current->next != NULL; current = current->next) {

        // compare current value with next value
        if (current->value < current->next->value) {
            current_length++;
        } else {
        
            if (max_length < current_length) {

                max_length = current_length;
                flas_index = total - current_length;
            }

            current_length = 1;
        }

        total++;
    }

    if (max_length < current_length) {

        max_length = current_length;
        flas_index = total - max_length;
    }

    if (max_length == 1) {
        return NULL;
    }


    // Create new list to store flas
    List flas = ListNew();
    int counter = 0;
    for (List curr = l; curr != NULL; curr = curr->next) {
        
        // Where flas starts
        if (counter == flas_index) {

            while (max_length > 0) {
                // adding values to list
                flas = ListInsert(flas, curr->value);
                curr = curr->next;
                max_length = max_length - 1;
            }

            break;
        }

        counter++;
    }


    return flas;
}

