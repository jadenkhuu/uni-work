// BFS maze solver

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Cell.h"
#include "matrix.h"
#include "Maze.h"
#include "Queue.h"

bool solve(Maze m) {
    // TODO : Complete this function
    //       Feel free to add helper functions

    bool **visited = createBoolMatrix(MazeHeight(m), MazeWidth(m));
    Cell **pred = createCellMatrix(MazeHeight(m), MazeWidth(m));
    bool exit_found = false;
    
    Queue q = QueueNew();
    QueueEnqueue(q, MazeGetStart(m));
    Cell v;
    while (!QueueIsEmpty(q)) {
        v = QueueDequeue(q);
        
        if (MazeVisit(m, v) == true) {
            while (!(v.col == MazeGetStart(m).col && 
                v.row == MazeGetStart(m).row)) {
                    
                MazeMarkPath(m,v);
                v = pred[v.row][v.col];
            }
            MazeMarkPath(m,v);
            exit_found = true;
            break;
        }

        if (visited[v.row][v.col]) {
            continue;
        }
        visited[v.row][v.col] = true;
        
        Cell w = v;

        if (v.col + 1 <= MazeWidth(m) - 1) {
            w.col++;
            if (!MazeIsWall(m, w) && !visited[w.row][w.col]) {
                QueueEnqueue(q, w);
                pred[w.row][w.col] = v;
            }
            w.col--;
        }
        if (v.col - 1 >= 0) {
            w.col--;
            if (!MazeIsWall(m, w) && !visited[w.row][w.col]) {
                QueueEnqueue(q, w);
                pred[w.row][w.col] = v;
            }
            w.col++;
        }
        if (v.row + 1 <= MazeHeight(m) - 1) {
            w.row++;
            if (!MazeIsWall(m, w) && !visited[w.row][w.col]) {
                QueueEnqueue(q, w);
                pred[w.row][w.col] = v;
            }
            w.row--;
        }
        if (v.row - 1 >= 0) {
            w.row--;
            if (!MazeIsWall(m, w) && !visited[w.row][w.col]) {
                QueueEnqueue(q, w);
                pred[w.row][w.col] = v;
            }
            w.row++;
        }
    }

    freeCellMatrix(pred);
    freeBoolMatrix(visited);
    QueueFree(q);

    if (exit_found == true) {
        return true;
    } else {
        return false;
    }
    
}
