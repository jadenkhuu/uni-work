from random import *
import subprocess

n = 20
maxWeight = 20
counter = 0
output1 = ''
output2 = ''

while output1 == output2:
    counter = counter + 1
    print('test ' + str(counter))
    edges = [[0] * (n+1) for i in range(n)]
    unvisited = [i for i in range(1, n)]
    visited = [0]

    for i in range(n-1):
        index1 = randrange(0,len(unvisited))
        index2 = randrange(0,len(visited))
        weight = randint(1, maxWeight)
        edges[unvisited[index1]][visited[index2]] = weight
        edges[visited[index2]][unvisited[index1]] = weight
        visited.append(unvisited[index1])
        del unvisited[index1]

    for i in range(n):
        for j in range(i+1, n):
            if edges[i][j] == 0:
                if (random() < 1/40):
                    edges[i][j] = randint(1,maxWeight)
        stamina = max(edges[i][slice(0, n)])
        edges[i][n] = stamina

    minStamina = max([edges[i][n] for i in range(n)])

    f = open('city2.txt', 'w')
    f.write(str(n))
    f.write('\n')

    for i in range(n):
        f.write(str(i) + ' ')
        for j in range(n):
            if edges[i][j] != 0:
                f.write(str(j) + ' ' + str(edges[i][j]) + ' ')
        #if (random() < 1/100):
        #    f.write('i ')
        #else:
        f.write('n ')
        f.write('a' + str(i) + '\n')

    f.close()

    output1 = subprocess.check_output("echo \"run\" | ./game city2.txt agent2.txt 30 1", shell = True)
    output2 = subprocess.check_output("echo \"run\" | /web/cs2521/22T2/ass/ass2/reference/game city2.txt agent2.txt 30 1", shell = True)


with open("output1.txt", "wb") as f:
    f.write(output1)
with open("output2.txt", "wb") as f:
    f.write(output2)

print('failed xd')