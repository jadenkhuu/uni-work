#!/usr/bin/env python3

import sys
from collections import defaultdict

def get_counts(file):
    pods = defaultdict(int)
    whales = defaultdict(int)

    with open(file, 'r') as file:
        for line in file:
            observation = line.strip().split()
            species = " ".join(observation[2:]).lower()
            if species.endswith('s'):
                species = species[:-1]

            pods[species] += 1
            whales[species] += int(observation[1])

    return pods, whales

if __name__ == '__main__':
    files = sys.argv[1:]
    total_pods = defaultdict(int)
    total_whales = defaultdict(int)

    for file in files:
        pods, whales = get_counts(file)
        for species, pods in pods.items():
            total_pods[species] += pods
        for species, whales in whales.items():
            total_whales[species] += whales

    sorted_species = sorted(total_pods.keys())

    for species in sorted_species:
        pods = total_pods[species]
        whales = total_whales[species]
        print(f"{species} observations: {pods} pods, {whales} individuals")