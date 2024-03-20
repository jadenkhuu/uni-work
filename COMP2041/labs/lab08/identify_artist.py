#!/usr/bin/env python3
import sys, re, glob, math

data = []
for song in sys.argv[1:]:
    with open(song, 'r') as song_file:
        song_contents = song_file.read()

    mainWords = re.split(r'[^a-zA-Z]+', song_contents)
    mainWords = [word for word in mainWords if word]

    similar = []
    for lyricFile in glob.glob("lyrics/*.txt"):
        name = lyricFile.split('/')[-1].split('.')[0].replace('_', ' ')
        with open(lyricFile, 'r') as lyricFileObj:
            lyrics_contents = lyricFileObj.read()

        words = re.split(r'[^a-zA-Z]+', lyrics_contents)
        words = [word for word in words if word]
        numWords = len(words)

        probability = 0
        for key_word in mainWords:
            word = key_word.lower()
            count = 0
            for lyric_word in words:
                if word == lyric_word.lower():
                    count += 1
            frequency = (count + 1) / numWords
            probability += math.log(frequency)
        similar.append((name, probability))
    
    similar.sort(key=lambda x: x[1])
    data.append((song, similar[-1][0], similar[-1][1]))

for song, name, probability in data:
    print(f"{song} most resembles the work of {name} (log-probability={probability:.1f})")