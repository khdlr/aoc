import re
import numpy as np

data = open('5.dat').read().splitlines()
extract_points = re.compile("(\\d+),(\\d+) -> (\\d+),(\\d+)").match
lines = [[int(x) for x in extract_points(line).groups()] for line in data]

maxy = 0
maxx = 0
for y1, x1, y2, x2 in lines:
  maxy = max(maxy, y1, y2)
  maxx = max(maxx, x1, x2)

acc = np.zeros([maxy+1, maxx+1], np.int64)

def make_seq(a, b):
  if a < b: return np.arange(a, b+1)
  elif a > b: return np.arange(a, b-1, -1)
  else: return a

segments = 0
for y1, x1, y2, x2 in lines:
  if (y1 != y2) and (x1 != x2):
    continue
  segments += 1
  acc[make_seq(y1, y2), make_seq(x1, x2)] += 1

print(segments)
print((acc > 1).sum())

