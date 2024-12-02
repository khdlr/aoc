import numpy as np
from functools import reduce
from collections import Counter
from itertools import islice

data = [[int(x) for x in line] for line in open('9.dat').read().strip().splitlines()]
data = np.array(data)

padded = np.pad(data, 1, mode='constant', constant_values=-1)

C = padded[1:-1, 1:-1]
N = padded[2:, 1:-1]
S = padded[:-2, 1:-1]
E = padded[1:-1, 2:]
W = padded[1:-1, :-2]

ismin = reduce(np.logical_and, map(lambda x: (C < x) | (x == -1), [N, S, E, W]))

print(np.sum(ismin * (1 + data)))

def valid(state):
  return np.all(state != -1, axis=-1)

def fmt(state):
  basins = {}
  print('-' * 3 * state.shape[1])
  for y in range(state.shape[0]):
    for x in range(state.shape[1]):
      d = tuple(state[y, x])
      if d == (-1, -1):
        print('   ', end='')
        continue
      if d not in basins:
        basins[d] = len(basins)
      print(f'{basins[d]:02d} ', end='')
    print()
  print('-' * 3 * state.shape[1])
  print(basins)

def fmt_bool(ary):
  for y in range(ary.shape[0]):
    for x in range(ary.shape[1]):
      if ary[y, x]: print('x', end='')
      else: print(' ', end='')
    print()

def watershed_step(state):
  st_pad = np.pad(state, [(1,1), (1,1), (0,0)], mode='constant', constant_values=-1)
  s_N = st_pad[2:, 1:-1]
  s_S = st_pad[:-2, 1:-1]
  s_E = st_pad[1:-1, 2:]
  s_W = st_pad[1:-1, :-2]
  ## state: array of pointers 
  res = state.copy()

  swapmask = (N < C) & valid(s_N)
  res[swapmask] = s_N[swapmask]

  swapmask = (S < C) & valid(s_S)
  res[swapmask] = s_S[swapmask]

  swapmask = (E < C) & valid(s_E)
  res[swapmask] = s_E[swapmask]

  swapmask = (W < C) & valid(s_W)
  res[swapmask] = s_W[swapmask]

  res[C == 9] = -1
  return res

def basin_sizes(state):
  basins = [tuple(x) for x in state.reshape(-1, 2) if np.all(x != -1)]
  counts = Counter(basins)
  top_three = list(islice(sorted(counts.values(), key=lambda x: -x), 3))
  print(counts)
  print(top_three)
  print(np.prod(top_three))


state = -np.ones([*data.shape, 2], dtype=int)
state[ismin] = np.argwhere(ismin)
changed = True
while changed:
  oldstate = state
  state = watershed_step(state)
  if np.all(oldstate == state):
    changed = False

fmt(state)
print(basin_sizes(state))
