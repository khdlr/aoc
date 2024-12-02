import numpy as np

data = open('4.dat').read().strip().splitlines()

draws = [int(x) for x in data[0].split(',')]

grids = []
for line in data[1:]:
  if line == '':
    grids.append([])
    continue
  numbers = [int(x) for x in line.split()]
  grids[-1].append(numbers)
grids = np.asarray(grids)

def winning(grid, numbers_drawn):
  R = np.arange(5)

  marked = np.isin(grid, numbers_drawn)
  won = (
      marked.all(axis=2).any(axis=1)
      | marked.all(axis=1).any(axis=1)
      # | marked[..., R,   R].all(axis=1)
      # | marked[..., R, 4-R].all(axis=1)
  )
  return won, marked

won = False
i = 0
while not won:
  win, marked = winning(grids, draws[:i+1])
  if not np.any(win):
    i += 1
    continue
  won = True
  winner = np.argmax(win)
  print('Winning Board')
  print(grids[winner])
  print(draws[i])
  score = draws[i] * np.sum((~marked[winner]) * grids[winner])
  print(score)

all_won = False
win, _ = winning(grids, [])
i = 0
while not all_won:
  already_won = win
  win, marked = winning(grids, draws[:i+1])
  if not np.all(win):
    i += 1
    continue
  all_won = True
  loser = np.argmax(win & ~already_won)
  print('Losing Board')
  print(grids[loser])
  print(draws[i])
  score = draws[i] * np.sum((~marked[loser]) * grids[loser])
  print(score)
