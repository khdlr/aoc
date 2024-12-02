import numpy as np
from scipy.signal import convolve2d

data = [[int(x) for x in line] for line in open('11.dat').read().strip().splitlines()]
data = np.array(data)

flash_kernel = np.ones([3, 3], dtype=data.dtype)

def flash_cascade(state, flash_mask):
  oldstate = state
  flashes  = convolve2d(flash_mask, flash_kernel, mode='same')
  state    = state + flashes
  cascade_mask = (oldstate <= 9) & (state > 9)
  num_flashes  = np.sum(flash_mask)
  if np.any(cascade_mask):
    state, cascaded_flashes = flash_cascade(state, cascade_mask)
    num_flashes += cascaded_flashes

  return state, num_flashes

def simulation_step(state):
  state += 1
  state, num_flashes = flash_cascade(state, state > 9)
  state[state > 9] = 0
  return state, num_flashes

step = data.copy()
total_flashes = 0
for i in range(100):
  step, num_flashes = simulation_step(step)
  total_flashes += num_flashes
print('task1:', total_flashes)

step = data.copy()
synchronous = False
i = 0
while not synchronous:
  step, num_flashes = simulation_step(step)
  i += 1
  if np.all(step == 0):
    print(f'synchronous @ {i}!')
    synchronous = True


