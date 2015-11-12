# Matthew Yaspan
# Rock Band Assignment

import time
import pyglet
import sys
import threading
from pyglet import media

numThreads = 0
threadCount = 0
bootstrap = 0
numThreadsMutex = threading.Semaphore(1)
playMutex = threading.Semaphore(1)
mutex = threading.Semaphore(1)
turnstile_1 = threading.Semaphore(0)
turnstile_2 = threading.Semaphore(1)

def arrive(): 
  global mutex
  global threadCount
  global turnstile_1
  global turnstile_2
  global numThreads
  mutex.acquire()
  threadCount += 1
  if threadCount == numThreads:
    turnstile_2.acquire()
    turnstile_1.release()
  mutex.release()
  return

def waitToPlay():
  global mutex
  global threadCount
  global turnstile_1
  global turnstile_2
  global numThreads
  turnstile_1.acquire()
  turnstile_1.release()

def waitForFinish():
  global mutex
  global threadCount
  global turnstile_1
  global turnstile_2
  global numThreads

  mutex.acquire()
  threadCount -= 1
  if threadCount == 0:
    turnstile_1.acquire()
    turnstile_2.release()
  mutex.release()
  turnstile_2.acquire()
  turnstile_2.release()

def playInstrument(audio, pitches):
  global numThreads
  # arrive()
  #waitToPlay()
  #waitForFinish()

  player = media.Player()
  count = [0]
  while count[0] < len(pitches) :
    arrive()
    waitToPlay()
    playNote(audio, pitches, count, player)
    if count[0] == len(pitches):
      numThreadsMutex.acquire()
      numThreads -= 1
      numThreadsMutex.release()
    waitForFinish()
  return 0

def playNote( audio, pitches, count, player):
  sound = pyglet.media.load(audio, streaming=False)
  if pitches[count[0]].isdigit():
    pitch = pitches[count[0]]
    player.next_source()
    player.queue(sound)
    player.pitch = 2.0**(float(pitch)/12.0)
    #playMutex.acquire()
    player.play()
    #playMutex.release()
    time.sleep(0.2)
    #player.pause()
  else:
    time.sleep(0.2)
  count[0] += 1
  return

def main(argv):
  global threadCount
  global numThreads 
  files  = []
  tabs = []
  players = []
  threads = []
  tab_file = open(argv[1])
  lines = tab_file.readlines()

  for line in lines:
    temp =line.split(' ')
    if len(temp) == 2:
      files.append(temp[0])
      tabs.append(temp[1])
    
  for i in range(0, len(tabs)):
    threads.append(threading.Thread(target=playInstrument,
                   args=[files[i], tabs[i]]))
    numThreads += 1;

  for thread in threads:
    thread.start()
   
  for thread in threads:
    thread.join()


if __name__ == "__main__":
 main(sys.argv)
