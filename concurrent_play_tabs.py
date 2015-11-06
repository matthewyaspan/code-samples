# Matthew Yaspan

# Read in a file each one line containing name of audio file (string note)
# and then a string of tabs to simulate a guiar string


import time
import pyglet
import sys
import threading
from pyglet import media

#num_threads = 0
#mutex = Semaphore(1);
#turnstile = Semaphore(0);

def playInstrument(audio, pitches):  
  count = [0]
  clock = pyglet.clock
  clock.schedule_interval(playNote, .2, audio, pitches, count)

def playNote(dt, audio, pitches, count):
  if count[0] >= len(pitches):
    return 0
  sound = pyglet.media.load(audio, streaming=False)
  player = media.Player()
  if pitches[count[0]].isdigit():
    print pitches[count[0]]
    pitch = pitches[count[0]]
    player.next_source()
    player.pitch = 2.0**(float(pitch)/12.0)
    player.queue(sound);
    player.play(); 
  # player.pause()
  count[0] += 1
  return 0

def main(argv):
  
  files  = []
  tabs = []
  players = []
  threads = []
  tab_file = open(argv[1])
  lines = tab_file.readlines()
  print "num lines is: " + str(len(lines))

  for line in lines:
    temp =line.split(' ')
    if len(temp) == 2:
      files.append(temp[0])
      tabs.append(temp[1])
    
  i = 0
  for tab in tabs:
    threads.append(threading.Thread(target=playInstrument,
                   args=[files[i], tabs[i]]))
    i += 1

  for thread in threads:
    thread.start()
  pyglet.app.run()

if __name__ == "__main__":
 main(sys.argv)
