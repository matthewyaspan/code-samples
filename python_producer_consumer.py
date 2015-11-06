# Matthew Yaspan
# Python Producer consumer code
# I use a dictionary of dictionaries to keep track of each file's histogram
# Simultaneously, and each consumer thread loops until there are no more
# files available for it to process. If it comes upon an empty queue but
# there are still files to process, it release the items semaphore and
# repeats the loop.


import os.path
import sys
import collections
import threading
import Queue
import re 
master_dict = dict()
master_queue = Queue.Queue()
NUM_THREADS = 10
queueMutex = threading.Semaphore(1)
printMutex = threading.Semaphore(1)
numFilesMutex = threading.Semaphore(1)
items = threading.Semaphore(0)
filesToProcess = 0
started = threading.Semaphore(0)


def processFiles():
 """ This function is performed by a producer. Reads file for names of files to make histograms out of."""
 filesQueued = 0
 global filesToProcess
 for fileName in sys.stdin:
   if re.search('[a-zA-Z0-9]', fileName):
     fileName = fileName.split()
   if os.path.isfile(fileName[0]):
     queueMutex.acquire()
     master_queue.put(fileName[0])
     filesQueued += 1
     if filesQueued == 1:
       started.release()
     items.release()
     queueMutex.release()
     numFilesMutex.acquire()
     filesToProcess += 1
     numFilesMutex.release()
   else:
     sys.stderr.write("Unfortunately, we could not find a file by the name "
     + fileName + "\n")
  
    
 return


def HistStdout(fileName):
  """ Print out a completed histogram for an individual file to stdout"""
  global filesToProcess
  numFilesMutex.acquire()
  filesToProcess = filesToProcess - 1
  numFilesMutex.release()
  raw = master_dict[fileName]
  toPrint = collections.OrderedDict(sorted(raw.items()))
  for word in toPrint:
    printMutex.acquire()
    print fileName + ":\t" + word + " " + str(toPrint[word])   
    printMutex.release()


def createHist(fileName):
  """ Create a histogram from a given filename"""
  queueMutex.release()
  if fileName not in master_dict:
    master_dict[fileName] = dict()
    toRead = open(fileName)
    lines = toRead.readlines()
    for line in lines:
      words = line.split()
      for w in words:
        word = w.lower()
        if word in master_dict[fileName]:
          master_dict[fileName][word] += 1
        else:
          master_dict[fileName][word] = 1
  HistStdout(fileName)
  
 
def writeToHists():
  """ Loop looking for files to process. If none are found, restore items semaphore. If we are done, return."""
  global filesToProcess
  while True:
    numFilesMutex.acquire()
    boolean = filesToProcess <= 0    
    numFilesMutex.release()
    if boolean:
     items.release()
     return
    items.acquire()
    queueMutex.acquire()
    if  master_queue.empty():
      queueMutex.release()
    else:
      createHist(master_queue.get())
     

def printDict(outFile):
  temp = []
  for fileName in master_dict:
    temp.append(master_dict[fileName].items())
  
  temp = [pair for subdict in temp for pair in subdict]
  final_dict = collections.OrderedDict(sorted(temp))
 
  for word in final_dict:
    outFile.write(word + " " + str(final_dict[word]) + '\n')




def main(argv):
 global NUM_THREADS
 global started
 if "-m" in argv:
   NUM_THREADS = int(argv[argv.index("-m")+1])
   if NUM_THREADS < 2:
     sys.stderr.write("Sorry Bub. Can't have fewer than two threads. Setting to 2\n")
     NUM_THREADS = 2
 outFile = sys.stdout
 if "-o" in argv:
   outFile = open(argv[argv.index("-o")+1], "a")
 

 consumerThreads = []
 producerThread = threading.Thread(target=processFiles,
                                   args=[])


 for i in range(NUM_THREADS - 1):
  consumerThreads.append(threading.Thread(target=writeToHists,
                                          args=[]))
 
 producerThread.start()
 started.acquire()
 for thread in consumerThreads:
   thread.start()

 producerThread.join()
 for thread in consumerThreads:
   thread.join()
 
 printDict(outFile)


    




















if __name__ == "__main__" : main(sys.argv)
