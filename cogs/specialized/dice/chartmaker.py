from collections import OrderedDict
import matplotlib.pyplot as plt; plt.rcdefaults()
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
from timeit import default_timer as timer
import scipy.special
import math
import random
import sys
import time

def gBinom(n,k):
    if n<0:
        return (-1)**k*scipy.special.binom(-n,k)
    return scipy.special.binom(n,k)

def diceProb(n,t,d):
    def singleRollSum(j):
        return ((-1)**j)*gBinom(n,j)*gBinom(t-d*j-1,n-1)
    return d**(-n)*sum([singleRollSum(j) for j in range(int((t-n)/d)+1)])

def exactDraw(labels, values):
    return plt.plot(labels, values, alpha=0.5)

def approxDraw(number_of_dice, dice_face):
    if number_of_dice == 1:
        x = np.linspace(1, dice_face, 2)
        y = 1/dice_face 
        handle = plt.plot(x,[y,y], alpha=0.5)
    elif number_of_dice == 2:
        x = np.linspace(2, 2*dice_face, 3)
        endy = 1/dice_face**2
        midy = 1/dice_face
        handle = plt.plot(x, [endy,midy,endy], alpha=0.5)
    else:
        x = np.linspace(number_of_dice, number_of_dice * dice_face, 100)
        mu = number_of_dice * (dice_face/2+0.5)
        variance = number_of_dice * ((dice_face ** 2 - 1)/12) 
        sigma = math.sqrt(variance)
        handle = plt.plot(x, mlab.normpdf(x, mu, sigma), alpha=0.5)
    return handle

def predictedChart(number_of_dice, dice_face):
    plt.title("Predicted results for "+str(number_of_dice)+"d"+str(dice_face))
    plt.xlabel("Dice Totals")
    plt.ylabel("Probabilities")
    if number_of_dice * (dice_face - 1) <= 100:
        possResults = list(range(number_of_dice, number_of_dice * dice_face+1))
        probabilities= [diceProb(number_of_dice, i, dice_face) for i in possResults]
        handle = exactDraw(possResults, probabilities)
    else:
        handle = approxDraw(number_of_dice, dice_face)
    return handle

def actualChart(die_str, totals, frequency):
    plt.xlabel("Dice Totals")
    plt.ylabel("Number of occurences")
    newhandle = plt.plot(totals, frequency, alpha=0.5)
    ymin,ymax = plt.ylim()
    plt.ylim([0,ymax])

def dualChart(prefix, servername, die_str, resdict):
    nod, df = map(int, die_str.split('d'))
    predictedChart(nod, df)
    resarr = np.array([(resdict[str(i)] if str(i) in resdict else 0) for i in range(nod, nod*df+1)], dtype='float')
    total = np.sum(resarr)
    if total != 0:
        resarr /= total
    actualChart(die_str, np.arange(nod, nod*df+1), resarr)
    plt.title(prefix+" recorded results for "+die_str+ " on "+servername)
    plt.legend(["Predicted", "Actual"])
    filename = servername+'_'+prefix+'_'+die_str+'.png' 
    plt.savefig(filename)
    plt.clf()
    return filename 

if __name__ == "__main__":
    import progressbar
    number_of_dice = 4
    dice_face = 1000
    filename = 'out.png'

    beg = timer()
    predhandle = predictedChart(number_of_dice, dice_face)
    end = timer()
    print(end-beg)
    testCount = OrderedDict([(i, 0) for i in range(number_of_dice, number_of_dice*dice_face)])
    
    print("Inds:")
    print(list(testCount.keys())[0])
    print(list(testCount.keys())[-1])
    print(len(testData))
    bar = progressbar.ProgressBar(max_value=progressbar.UnknownLength)
    testData = [sum(random.randint(1,dice_face) for i in range(number_of_dice)) for j in range(1000000)]
    while len(testData) > 0:
        first = testData[0]
        testCount[first] = testData.count(first)/1000000
        testData = list(filter(lambda x: x != first, testData))
        bar.update(len(testData))
    print("Is Sorted:")
    print(list(testCount.keys())==sorted(list(testCount.keys())))
    print(len(testCount))
    beg = timer()
    actualChart("Jim the Test Muffin's", str(number_of_dice)+'d'+str(dice_face), list(testCount.keys()), list(testCount.values()))
    end = timer()
    print(end-beg)
    plt.legend(["Predicted", "Actual"])
    plt.savefig(filename)
    plt.clf()
    #def dataChart
