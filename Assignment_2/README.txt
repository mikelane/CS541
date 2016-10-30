
Michael Lane
10/30/2016
CS 541
Lab 2

Instructions:

1) Unzip the files into a directory
2) Follow the usage instructions below:

usage: clisp TSP_EA.lisp filename [option value]

REQUIRED
  filename: filename of the data file for the TSP

OPTIONAL: You can include a series of option value pairs after the filename in
          order to set the values of the hyperparameters. The option and values
          are described below.


  The available hyperparameters are:

    hyperparameter                      |  value  |  Command line option
  ------------------------------------------------------------------------
    *population-number*                 |  200    |  popnum
    *percentage-elite*                  |  20     |  eliteperc
    *percent-to-mutate*                 |  5      |  mutateperc
    *number-of-generations*             |  750    |  gennum
    *max-unchanged-generations-allowed* |  300    |  maxunch


 example: clisp TSP_EA.lisp test.txt popnum 100 mutateperc 10 maxunch 100


Note: This program has verbose output. However, the calculations will continue
until output similar to the following is displayed:

Shortest Circuit:
 1: city: 15 location: (148,94)
 2: city: 21 location: (131,102)
 3: city: 19 location: (83,92)
 4: city: 22 location: (49,48)
 5: city: 25 location: (85,13)
 6: city: 10 location: (123,22)
 7: city: 2 location: (161,38)
 8: city: 6 location: (179,43)
 9: city: 8 location: (177,75)
 10: city: 12 location: (169,124)
 11: city: 14 location: (160,145)
 12: city: 4 location: (169,164)
 13: city: 24 location: (123,196)
 14: city: 20 location: (86,188)
 15: city: 5 location: (45,186)
 16: city: 1 location: (54,153)
 17: city: 7 location: (69,133)
 18: city: 16 location: (7,146)
 19: city: 23 location: (4,99)
 20: city: 17 location: (12,72)
 21: city: 13 location: (37,68)
 22: city: 11 location: (24,44)
 23: city: 3 location: (74,18)
 24: city: 18 location: (73,65)
 25: city: 9 location: (86,81)
 26: city: 15 location: (148,94)
The circuit distance is 970.87225
The circuit fitness is 0.0010300017

In the example that generated this result, it took 1m59.112s to generate this
output.

Also, this program outputs its data to a data file located at
output/TSP_GA_OUT_<system time>.data I'll be using this to do a nifty analysis
of the effectiveness of the algorithm (which I'll be happy to show you).

