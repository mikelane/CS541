
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
    *population-number*                 |  100    |  popnum
    *percentage-elite*                  |  20     |  eliteperc
    *percent-to-mutate*                 |  5      |  mutateperc
    *number-of-generations*             |  100    |  gennum
    *max-unchanged-generations-allowed* |  100    |  maxunch


 example: clisp TSP_EA.lisp test.txt popnum 100 mutateperc 10 maxunch 100


Note: This program has verbose output. However, the calculations will continue
until output similar to the following is displayed:

Shortest Circuit:
 1: city: 15 location: (148,94)
 2: city: 12 location: (169,124)
 3: city: 4 location: (169,164)
 4: city: 9 location: (86,81)
 5: city: 18 location: (73,65)
 6: city: 22 location: (49,48)
 7: city: 5 location: (45,186)
 8: city: 20 location: (86,188)
 9: city: 24 location: (123,196)
 10: city: 14 location: (160,145)
 11: city: 21 location: (131,102)
 12: city: 19 location: (83,92)
 13: city: 7 location: (69,133)
 14: city: 1 location: (54,153)
 15: city: 16 location: (7,146)
 16: city: 23 location: (4,99)
 17: city: 17 location: (12,72)
 18: city: 13 location: (37,68)
 19: city: 11 location: (24,44)
 20: city: 3 location: (74,18)
 21: city: 25 location: (85,13)
 22: city: 10 location: (123,22)
 23: city: 2 location: (161,38)
 24: city: 6 location: (179,43)
 25: city: 8 location: (177,75)
 26: city: 15 location: (148,94)
The circuit distance is 1102.7378
The circuit fitness is 9.068339E-4

In the example that generated this result, it took 18.507s to generate this
output. However, the timing will be very dependent on the values of the
hyperparameters.

Also, this program outputs its data to a data file located at
output/TSP_GA_OUT_<system time>.data I'll be using this to do a nifty analysis
of the effectiveness of the algorithm (which I'll be happy to show you).

