Michael Lane
11/15/16

CS541: Artificial Intelligence
Lab 3 - Artificial Neural Network in Lisp

There are 3 lisp files that run my program:

 - mnist_digit_data_loader.lisp  Loads the binary mnist file into memory
 - nn_utils.lisp  This holds all the functions that are used on the neural
                  network, like forward-propagate, calculate-errors, etc.
 - lane_nn.lips  This runs the training and evaluation of the neural network

Obviously, this is much faster when run as a compiled file. I have included the
compiled files (.fas files); however, if you want to compile it yourself, you'd
follow these steps:

  1) Compile the mnist_digit_data_loader.lisp file and save it as mnist-loader

     $ clisp -c mnist_digit_data_loader.lisp -o mnist-loader

  2) Compile the nn files

     $ clisp -c nn_utils.lisp lane_nn.lisp

Once you have the compiled files, it is best to run the program in the clisp
REPL. That way, once the neural net loads, you can run various tests and you can
see the data. To do that enter the REPL

  $ clisp

Once you're in the REPL, load the lane_nn.fas file:

  [1]> (load "lane_nn.fas")

This will kick off the Neural Network training algorithm which will read in the
training and testing data (which takes a few minutes), it will then start the
training algorthm. For 100 epochs, it first shuffles the training data set, then
for each item in the training set, it forward propagates the item, calculates
the error terms for the output and hidden layers, it back propagates by updating
the weights.

After each epoch, it evaluates the accuracy of both the training and testing
data sets by forward propagating the activations and then using the index of the
max output node value as the classification. Once the accuracy and confusion
matrices have been calculated, the next epoch begins.

The algorithm runs reasonably quickly with 5 hidden nodes. I didn't time it, but
it seemed to take 20 or 30 minutes. With 49 hidden nodes, it took about 36
hours. So it doesn't scale well on a uniprocessor. It'd be perfectly simple to
make this a parallel algorithm that would fork off as many processes as weights
or weight vectors in order to save a great deal of time.

