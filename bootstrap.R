#' Bootsrapping an estimator of the mean of a pareto distribution.
#' Our model is an estimator of the mean built constructed sample data.
#' We randomly sample, with replacement, from the training data (the data we constructed our estimator from),
#' We will do this B times giving us B bootrstrapped datasets, we will then examine the behaviour of our 'model'
#' by fitting on each bootstrapped dataset. 
#' We can use this to infer statistics about our model, however note that some statistics generated this way
#' such as the prediction error of our model may be flawed due to overlap of data points between fitting and 
#' prediction.

 
#' @description 
#' Bootstrap B samples and compute statistic on them
#' 
#' @param B A number
#' @param statistic A function 
#' @param data A data frame
#' @return A series of statistic computed on each subset of data
bootstrap <- function(B, statistic, dataset){
  # replicate computes statistic B times
  boot.samples<- replicate(B, 
                          statistic(
                            sample(dataset, replace = TRUE)
                            )
                          )
  # data frame to store output statistics
  boot.stats <- data.frame(t0 = 0,
                           std.err = 0, 
                           bias = 0) 
  # compute observed value of statistic
  boot.stats$t0 <- statistic(dataset) 
  # standard deviation of statistics is an estimate of standard error
  boot.stats$std.err <- sd(boot.samples) 
  # bias of our estimator 
  boot.stats$bias <- mean(boot.samples) - boot.stats$t0
  return(boot.stats)
}

# in this case our model is just an estimate of the mean 
stat <- function(series, indices){
  return(mean(series[indices]))
}

set.seed(100)
# bootstrapping from an exponential distribution
x = rexp(100, 1)
b <- bootstrap(10, stat, x)

#' Next we will simulate a Pareto distribution
#' using the inverse transform sampling method.
#' To do so we will need to compute the CDF of 
#' the distribution and then find its inverse.
#' https://en.wikipedia.org/wiki/Inverse_transform_sampling
#' 
#' suppose we start with a Pareto distribution
#' with PDF of f(x) = (a*(l^a)) / (a+x)^(a+1)
#' this has CDF F(x) = 1 - (a/(a+x))^a
#' we can simulate draws from this distribution
#' using f(u) = l/((1-u)^(1/a)) - l

pareto <- function(n, alpha, lambda){
  u = runif(n)
  x = lambda/((1-u)^(1/alpha)) - lambda
  return(x)
}

x = pareto(100, 100, 2000) # try out and see how alpha scales and lambda shifts
hist(x)
pareto.boot <- bootstrap(100, stat, x)
