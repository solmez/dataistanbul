#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
#
# Glass Tower example
x <- c(48,24,32,61,51,12,32,18,19,24,21,41,29,21,25,23,42,18,23,13)
n <- length(x)
cat("Mean   : ", mean(x), "     Std err   : ",sd(x)/sqrt(n), '\n')
set.seed(7) #an arbitrary seed number
randx <- replicate(10000, mean(sample(x, n, replace=T)))
bs_mean <- mean(randx) ; bs_sd <- sd(randx)
cat("Mean_bs: ", bs_mean, "  Std.dev_bs: ", bs_sd, "\n")
CI <- quantile(randx, c(0.025,0.975))
cat("CI for bootstrapped samples:", CI)
hist(randx, freq=FALSE, col="grey")
abline(v=28.9, col="red")
