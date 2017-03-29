#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
#
# Data for beer and water (as provide in John Rauser's talk)
B <- c(27,19,20,20,23,17,21,24,31,26,28,20,27,19,25,31,24,28,24,29,21,21,18,27,20)
W <- c(21,19,13,22,15,22,15,22,20,12,24,24,21,19,18,16,23,20)
nB <- length(B) ; nW <- length(W)
N <- nB + nW
deltamu <- (mean(B) - mean(W))
Combined <- c(B,W)
Nreps <- 50000 #number of iterations
meanDiff <- numeric(Nreps)
Ndeltamu <- 0
set.seed(11) #for reproducibility
for(i in 1:Nreps) {
  data <- sample(Combined, N, replace=FALSE)
  newB <- data[1:nB]
  newW <- na.omit(data[nB+1:N])
  meanDiff[i] <- mean(newB) - mean(newW)
  if(meanDiff[i] > deltamu) {
    Ndeltamu <- Ndeltamu + 1
  }
}
cat("Number of cases where diff > 4.4: ", Ndeltamu, '\n')
cat("The ratio of trials for diff > 4.4 to total # of sims: ",Ndeltamu/Nreps, '\n')
hist(meanDiff, freq=FALSE, col="grey")
curve(dnorm(x, mean(meanDiff), sd(meanDiff)), add=TRUE, col="darkblue", lwd=2)
points(x=4.4, y=0, pch=16)
abline(v=4.4, col="red")