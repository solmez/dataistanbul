#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
#
# Probabilty of a fair coin showing 22 Heads by chance in 30 tosses
# Random number generation for simulating a binomial distribution
N = 10000 #number of trials
M = 0
set.seed(10)
for (i in 1:N) {
  x1 <- sample(0:1, 30, replace=T)
  if (sum(x1) >= 22) {
    M = M + 1
  }
}
cat("Ratio of trials with heads more than 22 to all: ", M/N, '\n')
# Now do the same by using a binomial dist.
prob <- print(1-pbinom(21, 30, .5))
cat("Theoretical value of prob:", prob)
print(pbinom(30, size=30, prob=1/2) - pbinom(21, size=30, prob=1/2))
print(diff(pbinom(c(21,30), size = 30, prob = 1/2)))
#
#rbinom() is used to simulate N independent binomial random variables.
print(hist(rbinom(10000,30,.5),freq=FALSE, breaks=seq(0.5,30.5,1), ylim=c(0,0.15)))
print(lines(seq(0,30,1),dbinom(seq(0,30,1),30,0.5), col="darkblue", lwd="2"))
points(x=22, y=0, pch=16)
abline(v=22, col="red")

