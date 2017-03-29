#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
#
# We'll try to point out the effect of bias on the sample variance and show the Bessel correction
par(xpd=FALSE) #to prevent horizontal lines extending beyond plotting region
set.seed(2) #for reproducibility
#Population we'll use during the simulations
pop <- sample(1:20, 400, replace=TRUE) #random sample of 400 in the range of [1,20] 
popmean <- mean(pop) ; popvar <- var(pop) ; popsd <- sd(pop)
pdist <- hist(pop, main=paste("Population dist. with mean=", round(popmean, digits=2),
                              "variance=", round(popvar, digits=2), "and N= 400"),
              col="yellow", xlab="Value of random var", ylab="Counts",
              breaks=0:(max(pop)+1), xaxt="n", right=FALSE, ylim=c(0,30))
axis(1, at=pdist$mids, labels=0:max(pop))
sampSize <- c(2,3,4,5,6,7,8,9,10) #number of sample sizes under investigation
N <- 5000 #number of iterations
ratio <- rep(NA,10) ; varx <- rep(NA,N) ; meanx <- rep(NA,N) #initialize variables
plot(c(0,100)~c(0,20), col=NA, main="Sample statistics (hor/ver lines: population values)",
     xlab="Sample mean", ylab="Sample variance", ylim=c(-2,102))
colorcode <- c("red", "blue", "grey47", "green", "cyan", "yellow", "orange", "rosybrown1", "magenta")
#
for (j in 1:9) {
  for (i in 1:N) {
    x <- sample(pop, sampSize[j], replace=FALSE)
    meanx[i] <- mean(x)
    varx[i] <- sum((x-meanx[i])^2)/sampSize[j]
  }
  points( varx~meanx, col=colorcode[j] ) 
  ratio[j] <- 100*(mean(varx)/popvar)
}
abline(h=popvar, col="black")
abline(v=mean(pop), col="black")
legend('topright', legend=c("2","3","4","5","6","7","8","9","10"), 
       col=c("red", "blue", "grey47", "green", "cyan", "yellow", "orange", "rosybrown1", "magenta"), pch=1) 
#
bp <- barplot(ratio, main="Sample size vs Variance", xlab="Sample size",
              col=c("red", "blue", "grey47", "green", "cyan", "yellow", "orange", "rosybrown1", "magenta"),
              ylab="Sample var / Population var", ylim=c(0,100))
axis(1, at=bp,labels=2:11)
abline(h=c(seq(0,100,10)), col="gray")

