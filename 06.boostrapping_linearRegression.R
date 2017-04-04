#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
#
# Using bootstrapping to get a sense of variability in the coefficients (intercept and slope)
# of linear regression analysis. We'll compute the confidence intervals on both regression coeff's. 
#
windsp <- c(8.1, 8.4, 8.8, 8.7, 9, 9.1, 9.2, 9.3, 9.4, 9.6, 9.9, 10, 10, 10.5, 10.6, 10.6, 11.2, 11.8, 12.6)
height <- c(21, 19, 18, 16, 15, 17, 17, 17, 19, 14, 14, 15, 11, 12, 12, 13, 10, 8, 9)
HWdata <- as.data.frame(cbind(height,windsp)) #needed for the boot pckg
fit0 <- lm(height ~ windsp, data=HWdata)
print(summary(fit0))
plot(windsp,height, xlab="Wind Speed", ylab="Height", pch=16, col="red")
abline(fit0, col="blue")
plot(fit0$fitted.values,fit0$residuals,pch=16) #check for homoscedasticity
abline(h=0, col='grey') 
car::qqPlot(fit0$residuals,pch=16) #test for normality
qqline(fit0$residuals) 
#
set.seed(4)
#
n <- length(height) ; Nreps <- 1000
b0 <- NULL ; b1 <- NULL
plot(windsp,height, xlab="Wind Speed", ylab="Height", pch=16, col="black")
for (iter in 1:Nreps) {
  databs <- HWdata[sample(1:n, size=n, replace=T),]
  fitbs <- lm(height ~ windsp, data=databs)
  b0 <- append(b0,fitbs$coefficients[[1]])  #intercept
  b1 <- append(b1,fitbs$coefficients[[2]])  #slope 
  abline(fitbs, col="gray")
}
abline(fit0, col="blue")
#
cat("Bootstrapped intercept & std. error: ", mean(b0), sd(b0), '\n')
cat("CI for the intercept               : ", quantile(b0, c(0.025,0.975)), '\n')
cat("Bootstrapped slope & std. error    : ", mean(b1), sd(b1), '\n')
cat("CI for the slope                   : ", quantile(b1, c(0.025,0.975)))
abline(mean(b0),mean(b1), col="red")
#
# Run with the boot package
library(boot)
index <- n
boot.fn <- function(data, index) return(coef(lm(height~windsp, data=HWdata, subset=index)))
cat('\n\n')
print(summary(boot(HWdata, boot.fn, R=Nreps)))
#
#
smoothScatter(b1,b0, xlab="slope", ylab="intercept")
points(x=fit0$coefficients[[2]], y=fit0$coefficients[[1]], pch=16, col="red")

scatterhist <- function(x, y, xlab = "", ylab = "", plottitle="", 
                        xsize=1, cleanup=TRUE,...){
  # save the old graphics settings-- they may be needed
  def.par <- par(no.readonly = TRUE)
  zones <- matrix(c(1,1,1, 0,5,0, 2,6,4, 0,3,0), ncol = 3, byrow = TRUE)
  layout(zones, widths=c(0.3,4,1), heights = c(1,3,10,.75))
  # tuning to plot histograms nicely
  xhist <- hist(x, plot = FALSE)
  yhist <- hist(y, plot = FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  # for all three titles: 
  # drop the axis titles and omit boxes, set up margins
  par(xaxt="n", yaxt="n",bty="n",  mar = c(.3,2,.3,0) +.05)
  # fig 1 from the layout
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(plottitle), cex=2)
  # fig 2
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(ylab), cex=1.5, srt=90)
  # fig 3
  plot(x=1,y=1,type="n",ylim=c(-1,1), xlim=c(-1,1))
  text(0,0,paste(xlab), cex=1.5)
  # fig 4, the first histogram, needs different margins
  # no margin on the left
  par(mar = c(2,0,1,1))
  barplot(yhist$counts, axes = FALSE, xlim = c(0, top),
          space = 0, horiz = TRUE)
  # fig 5, other histogram needs no margin on the bottom
  par(mar = c(0,2,1,1))
  barplot(xhist$counts, axes = FALSE, ylim = c(0, top), space = 0)
  # fig 6, finally, the scatterplot-- needs regular axes, different margins
  par(mar = c(2,2,.5,.5), xaxt="s", yaxt="s", bty="n")
  # this color allows traparency & overplotting-- useful if a lot of points
  plot(x, y , pch=19, col="#00000022", cex=xsize, ...)
  # reset the graphics, if desired 
  if(cleanup) {par(def.par)}
}

scatterhist(b1, b0, xlab="slope", ylab="intercept",   
            "Range of intercept vs slope", xsize =2, cleanup=FALSE)
abline(lm(b0~b1))
#lines(lowess(b1,b0))
points(x=fit0$coefficients[[2]], y=fit0$coefficients[[1]], pch=16, cex=2.5, col="red")

