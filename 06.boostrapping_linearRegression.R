#dataistanbul talk - April 5th, 2017
#HS Olmez (c) 2017
#
# Using bootstrapping to get a sense of variability in the coefficients (intercept and slope)
# of linear regression analysis. We'll compute the confidence intervals on both regression coeff's. 
#
height <- c(8,9,10,11,12,12,13,14,14,15,15,16.5,17.5,17.5,17.5,18,19,19,21)
windsp <- c(11.8,12.6,11.2,11.1,10.5,10.6,10.6,9.95,9.6,10,9,8.75,9.1,9.2,9.25,8.9,8.3,9.35,8.1)
set.seed(4)
fit0 <- lm(height ~ windsp)
print(summary(fit0))
plot(windsp,height, xlab="Wind Speed", ylab="Height", pch=16, col="red")
abline(fit0, col="blue")
#check for homoscedasticity / constant variance
plot(fit0$fitted.values,fit0$residuals,pch=16)
abline(h=0, col='grey')
#test for normality
car::qqPlot(fit0$residuals,pch=16)
qqline(fit0$residuals)
#
plot(windsp,height, xlab="Wind Speed", ylab="Height", pch=16, col="black")
n <- length(height)
Nreps <- 1000
xbs <- vector() ; ybs <- vector()
for (j in 1:Nreps) {
  ranindx <- sample(1:n, n, replace=T)
  x <- vector() ; y <- vector()
  for (i in ranindx) {
    x <- append(x,windsp[i]) #dependent variable
    y <- append(y,height[i]) #independent variable
  }
  fit <- lm(y ~ x)
  ybs <- append(ybs,fit$coefficients[[1]])  #intercept
  xbs <- append(xbs,fit$coefficients[[2]])  #slope 
  abline(fit, col="red")
}
abline(fit0, col="blue")
cat("CI for the intercept: ", quantile(ybs, c(0.025,0.975)), '\n')
cat("CI for the slope    : ", quantile(xbs, c(0.025,0.975)))
smoothScatter(xbs,ybs, xlab="slope", ylab="intercept")
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

scatterhist(xbs, ybs, xlab="slope", ylab="intercept",   
            "Range of intercept vs slope", xsize =2, cleanup=FALSE)
abline(lm(ybs~xbs))
#lines(lowess(xbs,ybs))
points(x=fit0$coefficients[[2]], y=fit0$coefficients[[1]], pch=16, col="red")
