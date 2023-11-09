library(car)
library(gvlma)

# simple linear regression
fitWomen <- lm(weight ~ height, data=women)
summary(fitWomen)

women$weight
fitted(fitWomen)
residuals(fitWomen)
plot(women$height, women$weight,
      xlab = "Height (in)",
      ylab = "Weight (lbs)")
abline(fitWomen)


# polynomial regression
fit2Women <- lm(weight ~ height + I(height^2), data=women)
summary(fit2Women)

women$weight
fitted(fit2)
residuals(fit2)
plot(women$height, women$weight,
     xlab = "Height (in)",
     ylab = "Weight (lbs)")
lines(women$height, fitted(fit2Women))

# n-th degree polynomial regression
fit3 <- lm(weight ~ height + I(height^2) + I(height^3), data=women)
summary(fit3)
fitted(fit3)
residuals(fit3)

# plotting of the bivariate relationship
scatterplot(weight ~ height, data=women, spread=FALSE, lty.smooth=2, pch=19,
            main="Women Age 30-39",
            xlab="Height (in)",
            ylab="Weight (lbs)")

# Multiple Linear Regression
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
cor(states)
scatterplotMatrix(states, spread=FALSE, lty.smooth=2, main="Scatter Plot Matrix")

fitMurders <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

# MLR w/ a significant interaction term
fitCars  <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fitCars)

########################################################
########################################################

# Regression Diagnostics
confint(fitMurders)

# a typical approach
# lm women setting parameters
par(mfrow=c(2,2))
plot(fitWomen)

plot(fit2Women)

# outliers 13 and 15 can be removed based on cooks distance
newfit2Women <- lm(weight ~ height + I(height^2), data=women[-c(13,15),])
plot(newfit2Women)

#enhanced approach to murders LM
par(mfrow=c(1)
qqPlot(fitMurders, labels=row.names(states), id.method="identify", simulate=TRUE, main="Q-Q Plot")

residplot <- function(fitMurders, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
              xlab="Studentized Residual", main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

#distribution of Errors
residplot(fitMurders)

#independence of errors
durbinWatsonTest(fitMurders)

#linearity
crPlots(fitMurders)

#assesing homoscedasciticity
  #non constant variance test
ncvTest(fitMurders)
spreadLevelPlot(fitMurders)

# Global Validation of LM Assumption
gvmodel <-gvlma(fitMurders)
summary(gvmodel)

########################################################
########################################################

# Multicollinearity
  #Variance Inflation Factor
vif(fitMurders)
sqrt(vif(fitMurders)) > 2 # indicates the degree to which the confidence interval for that variable’s regression parameter is expanded relative to a model with uncorrelated predictors 
