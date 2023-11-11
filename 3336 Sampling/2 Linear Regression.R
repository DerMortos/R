library(car)

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
fitted(fit2Women)
residuals(fit2Women)
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
summary(fitMurders)

# MLR w/ a significant interaction term
fitCars  <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fitCars)

# Regression Diagnostics
########################################################
########################################################

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
par(mfrow=c(1,1))
qqPlot(fitMurders, labels=row.names(states), id.method="identify",
          simulate=TRUE, main="Q-Q Plot")

residplot <- function(fitMurders, nbreaks=10) {
  z <- rstudent(fitMurders)
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
library(gvlma)
gvmodel <-gvlma(fitMurders)
summary(gvmodel)

# Multicollinearity
  #Variance Inflation Factor
vif(fitMurders)
sqrt(vif(fitMurders)) > 2 # indicates the degree to which the confidence
                          # interval for that variable’s regression parameter is
                      # expanded relative to a model with uncorrelated predictors 

# Unusual Observations
########################################################
########################################################

# Bonfeerroni
outlierTest(fitMurders)

# High Leverage Points
hat.plot <- function(fitMurders) {
        p <- length(coefficients(fitMurders))
        n <- length(fitted(fitMurders))
        plot(hatvalues(fitMurders), main = "Index Plot of Hat Values")
        abline(h=c(2,3)*p/n, col="red", lty=2)
        identify(1:n, hatvalues(fitMurders), names(hatvalues(fitMurders)))
        }
hat.plot(fitMurders)

# Influential Observations
  # Cook's Plot
cutoff <- 4/(nrow(states)-length(fitMurders$coefficients)-2)
plot(fitMurders, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

# Added Variabnle Plots
avPlots(fitMurders, ask=FALSE, onepage=TRUE, id.method="identify")

# Influence Plot
influencePlot(fitMurders, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's Distance")

# Corrective Measures
########################################################
########################################################

# Deleting observations
  # Deleting outliesr can impve a datasets fit

#Transforming variables
# -2        -1      -0.5        0         0.5       1       2
# 1/Y^2     1/y     1/sqrt(Y)   log(Y)    sqrt(Y)   None    Y^2

# Box-Cox transformationn to normality
summary(powerTransform(states$Murder))

# boxTidwell used to generate maximum-likelihood estimates of predictor powers 
# that can improve linearity
boxTidwell(Murder~Population + Illiteracy, data=states)
          # results suggest that neither varable needs to be transformed

# Selecting the "best" Regression Model
########################################################
########################################################

# Comparing Models nested models using the ANOVA function (required nested models)
fitMurders2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fitMurders2,fitMurders)

# Comparing Models with the AIC (Nested models not requried)
        # What is meant by nested models?
AIC(fitMurders, fitMurders2)

# Variable Selection
  # Step-wise Regression
  
  #Backward stepwise selection
  library(MASS)
  stepAIC(fitMurders, directions="backward")

  # All Subsets Regression
  library(leaps)
  leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost,
                      data=states, nbest=4)
  plot(leaps, scale="adjr2")
  
  subsets(leaps, statistic="cp", main="Cp Plot for All Subsets Regression")
  abline(1,1,lty=2,col="red")
  
# Taking The Analysis Further
########################################################
########################################################

# Cross Validation
  # Function for k-fold cross-validated R-Square
shrinkage <- function(fitMurders, k=10){
  require(bootstrap)
    
  theta.fitMurders <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fitMurders,x){cbind(1,x)%*%fitMurders$coeff}
    
  x <- fitMurders$model[,2:ncol(fitMurders$model)]
  y <- fitMurders$model[,1]
    
  results <- crossval(x,y, theta.fitMurders, theta.predict, ngroup=k)
  r2 <- cor(y, fitMurders$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}
  
shrinkage(fitMurders)
shrinkage(fitMurders2)

# Relative Importance
zstates <- as.data.frame(scale(states))
zfitMurders <- lm(Murder~Population + Income + Illiteracy + Frost, data=zstates)
coef(zfitMurders)

# Relative Weights for Calculating Relative Importance of predictors
relweights <- function(fitMurders,...){
  R <- cor(fitMurders$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  lbls <- names(fitMurders$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg=lbls,
          ylab="% of R-Square",
          xlab="Predictor Variables",
          main="Relative Importance of Predictor Variables",
          sub=paste("R-Square=", round(rsquare, digits=3)),
          ...)
  return(import)
}

relweights(fitMurders, col="lightgrey")
