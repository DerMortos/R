# Descriptive Statistics

vars <-c("mpg", "hp", "wt")
head(mtcars[vars])

# skewedness: horizontal pull on the data
# kurtosis: vertical pull (peak's height)
    # negative Kurtosis:  Platykurtic
    # Positive Kurtosis:  LeptoKurtic

# Base R install does not provide skew or kurtosis.
mystats <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <-x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
}

sapply(mtcars[vars], mystats)
sapply(mtcars[vars], range)

# Desriptive statistics by grouping (automatic/manual)
aggregate(mtcars[vars], by=list(am=mtcars$am), mean)
aggregate(mtcars[vars], by=list(am=mtcars$am), sd)

dstats <- function(y)(c(mean=mean(y), sd=sd(y)))
by(mtcars[vars], mtcars$am, dstats)

###########################################################
# 7.2 Frequency and Contingency Tables
###########################################################
library(vcd)
head(Arthritis)
summary(Arthritis)

# One-way Tables
mytable <- with(Arthritis, table(Improved)) # frequency count
prop.table(mytable)                         # proportions
prop.table(mytable)*100                     # percentages

# Two-way Tables
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
mytable
margin.table(mytable,1)
margin.table(mytable,2)

# Three-way contingency table
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis)
mytable
ftable(mytable)
# marginal frequencies
margin.table(mytable,1)
margin.table(mytable,2)
margin.table(mytable,3)

# treatment x Improved marginal frequencies
margin.table(mytable, c(1,3))
# improve proportions for treatment x sex
ftable(prop.table(mytable, c(1,2)))
ftable(addmargins(prop.table(mytable, c(1,2)),3))

# tests of independence

#Chi-square test of independence
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)           # treatment and improved not independent
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)           # gender and improved independent

# Fisher's exact test
# null hypothesis of independence of rows and columns
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

# Cochran-Mantel-Haeszel test
# tests the hypothesis that treatment and improved variables 
# are independent witin each level Sex.
# Test assumes that there's no three-way interaction.
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

###########################################################
# 7.2 Frequency and Contingency Tables
###########################################################
