################################
## Read data
## Read the .txt file holding the data
HE.orig <- read.table("soenderborg2_data.csv", sep=" ", header=TRUE, as.is=TRUE)

## The first column 't' needs to be changed from a character to a date.
## Add an extra column
HE.orig$time <- as.Date(x = HE.orig$t, format = "%Y-%m-%d")

## Select the heating period 2009/2010 and the four houses
HE <- subset(HE.orig, ("2009-10-15" <= time & time < "2010-04-16")&
                       (houseId==3|houseId==5|houseId==10|houseId==17))


################################
## Remove rows with one or more NA, hence missing observation 
HE <- na.omit(HE)


################################
## For the short summary of the data
summary(HE)
str(HE)

HE3 <- subset(HE.orig, ("2009-10-15" <= time & time < "2010-04-16")&
               (houseId==3))
HE5 <- subset(HE.orig, ("2009-10-15" <= time & time < "2010-04-16")&
                (houseId==5))
HE10 <- subset(HE.orig, ("2009-10-15" <= time & time < "2010-04-16")&
                (houseId==10))
HE17 <- subset(HE.orig, ("2009-10-15" <= time & time < "2010-04-16")&
                (houseId==17))




################################
## Empirical correlation between the modelling variables
cor(HE[,c(3:6)])


################################
## Scatter plots to see the relations
plot(HE$Q, HE$Ta, xlab='Q heating power [kW]',
                               ylab='Ta ambient temperature [C] ')
plot(HE$Ta, HE$G, xlab='Ta ambient temperature [C]',
                               ylab='G global radiation [W/m2]')
plot(HE$G, HE$Ws, xlab='G global radiation [W/m2]',
                               ylab='Ws Wind speed [m/s]')


################################
## For calculations of the correlation between Q and Ta
cov(HE$Q,HE$Ta)
var(HE$Q)
var(HE$Ta)


################################
## Multiple linear regression model with Ta, G og Ws as explanatory 
## variables and Q as dependent variable
fit1 <- lm(Q ~ Ta + G + Ws, data=HE, na.action=na.exclude)
## Print a summary of the estimation result
summary(fit1)


################################
## Observed model output vs. fitted: The assumption of linearity
plot(HE$Q, fit1$fitted.values, pch=16, cex=0.9, 
     xlab='kW', ylab='Fitted heating power',
     main="Validation of the linearity assumption")

## residual vs. each of the explanatory variables
## Assumption of linearity
plot(HE$G, fit1$residuals, pch=16, cex=0.9,
     xlab='G global radiation', ylab='Residual', 
     main="Validation of the linearity assumption")

plot(HE$Ta, fit1$residuals, pch=16, cex=0.9,
     xlab='Ta ambient temperature', ylab='Residual',
     main="Validation of the linearity assumption")

plot(HE$Ws, fit1$residuals, pch=16, cex=0.9,
     xlab='Ws wind speed', ylab='Residual',
     main="Validation of the linearity assumption")

## residuals vs. fittet: Antagelsen om konstant varians
plot(fit1$fitted.values, fit1$residuals, pch=16, cex=0.9,
     xlab='Fitted values', ylab='Residual',
     main="Validation of variance homogenity")

## normal plot of residuals: Normalfordelingsantagelsen
qqnorm(fit1$residuals, xlab='z-scores', ylab='Residual', 
       main='Validation of normal distribution assumption')
qqline(fit1$residuals)


################################
## t-quantile for calculations of the confidence intervals
qt(0.975, fit1$df.residual)

## R command that gives the confidence intervals for all 
## the coefficient estimates
confint(fit1, level=0.95)


################################
## Includes t-test values for each coefficient for the null hypothesis 
## that it is different from zero
summary(fit1)

## Alternatively it can be decided using the critical levels
qt(0.975, fit1$df.residual)

fit2 <- lm(Q ~ Ta + G, data=HE, na.action=na.exclude)
summary(fit2)

################################
## Select observations from some days which are not the data on 
## which models was fitted for prediction
HE.sel <- subset(HE.orig, (time=="2008-12-06" & houseId==3)|
                          (time=="2009-02-22" & houseId==5)|
                          (time=="2009-03-12" & houseId==10)|
                          (time=="2009-04-01" & houseId==17))

## Print the observations
HE.sel


fitSelected <- fit2

## Prediction and prediction interval is given by
predict(fitSelected, newdata=HE.sel, interval="prediction", level=0.95)

fitSelected
se(fit2)