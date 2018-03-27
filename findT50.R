## Script to help find T50 point of germination 
## by Nick Pullen 2018-03-27

## read in your own data here
## germData = read.table("")

## Using example data
germData = structure(list(hours = c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108
), germination = c(0, 0, 0.07, 0.21, 0.5, 0.71, 0.93, 1, 1, 1
)), .Names = c("hours", "germination"), row.names = c(NA, -10L
), class = "data.frame")

## Your data should look like this after reading it into R
   hours germination
1      0        0.00
2     12        0.00
3     24        0.07
4     36        0.21
5     48        0.50
6     60        0.71
7     72        0.93
8     84        1.00
9     96        1.00
10   108        1.00

## Determine non-linear least squares of the sigmoid model
fitmodel <- nls(germination ~ a/(1 + exp(-b * (hours - c))), start=list(a=1,b=0.35,c=50), data=germData)
## summary(fitmodel) ## to see estimated coefficients
## You can also consider the following model which sets the upper asymptote to = 1
## fitmodel <- nls(germination ~ 1/(1 + exp(-b * (hours - c))), start=list(b=0.35,c=50), data=germData)

## Set up an x-vector of hours of the germination experiment
timepoints = seq(min(germData$hours), max(germData$hours), length.out = 200)

## Make a data frame of the timepoints and estimated sigmoid curve
predictedFitData = data.frame(hours = timepoints, fitCurve = predict(fitmodel, newdata = data.frame(hours=timepoints)))

## Plot the data and fit curve
plot(germData)
lines(predictedFitData)

## Find the nearest timepoint to 0.5 i.e. 50% germination
nearestT50index = findInterval(0.5, predictedFitData$fitCurve)

## Get the answer
predictedFitData[nearestT50index,]
