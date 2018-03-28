## Script to help find T50 point of germination for many samples
## by Nick Pullen 2018-03-28

## Example data
allData = structure(list(hours = c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 
0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 0, 12, 24, 36, 48, 60, 
72, 84, 96, 108), germination = c(0, 0.05, 0.08, 0.11, 0.41, 
0.68, 0.92, 1, 1, 1, 0, 0.1, 0.11, 0.25, 0.49, 0.78, 0.98, 
1, 1, 1, 0, 0.04, 0.1, 0.2, 0.45, 0.78, 1, 1, 1, 1), sample = c("a", 
"a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", 
"b", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c", "c", "c", 
"c", "c", "c")), .Names = c("hours", "germination", "sample"), row.names = c(NA, 
-30L), class = "data.frame")

## Your data should look like this after reading it into R
   hours germination sample
1      0        0.00      a
2     12        0.05      a
3     24        0.08      a
4     36        0.11      a
5     48        0.41      a
6     60        0.68      a
7     72        0.92      a
8     84        1.00      a
9     96        1.00      a
10   108        1.00      a
11     0        0.00      b
12    12        0.10      b
13    24        0.11      b
14    36        0.25      b
15    48        0.49      b
16    60        0.78      b
17    72        0.98      b
18    84        1.00      b
19    96        1.00      b
20   108        1.00      b
21     0        0.00      c
22    12        0.04      c
23    24        0.10      c
24    36        0.20      c
25    48        0.45      c
26    60        0.78      c
27    72        1.00      c
28    84        1.00      c
29    96        1.00      c
30   108        1.00      c

## Function that calculates the T50 and returns the function (i.e. sigmoid) parameters
T50function = function(dataset, rate) {
  ## Determine non-linear least squares of the sigmoid model
  ## As the nls function is quite sensitive we catch any errors
  fitmodel <- tryCatch(nls(germination ~ a/(1 + exp(-b * (hours - c))), start=list(a=1,b=rate,c=50), data=dataset), error=function(e){cat("ERROR in sample ***", unique(dataset$sample), "*** Reason: ", conditionMessage(e), "\n", sep=""); return(NULL)})
  ## If the sample errored then return NULL, which enables us to move onto the next sample
  if (is.null(fitmodel)) {cat("Trying next sample\n"); return(NULL)}
  ## Set up an x-vector of hours of the germination experiment
  timepoints = seq(min(dataset$hours), max(dataset$hours), length.out = 200)
  ## Make a data frame of the timepoints and estimated sigmoid curve
  predictedFitData = data.frame(hours = timepoints, fitCurve = predict(fitmodel, newdata = data.frame(hours=timepoints)))
  ## Find the nearest timepoint to 0.5 i.e. 50% germination
  nearestT50index = findInterval(0.5, predictedFitData$fitCurve)
  ## Return the T50 and curve parameters (in case you want to plot the function)
  return(c(T50hours = predictedFitData[nearestT50index, "hours"], parameter.a = coefficients(fitmodel)["a"], parameter.b = coefficients(fitmodel)["b"], parameter.c = coefficients(fitmodel)["c"]))
}

## Run the above function over all the different samples, by first splitting the data frame into sample-sized pieces
result = lapply(X=split(allData, allData$sample), FUN=T50function, rate=0.35)

$a
     T50hours parameter.a.a parameter.b.b parameter.c.c 
   52.1005025     1.0164923     0.1072542    52.5294666 

$b
     T50hours parameter.a.a parameter.b.b parameter.c.c 
  47.21608040    1.02659492    0.09466735   47.85366635 

$c
     T50hours parameter.a.a parameter.b.b parameter.c.c 
   48.3015075     1.0230344     0.1099093    49.2392127

## Trying with a small change in the rate parameter makes sample a have an error
lapply(X=split(allData, allData$sample), FUN=T50function, rate=0.45)
ERROR in sample ***a*** Reason: singular gradient
Trying next sample
$a
NULL

$b
     T50hours parameter.a.a parameter.b.b parameter.c.c 
  47.21608040    1.02659490    0.09466735   47.85366451 

$c
     T50hours parameter.a.a parameter.b.b parameter.c.c 
   48.3015075     1.0230344     0.1099093    49.2392104 

