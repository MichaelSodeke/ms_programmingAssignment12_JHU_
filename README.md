# OVERVIEW
## [Predicting Human Activity]

### Background
Traditionally, **Human Activity Recognition** has focused on predicting *which* activity was
performed at a specific point in time. However, researchers at Groupware focused on
investigating *how well* an activity was performed by the wearer. Six young participants
between 20-28 years were asked to perform a series of five activities to assist Groupware
with this investigation.

### Model Selection
The goal of CASE I was to select a model function appropriate for predicting the manner in
which participants perfored a series of dumbbell exercises. It turned out that the Random Forest
algorithm (with all covariates) performed the best out of the four, with an out of sample error
(OSE) of 0.0012.

### Random Forest Prediction
CASE II focused on employing the Random Forest algorthm selected in CASE I for predicting
the manner in which participants perfomred a certain activity. A score of 20/20 was receieved after
applying prediction results on the quiz.

### Instructions
Within this directory run the following code. Note: make sure to install the required packages
in R-script file `0-loadPackages.r`
```
dir()

source("n-runAll.r")
```

# CITATIONS

[no citations needed].