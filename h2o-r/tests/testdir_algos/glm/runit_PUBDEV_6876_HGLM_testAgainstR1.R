setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")
library(MASS)
##
# Comparison of H2O to R with varying link functions for the Negative Binomial family on prostate dataset
# Link functions: log (canonical link)
#				  identity
# With fixed and optimized theta values.
##

test.linkFunctions <- function(){
  print("Read in prostate data.")
  h2o.data = h2o.uploadFile(locate("smalldata/prostate/prostate_complete.csv.zip"), destination_frame="h2o.data")    
  R.data = as.data.frame(as.matrix(h2o.data))
  print("Testing for family: Negative Binomial")
  print("Set variables for h2o.")
  myY = "GLEASON"
  myX = c("ID","AGE","RACE","CAPSULE","DCAPS","PSA","VOL","DPROS")
  print("Define formula for R")
  R.formula = (R.data[,"GLEASON"]~.) 
  ##################  Test with fixed theta and log link
  print("Create models with canonical link: LOG and fixed theta")
  browser()

  	print("Compare model deviances for link function log")


  	h2onegbinomialLog <- h2o.glm(x=myX, y=myY, training_frame=h2o.data, family="negbinomial", link="log",alpha=0.5,
  	                                     lambda=0, nfolds=0, theta=1.0)
    rnegbinomialLog <- glm(formula=R.formula, data=R.data[,2:9], family=negative.binomial(link=log, theta=1.0), na.action=na.omit)
    compareModels(h2onegbinomialLog, rnegbinomialLog)
  	print("Compare model deviances for link function IDENTITY")

  	print("Create models with link: IDENTITY")
  	h2onegbinomialIdentity <- h2o.glm(x=myX, y=myY, training_frame=h2o.data, family="negbinomial", link="log",alpha=0.5,
  	                             lambda=0, nfolds=0, theta=1.0)  rpoissonIdentity <- glm(formula=R.formula, data=R.data[,2:9], family=negative.binomial(link=identity), na.action=na.omit)
    compareModels(h2onegbinomialIdentity, rpoissonIdentity)
    }

compareModels <- function(h2oModel, rModel){
  # compare deviance
  h2oDeviance = h2oModel@model$training_metrics@metrics$residual_deviance / h2oModel@model$training_metrics@metrics$null_deviance
  rDeviance = deviance(rModel)  / h2oModel@model$training_metrics@metrics$null_deviance
  difference = rDeviance - h2oDeviance
  if (difference > 0.01) {
    print(cat("Deviance in H2O: ", h2oDeviance))
    print(cat("Deviance in R: ", rDeviance))
    #  checkTrue(difference <= 0.01, "h2o's model's residualDeviance/nullDeviance is more than 0.01 lower than R's model's")
  }
  
  # compare 
}

doTest("Comparison of H2O to R with varying link functions for the Negative Binomial family", test.linkFunctions)


