setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")
library(MASS)
library(hglm)
##
# Copy example from HGLM package and compare results from our implementation.
##

test.HGLMData1 <- function() {
  browser()
  set.seed(123)
  n.clus <- 5 # number of clusters
  n.per.clus <- 20 # Number of points within each cluster
  sigma2_u <- 0.2  # variance of random effect
  sigma2_e <- 1 # residual_deviance
  n <- n.clus*n.per.clus
    X <-matrix(1,n,1)
  Z <- diag(n.clus)%x%rep(1,n.per.clus)
  a <- rnorm(n.clus, 0, sqrt(sigma2_u))
  e <- rnorm(n, 0, sqrt(sigma2_e))
  mu <- 0
  y <- mu+Z%*%a+e
  lmm <- hglm(y=y, X=X, Z=Z)
  summary(lmm)
 }

doTest("Comparison of H2O to R with HGLM 1", test.HGLMData1)


