library(testthat)
library(hydromad)
library(doParallel)

source("helper-parallel-tests.R")
# source("tests/testthat/helper-parallel-tests.R")

context("Checking parallelisation works on all platforms")

# Take data from evalPars docs
set.seed(0)
data(Cotter)
obs <- Cotter[1:1000]
modx <- hydromad(obs,
  sma = "cmd",
  routing = "expuh"
)

myObjective <- function(Q, X, ...) sum(Q - X, na.rm = T)

# Set up mod with user defined SMA. Have to manually input default pars
modx2 <- hydromad(obs,
  sma = "cmd2",
  routing = "expuh",
  f = c(0.01, 3.00),
  e = c(0.01, 1.50),
  d = c(50, 500),
  shape = 0
)

pars <- parameterSets(getFreeParsRanges(modx), 10, method = "random")

# Serial execution
test_that("Serial", {
  s <- evalPars(pars,
    object = modx,
    objective = hmadstat("r.squared")
  )
  expect_equal(length(s), 10)
  expect_equal(sum(is.na(s)), 0)
})

test_that("Serial with custom objFun", {
  s <- evalPars(pars,
    object = modx,
    objective = myObjective
  )
  expect_equal(length(s), 10)
  expect_equal(sum(is.na(s)), 0)
})

test_that("Serial with custom SMA", {
  s <- evalPars(pars,
    object = modx2,
    objective = myObjective
  )
  expect_equal(length(s), 10)
  expect_equal(sum(is.na(s)), 0)
})

# Parallel
# Set up parallel
# Use PSOCK as a more stringent test
# FORK (macOS/linux only) will work as it clones the master, PSOCK doesn't
cl <- parallel::makeCluster(spec = 2, type = "PSOCK")
registerDoParallel(cl)

# This needs to be done to export custom SMAs
clusterExport(cl, "cmd2.sim")

test_that("Parallel foreach", {
  p <- evalPars(pars,
    object = modx,
    objective = hmadstat("r.squared"),
    parallel = "foreach"
  )
  expect_equal(length(p), 10)
  expect_equal(sum(is.na(p)), 0)
})

test_that("Parallel foreach with custom objFun", {
  p <- evalPars(pars,
    object = modx,
    objective = myObjective,
    parallel = "foreach"
  )
  expect_equal(length(p), 10)
  expect_equal(sum(is.na(p)), 0)
})

test_that("Parallel foreach with custom SMA", {
  p <- evalPars(pars,
    object = modx2,
    objective = hmadstat("r.squared"),
    parallel = "foreach"
  )
  expect_equal(length(p), 10)
  expect_equal(sum(is.na(p)), 0)
})

# clusterApply
test_that("Parallel clusterApply", {
  p <- evalPars(pars,
    object = modx,
    objective = hmadstat("r.squared"),
    parallel = "clusterApply"
  )
  expect_equal(length(p), 10)
  expect_equal(sum(is.na(p)), 0)
})

test_that("Parallel clusterapply with custom objFun", {
  p <- evalPars(pars,
    object = modx,
    objective = myObjective,
    parallel = "clusterApply"
  )
  expect_equal(length(p), 10)
  expect_equal(sum(is.na(p)), 0)
})

test_that("Parallel clusterapply with custom objFun", {
  p <- evalPars(pars,
    object = modx2,
    objective = myObjective,
    parallel = "clusterApply"
  )
  expect_equal(length(p), 10)
  expect_equal(sum(is.na(p)), 0)
})

stopCluster(cl)

# Test options
# Copy of original options
original_opts <- hydromad.options()

# Modify the opts and set them as default
modified_opts <- hydromad.options()
modified_opts$cmd$f <- c(1.5, 4.0)
hydromad.options(modified_opts)

test_that("Opts are equal in a function env", {
  fun_cmd <- hydromad.getOption('cmd')
  expect_equal(fun_cmd$f, modified_opts$cmd$f)
})

# Start cluster again
cl <- parallel::makeCluster(spec = 2, type = "PSOCK")
registerDoParallel(cl)

# Parallel opts testing
test_that("Expect opts to match original opts with no export", {
  par_cmd <- foreach(i=1, .packages='hydromad') %dopar% {
    return(hydromad.getOption('cmd'))
  }
  expect_equal(par_cmd[[1]]$f, original_opts$cmd$f)
})

# Export opts
opts <- modified_opts
clusterExport(cl, 'opts', envir=.GlobalEnv)
test_that("Expect opts to be same with export", {
  par_cmd <- foreach(i=1, .packages='hydromad') %dopar% {
    hydromad.options(opts)
    return(hydromad.getOption('cmd'))
  }
  expect_equal(par_cmd[[1]]$f, modified_opts$cmd$f)
})

stopCluster(cl)