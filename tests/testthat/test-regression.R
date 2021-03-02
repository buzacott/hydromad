library(testthat)
library(hydromad)

context("Model regression testing")

test_that("HBV HBVLand", {
  load('vignettes/data/hbv_vignette.Rdata')
  mod <- hydromad(DATA = hbv_land,
                  sma = "hbv",
                  routing = "hbvrouting",
                  tt = -1.76, # Snow
                  cfmax = 2.98,
                  sfcf = 0.73, 
                  cfr = 0.05,
                  cwh = 0.1,
                  fc = 285, # Soil
                  lp = 0.75, 
                  beta = 3.43,
                  cet = 0.1,
                  perc = 1.02, # Routing
                  uzl = 17.6,
                  k0 = 0.25,
                  k1 = 0.09,
                  k2 = 0.06, 
                  maxbas = 2.4,
                  return_state = TRUE,
                  return_components = TRUE,
                  initialise_sm = TRUE,
                  initial_slz = hbv_land$Q[1] / 0.06, # first Q timestep / k2
                  warmup = 0,
                  PET = list("PET"=hbv_land_pet, "Tmean" = hbv_land_tmean))
  
  # Expected output from HBVlight
  exp <- zooreg(hbvl_hbvland_results[,-1],
                order.by = hbvl_hbvland_results$Date)
  
  # SMA
  expect_equal(mod$U$Snow, exp$Snow, tolerance = 1e-3)
  expect_equal(mod$U$SM,   exp$SM,   tolerance = 1e-3)
  expect_equal(mod$U$PET,  exp$PET,  tolerance = 1e-3)
  expect_equal(mod$U$AET,  exp$AET,  tolerance = 1e-3)
  expect_equal(mod$U$U,    exp$Recharge,  tolerance = 1e-3)
  
  # Routing
  expect_equal(mod$fitted.values$SUZ, exp$SUZ, tolerance = 1e-3)
  expect_equal(mod$fitted.values$SLZ, exp$SLZ, tolerance = 1e-3)
  expect_equal(mod$fitted.values$Q0,  exp$Q0,  tolerance = 1e-3)
  expect_equal(mod$fitted.values$Q1,  exp$Q1,  tolerance = 1e-3)
  expect_equal(mod$fitted.values$Q2,  exp$Q2,  tolerance = 1e-3)
  
  # Simulated streamflow
  expect_equal(mod$fitted.values$X, exp$Qsim, tolerance = 1e-3)
})

test_that("HBV Corin", {
  load('vignettes/data/hbv_vignette.Rdata')
  data("Corin")
  mod <- hydromad(DATA = Corin,
                  sma = "hbv",
                  routing = "hbvrouting",
                  tt = 0, # Snow
                  cfmax = 6.36,
                  sfcf = 1, 
                  cfr = 0.05,
                  cwh = 0.19,
                  fc = 317, # Soil
                  lp = 0.89, 
                  beta = 2.68,
                  perc = 0.89, # Routing
                  uzl = 71.34,
                  k0 = 0.11,
                  k1 = 0.15,
                  k2 = 0.04, 
                  maxbas = 1.5,
                  return_state = TRUE,
                  return_components = TRUE,
                  initialise_sm = TRUE,
                  initial_slz = Corin$Q[1] / 0.04, # first Q timestep / k2
                  warmup = 0)
  
  # Expected output from HBVlight
  exp <- zooreg(hbvl_corin_results[,-1],
                order.by = hbvl_corin_results$Date)
  
  # SMA
  expect_equal(mod$U$Snow, exp$Snow, tolerance = 1e-3)
  expect_equal(mod$U$SM,   exp$SM,   tolerance = 1e-3)
  expect_equal(mod$U$PET,  exp$PET,  tolerance = 1e-3)
  expect_equal(mod$U$AET,  exp$AET,  tolerance = 1e-3)
  expect_equal(mod$U$U,    exp$Recharge,  tolerance = 1e-3)
  
  # Routing
  expect_equal(mod$fitted.values$SUZ, exp$SUZ, tolerance = 1e-3)
  expect_equal(mod$fitted.values$SLZ, exp$SLZ, tolerance = 1e-3)
  expect_equal(mod$fitted.values$Q0,  exp$Q0,  tolerance = 1e-3)
  expect_equal(mod$fitted.values$Q1,  exp$Q1,  tolerance = 1e-3)
  expect_equal(mod$fitted.values$Q2,  exp$Q2,  tolerance = 1e-3)
  
  # Simulated streamflow
  expect_equal(mod$fitted.values$X, exp$Qsim, tolerance = 1e-3)
})

