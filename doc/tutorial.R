## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(hydromad)
require(grid)
set.seed(0)

## ----model-framework, fig.width=6, fig.height=1, fig.cap="The modelling framework in the **hydromad** package.", echo=FALSE, fig.align='center'----
## framework diagram structure:
## --> |box| --> |box| -->
grid.newpage()
pushViewport(viewport(gp = gpar(fontsize = 10)))
arr <- arrow(length = unit(1, "char"))
## first arrows: rainfall and evaporation (inputs)
grid.lines(y = 0.75, x = c(0.0, 0.2), arrow = arr)
grid.lines(y = 0.5, x = c(0.0, 0.2), arrow = arr)
grid.lines(y = 0.25, x = c(0.0, 0.2), arrow = arr, gp = gpar(lty=2))
grid.text(y = 0.75, x = 0.1, label = "rainfall \n")
grid.text(y = 0.5, x = 0.1, label = "temp. / PET \n")
grid.text(y = 0.25, x = 0.1, label = "other inputs \n")
## first box: loss module
grid.rect(x = 0.3, width = 0.2, height = 0.9)
grid.text(x = 0.3, label = "Soil Moisture\nAccounting (SMA)\nmodel")
## second arrow: effective rainfall
grid.lines(y = 0.5, x = c(0.4, 0.6), arrow = arr)
grid.text(y = 0.5, x = 0.5, label = "effective\n rainfall")
## second box: routing module
grid.rect(x = 0.7, width = 0.2, height = 0.9)
grid.text(x = 0.7, label = "(unit hydrograph)\n routing model")
## third arrow: streamflow (output)
grid.lines(y = 0.5, x = c(0.8, 1.0), arrow = arr)
grid.text(y = 0.5, x = 0.9, label = "streamflow \n")
upViewport()

## ----load-package-------------------------------------------------------------
library(hydromad)

## ----load-Cotter-data---------------------------------------------------------
data(Cotter)

## ----rawdataplot-code, eval=FALSE---------------------------------------------
#  xyplot(Cotter)

## ----rawdataplot-code2, eval=FALSE--------------------------------------------
#  xyplot(window(Cotter, start = "1974-01-01", end = "1975-01-01"))

## ----dataplot, fig.width=6, fig.height=5, fig.cap="Input data, averaged over months", fig.align='center'----
monthlyPQE <- aggregate(Cotter, as.yearmon, mean)
xyplot(monthlyPQE,
       screens = c("Streamflow (mm/day)", "Areal rain (mm/day)", "Temperature (deg. C)"),
       xlab = NULL)

## ----rmdataplot, include=FALSE------------------------------------------------
rm(monthlyPQE)

## ----runoff-ratio-------------------------------------------------------------
ok <- complete.cases(Cotter[,1:2])
with(Cotter, sum(Q[ok]) / sum(P[ok]))

## ----rollccf-plot, fig.height=7, fig.cap="Cross-correlation between rainfall and streamflow rises, in two rolling windows of width 90 days and 365 days.", fig.align="center"----
x <- rollccf(Cotter)
xyplot(x, xlim = extendrange(as.Date(c("1980-01-01","1990-01-01"))))

## ----define-periods-----------------------------------------------------------
ts70s <- window(Cotter, start = "1970-01-01", end = "1979-12-31")
ts80s <- window(Cotter, start = "1980-01-01", end = "1989-12-31")
ts90s <- window(Cotter, start = "1990-01-01", end = "1999-12-31")

## ----cotterMod----------------------------------------------------------------
cotterMod <- hydromad(ts90s, sma = "cwi", routing = "expuh",
                      tau_s = c(5,100), tau_q = c(0,5), v_s = c(0,1))
print(cotterMod)

## ----model-fit, warning=FALSE, message=FALSE----------------------------------
cotterMod <- update(cotterMod, routing = "armax", rfit = list("sriv", order = c(n=2, m=1)))
cotterFit <- fitByOptim(cotterMod, samples = 100, method = "PORT")

## ----obs-mod-plot, fig.cap='Observed vs modelled streamflow in part of the calibration period.'----
xyplot(cotterFit, with.P = TRUE, xlim = as.Date(c("1994-01-01", "1997-01-01")))

## ----print-hydromad-----------------------------------------------------------
print(cotterFit)

## ----summary-model-code-------------------------------------------------------
summary(cotterFit)

## ----update-newdata-----------------------------------------------------------
sim70s <- update(cotterFit, newdata = ts70s)
sim80s <- update(cotterFit, newdata = ts80s)
simAll <- update(cotterFit, newdata = Cotter)

## ----verification-period-one--------------------------------------------------
tsVerif <- Cotter
tsVerif$Q[time(ts90s)] <- NA
simVerif <- update(cotterFit, newdata = tsVerif)

## ----runlist------------------------------------------------------------------
allMods <- runlist(calibration = cotterFit, sim70s, sim80s, simVerif)

## ----obs-mod-plots, fig.align='center', fig.cap="Observed vs modelled streamflow in validation periods."----
xyplot(allMods[2:3], scales = list(y = list(log = TRUE)))

## ----mod-cal-stats-table-code, eval=FALSE-------------------------------------
#  summary(allMods)

## ----mod-cal-stats, echo=FALSE------------------------------------------------
knitr::kable(summary(allMods), caption = "Performance statistics for a set of models.")

## ----mod-1990s-summary-table-code, eval=FALSE---------------------------------
#  summary(simAll, breaks = "5 years")

## ----mod-1990s-summary-table, echo=FALSE--------------------------------------
knitr::kable(summary(simAll, breaks = "5 years"), caption="Viewing a break-down the performance of a model over 5-year blocks.")

## ----r2-breaks-plot, fig.cap="Performance statistics plotted over time in regular 2 year blocks. The runoff coefficient and observed streamflow data are also shown.", fig.align='center'----
twoYrStats <- summary(simAll, breaks = "2 years")
statSeries <- twoYrStats[,c("r.squared", "r.sq.sqrt", "rel.bias", "runoff")]
## cut off crazy R Squared values below 0 (for plotting)
statSeries[,1] <- pmax(statSeries[,1], 0)
statSeries[,2] <- pmax(statSeries[,2], 0)
c(xyplot(statSeries, type = "s", lwd = 2,
         ylab = "statistic", xlab = NULL),
  `observed streamflow` = xyplot(observed(simAll)),
  layout = c(1,5), x.same = TRUE) +
    layer_(panel.refline(h = 0, v = time(statSeries)))

## ----fdc-plot-code, eval=FALSE------------------------------------------------
#  qqmath(cotterFit, scales = list(y = list(log = TRUE)), type = c("l","g"))

## ----fdc-plot, height=7, fig.cap="Log-normal Daily Flow Duration Curve for models in simulation.", fig.align='center'----
qqmath(allMods, type = c("l","g"), 
       scales = list(y = list(log = TRUE)),
       xlab = "Standard normal variate",
       ylab = "Flow (mm/day)", 
       f.value = ppoints(100), tails.n = 50,
       as.table = TRUE)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  hydromad(ts90s, sma = "cwi", l = c(0, 200), e = 0.166)

## ----try-model-orders-table-code, eval=F--------------------------------------
#  ihSpec <- hydromad(ts90s, sma = "cwi", tw = 10, f = 1,
#                     routing = "armax")
#  osumm <- tryModelOrders(update(ihSpec, rfit = "sriv"),
#                          n = 0:3, m = 0:3, delay = 0)
#  summary(osumm)

## ----try-model-orders-table, echo = F, warning = F, message = F---------------
ihSpec <- hydromad(ts90s, sma = "cwi", tw = 10, f = 1, 
                   routing = "armax")
osumm <- tryModelOrders(update(ihSpec, rfit = "sriv"),
                        n = 0:3, m = 0:3, delay = 0)
knitr::kable(summary(osumm), digits = 3, caption = "Fit and information statistics from fitting different unit hydrograph transfer functions with SRIV algorithm. ARPE is the Average Relative Parameter Error estimated by SRIV. The  effective rainfall input was generated by an `ihacres` CWI model
  with fixed parameters.")

## ----view-files, echo=TRUE, eval=FALSE----------------------------------------
#  file.show("pq_cotter.csv")

## ----view-files-script, echo=FALSE--------------------------------------------
cat(readLines("pq_cotter.csv", n = 5), "...", sep = "\n")

## ----read-files, echo=TRUE----------------------------------------------------
## rain and flow data
pqdat <- read.table("pq_cotter.csv", sep = ",",
                    col.names = c("P", "Q", "Date"), as.is = TRUE)
## temperature data
tdat <- read.table("t_cotter.csv", sep = ",",
                   col.names = c("T", "Date"), as.is = TRUE)

## ----view-str, echo=TRUE------------------------------------------------------
str(pqdat)
str(tdat)

## ----convert-dates, echo=TRUE-------------------------------------------------
pqdat$Date <- as.Date(pqdat$Date, "%d/%m/%Y")
tdat$Date <- as.Date(tdat$Date, "%d/%m/%Y")

## ----convert-dates-from-columns, echo=TRUE, eval=FALSE------------------------
#  pqdat$Date <- with(pqdat, as.Date(ISOdate(yr, mon, day)))

## ----missing-values, echo=TRUE------------------------------------------------
pqdat$P[pqdat$P < 0] <- NA
pqdat$Q[pqdat$Q < 0] <- NA
tdat <- subset(tdat, !is.na(Date))

## ----convert-to-mm, echo=TRUE-------------------------------------------------
pqdat$Q <- convertFlow(pqdat$Q, from = "ML", area.km2 = 148)

## ----zoo-objects, echo=TRUE---------------------------------------------------
library(zoo)
tsPQ <- zoo(pqdat[,1:2], pqdat$Date, frequency = 1)
tsT <- zoo(tdat[,1], tdat$Date, frequency = 1)

## ----zoo-merge, echo=TRUE-----------------------------------------------------
Cotter <- merge(tsPQ, E = tsT, all = FALSE)

## ----zoo-head, echo=TRUE------------------------------------------------------
head(Cotter, 6)
range(time(Cotter))

## ----zoo-na-trim, echo=TRUE---------------------------------------------------
Cotter <- na.trim(Cotter)

## ----datasummary-code, echo=TRUE, eval=FALSE----------------------------------
#  summary(Cotter)

## ----datasummary, echo=F------------------------------------------------------
summ <- numericSummary(Cotter)
knitr::kable(summ, caption="Data summary.
P = precipitation (mm/day), E = temperature (deg. C), Q = streamflow (mm/day).")

