## hydromad: Hydrological Modelling and Analysis of Data
##
## Copyright (c) Felix Andrews <felix@nfrac.org>
##

## Entries in hydromad.stats should all be functions with arguments (Q, X, ...)
## (and possible more: 'U', 'DATA', 'model').
## Expressions in the body of the function are allowed to be wrapped in .()
## in which case the fitting functions will cache the expression. It must
## only refer to 'Q' and/or 'DATA'.
## Note that hydromad.stats() inserts a function .() in these
## functions' environments which does nothing, therefore allowing them to be
## called directly even when the .() chunks have not been evaluated / cached.


## This is called once by fitting functions etc.


#' Statistics for use in hydromad
#'
#' These functions provide access to a built-in set of statistics, and also
#' allow the user to change or add named statistics.  Its usage is similar to
#' \code{\link{hydromad.options}}.
#'
#' \code{hmadstat} returns the named statistical function, while
#' \code{hydromad.stats} gets or sets a list of named functions.
#'
#' The default set of available statistics can be listed with
#' \code{str(hydromad.stats())}, and consists of:
#'
#' \describe{ \item{abs.err}{ the mean absolute error. }
#' \item{RMSE}{ Root Mean Squared Error.  }
#' \item{bias}{ bias in data units, \eqn{\sum ( X - Q )} }
#' \item{rel.bias}{ bias as a fraction of the total observed flow, \eqn{\sum
#' ( X - Q ) / \sum Q} (excluding any time steps with missing values).  }
#' \item{r.squared}{ R Squared (Nash-Sutcliffe Efficiency), \eqn{1 - \sum (Q-
#' X)^2 / \sum (Q - \bar{Q})^2} }
#' \item{r.sq.sqrt}{ R Squared using square-root
#' transformed data (less weight on peak flows), \eqn{1 - \frac{\sum |\sqrt{Q}
#' - \sqrt{X}|^2 }{ \sum |\sqrt{Q} - \bar{\sqrt{Q}}|^2 }} }
#' \item{r.sq.log}{ R Squared using log transformed data, with an offset:
#' \eqn{1 - \frac{\sum | \log{(Q+\epsilon)} - \log{(X+\epsilon)}|^2 }
#' {\sum |\log{(Q+\epsilon)} - \bar{\log{(Q+\epsilon)}}|^2 }}.
#' Here \eqn{\epsilon} is the 10 percentile (i.e. lowest decile) of the non-
#' zero values of Q. }
#' \item{r.sq.boxcox}{ R Squared using a Box-Cox transform. The power lambda is
#' chosen to fit Q to a normal distribution. When lambda = 0 it is a log
#' transform; otherwise it is \eqn{y_* = \frac{(y+\epsilon)^\lambda - 1}
#' {\lambda}} Here \eqn{\epsilon} is the 10 percentile (i.e. lowest decile) of
#' the non-zero values of Q. }
#' \item{r.sq.diff}{ R Squared using differences between successive time steps,
#' i.e. rises and falls. }
#' \item{r.sq.monthly}{ R Squared with data aggregated into calendar months. }
#' \item{r.sq.smooth5}{ R Squared using data smoothed with a triangular kernel
#' of width 5 time steps: \code{c(1,2,3,2,1)/9}. }
#' \item{r.sq.seasonal}{ R Squared where the reference model is the mean in
#' each calendar month, rather than the default which is the overall mean. }
#' \item{r.sq.vartd}{ \code{\link{nseVarTd}} R Squared where the modelled peaks
#' have been coalesced to observed peaks, minimising timing error. Note that
#' this statistic requires \code{event} to be specified using
#' \code{\link{eventseq}} }
#' \item{persistence}{ R Squared where the reference model predicts each time
#' step as the previous observed value. This statistic therefore represents a
#' model's performance compared to a \emph{naive} one-time-step forecast. }
#' \item{X0}{ correlation of modelled flow with the model residuals. }
#' \item{X1}{ correlation of modelled flow with the model residuals from the
#' previous time step. }
#' \item{ARPE}{ Average Relative Parameter Error. Requires that a variance-
#' covariance matrix was estimated during calibration.}
#' \item{KGE}{ Kling-Gupta Efficiency. \eqn{KGE = 1 - \sqrt{(r - 1)^2 + (\alpha
#' - 1)^2 + (\Beta - 1)^2}}, where r is the linear correlation between
#' observations and simulations, α a measure of the flow variability error, and
#' β a bias term. Equivalently, it can be expressed as: \code{1 -
#' sqrt( cor(X, Q)^2 + (sd(X)/sd(Q) - 1)^2 + (mean(X)/mean(Q) - 1)^2 )}. }
#' }
#'
#' @importFrom zoo coredata rollmean
#' @importFrom stats complete.cases quantile time ave fitted lag cor
#' @importFrom latticeExtra simpleSmoothTs
#' @importFrom utils head tail
#'
#'
#' @aliases hydromad.stats hmadstat
#' @param name character giving the name of a statistic.
#' @param DATA,Q If either \code{DATA} or \code{Q} is given, the returned
#' statistic function will be pre-evaluated with the given data. Technically,
#' any special \code{.()} expressions in the named objective function will be
#' evaluated and may refer to variables \code{DATA} and/or \code{Q}.  These
#' chunks are cached so that repeated evaluation of the objective function is
#' faster; but it is then only valid for models using the same dataset.
#' Remember that objective functions are normally evaluated on time series with
#' the warmup period removed, not the whole time series passed to
#' \code{hydromad}. The observed data excluding warmup can be extracted from a
#' model object as \code{Q = observed(model)} or \code{DATA = observed(model,
#' select = TRUE)}.
#' @param ...  new stats can be defined, or existing ones modified, using one
#' or more arguments of the form 'name = value' or by passing a list of such
#' tagged values.  Existing values can be retrieved by supplying the names (as
#' character strings) of the components as unnamed arguments.
#' @param objective Placeholder
#' @param model Placeholder
#' @seealso \code{\link{buildTsObjective}}, \code{\link{nseStat}},
#' \code{\link{objFunVal}}, \code{\link{summary.hydromad}}
#' @references Gupta, H.V., Kling, H., Yilmaz, K.K., & Martinez, G.F. (2009).
#' Decomposition of the mean squared error and NSE performance criteria:
#' Implications for improving hydrological modelling. Journal of Hydrology,
#' 377(1), 80–91. https://doi.org/10.1016/j.jhydrol.2009.08.003
#' @keywords programming
#' @examples
#'
#' ## see current set of stats
#' names(hydromad.stats())
#'
#' ## extract only one
#' hmadstat("RMSE")
#'
#' ## calculate stat value with random data
#' hmadstat("rel.bias")(Q = 1:10, X = 2:11)
#'
#' ## add a new objective function.
#' ## A weighted combination of NSE and bias
#' ## as proposed by Neil Viney
#' ## OF = NSE – 5*|ln(1+Bias)|^2.5
#'
#' hydromad.stats("viney" = function(Q, X, ...) {
#'   hmadstat("r.squared")(Q, X, ...) -
#'     5 * (abs(log(1 + hmadstat("rel.bias")(Q, X)))^2.5)
#' })
#' @export
hydromad.stats <- function(...) {
  ## this would have been really simple if only form allowed were
  ## lattice.options("foo", "bar") and
  ## lattice.options(foo=1, bar=2). But it could also be
  ## lattice.options(foo=1, "bar"), which makes some juggling necessary

  new <- list(...)
  if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) new <- new[[1]]
  old <- .HydromadEnv$stats

  ## if no args supplied, returns full options list
  if (length(new) == 0) {
    return(old)
  }

  nm <- names(new)
  if (is.null(nm)) {
    return(old[unlist(new)])
  } ## typically getting options, not setting
  isNamed <- nm != "" ## typically all named when setting, but could have mix
  if (any(!isNamed)) nm[!isNamed] <- unlist(new[!isNamed])

  ## so now everything has non-"" names, but only the isNamed ones should be set
  ## everything should be returned, however

  retVal <- old[nm]
  names(retVal) <- nm
  nm <- nm[isNamed]

  newfuns <- new[nm]

  ## ensure that everything being assigned is of the required type:
  ## functions of (Q, X, ...)
  if (!all(sapply(newfuns, is.function))) {
    stop("items stored in hydromad.stats must be functions")
  }
  if (!all(sapply(newfuns, function(x) "..." %in% names(formals(x))))) {
    stop("items stored in hydromad.stats must be functions accepting '...'")
  }
  ## insert a special function into its environment to allow direct evaluation:
  for (x in newfuns) {
    assign(".", base::force, environment(x))
  }

  ## this used to be

  ## modified <- updateList(retVal[nm], new[nm])
  ## .LatticeEnv$lattice.options[names(modified)] <- modified

  ## but then calling lattice.options(foo = NULL) had no effect
  ## because foo would be missing from modified.  So, we now do:

  updateList <- function(x, val) {
    if (is.null(x)) x <- list()
    utils::modifyList(x, val)
  }
  .HydromadEnv$stats <- updateList(old, newfuns)

  ## return changed entries invisibly
  invisible(retVal)
}



#' @rdname hydromad.stats
#' @export
buildCachedObjectiveFun <-
  function(objective, model,
           DATA = observed(model, select = TRUE), Q = DATA[, "Q"]) {
    buildCachedObjectiveFun1 <- function(objective) {
      ## should not call bquote() on an object twice, or it breaks...
      if (isTRUE(attr(objective, "cached"))) {
        return(objective)
      }
      ## both 'Q' and 'DATA' can be referred to in .() expressions
      ## evaluate and replace .() expressions in body of objective
      if (inherits(objective, "formula")) {
        if (length(objective) > 2) {
          warning("left hand side of formula ignored")
        }
        try(objective[[2]] <-
          eval(substitute(bquote(x), list(x = objective[[2]]))))
      } else if (is.function(objective)) {
        try(body(objective) <-
          eval(substitute(bquote(x), list(x = body(objective)))))
      } else {
        stop(
          "'objective' should be a function or formula, not a ",
          toString(class(objective))
        )
      }
      attr(objective, "cached") <- TRUE
      objective
    }
    if (is.list(objective)) {
      lapply(objective, buildCachedObjectiveFun1)
    } else {
      buildCachedObjectiveFun1(objective)
    }
  }


.defaultHydromadStats <- function() {
  ## keep R CMD check happy:
  . <- function(x) x

  ## default set of stats
  list(
    "bias" = function(Q, X, ...) mean(X - Q, na.rm = TRUE),
    "rel.bias" = function(Q, X, ...) {
      ok <- complete.cases(coredata(X), coredata(Q))
      mean((coredata(X) - coredata(Q))[ok]) / mean(coredata(Q)[ok])
    },
    "abs.err" = function(Q, X, ...) mean(abs(X - Q), na.rm = TRUE),
    "RMSE" = function(Q, X, ...) sqrt(mean((X - Q)^2, na.rm = TRUE)),
    "r.squared" = function(Q, X, ...) {
      nseStat(coredata(Q), coredata(X), ...)
    },
    "r.sq.sqrt" = function(Q, X, ...) {
      nseStat(coredata(Q), coredata(X), ..., trans = sqrt)
    },
    "r.sq.log" = function(Q, X, ...) {
      nseStat(coredata(Q), coredata(X), ..., trans = function(x) {
        log(x + .(quantile(coredata(Q)[coredata(Q) > 0], 0.1, na.rm = TRUE, names = FALSE)))
      })
    },
    "r.sq.boxcox" = function(Q, X, ...) {
      .(buildTsObjective(Q, boxcox = TRUE))(Q, X, ...)
    },
    "r.sq.rank" = function(Q, X, ...) {
      nseStat(Q, X, ..., trans = function(x) {
        rank(round(log10(zapsmall(x, digits = 3)), digits = 2),
          na.last = "keep"
        )
      })
    },
    "r.sq.diff" = function(Q, X, ...) {
      nseStat(diff(Q), diff(X), ...)
    },
    "r.sq.monthly" = function(Q, X, ...) {
      .(buildTsObjective(Q, groups = cut(time(Q), "months")))(Q, X, ...)
    },
    "r.sq.365" = function(Q, X, ...) {
      objfun <- .({
        Q365 <- rollmean(Q, 365, fill = NA)
        function(Q, X, ...) {
          nseStat(Q365, rollmean(X, 365, fill = NA), ...)
        }
      })
      objfun(Q, X, ...)
    },
    "r.sq.smooth5" = function(Q, X, ...) {
      nseStat(Q, X, ..., trans = function(x) {
        simpleSmoothTs(x, width = 5, c = 2)
      })
    },
    "r.sq.seasonal" = function(Q, X, ...) {
      nseStat(Q, X,
        ref = .(ave(Q, months(time(Q)), FUN = function(x) mean(x, na.rm = TRUE))),
        ...
      )
    },
    "r.sq.vs.tf" = function(Q, X, ..., DATA) {
      ref <-
        .(fitted(hydromad(DATA,
          sma = "scalar", routing = "armax",
          rfit = list("sriv", order = c(2, 1))
        )))
      nseStat(Q, X, ref = ref, ...)
    },
    "r.sq.vs.tf.bc" = function(Q, X, ..., DATA) {
      objfun <- .({
        ref <-
          fitted(hydromad(DATA,
            sma = "scalar", routing = "armax",
            rfit = list("sriv", order = c(2, 1))
          ))
        buildTsObjective(Q, ref = ref, boxcox = TRUE)
      })
      objfun(Q, X, ...)
    },
    "persistence" = function(Q, X, ...) {
      nseStat(Q, X, ref = lag(Q, -1), ...)
    },
    "persistence.bc" = function(Q, X, ...) {
      objfun <- .({
        ref <- lag(Q, -1)
        buildTsObjective(Q, ref = ref, boxcox = TRUE)
      })
      objfun(Q, X, ...)
    },

    "e.rain5" = function(Q, X, ..., DATA) {
      objfun <- .({
        ev <- eventseq(DATA$P,
          thresh = 5, inthresh = 1,
          indur = 4, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum)
      })
      objfun(Q, X, ...)
    },
    "e.rain5.log" = function(Q, X, ..., DATA) {
      objfun <- .({
        ev <- eventseq(DATA$P,
          thresh = 5, inthresh = 1,
          indur = 4, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum, boxcox = 0)
      })
      objfun(Q, X, ...)
    },
    "e.rain5.bc" = function(Q, X, ..., DATA) {
      objfun <- .({
        ev <- eventseq(DATA$P,
          thresh = 5, inthresh = 1,
          indur = 4, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum, boxcox = TRUE)
      })
      objfun(Q, X, ...)
    },

    # = flowq90_indur4_mindur5_mingap5

    "e.q90" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum)
      })
      objfun(Q, X, ...)
    },
    "e.q90.log" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum, boxcox = 0)
      })
      objfun(Q, X, ...)
    },
    "e.q90.bc" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum, boxcox = TRUE)
      })
      objfun(Q, X, ...)
    },

    "e.q90.all" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, all = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum)
      })
      objfun(Q, X, ...)
    },
    "e.q90.all.log" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, all = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum, boxcox = 0)
      })
      objfun(Q, X, ...)
    },
    "e.q90.all.bc" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, all = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = sum, boxcox = TRUE)
      })
      objfun(Q, X, ...)
    },

    "e.q90.min" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, continue = TRUE
        )
        buildTsObjective(Q, groups = ev, FUN = min, na.rm = TRUE)
      })
      objfun(Q, X, ...)
    },
    "e.q90.min.log" = function(Q, X, ...) {
      objfun <- .({
        q90 <- quantile(coredata(Q), 0.9, na.rm = TRUE)
        ev <- eventseq(Q,
          thresh = q90, indur = 4,
          mindur = 5, mingap = 5, continue = TRUE
        )
        buildTsObjective(Q,
          groups = ev, FUN = min, na.rm = TRUE,
          boxcox = 0
        )
      })
      objfun(Q, X, ...)
    },

    "ar1" = function(Q, X, ...) {
      cor(head(Q - X, -1), tail(Q - X, -1), use = "complete")
    },
    "X0" = function(Q, X, ...) cor(Q - X, X, use = "complete"),
    "X1" = function(Q, X, ...) cor(head(Q - X, -1), tail(X, -1), use = "complete"),
    "U1" = function(Q, X, ..., U) cor(head(Q - X, -1), tail(U, -1), use = "complete"),
    "r.sq.vartd" = function(Q, X, ..., event) {
      nseVarTd(Q, X, event, ...)
    },
    "KGE" = function(Q, X, ...) {
      ok <- complete.cases(coredata(X), coredata(Q))
      1 - sqrt(
        (cor(X, Q, use = "complete") - 1)^2 +
          (mean(X[ok]) / mean(Q[ok]) - 1)^2 +
          (sd(X[ok]) / sd(Q[ok]) - 1)^2
      )
    }
  )
}

#' @rdname hydromad.stats
#' @export
hmadstat <- function(name, DATA = NULL, Q = DATA[, "Q"]) {
  STATFUN <- .HydromadEnv$stats[[name]]
  if (!is.null(DATA) || !is.null(Q)) {
    STATFUN <- buildCachedObjectiveFun(STATFUN, DATA = DATA, Q = Q)
  }
  STATFUN
}

## this function based on lattice::lattice.options, but with some extra bits
