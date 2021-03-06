% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitByDream.R
\name{fitByDream}
\alias{fitByDream}
\title{Fit a hydromad model using the DREAM (DiffeRential Evolution Adaptive
Metropolis) algorithm.}
\usage{
fitByDream(
  MODEL,
  loglik = hydromad.getOption("loglik"),
  control = hydromad.getOption("dream.control"),
  vcov = TRUE,
  save = NULL
)
}
\arguments{
\item{MODEL}{a model specification created by \code{\link{hydromad}}. It
should not be fully specified, i.e one or more parameters should be defined
by \emph{ranges} of values rather than exact values.}

\item{loglik}{log-likelihood function (log of the posterior probability
density), given as a \code{function(Q, X, ...)}.  See
\code{\link{objFunVal}}.}

\item{control}{settings for the DREAM algorithm. See
\code{\link[dream]{dream}}.}

\item{vcov}{if \code{vcov = TRUE}, the parameter variance-covariance matrix
will be estimated from the last half of the sequences.  It can be extract
using \code{\link{vcov}}.}

\item{save}{Optional \code{function(pars,objective value,model)} that will
be called for every model evaluation, for example to save every model run.}
}
\value{
the best model from those sampled, according to the given
\code{loglik} function. Also, these extra elements are inserted:
\item{fit.result}{ the result from \code{\link[dream]{dream}}.  }
\item{objective}{ the \code{loglik} function used.  } \item{funevals}{ total
number of evaluations of the model simulation function.  } \item{timing}{
timing vector as returned by \code{system.time}.  }
}
\description{
Fit a hydromad model using the DREAM (DiffeRential Evolution Adaptive
Metropolis) algorithm. This is a Markov Chain Monte Carlo algorithm which
gives estimates of the joint probability distribution of parameters
according to a likelihood function. The fitting function returns the maximum
likelihood model, but the full MCMC results are also available as component
\code{$fit.result}. The result can also be used to define a
\link[=defineFeasibleSet]{feasible parameter set}.
}
\examples{


if (requireNamespace("dream", quietly = TRUE)) {
  data(Cotter)
  x <- Cotter[1:1000]

  ## IHACRES CWI model with power law unit hydrograph
  modx <- hydromad(x, sma = "cwi", routing = "powuh")
  modx

  ## a very short run! just to demonstrate methods
  foo <- fitByDream(modx, control = list(ndraw = 500))

  summary(foo)

  ## parameter correlation matrix with symbols
  symnum(cov2cor(vcov(foo)))

  ## return value from dream:
  str(foo$fit.result)

  ## plot log-likelihood value convergence over time
  xyplot(window(optimtrace(foo, raw = TRUE), start = 50),
    superpose = TRUE, auto.key = FALSE,
    xlab = "function evaluations", ylab = "neg. log likelihood"
  )

  ## calculate corresponding objective function values over time.
  xyplot(optimtrace(foo, objective = ~ -hmadstat("r.squared")(Q, X)),
    xlab = "function evaluations", ylab = "negative R Squared"
  )

  ## MCMC diagnostics and more are available:
  methods(class = "dream")
}
}
\seealso{
\code{\link[dream]{dream}}, \code{\link{objFunVal}}
}
\author{
Felix Andrews \email{felix@nfrac.org}
}
\keyword{optimization}
