\name{fit2clusters}
\alias{fit2clusters}
\title{Flexible two-cluster mixture fit of a numeric vector}
\usage{
  fit2clusters(Y, Ylabel = "correlation", Ysigsq,
    piStart = c(0.5, 0.5), VStart = c(0.1, 0.1),
    psiStart = c(0, 0.1), NinnerLoop = 1, nReps = 500,
    psi0Constraint, V0Constraint, sameV = FALSE,
    estimatesOnly = TRUE, plotMe = TRUE, testMe = FALSE,
    Ntest = 5000, simPsi = c(0, 0.4), simPi = c(2/3, 1/3),
    simV = c(0.05^2, 0.05^2), simAlpha = 5, simBeta = 400,
    seed)
}
\arguments{
  \item{Y}{The vector of numbers to fit.}

  \item{Ylabel}{Label for the Y axis in a density fit
  figure.}

  \item{piStart}{Starting values for the component
  proportions.}

  \item{Vstart}{Starting values for the component
  variances.}

  \item{psiStart}{Starting values for the component means}

  \item{NinnerLoop}{Number of iterations in the "C" loop of
  ECM.}

  \item{nReps}{Upper limit of number of EM steps.}

  \item{psi0Constraint}{If not missing, a fixed value for
  the first component mean.}

  \item{V0Constraint}{If not missing, a fixed value for the
  first component variance.}

  \item{sameV}{If TRUE, the components have the same
  variance.}

  \item{estimatesOnly}{If TRUE, return only the estimates.
  Otherwise, returns details per observations, and return
  the estimates as an attribute.}

  \item{plotMe}{If TRUE, plot the mixture density and
  kernel smooth estimates.}

  \item{Ntest}{For testing purposes, the number of
  replications of simulated data.}

  \item{simPsi}{For testing purposes, the true means.}

  \item{simPi}{For testing purposes, the true proportions}

  \item{simV}{For testing purposes, the true variances.}

  \item{simAlpha}{For testing purposes, alpha parameter in
  rgamma for measurement error variance.}

  \item{simBeta}{For testing purposes, beta parameter in
  rgamma for measurement error variance.}

  \item{seed}{For testing purposes, random seed.}
}
\value{
  If estimatesOnly is TRUE, return only the estimates:
  Otherwise, return a dataframe of details per
  observations, and return the estimates as an attribute.
  The observation details are: \item{pi1}{The probability
  of the 2nd mixture component} \item{psi0}{The mean of the
  first component (psi0Constraint if provided)}
  \item{psi1}{The mean of the second component }
  \item{Var0}{The variance of the first component
  (V0Constraint if provided)} \item{Var1}{The variance of
  the second component }

  The observation details are: \item{Y}{The original
  observations.} \item{Ysigsq}{The original measurement
  variances.} \item{posteriorOdds}{Posterior odds of being
  in component 2 of the mixture.}
  \item{postProbVar}{Estimated variance of the posterior
  probability, using the delta method.}
}
\description{
  \code{fit2clusters} uses an ECM algorithm to fit a
  two-component mixture model. It is more flexible than
  mclust in some ways, but it only deals with
  one-dimensional data.
}
\details{
  See the document "ECM_algorithm_for_two_clusters.pdf".
}

