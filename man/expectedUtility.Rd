\name{expectedUtility}
\alias{expectedUtility}
\title{Expected utility of an ID mapping, ID filtering, or other
bioinformatics data preparation method}
\usage{
  expectedUtility(dataset, label = "",
    bootModelCorClusters,
    columnsToRemove = c("Utp", "Lfp", "deltaPlus", pi1Hat),
    Utp, Lfp, deltaPlus)
}
\arguments{
  \item{dataset}{A data frame or list from a call to
  \code{fit2clusters}, the posterior probabilities for each
  observation, their variance estimates.}

  \item{label}{A text string describing the method being
  studied, to label the return value. This is handy for
  using \code{rbind} to combine results for different
  methods.}

  \item{bootModelCorClusters}{Source for mixture model
  estimates. If missing, extracted from calling frame.}

  \item{columnsToRemove}{Names of columns to remove from
  return value.}

  \item{Utp}{Utility of a true positive.}

  \item{Lfp}{Loss of a false positive.}

  \item{deltaPlus}{Parameter defined as Pr("+" | "+" or
  "0")}
}
\value{
  A data frame with just one row. The columns are:
  \item{Utp}{ Utility of a true positive.} \item{Lfp}{Loss
  of a false positive.} \item{deltaPlus}{Parameter defined
  as Pr("+" | "+" or "0")} \item{deltaZero}{Parameter
  defined as Pr("0" | "0" or "x")} \item{nPairs}{Number of
  ID pairs selected by the method.} \item{pi1Hat}{The
  estimate of the probability of the high-correlation
  component; obtained from } \item{PrPlus}{Estimated
  probability that an ID pair is in the "+" group.}
  \item{PrTrue}{Estimated probability that an ID pair is in
  the "+" or "0" group: \code{PrPlus/deltaPlus}}
  \item{PrFalse}{Estimated probability that an ID pair is
  in the "-" group.} \item{Utrue}{The component of expected
  utility from "true positives": \code{PrTrue * Utp}.}
  \item{Lfalse}{The (negative) component of expected
  utility from "false positives": \code{PrFalse * Lfp}.}
  \item{Eutility1}{The average expected utility per ID
  pair: Utrue-Lfalse.} \item{Eutility}{The total expected
  utility, summing over ID pairs:
  \code{nrow(dataset)*Eutility1}.}

  \details{ The \code{dataset} should be a dataframe with
  one row per ID pair, and the following columns: \itemize{
  \item\code{Utp}{ Utility of a true positive.}
  \item\code{Lfp}{ Loss of a false positive.}
  \item\code{postProb}{ The posterior probabilities for
  each observation} \item\code{postProbVar}{ The variances
  of the posterior probabilities, usually estimated from
  the bootstrap using \code{Boot}} } }
}
\description{
  \code{expectedUtility} calculates mean expected utility
  and total expected utility across pairs of features from
  two bioinformatics platforms. It is used to evaluate an
  ID mapping, ID filtering, or other bioinformatics data
  preparation method.
}

