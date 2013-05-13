#'Expected utility of an ID mapping, ID filtering, or other 
#' bioinformatics data preparation method 
#' 
#' \code{expectedUtility} calculates mean
#' expected utility and total expected utility across pairs of features from two
#' bioinformatics platforms. It is used to evaluate an ID mapping, ID filtering, or other 
#' bioinformatics data preparation method.
#'
#' @param dataset A data frame or list from a call to \code{fit2clusters}, the
#'   posterior probabilities for each observation, their variance estimates.
#' @param label A text string describing the method being studied, to label the return value. This is
#'   handy for using \code{rbind} to combine results for different methods.
#' @param bootModelCorClusters Source for mixture model estimates. If missing, extracted from calling frame.
#' @param  columnsToRemove Names of columns to remove from return value.
#' @param Utp Utility of a true positive.
#' @param Lfp Loss of a false positive. 
#' @param deltaPlus Parameter defined as Pr("+" | "+" or "0")

#' @return A data frame with just one row. The columns are: 
#'   \item{Utp}{ Utility of a true positive.}
#'   \item{Lfp}{Loss of a false positive.} 
#'   \item{deltaPlus}{Parameter defined as Pr("+" | "+" or "0")} 
#'   \item{deltaZero}{Parameter defined as Pr("0" | "0" or "x")} 
#'   \item{nPairs}{Number of ID pairs selected by the method.} 
#'   \item{pi1Hat}{The estimate of the probability of the high-correlation component; obtained from  } 
#'   \item{PrPlus}{Estimated probability that an ID pair is in the "+" group.} 
#'   \item{PrTrue}{Estimated probability that an ID pair is in the "+" or "0" group: \code{PrPlus/deltaPlus}} 
#'   \item{PrFalse}{Estimated probability that an ID pair is in the "-" group.} 
#'   \item{Utrue}{The component of expected utility from "true positives": \code{PrTrue * Utp}.} 
#'   \item{Lfalse}{The (negative) component of expected utility from "false positives": \code{PrFalse * Lfp}.} 
#'   \item{Eutility1}{The average expected utility per ID pair: Utrue-Lfalse.} 
#'   \item{Eutility}{The total expected utility, summing over ID pairs: \code{nrow(dataset)*Eutility1}.} 
#'   
#' \section Details:{
#'   The \code{dataset} should be a dataframe with one row per
#'   ID pair, and the following columns: 
#'     \itemize{
#'       \item\code{Utp}{ Utility
#'                        of a true positive.}
#'       \item\code{Lfp}{ Loss of a false
#'                        positive.} 
#'       \item\code{postProb}{ The posterior probabilities
#'                             for each observation} 
#'       \item\code{postProbVar}{ The variances
#'                                of the posterior probabilities, usually estimated from
#'                                the bootstrap using \code{Boot}}
#'     }
#' }


expectedUtility = 
  function(dataset, label="", 
           bootModelCorClusters,
           columnsToRemove = c("Utp","Lfp","deltaPlus",pi1Hat),
           Utp,Lfp,deltaPlus
  ){
    if(missing(bootModelCorClusters))
      bootModelCorClusters = get("bootModelCorClusters")
    if(missing(Utp))
      Utp = get("Utp")
    if(missing(Lfp))
      Lfp = get("Lfp")
    if(missing(deltaPlus))
      deltaPlus = get("deltaPlus")
    postProbVar = pmax(dataset$postProbVar, guarantee) 
    PrPlus = sum(dataset$postProb/postProbVar)/
      sum(1/postProbVar)
    #   if(compare) cat("In ", sum(subset), "probesets, PplusM=", PplusM, 
    #                   " compared with pi1Hat=",
    #                   attr(bootModelCorClusters, "estimates")["pi1"], "\n") 
    #deltaZero = 
    result = data.frame(label=label,
                        Utp=Utp, Lfp=Lfp, 
                        deltaPlus=deltaPlus,
                        nPairs=nrow(dataset),
                        pi1Hat=attr(bootModelCorClusters, "estimates")["pi1"],
                        PrPlus= PrPlus,
                        PrTrue= PrTrue<-PrPlus / deltaPlus,
                        PrFalse= PrFalse<-1 - PrTrue,
                        Utrue=  Utrue<-PrTrue * Utp,
                        Lfalse= Lfalse<-PrFalse * Lfp,
                        Eutility1= Utrue-Lfalse,
                        Eutility= nrow(dataset)*(Utrue-Lfalse))
    if(!is.null(columnsToRemove) & !is.na(columnsToRemove)
       & !(length(columnsToRemove)==0))
      result = result[-match(columnsToRemove, names(result))]
    rownames(result) = label
    return(result)
  }