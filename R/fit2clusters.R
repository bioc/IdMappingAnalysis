#' Flexible two-cluster mixture fit of a numeric vector
#' 
#' \code{fit2clusters} uses an ECM algorithm to fit a two-component mixture
#' model. It is more flexible than mclust in some ways, but it only deals with
#' one-dimensional data.
#' 
#' @param Y The vector of numbers to fit.
#' @param Ysigsq The vector of variance estimates for Y.
#' @param Ylabel Label for the Y axis in a density fit figure.
#' @param piStart Starting values for the component proportions.
#' @param VStart Starting values for the component variances.
#' @param psiStart  Starting values for the component means
#' @param NinnerLoop Number of iterations in the "C" loop of ECM.
#' @param nReps Upper limit of number of EM steps.
#' @param psi0Constraint If not missing, a fixed value for the first component
#'   mean.
#' @param V0Constraint If not missing, a fixed value for the first component
#'   variance.
#' @param sameV If TRUE, the components have the same variance.
#' @param estimatesOnly If TRUE, return only the estimates. Otherwise, returns
#'   details per observations, and return the estimates as an attribute.
#' @param plotMe If TRUE, plot the mixture density and kernel smooth estimates.
#' @param testMe If TRUE, run a code test.
#' @param Ntest For testing purposes, the number of replications of simulated
#'   data.
#' @param simPsi For testing purposes, the true means.
#' @param simPi For testing purposes, the true proportions
#' @param simV For testing purposes, the true variances.
#' @param simAlpha For testing purposes, alpha parameter in rgamma for
#'   measurement error variance.
#' @param simBeta For testing purposes, beta parameter in rgamma for measurement
#'   error variance.
#' @param seed For testing purposes, random seed.
#' @param ... Not used; testing roxygen2.
#'   
#'   
#' @return  If estimatesOnly is TRUE, return only the estimates:
#' Otherwise, return a dataframe of 
#'   details per observations, and return the \code{estimates} as an attribute.
#'   The \code{estimates} details are:
#'   \item{pi1}{The probability of the 2nd mixture component}
#'   \item{psi0}{The mean of the first component (psi0Constraint if provided)}
#'   \item{psi1}{The mean of the second component }
#'   \item{Var0}{The variance of the first component (V0Constraint if provided)}
#'   \item{Var1}{The variance of the second component }
#'   
#'   The \code{observations} details are:
#'   \item{Y}{The original observations.}
#'   \item{Ysigsq}{The original measurement variances.}
#'   \item{posteriorOdds}{Posterior odds of being in component 2 of the mixture.}
#'   \item{postProbVar}{Estimated variance of the posterior probability, using the delta method.}
#'   
#'   @details See the document "ECM_algorithm_for_two_clusters.pdf".




fit2clusters = function(Y, Ylabel="correlation",
			Ysigsq,
                        piStart = c(0.5, 0.5),
                        VStart = c(0.1,0.1),
                        psiStart = c(0,0.1),
                        NinnerLoop = 1,
                        nReps=500,
                        psi0Constraint,
                        V0Constraint,
                        sameV=FALSE,
                        estimatesOnly=TRUE,
                        plotMe = TRUE,
                        testMe=FALSE,
                        Ntest = 5000,
                        simPsi = c(0, 0.4),
                        simPi = c(2/3, 1/3),
                        simV = c(0.05^2, 0.05^2),
                        simAlpha = 5,
                        simBeta = 400,
                        seed, ...
			) {
  ### EM algorithm for 2 clusters, 
  ### with constraints on the cluster means and variances, and known data variances
  if(!missing(seed) & !is.na(seed)) {
	cat("Assigning this value to .Random.seed :", seed, "\n")
	assign(".Random.seed", seed, pos=1)
  }
  if(testMe) {
    #  NA ==>  a new dataset.
    simData = data.frame(G = 1+rbinom(Ntest, 1, simPi[2]))
    simData$Ysigsq = rgamma(Ntest, simAlpha, simBeta)
    simData$sd = sqrt(simV[simData$G] +simData$Ysigsq)
    simData$Y = simPsi[simData$G] + 
      rnorm(Ntest)*sqrt(simV[simData$G]) + 
      rnorm(Ntest)*sqrt(simData$Ysigsq)
    print(summary(simData$Y))
    Y = simData$Y
    Ysigsq = simData$Ysigsq
  }
  ###############  Begining of EM algorithm  ##############
  piStar = piStart
  VStar = VStart
  psiStar = psiStart
  stopMe = FALSE
  iRep = 0
  while(1) {
    iRep = iRep + 1
    #    catn(", ", missing(V0Constraint))
    if(!missing(V0Constraint)) 
      VStar[1] = V0Constraint
    if(!missing(psi0Constraint)) 
      psiStar[1] = psi0Constraint
    #    print(psiStar)
    piStarOdds = piStar[2]/piStar[1]
    piStarOddsGK = piStarOdds * 
      dnorm(Y, psiStar[2], sqrt(VStar[2] + Ysigsq)) /
      dnorm(Y, psiStar[1], sqrt(VStar[1] + Ysigsq))
    piStarGK = cbind(1/(1+piStarOddsGK), piStarOddsGK/(1+piStarOddsGK))
    EstarN = apply(piStarGK, 2, sum)
    piStar = apply(piStarGK, 2, mean)
    psiHat = psiStar
    VHat = VStar
    for(iRepInner in 1:NinnerLoop) {
      varHatTotal = colSums(outer(Y, psiHat, "-")^2 * piStarGK) 
      sigsqTotal  = Ysigsq %*% piStarGK
      VHat = pmax(0, varHatTotal - sigsqTotal) / EstarN
      if(!missing(V0Constraint)) 
        VHat[1] = V0Constraint
      psiHat = colSums( Y %*% (piStarGK
                               / outer(Ysigsq, VHat, "+"))) /
        colSums( piStarGK 
                 / outer(Ysigsq, VHat, "+"))
      if(!missing(psi0Constraint)) 
        psiHat[1] = psi0Constraint
      if(sameV) 
        VHat[1] = VHat[2] = mean(VHat[1], VHat[2])
      if(max(abs(psiHat-psiStar), abs(VHat-VStar)) < 1e-7) 
        stopMe = TRUE;
      psiHat -> psiStar
      VHat -> VStar    
    }
    if(iRep >= nReps) stopMe = TRUE
    if(stopMe) break
  }
  cat(iRep, ifelse(iRep==nReps, ".  Loop exhausted.", ".  Converged."), "\n")
  
  if(plotMe) {
    # require("ggplot2")  no thanks.
    options(echo=F)
    plot(col="blue", type="l", Ytemp<-seq(-1,1,length=100),
         xlab=Ylabel, ylab="density",
         piStar[1]*dnorm(Ytemp, psiStar[1], sqrt(VStar[1] + mean(Ysigsq)))
         +
           piStar[2]*dnorm(Ytemp, psiStar[2], sqrt(VStar[2]  + mean(Ysigsq)))
    )
    for(g in 1:2) lines(col="blue", lty=2, Ytemp<-seq(-1,1,length=100),
                        piStar[g]*dnorm(Ytemp, psiStar[g], sqrt(VStar[g]  + mean(Ysigsq))))
    lines(density(Y), lwd=2, col="black")
    
    ###  Should we make a better choice than the means of the Ysigsq?
    if(testMe) {
      for(g in 1:2) 
        lines(col="red", lty=2, Ytemp<-seq(-1,1,length=100),
              piStar[g]*dnorm(Ytemp, simPsi[g], 
                              #                                     sqrt(simV[g]  + mean(simData$Ysigsq))))
                              sqrt(simV[g]  )))
      lines(col="red", Ytemp<-seq(-1,1,length=100),
            piStar[1]*dnorm(Ytemp, simPsi[1], 
                            #                                    sqrt(simV[1] + mean(simData$Ysigsq)))
                            sqrt(simV[1] ))
            +
              piStar[2]*dnorm(Ytemp, simPsi[2], 
                              #                                    sqrt(simV[2] + mean(simData$Ysigsq))))
                              sqrt(simV[2] )))
    }
    if(testMe) { 
      legendLegend = c("truth", "component","data smooth","estimate","component")
      legendColor = c("red", "red", "black", "blue", "blue")
      legendLty = c(1,2,1,1,2)
      legendLwd = c(1,1,2,1,1)
    } else {
      legendLegend = c("data smooth","estimate","component")
      legendColor = c("black", "blue", "blue")
      legendLty = c(1,1,2)
      legendLwd = c(2,1,1)
    }
    legend(x=par("usr")[1], y=par("usr")[4], legend=legendLegend,
           col=legendColor, lty=legendLty, lwd=legendLwd)
    options(echo=T)
  }
  estimates = c(pi1=piStar[2], psi0=psiHat[1],
                psi1=psiHat[2], Var0=VStar[1], Var1=VStar[2])
  posteriorOdds = 
    piStar[2]*dnorm(Y, psiHat[2], sqrt(VStar[2] + Ysigsq)) / 
    piStar[1]/dnorm(Y, psiHat[1], sqrt(VStar[1] + Ysigsq))
  postProb = posteriorOdds/(1+posteriorOdds)
  postProbVar = Ysigsq * (postProb*(1-postProb))^2 *
    ((Y-psiHat[1])/(VStar[1]+Ysigsq) - (Y-psiHat[2])/(VStar[2]+Ysigsq))^2
  
  if(testMe) {
    simTruth = c(pi1=simPi[2], psi0=simPsi[1],
                 psi1=simPsi[2], Var0=simV[1], Var1=simV[2])
    estimates = data.frame(row.names=c("true","estimated"),
                           rbind(simTruth, estimates))
  }
  if(estimatesOnly) returnVal = estimates
  else {
    returnVal = data.frame(Y,Ysigsq,postProb,posteriorOdds,postProbVar)
    attr(x=returnVal, which="estimates") = estimates
  }
  if(testMe) attr(x=returnVal, which="Y") = Y 
  if(testMe) attr(x=returnVal, which="Ysigsq") = Ysigsq 
  return(returnVal)
}

# clusterFitTest =
#   (fit2clusters(testMe=T, seed=NA, simAlpha=50, simBeta=1000,
#                 simV=c(0.1,0.1)^2
#                 , psi0Constraint=0, 
#                 #              , V0Constraint=.02
#                 , estimatesOnly=T
#   ))
# clusterFitTest
# print(summary(attr(clusterFitTest, "Ysigsq")))
