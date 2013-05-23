%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DataFilter$minAvgCountConstraint}
\alias{DataFilter$minAvgCountConstraint}
\alias{minAvgCountConstraint.DataFilter}
\alias{DataFilter.minAvgCountConstraint}
\alias{minAvgCountConstraint,DataFilter-method}

 \title{Perform mean based thresholding of an input vector}
 \description{Perform mean based thresholding of an input vector.}

 \usage{
## Static method (use this):
## DataFilter$minAvgCountConstraint(x, filtParams, ...)

## Don't use the below:
\method{minAvgCountConstraint}{DataFilter}(static, x, filtParams, ...)
}

 \arguments{
 \item{x}{\code{\link[base]{numeric}} input \code{\link[base]{vector}}.}
 \item{filtParams}{\code{\link[base]{vector}} of constraint parameters. If the mean of an input vector
 is less than filterParams[1] the whole vector is replaced by \code{\link[base]{NA}} 's vector of the same length.}
 \item{...}{Not used}
 }

 \value{
 \code{\link[base]{numeric}} \code{\link[base]{vector}} or vector of \code{\link[base]{NA}} 's depending on thresholding criteria.
 }

 \examples{
 #set to NA protein count rows which contain less than 50 percent of counts >=2
 fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
   byRows=TRUE,filterFun=DataFilter$minAvgCountConstraint,filtParams=0.5,verbose=TRUE);

 #print the number of rows set to NA
 sum(is.na(rowSums(fltExperimentSet[,-1])))
 }

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
