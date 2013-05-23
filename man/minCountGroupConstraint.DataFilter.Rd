%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DataFilter$minCountGroupConstraint}
\alias{DataFilter$minCountGroupConstraint}
\alias{minCountGroupConstraint.DataFilter}
\alias{DataFilter.minCountGroupConstraint}
\alias{minCountGroupConstraint,DataFilter-method}

 \title{Perform minimum count based thresholding of an input vector subdivided into groups}
 \description{Perform minimum count based thresholding of an input vector subdivided into groups.}

 \usage{
## Static method (use this):
## DataFilter$minCountGroupConstraint(X, filtParams, ...)

## Don't use the below:
\method{minCountGroupConstraint}{DataFilter}(static, X, filtParams, ...)
}

 \arguments{
 \item{X}{\code{\link[base]{numeric}} input \code{\link[base]{vector}}.}
 \item{filtParams}{\code{\link[base]{vector}} of constraint parameters. The filtParams[3] is a list of integer vectors,
 each containing the indexes of an input X vector data and thus defining the groups within the input vector.
 If the fraction of input elements for a given group (defined by filtParam[3]) which value is greater
 or equal filtParams[1] is less than filtParam[2], and if all groups satisfy this condition, then the
 input vector is replaced by NAs, otherwise it is kept intact.}
 \item{...}{Not used}
 }

 \value{
 \code{\link[base]{numeric}} \code{\link[base]{vector}} or vector of \code{\link[base]{NA}} 's depending on passing the constraints criteria.
 }

 \examples{
 #derive sample groups from outcomes
 outcomes<-unique(examples$outcomeMap[,2]);
 groups<-list();
 for(outcome in outcomes)
	groups[[outcome]]<-which(examples$outcomeMap[,2]==outcome);

 #perform filtering by groups
 fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
   byRows=TRUE,filterFun=DataFilter$minCountGroupConstraint,filtParams=list(1,0.3,groups),verbose=TRUE);

 #print the number of rows set to NA
 sum(is.na(rowSums(fltExperimentSet[,-1])))
 }

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
