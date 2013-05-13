%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{minCountConstraint.DataFilter}
\alias{minCountConstraint.DataFilter}
\alias{DataFilter.minCountConstraint}
\alias{minCountConstraint.DataFilter}
\alias{minCountConstraint,DataFilter-method}


 \title{Perform minimum count based thresholding of an input vector}
 \description{Perform minimum count based thresholding of an input vector.}

\usage{DataFilter$minCountConstraint(x, filtParams, ...)}

 \arguments{
 \item{x}{\code{\link[base]{numeric}} input \code{\link[base]{vector}}.}
 \item{filtParams}{\code{\link[base]{vector}} of constraint parameters. If the fraction of input elements
 which value is greater or equal filtParams[1] is less than filtParam[2],
 the whole vector is replaced be NA vector of the same length.}
 \item{...}{Not used}
 }

 \value{
 \code{\link[base]{numeric}} \code{\link[base]{vector}} or \code{\link[base]{vector}} of \code{\link[base]{NA}} 's depending on thresholding criteria.
 }

 \examples{
 #set to NA protein count rows which contain less than 50 percent of counts >=2
 fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
   byRows=TRUE,filterFun=DataFilter$minCountConstraint,filtParams=c(2,0.5),verbose=TRUE);

 #print the number of rows set to NA
 sum(is.na(rowSums(fltExperimentSet[,-1])))
 }

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
