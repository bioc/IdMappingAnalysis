%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{log10.DataFilter}
\alias{log10.DataFilter}
\alias{DataFilter.log10}
\alias{log10.DataFilter}
\alias{log10,DataFilter-method}


 \title{Compute log10 of a numerical vector combined with thresholding on minimum value}
 \description{Compute log10 of a numerical vector combined with thresholding on minimum value.}

\usage{DataFilter$log10(x, filtParams, ...)}

 \arguments{
 \item{x}{\code{\link[base]{numeric}} input \code{\link[base]{vector}}.}
 \item{filtParams}{\code{\link[base]{vector}} of constraint parameters. If a particular
 output element is less or equal than filtParams[1] it is assigned the filtParams[2] value.}
 \item{...}{Not used}
 }

 \value{
 clipped log10 of an input \code{\link[base]{numeric}} \code{\link[base]{vector}}.
 }

 \examples{
 #compute log10 transform of mrna experiment data replacing (clipping)
 #the output values with log10(0.5)for input values < 0.5
 fltExperimentSet<-DataFilter$do.apply(examples$mrnaExperimentSet,
         byRows=TRUE,filterFun=DataFilter$log10,filtParams=c(0.5,log10(0.5)),verbose=TRUE);

 #print the number of elements clipped
 sum(fltExperimentSet[,-1]==log10(0.5))-sum(examples$mrnaExperimentSet[,-1]<=0.5);
 }

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
