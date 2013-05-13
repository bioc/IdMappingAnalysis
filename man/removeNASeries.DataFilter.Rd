%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{removeNASeries.DataFilter}
\alias{removeNASeries.DataFilter}
\alias{DataFilter.removeNASeries}
\alias{removeNASeries.DataFilter}
\alias{removeNASeries,DataFilter-method}


 \title{Remove NA series from the experiment set}
 \description{Remove NA series from the experiment set.}

\usage{DataFilter$removeNASeries(experimentSet, byRows=TRUE, verbose=FALSE, ...)}

 \arguments{
 \item{experimentSet}{\code{\link[base]{data.frame}} containing probeID (first column or row)
 and data arranged by series (the rest of columns or rows)}
 \item{byRows}{\code{\link[base]{logical}} indicating if series arranged by rows (\code{\link[base:logical]{TRUE}})
 or by columns (\code{\link[base:logical]{FALSE}}). Default is \code{\link[base:logical]{TRUE}}.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 The experiment \code{\link[base]{data.frame}} with rows or columns containing all \code{\link[base]{NA}} 's removed.
 }

 \examples{
 #set to NA protein count rows which average count is less than 50 percent
 fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
   byRows=TRUE,filterFun=DataFilter$minAvgCountConstraint,filtParams=0.5,verbose=TRUE);

 #remove NA series
 fltExperimentSet<-DataFilter$removeNASeries(fltExperimentSet,byRows=TRUE,verbose=TRUE);
 dim(fltExperimentSet);
 }

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
