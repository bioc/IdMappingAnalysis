%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{do.apply.DataFilter}
\alias{do.apply.DataFilter}
\alias{DataFilter.do.apply}
\alias{do.apply.DataFilter}
\alias{do.apply,DataFilter-method}


 \title{Filter experiment using constraints}

 \description{
 Filter experiment using constraints.
 The function applyies the constraint filter to the experiment \code{\link[base]{data.frame}}, by rows or columns,
 replacing the content of each row or column with constraint filter output. In most cases, if the
 given row or column passes the constraint check, it is left intact and replaced by \code{\link[base]{NA}} 's otherwise.
 One of the purposes of this function is to perform the quality control of experiment data and filter out
 series which do not pass the QC by applying the removeNASeries method as the last step.
 }

\usage{DataFilter$do.apply(experimentSet, byRows=TRUE, filterFun=NULL, filtParams=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{experimentSet}{\code{\link[base]{data.frame}} containing probeID (first column/row)
 and data arranged by series (the rest of columns/rows)}
 \item{byRows}{\code{\link[base]{logical}} indicating if series arranged by rows (\code{\link[base:logical]{TRUE}})
 or by columns (\code{\link[base:logical]{FALSE}}). Default is \code{\link[base:logical]{TRUE}}.}
 \item{filterFun}{The filtering function applyied to the experiment set,
 typically the constraint filter.}
 \item{filtParams}{Filtering function parameters.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 The \code{\link[base]{data.frame}} with data filtered by filterFun.
 }

 \seealso{For more information see \code{\link{DataFilter}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
