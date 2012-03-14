%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getStats.IdMapCounts}
\alias{getStats.IdMapCounts}
\alias{IdMapCounts.getStats}
\alias{getStats.IdMapCounts}
\alias{getStats,IdMapCounts-method}


 \title{Retrieves a set of unique counts of secondary IDs}
 \description{
 Retrieves a set of unique counts of secondary IDs
 over the all ID Maps and then computes the number
 of entries and percentage corresponding to each count
 for all ID Maps within the given object
 }

 \usage{\method{getStats}{IdMapCounts}(this, percent=TRUE, digits=1, summary=FALSE, cutoff=3, verbose=FALSE, ...)}

 \arguments{
 \item{idCounts}{ IdMapCounts object or a list of objects on which summary is computed}
 \item{percent}{ logical indicating wherether the percentage column(s) should be included into summary}
 \item{digits}{ integer indicating the number of decimal places in percentage column}
 \item{summary}{ if \code{\link[base:logical]{TRUE}}, the summary of counts is generated. Default is \code{\link[base:logical]{FALSE}}.}
 \item{cutoff}{ If summary is \code{\link[base:logical]{TRUE}}, all data for counts greater than cutoff are collapsed (summarized) into one row. Default is 3.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages.Default is \code{\link[base:logical]{FALSE}}}
 \item{...}{Not used}
 }

 \value{
 Data frame representing a set of unique counts of secondary IDs.
 if summary is FALSE, the resulting data frame represent the number of entries and percentage corresponding to each count
 for all ID Maps within the object. If summary is TRUE, the resulting data frame represent the formatted summary of count statistics,
 including summarized data for counts grater than cutoff, maximum number of secondary IDs per primary ID
 and the total number of secondary IDs for each DB within the IdMapCounts  object.
 }

 \examples{
 jointIdMap<-JointIdMap(examples$identDfList);
 cnts<-jointIdMap$getCounts(verbose=TRUE);
 cnts$getStats();
 cnts$getStats(summary=TRUE,cutoff=4);
 }
 \seealso{For more information see \code{\link{IdMapCounts}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
