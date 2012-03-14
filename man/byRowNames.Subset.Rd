%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  subset.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{byRowNames.Subset}
\alias{byRowNames.Subset}
\alias{Subset.byRowNames}
\alias{byRowNames.Subset}
\alias{byRowNames,Subset-method}


 \title{Extract subset of columns from a data frame or a list of data frames}

 \description{
 Extract subset of columns from a data frame or a list of data frames.
 This function outputs a data frame which set of rows is a product of merging according to mergeOp
 of the imput data frame row set with a given set of names (subset) ordred either by the original row set
 or by a a subset parameter according to orderBySubset flag. In case the merging operation produces the rows
 which are not in an original data frame, the new rows are filled with value from na.value parameter.
 }

 \synopsis{byRowNames.Subset(static, frameData, subset, orderBySubset=TRUE, mergeOp=intersect, na.value="", verbose=FALSE, ...)}
\usage{Subset$byRowNames(frameData, subset, orderBySubset=TRUE, mergeOp=intersect, na.value="", verbose=FALSE, ...)}

 \arguments{
 \item{frameData}{Input \code{\link[base]{data.frame}} or a \code{\link[base]{list}} of data frames.}
 \item{subset}{\code{\link[base]{character}} vector of names partially intersecting with given data frame
 row names and defining the set on which merging is to be performed.}
 \item{orderBySubset}{Determines if the resulting data frame(s) rows will be ordered by subset (\code{\link[base:logical]{TRUE}}
 or by the original row set (\code{\link[base:logical]{FALSE}}). Default is \code{\link[base:logical]{TRUE}}.}
 \item{mergeOp}{The merging operation to be performed on row names. Default in 'intersect.}
 \item{na.value}{The value which should be used to field the empty rows in the
 output data frame(s). Default is "".}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} or list of data frames which row set is a product of
 row merging on a subset according to the mergeOp.
 }

 \examples{
 proteins<-examples$jointIdMap_corr$primaryIDs();
 msms.subset<-Subset$byRowNames(examples$msmsExperimentSet,proteins,orderBySubset=TRUE);
 dim(examples$msmsExperimentSet);
 dim(msms.subset);
 }

 \seealso{For more information see \code{\link{Subset}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
