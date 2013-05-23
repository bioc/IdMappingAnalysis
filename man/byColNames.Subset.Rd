%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/subset.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Subset$byColNames}
\alias{Subset$byColNames}
\alias{byColNames.Subset}
\alias{Subset.byColNames}
\alias{byColNames,Subset-method}

 \title{Extract subset of columns from a data frame or a list of data frames}

 \description{
 Extract subset of columns from a data frame or a list of data frames.
 This function outputs a data frame which set of columns is a product of merging according to mergeOp
 of the imput data frame column set with a given set of names (subset) ordred either by the original column set
 or by a a subset parameter according to orderBySubset flag. In case the merging operation produces the columns
 which are not in an original data frame, the new columns are filled with value from na.value parameter.
 }

 \usage{
## Static method (use this):
## Subset$byColNames(frameData, subset, orderBySubset=TRUE, mergeOp=intersect, na.value="", verbose=FALSE, ...)

## Don't use the below:
\method{byColNames}{Subset}(static, frameData, subset, orderBySubset=TRUE, mergeOp=intersect, na.value="", verbose=FALSE, ...)
}

 \arguments{
 \item{frameData}{Input \code{\link[base]{data.frame}} or a list of data frames.}
 \item{subset}{\code{\link[base]{character}} \code{\link[base]{vector}} of names partially intersecting with given \code{\link[base]{data.frame}}
 column names and defining the set on which merging is to be performed.}
 \item{orderBySubset}{Determines if the resulting data frame(s) columns will be ordered by subset (\code{\link[base:logical]{TRUE}})
 or by the original column set (\code{\link[base:logical]{FALSE}}). Default is \code{\link[base:logical]{TRUE}}.}
 \item{mergeOp}{The merging operation to be performed on column names. Default in 'intersect'.}
 \item{na.value}{The value which should be used to field the empty columns
 in the output \code{\link[base]{data.frame}} (s). Default is "".}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} or list of data frames which column set is a product of
 column merging on a subset according to the mergeOp.
 }

 \examples{
 commonSamples<-intersect(colnames(examples$msmsExperimentSet),
		colnames(examples$msmsExperimentSet));
 mrna.subset<-Subset$byColNames(examples$mrnaExperimentSet,
		commonSamples,orderBySubset=TRUE);
 mrna.subset[1:10,1:5];
 }

 \seealso{For more information see \code{\link{Subset}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
