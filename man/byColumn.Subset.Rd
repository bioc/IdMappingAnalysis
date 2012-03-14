%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  subset.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{byColumn.Subset}
\alias{byColumn.Subset}
\alias{Subset.byColumn}
\alias{byColumn.Subset}
\alias{byColumn,Subset-method}


 \title{# Extract subset of rows from a data frame or a list of data frames
 by intersecting on a particular column}

 \description{
 # Extract subset of rows from a data frame or a list of data frames
 by intersecting on a particular column.
 This function outputs a data frame which set of rows is a product of intersecting on a particular column
 of the imput data frame(s) rows with a given set of names (subset).
 }

 \synopsis{byColumn.Subset(static, frameData, subset, column, verbose=FALSE, ...)}
\usage{Subset$byColumn(frameData, subset, column, verbose=FALSE, ...)}

 \arguments{
 \item{frameData}{Input \code{\link[base]{data.frame}} or a \code{\link[base]{list}} of data frames.}
 \item{subset}{\code{\link[base]{character}} \code{\link[base]{vector}} of names partially intersecting with given \code{\link[base]{data.frame}}
 column and defining the set on which merging is to be performed.}
 \item{column}{The column on which the intersection is to be performed.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} or list of data frames which row set is a product of
 intersecting of a particular column with a (partially intersecting) subset.
 }

 \examples{
 commonSamples<-intersect(colnames(examples$msmsExperimentSet),
		colnames(examples$msmsExperimentSet));
 mrna.subset<-Subset$byColNames(examples$mrnaExperimentSet,
		commonSamples,column=1);
 mrna.subset[1:10,1:5];
 }

 \seealso{For more information see \code{\link{Subset}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
