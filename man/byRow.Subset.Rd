%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  subset.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{byRow.Subset}
\alias{byRow.Subset}
\alias{Subset.byRow}
\alias{byRow.Subset}
\alias{byRow,Subset-method}


 \title{Extract subset of columns from a data frame or a list of data frames
 by intersecting on a particular row}

 \description{
 Extract subset of columns from a data frame or a list of data frames
 by intersecting on a particular row.
 This function outputs a data frame which set of columns is a product of intersecting on a particular row
 of the imput data frame(s) columns with a given set of names (subset).
 }

\usage{Subset$byRow(frameData, subset, row, verbose=FALSE, ...)}

 \arguments{
 \item{frameData}{Input \code{\link[base]{data.frame}} or a \code{\link[base]{list}} of data frames.}
 \item{subset}{\code{\link[base]{character}} \code{\link[base]{vector}} of names partially intersecting with given \code{\link[base]{data.frame}}
 row and defining the set on which merging is to be performed.}
 \item{row}{The row on which the intersection is to be performed.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} or list of data frames which column set is a product of
 intersecting of a particular row with a (partially intersecting) subset.
 }

 \examples{
 #extract msms experiment subset containing only samples
 #for which the event count for Uniprot='P04264' takes predefined values 1,2 and 3
 cnts<-c(1,2,3);
 subset.cnts<-Subset$byRow(examples$msmsExperimentSet[-1],cnts,row="P04264");
 dim(subset.cnts);
 }

 \seealso{For more information see \code{\link{Subset}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
