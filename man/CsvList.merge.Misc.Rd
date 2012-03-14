%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  misc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{CsvList.merge.Misc}
\alias{CsvList.merge.Misc}
\alias{Misc.CsvList.merge}
\alias{CsvList.merge.Misc}
\alias{CsvList.merge,Misc-method}


 \title{Pairwise merge of two string vectors}

 \description{
 Pairwise merge of two string vectors.
 }

 \synopsis{CsvList.merge.Misc(static, s1, s2, mergeOp, asStrings=TRUE, ...)}
\usage{Misc$CsvList.merge(s1, s2, mergeOp, asStrings=TRUE, ...)}

 \arguments{
 \item{s1}{First string vector.}
 \item{s2}{Second string vectors.}
 \item{mergeOp}{Merging operation (intersect, setdiff etc.)}
 \item{asStrings}{If TRUE (default), convert the resulting string vector
 into the vector of comma separated strings.}
 \item{...}{Not used}
 }

 \value{
 If asStrings=TRUE, the vector of comma separated strings representing the merging results,
 and if asStrings=FALSE, then result is a \code{\link[base]{matrix}} of strings where each row
 represents the pairwise merge.
 }

 \examples{
 mrg<-Misc$CsvList.merge(c("a","b"),c("d","f"),mergeOp=union);
 }

 \seealso{For more information see \code{\link{Misc}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
