%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{swapKeys.UniquePairs}
\alias{swapKeys.UniquePairs}
\alias{UniquePairs.swapKeys}
\alias{swapKeys.UniquePairs}
\alias{swapKeys,UniquePairs-method}


 \title{Swap the primary and secondary key columns}
 \description{Swap the primary and secondary key columns.}

 \synopsis{swapKeys.UniquePairs(static, uniquePairs, ...)}
\usage{UniquePairs$swapKeys(uniquePairs, ...)}

 \arguments{
 \item{uniquePairs}{The UniquePairs object or a list of UniquePairs objects which keys to be swapped.}
 \item{...}{Not used.}
 }
 \value{UniquePairs object or list of UniquePairs objects with swapped keys.}

 \examples{
 pairs<-UniquePairs$create(examples$identDfList[[1]]);
 pairs[1:10,];
 swapped_pairs<-UniquePairs$swapKeys(pairs);
 swapped_pairs[1:10,];
 }

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
