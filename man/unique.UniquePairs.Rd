%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{unique.UniquePairs}
\alias{unique.UniquePairs}
\alias{UniquePairs.unique}
\alias{unique.UniquePairs}
\alias{unique,UniquePairs-method}


 \title{Extract unique elements}

 \description{
 Removes duplicate ID pairs from a given UniquePairs object so the resuting
 UniquePairs object contains only unique ID pairs
 }

 \usage{\method{unique}{UniquePairs}(x, incomparables=FALSE, verbose=FALSE, ...)}

 \arguments{
\ item{incomparables}{Not used (defined for compatibility with generic).}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \value{UniquePairs object with dublicate ID pairs removed.}

 \examples{
 pairs1<-UniquePairs$create(examples$identDfList[[1]]);
 pairs2<-UniquePairs$create(examples$identDfList[[2]]);
 dup_pairs<-UniquePairs(rbind(as.data.frame(pairs1),as.data.frame(pairs2)));
 dim(dup_pairs);
 unique_pairs<-dup_pairs$unique();
 dim(unique_pairs);
 }

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
