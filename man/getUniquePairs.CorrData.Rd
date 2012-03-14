%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getUniquePairs.CorrData}
\alias{getUniquePairs.CorrData}
\alias{CorrData.getUniquePairs}
\alias{getUniquePairs.CorrData}
\alias{getUniquePairs,CorrData-method}


 \title{Extract unique pairs from the CorrData object}

 \description{Extract unique pairs from the CorrData object.}

 \usage{\method{getUniquePairs}{CorrData}(this, verbose=FALSE, ...)}

 \arguments{
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{UniquePairs object}

 \examples{
 uniquePairs<-examples$corrData$getUniquePairs();
 uniquePairs[1:10,];
 }

 \seealso{For more information see \code{\link{CorrData}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
