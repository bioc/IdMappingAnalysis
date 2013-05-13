%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  uniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{create.UniquePairs}
\alias{create.UniquePairs}
\alias{UniquePairs.create}
\alias{create.UniquePairs}
\alias{create,UniquePairs-method}



 \title{Create a UniquePairs object from a single IdMap or a list of IdMap objects}

 \description{
 Create a UniquePairs object by converting a single IdMap or a list of Id Maps into
 a single or list of unique pairs data structures optionally intersecting
 the secondary ID set with an external secondary ID set if farther data
 size reduction is necessary
 }

\usage{UniquePairs$create(idMapSet, secondaryIDs=NULL, keepMissing=FALSE, verbose=FALSE, ...)}

 \arguments{
 \item{idMapSet}{an object or a list of object which can be coerced
 into IdMap object(s) to be converted into UniquePairs object (list).}
 \item{secondaryIDs}{optional secondary ID list on which the resulting UniquePairs
 object is intersected. Default is NULL (not present).}
 \item{keepMissing}{ogical indicating if the rows with empty secondary IDs should removed
 from the resulting object. Default is FALSE (keep such rows)}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{A UniquePairs object or a \code{\link[base]{list}} of UniquePairs objects}

 \examples{
 uniquePairs<-UniquePairs$create(IdMap(examples$identDfList[[1]]));
 uniquePairsList<-UniquePairs$create(examples$identDfList,verbose=TRUE);
 }

 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
