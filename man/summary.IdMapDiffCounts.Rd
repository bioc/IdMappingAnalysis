%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/idMapDiffCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{summary.IdMapDiffCounts}
\alias{summary.IdMapDiffCounts}
\alias{IdMapDiffCounts.summary}
\alias{summary,IdMapDiffCounts-method}

 \title{Get a compaund event counts summary report}

 \description{
 Get a counts summary report where each row is a count of matches
 corresponding to a particular compound event
 }

 \usage{
\method{summary}{IdMapDiffCounts}(object, ...)
}

 \arguments{
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} with a single 'counts' column
 where each row is a count of matches corresponding to a particular compound event.
 }

 \examples{
 #create IdMapDiffCounts object
 IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
 jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
 diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
 diffCounts<-IdMapDiffCounts(diffs);

 # get summary
 diffCounts$summary();
 }

 \seealso{For more information see \code{\link{IdMapDiffCounts}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
