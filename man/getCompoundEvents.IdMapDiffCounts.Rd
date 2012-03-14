%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapDiffCounts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCompoundEvents.IdMapDiffCounts}
\alias{getCompoundEvents.IdMapDiffCounts}
\alias{IdMapDiffCounts.getCompoundEvents}
\alias{getCompoundEvents.IdMapDiffCounts}
\alias{getCompoundEvents,IdMapDiffCounts-method}


 \title{Get compound events}

 \description{
 Transforms the IdMapDiffCounts triplet <A-A*B, A*B, B-A*B>
 into the <excess both, excess left etc.> form suitable for using
 within the fountain plot. Computed on the fly within the IdMapDiffCounts$summary()
 and IdMapDiffCounts$plot() functions.
 }

 \usage{\method{getCompoundEvents}{IdMapDiffCounts}(this, verbose=FALSE, ...)}

 \arguments{
 \item{verbose}{If \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} with 7 columns each corresponding to a particular compaund set
 derived from the event triplet <A-A*B, A*B, B-A*B>
 and with rows containing mathches (TRUE or FALSE) for a given primary ID.
 The columns and the compaund set description are as follows:
 \item{excess.both}{A and B have non-empty intersection and both sets have unique elements}
 \item{excess.left}{A and B have non-empty intersection and only A has unique elements}
 \item{excess.right}{A and B have non-empty intersection and only B has unique elements}
 \item{same.list}{Non-empty A and B are the same}
 \item{in.left.only}{Only A set is non-empty}
 \item{in.right.only}{Only B set is non-empty}
 \item{in.neither}{Both sets are empty}
 }

 \examples{
 #create IdMapDiffCounts object
 IDs<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
 jointIdMap<-JointIdMap(examples$identDfList,primaryIDs=IDs);
 diffs<-jointIdMap$getDiff("NetAffx_F","DAVID_Q",verbose=TRUE);
 diffCounts<-IdMapDiffCounts(diffs);

 # get compound events
 events<-diffCounts$getCompoundEvents();
 events[1:10,];
 }

 \seealso{For more information see \code{\link{IdMapDiffCounts}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
