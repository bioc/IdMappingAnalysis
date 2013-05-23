%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{as.MultiSet.CorrData}
\alias{as.MultiSet.CorrData}
\alias{CorrData.as.MultiSet}
\alias{as.MultiSet,CorrData-method}

 \title{Convert CorrData object into MultiSet object}

 \description{
 Convert CorrData object into MultiSet object.# }

 \usage{
\method{as.MultiSet}{CorrData}(this, ...)
}

 \arguments{
 \item{...}{Not used}
 }

 \value{
 MultiSet object which assayData contain two matrices corresponding to the transposed data frames
 (with the mapping identifier column stripped out) encapsulated within the given CorrData object.
 Due to the nature of the MultiSet object, the sample names of the resulting MultiSet object should
 be accessed using featureNames() method, and the set of experiment identifiers should be accesed
 through the call to colnames on the list returned by the assayData() method.
 }

 \examples{
 library(Biobase);
 #convert CorrData object to MultiSet
 mset<-as.MultiSet(examples$corrData);
 #get sample names
 samples<-featureNames(mset);
 print(samples);
 #retrieve primary and secondary keys
 annotation(mset);
 #retreive primary and secondary IDs
 uniprotIDs<-colnames(assayData(mset)[[1]]);
 affyIDs<-colnames(assayData(mset)[[2]]);
 uniprotIDs[1:20];
 affyIDs[1:20];
 }

 \seealso{For more information see \code{\link{CorrData}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
