%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{pack.experiments.CorrData}
\alias{pack.experiments.CorrData}
\alias{CorrData.pack.experiments}
\alias{pack.experiments.CorrData}
\alias{pack.experiments,CorrData-method}


 \title{Convert aligned experiment data into the data structure incapsulated within the CorrData object}

 \description{Convert aligned experiment data into the data structure incapsulated within the CorrData object.
 }

\usage{CorrData$pack.experiments(experimentSetList, uniqueTuples, verbose=FALSE, ...)}

 \arguments{
 \item{experimentSetList}{ list of aligned ExperimentSet to be converted.}
 \item{uniqueTuples}{ set of unique pairs (triplets ets) each column of which corresponds to the primary key
 of the corresponding ExperimentSet object in the list.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 a \code{\link[base]{list}} of two or more elements with names corresponding to the primary IDs for each
 experiment modality where each element contains a data frame with primary
 IDs of a given modality in the first column while the rest of columns contain the experiment data.
 The names of the data columns in both data frames are identical and correspond to the sample IDs.
 }

 \seealso{For more information see \code{\link{CorrData}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
