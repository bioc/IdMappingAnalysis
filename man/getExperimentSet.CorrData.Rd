%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  corrData.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getExperimentSet.CorrData}
\alias{getExperimentSet.CorrData}
\alias{CorrData.getExperimentSet}
\alias{getExperimentSet.CorrData}
\alias{getExperimentSet,CorrData-method}


 \title{Get experiment set data frame for a given modality}

 \description{Get experiment set data frame for a given modality.}

 \usage{\method{getExperimentSet}{CorrData}(this, modality, as=c("data.frame", "ExpressionSet"), IDs=NULL, verbose=FALSE, ...)}

 \arguments{
 \item{modality}{Experiment modality for which data to be retrieved, i.e. 'Uniprot', 'Affy' etc.}
 \item{as}{\code{\link[base]{character}} string defining in what format to the experiment data should be returned.
 The possible values are either 'data.frame' (default) or 'ExpressionSet'.}
 \item{IDs}{Optional character vector of ID subset for which data to be retrieved,
 or \code{\link[base]{NULL}} (default) in which case data for all IDs returned.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 Depending on 'as' argument, either EspressionSet or \code{\link[base]{data.frame}}
 containing experiment data for a given modality.
 }

 \examples{
 #retrieve msms ('Uniprot') experiment data as a data frame
 expSetDF<-examples$corrData$getExperimentSet(modality="Uniprot",as="data.frame");
 expSetDF[1:10,1:5];

 #retrieve mrns ('Affy') experiment data as an ExpressionSet
 expSet<-examples$corrData$getExperimentSet(modality="Affy",as="ExpressionSet");
 print(expSet);
 expSetDF<-t(as(expSet,"data.frame"));
 expSetDF[1:10,1:5];
 }

 \seealso{For more information see \code{\link{CorrData}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
