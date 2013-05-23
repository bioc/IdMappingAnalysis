%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/corr.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Corr}
\docType{class}
\alias{Corr}

 \encoding{latin1}

 \title{The Corr class}

 \description{
  Package:  IdMappingAnalysis \cr
\bold{Class Corr}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[IdMappingAnalysis]{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{Corr}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{Corr}\cr
extends \link[IdMappingAnalysis]{IdMapBase}\cr



 Create the Corr object by performing correlations on the CorrData object
 using the correlation algorithm defined by the method argument.
 The Corr object encapsulates a \code{\link[base]{data.frame}} containing three columns:
 the first two are unique pairs and the third is a correlation results with a column name
 reflecting the correlation method ('pearson', 'spearman' or 'kendall').
 }

 \usage{
Corr(corrData=NULL, method="pearson", verbose=FALSE, ...)
}

 \arguments{
 \item{corrData}{CorrData object on which correlation is performed
  or a \code{\link[base]{data.frame}} compliant with the Corr object internal data frame format.}
 \item{method}{Correlation method ('pearson', 'spearman' or 'kendall').
 Default is 'pearson'.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:getData.Corr]{getData}} \tab Extract correlation results from the Corr object.\cr
 \tab \code{\link[IdMappingAnalysis:getUniquePairs.Corr]{getUniquePairs}} \tab Extract unique pairs from the Corr object.\cr
 \tab \code{\link[IdMappingAnalysis:plot.Corr]{plot}} \tab Plot the density distributions for correlation object(s).\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
 corr<-Corr(examples$corrData,method="spearman",verbose=TRUE);
 class(corr);
 corr[1:10,];
 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
