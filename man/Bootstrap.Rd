%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/bootstrap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Bootstrap}
\docType{class}
\alias{Bootstrap}

 \encoding{latin1}

 \title{The Bootstrap class}

 \description{
  Package:  IdMappingAnalysis \cr
\bold{Class Bootstrap}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[IdMappingAnalysis]{IdMapBase}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{Bootstrap}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{Bootstrap}\cr
extends \link[IdMappingAnalysis]{IdMapBase}\cr


 The Bootstrap object encapsulates a data frame containing
 the unique pairs in the first two columns and the correlation results, sd and bias
 obtained from the bootstrapping procedure in the next 3 columns
 During the object creation, the bootstrapping procedure is applyied to each row
 of the experiment set pairs from the CorrData object optionally applying the Fisher transform
 to the correlation data.
 }

 \usage{
Bootstrap(corrData=NULL, Fisher=FALSE, R=200, verbose=FALSE, ...)
}

 \arguments{
 \item{corrData}{CorrData object on which the correlation related bootstrapping is performed.}
 \item{Fisher}{If \code{\link[base:logical]{TRUE}}, the Fisher transform of data is performed during bootstrapping. Default is \code{\link[base:logical]{FALSE}}.}
 \item{R}{The number of bootstrap replicates. Default is 200.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \value{
 A Bootstrap object encapsultating the \code{\link[base]{data.frame}} with following columns:
 \item{column 1}{the first component (primary IDs) of unique pairs. The column name corresponds to the primary key
 of a source ID Map}
 \item{column 2}{the second component (secondary IDs) of unique pairs. The column name corresponds to the secondary key
 of a source ID Map}
 \item{'corr' column}{contains the correlation values obtained from bootstrapping}
 \item{'sd' column}{contains the correlation sd values obtained from bootstrapping}
 \item{'bias' column}{contains the correlation bias values obtained from bootstrapping}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:plot.Bootstrap]{plot}} \tab Scatterplot of bootstrapped results: sd vs correlation .\cr
}


 \bold{Methods inherited from IdMapBase}:\cr
[, aligned, as.data.frame, dim, dimnames, getName, primaryIDs, primaryKey, secondaryKey

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
 bootstrap<-Bootstrap(examples$corrData,R=20,verbose=TRUE);
 class(bootstrap);
 bootstrap[1:10,];
 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
