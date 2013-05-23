%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/mixture.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Mixture}
\docType{class}
\alias{Mixture}

 \encoding{latin1}

 \title{The Mixture class}

 \description{
  Package:  IdMappingAnalysis \cr
\bold{Class Mixture}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{Mixture}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{Mixture}\cr
extends \link[R.oo]{Object}\cr



 The constructor creates a model from a single Corr object using the number
 of clusters defined by G determining the optimal number of clusters by default
 and optionally using the Fisher transform.
 }

 \usage{
Mixture(corr=NULL, G=c(1:5), Fisher=FALSE, verbose=FALSE, ...)
}

 \arguments{
 \item{corr}{ Corr object on wich mixture modeling is performed.}
 \item{G}{ number of components in mixture model. If G is a vector, the optimal
 number of components is determined. G is a vector (1:5) by default.}
 \item{Fisher}{ if \code{\link[base:logical]{TRUE}}, the Fisher transform of correlation data is performed before
 the model is fitted. Default is \code{\link[base:logical]{FALSE}}.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used.}
 }

 \value{
 The resulting Mixture object encapsulates a data member '.model'
 containing the results of mixture modeling represented by the \code{\link[base]{list}}
 with following components:
 \item{corr}{the correlation data}
 \item{clust}{the clustering results data structure returned by Mclust()}
 \item{sd}{standard deviation derived from clust$parameters$variance$sigmasq}
 \item{density}{the correlation density distribution}
 \item{marginalDensity}{the marginal density}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:clust.Mixture]{clust}} \tab Retrieve the custering results data structure.\cr
 \tab \code{\link[IdMappingAnalysis:getData.Mixture]{getData}} \tab Extract mixture component data from the Mixture object.\cr
 \tab \code{\link[IdMappingAnalysis:getStats.Mixture]{getStats}} \tab Get mixture component model summary info.\cr
 \tab \code{\link[IdMappingAnalysis:plot.Mixture]{plot}} \tab Plot the results of mixture modeling.\cr
 \tab \code{\link[IdMappingAnalysis:primaryKey.Mixture]{primaryKey}} \tab Retrieves a primary key for a given Mixture object.\cr
 \tab \code{\link[IdMappingAnalysis:secondaryKey.Mixture]{secondaryKey}} \tab Retrieves a secondary key for a given Mixture object.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
 mixture<-Mixture(examples$corr,G=c(1:4),Fisher=TRUE,verbose=TRUE);
 class(mixture);
 names(mixture$.model)
 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
