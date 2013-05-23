%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/dataFilter.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{DataFilter}
\docType{class}
\alias{DataFilter}

 \encoding{latin1}

 \title{The DataFilter class}

 \description{
 Package:  IdMappingAnalysis \cr
\bold{Class DataFilter}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{DataFilter}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{DataFilter}\cr
extends \link[R.oo]{Object}\cr



 Serves as a wrapper for data data filtering functions
 define as static methods of the DataFilter class.
 }

 \usage{
DataFilter()
}

 \arguments{
 \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[IdMappingAnalysis:do.apply.DataFilter]{do.apply}} \tab Filter experiment using constraints.\cr
 \tab \code{\link[IdMappingAnalysis:fisherTransform.DataFilter]{fisherTransform}} \tab Compute the Fisher transform.\cr
 \tab \code{\link[IdMappingAnalysis:fisherTransformInverse.DataFilter]{fisherTransformInverse}} \tab Compute the Fisher inversed transform.\cr
 \tab \code{\link[IdMappingAnalysis:fisherTransformJacobean.DataFilter]{fisherTransformJacobean}} \tab Compute the Fisher transform Jacobean.\cr
 \tab \code{\link[IdMappingAnalysis:logTen.DataFilter]{logTen} \tab Compute log10 of a numerical vector combined with thresholding on minimum value.\cr
 \tab \code{\link[IdMappingAnalysis:minAvgCountConstraint.DataFilter]{minAvgCountConstraint}} \tab Perform mean based thresholding of an input vector.\cr
 \tab \code{\link[IdMappingAnalysis:minCountConstraint.DataFilter]{minCountConstraint}} \tab Perform minimum count based thresholding of an input vector.\cr
 \tab \code{\link[IdMappingAnalysis:minCountGroupConstraint.DataFilter]{minCountGroupConstraint}} \tab Perform minimum count based thresholding of an input vector subdivided into groups.\cr
 \tab \code{\link[IdMappingAnalysis:removeNASeries.DataFilter]{removeNASeries}} \tab Remove NA series from the experiment set.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Alex Lisovich, Roger Day}
\keyword{classes}
