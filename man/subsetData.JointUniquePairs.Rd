%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/jointUniquePairs.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{subsetData.JointUniquePairs}
\alias{subsetData.JointUniquePairs}
\alias{JointUniquePairs.subsetData}
\alias{subsetData,JointUniquePairs-method}

 \title{Subset data on a UniquePairsMatch object}

 \description{Subset data on a UniquePairsMatch object.}

 \usage{
\method{subsetData}{JointUniquePairs}(this, data, expr, ...)
}

 \arguments{
 \item{data}{\code{\link[base]{numeric}} vector to subset. Should be of the same length as ncol(this).}
 \item{expr}{\code{\link[base]{expression}} or a \code{\link[base]{character}} string representing an expression
 according to which the indicator variable columns of match object will be combined
 to form the logical vector of data vector elements inclusion.}
 \item{...}{Not used}
 }

 \value{subset of the original data vector.}

 \examples{
 #perform Wilcoxon test for EnVision/Affy corellation data
 data1<-examples$jointUniquePairs$subsetData(examples$corr$getData(),
		"EnVision_Q & !NetAffx_Q");
 data2<-examples$jointUniquePairs$subsetData(examples$corr$getData(),
           "!EnVision_Q & NetAffx_Q");
 wilcox<-wilcox.test(data1,data2);
 wilcox[names(wilcox)];
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
