%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.extensions.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getCorrDataFrame.JointUniquePairs}
\alias{getCorrDataFrame.JointUniquePairs}
\alias{JointUniquePairs.getCorrDataFrame}
\alias{getCorrDataFrame.JointUniquePairs}
\alias{getCorrDataFrame,JointUniquePairs-method}


 \title{Merge JointUniquePairs and Corr objects into a single data frame}

 \description{
 This is a convinience function allowing to derive a data frame from the JointUniquePairs and Corr object.
 The structure of the resulting data frame is similar to the one of JointUniquePairs object, but \code{\link[base]{logical}} values
 indicating the presence of the given match pair for a particular ID Map are replaced whith correlation values
 from Corr object if the \code{\link[base]{logical}} value is \code{\link[base:logical]{TRUE}} or by \code{\link[base]{NA}} 's otherwise. Note, that the Corr object should
 correspond to the union of all ID Maps in consideration, i.e. should represent a full group.
 }

 \usage{\method{getCorrDataFrame}{JointUniquePairs}(this, corr, groups=NULL, full.group=FALSE, verbose=FALSE, ...)}

 \arguments{
 \item{corr}{Corr object from which the Corr object(s) retrieved
 for each particular DB based on which the patricular Corr object is retrieved.}
 \item{groups}{Optional \code{\link[base]{list}} of DB names from the JointUniquePairs object defining the set of
 match groups to be used for retrieving the corresponding Corr objects.
 If \code{\link[base]{NULL}}, all match groups within the JointUniquePairs object are used.}
 \item{full.group}{Determines if the resulting Corr object \code{\link[base]{list}} should represent a full group. Default is \code{\link[base:logical]{FALSE}}.}
 \item{verbose}{if \code{\link[base:logical]{TRUE}} enables diagnostic messages. Default is \code{\link[base:logical]{FALSE}}.}
 \item{...}{Not used}
 }

 \value{
 A \code{\link[base]{data.frame}} similar to the one encapsultaed wwithin the JointUniquePairs object, but the \code{\link[base]{logical}} values
 indicating the presence of the given match pair for a particular ID Map are replaced by correlation values
 from Corr object if the \code{\link[base]{logical}} value is \code{\link[base:logical]{TRUE}} or by \code{\link[base]{NA}} 's otherwise.
 }


 \examples{
 corrDF<- examples$jointUniquePairs$getCorrDataFrame(examples$corr,
	groups=c("NetAffx_Q", "DAVID_Q", "EnVision_Q"),
     full.group=FALSE, verbose=TRUE);
 corrDF[1:10,];
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
