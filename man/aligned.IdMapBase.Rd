%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMapBase.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{aligned.IdMapBase}
\alias{aligned.IdMapBase}
\alias{IdMapBase.aligned}
\alias{aligned.IdMapBase}
\alias{aligned,IdMapBase-method}


 \title{Checks if two IdMapBase objects match on column names and primary ID set}
 \description{Checks if two IdMapBase objects match on column names and primary ID set.}

 \usage{\method{aligned}{IdMapBase}(this, other, ...)}

 \arguments{
 \item{other}{ The second ID Map object to check the matching against.}
 \item{...}{ Not used.}
 }
 \value{\code{\link[base:logical]{TRUE}} if two IdMapBase objects are matching, otherwise \code{\link[base:logical]{FALSE}}.}

 \examples{
 obj1<-IdMapBase(examples$identDfList[[1]]);
 obj2<-IdMapBase(examples$identDfList[[2]]);
 aligned(obj1,obj2);

 obj3<-IdMapBase(obj1[1:10,]);
 DF<-cbind(obj1[1:10,1],obj2[1:10,2]);
 colnames(DF)<-colnames(obj3);
 obj4<-IdMapBase(DF);
 aligned(obj3,obj4);
 }
 \seealso{For more information see \code{\link{IdMapBase}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
