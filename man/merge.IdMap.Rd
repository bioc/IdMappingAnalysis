%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  idMap.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{merge.IdMap}
\alias{merge.IdMap}
\alias{IdMap.merge}
\alias{merge.IdMap}
\alias{merge,IdMap-method}


 \title{Merge the IdMap object with a second IdMap object or a list of IdMap objects}

 \description{
 Merges the IdMap object (this) with a second IdMap object or a list of IdMap objects on secondary IDs
 (second column) usind the mergeOp and on row names using the rowMergeOp.
 the result is then ordered by the primary key of 'this' IdMap object.
 The method can be used as object specific as well as static.
 In the latter case the method merges the list of IdMap objects
 by using IdMap$merge(idMapSet) call signature.
 }

 \usage{\method{merge}{IdMap}(x, y, mergeOp, rowMergeOp=mergeOp, verbose=FALSE, ...)}

 \arguments{
 \item{idMapSet}{ IdMap object or a list of ID Map objects to merge with.}
 \item{mergeOp}{ merge operation (intersect, union etc) performed on the secondary ID list.}
 \item{rowMergeOp}{ merge operation (intersect, union etc) performed on the rows of merged ID Maps.}
 \item{verbose}{ if \code{\link[base:logical]{TRUE}} enables diagnostic messages.}
 \item{...}{Not used.}
 }
 \value{An IdMap object containing the merge result.}

 \examples{
 obj1<-IdMap(examples$identDfList[[1]]);
 obj2<-IdMap(examples$identDfList[[2]]);

 #intersection
 mergedObj<-merge(obj1,obj2,intersect);
 rbind(dim(obj1),dim(obj2),dim(mergedObj));

 #difference
 mergedObj<-merge(obj1,obj2,setdiff);
 rbind(dim(obj1),dim(obj2),dim(mergedObj));

 #union using the static call
 mergeObj<-IdMap$merge(examples$identDfList,union,verbose=TRUE);
 }
 \seealso{For more information see \code{\link{IdMap}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
