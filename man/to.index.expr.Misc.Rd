%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  misc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{to.index.expr.Misc}
\alias{to.index.expr.Misc}
\alias{Misc.to.index.expr}
\alias{to.index.expr.Misc}
\alias{to.index.expr,Misc-method}


 \title{Convert expression into index expression for a given list or data frame object}

 \description{
 Convert expression into index expression for a given list or data frame object.
 Uses the input expression to generate the corresponding expression on a given
 list using the expression variables to address the list items,
 i.e. for an object X and expression 'a+b' generate the expression 'X$a + X$b'
 }

 \synopsis{to.index.expr.Misc(static, obj, expr, ...)}
\usage{Misc$to.index.expr(obj, expr, ...)}

 \arguments{
 \item{obj}{Input \code{\link[base]{list}} or 2data.frame -derived object on which the expression to be constructed.}
 \item{expr}{\code{\link[base]{expression}} or \code{\link[base]{character}} string from which the indexed
 expression on the input object to be constructed.}
 \item{...}{Not used}
 }

 \value{
 Indexed \code{\link[base]{expression}} on a given \code{\link[base]{list}} or \code{\link[base]{data.frame}} object.
 }

 \examples{\dontrun{
 indexed.expression<-to.index.expr(G,"a | b&c");
 }}

 \seealso{For more information see \code{\link{Misc}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
