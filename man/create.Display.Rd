%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{create.Display}
\alias{create.Display}
\alias{Display.create}
\alias{create.Display}
\alias{create,Display-method}


 \title{Open a new display device}

 \description{Open a new display device.}

 \synopsis{create.Display(static, width=7, height=7, bg="transparent", ...)}
\usage{Display$create(width=7, height=7, bg="transparent", ...)}

 \arguments{
 \item{width}{Display device width in inches. Default is 7.}
 \item{height}{Display device height in inches. Default is 7.}
 \item{bg}{Device background. Default is 'transparent'.}
 \item{...}{Not used}
 }

 \examples{\dontrun{
 Display$create(width=10,height=7,bg="green");
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
