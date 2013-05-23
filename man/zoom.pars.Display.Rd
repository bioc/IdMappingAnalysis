%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Display$zoom.pars}
\alias{Display$zoom.pars}
\alias{zoom.pars.Display}
\alias{Display.zoom.pars}
\alias{zoom.pars,Display-method}

 \title{Zoom graphics parameters}

 \description{Zoom graphics parameters.}

 \usage{
## Static method (use this):
## Display$zoom.pars(zoom, par.names=c("lwd", "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", "mai"), ...)

## Don't use the below:
\method{zoom.pars}{Display}(static, zoom, par.names=c("lwd", "cex", "cex.axis", "cex.lab", "cex.main", "cex.sub", "mai"), ...)
}

 \arguments{
 \item{zoom}{Zoom factor.}
 \item{par.names}{Graphics parameters to zoom.}
 \item{...}{Not used}
 }

 \value{
 Previous values of altered graphics parameters.
 }

 \examples{\dontrun{
 Display$zoom.pars(zoom=3);
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
