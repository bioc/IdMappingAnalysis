%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Display$line.unsorted}
\alias{Display$line.unsorted}
\alias{line.unsorted.Display}
\alias{Display.line.unsorted}
\alias{line.unsorted,Display-method}

 \title{Draw a curve from unsorted points}

 \description{Draw a curve from unsorted points.}

 \usage{
## Static method (use this):
## Display$line.unsorted(x, y, type="l", ...)

## Don't use the below:
\method{line.unsorted}{Display}(static, x, y, type="l", ...)
}

 \arguments{
 \item{x}{The (unsorted) x coordinates of points to plot.}
 \item{y}{The correponding y coordinates of points to plot.}
 \item{type}{Line type. Default is "l"."}
 \item{...}{Additional graphical parameters}
 }

 \examples{\dontrun{
 plot.new();
 plot.window(c(min(x),max(x)),c(min(y),max(y)));
 Display$line.unsorted(x,y);
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
