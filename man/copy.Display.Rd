%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Display$copy}
\alias{Display$copy}
\alias{copy.Display}
\alias{Display.copy}
\alias{copy,Display-method}

 \title{Save current plot to the file}

 \description{
 Save current plot to the file.
 }

 \usage{
## Static method (use this):
## Display$copy(filename, zoom=1, plotFun=NULL, plotArgs=list(), type=c("png", "jpeg", "jpg", "tiff", "bmp", "pdf"), res=72, quality=100, compression=c("none", "rle", "lzw", "jpeg", "zip"), ...)

## Don't use the below:
\method{copy}{Display}(static, filename, zoom=1, plotFun=NULL, plotArgs=list(), type=c("png", "jpeg", "jpg", "tiff", "bmp", "pdf"), res=72, quality=100, compression=c("none", "rle", "lzw", "jpeg", "zip"), ...)
}

 \arguments{
 \item{filename}{The filename under which to save the plot.}
 \item{zoom}{Copy zoom factor. Default is 1.}
 \item{plotFun}{If NULL (default), copies the content of the current device to the file,
 otherwise uses plotFun to plot into the file.}
 \item{plotArgs}{The \code{\link[base]{list}} of parameters for the poltFun. Default is an empty \code{\link[base]{list}}.}
 \item{type}{The type of plot, PNG, JPEG, TIFF, or PDF. Default is PNG.}
 \item{res}{DPI resolution. Default is 72dpi.}
 \item{quality}{If type is JPEG, the 'quality' of the JPEG image, as a percentage.
 Smaller values will give more compression but also more degradation of the image.
 Default is 100 (best quality).}
 \item{compression}{If type is TIFF the type of compression to be used.
 Could be 'none', 'rle', 'lzw', 'jpeg' or 'zip'. Default is 'none'.}
 \item{...}{Not used}
 }

 \value{
 Name of the file the plot was saved to.
 }

 \examples{\dontrun{
 plot(c(1:20),c(1:20));
 Display$copy("plot.png");
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
