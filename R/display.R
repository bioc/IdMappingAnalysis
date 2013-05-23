
###########################################################################/**
#' The Display class
#
#' The \code{Display} classs serves as a wrapper for a set of graphical functions 
#' defined as static methods of Display class
#' 
#'
#' @param
#' @author{Alex Lisovich, Roger Day}

setConstructorS3("Display",function(){
	extend(Object(),"Display");
})



###########################################################################/**
#' 
#' Plot loess transformed data
#
#' \code{line.loess} will plot loess transformed data
#' 
#' 
#' @param x The x coordinates of points to plot.
#' @param y The y coordinates of points to be loess transformed for plotting.
#' @param ... Additional graphical parameters
#
#' @examples
#' \dontrun{
#' plot.new();
#' plot.window(c(min(x),max(x)),c(min(y),max(y)));
#' Display$line.loess2(x,y);
#' }
#
#' @seealso{Display}
#' @author{Alex Lisovich, Roger Day}

setMethodS3("line.loess","Display",function(static,x, y, ...) {
	loess.out = loess(y ~ x)
	inds<-order(loess.out$x);
	lines(loess.out$x[inds], loess.out$fitted[inds], ...)
},static=TRUE)


###########################################################################/**
#' 
#' Draw a curve from unsorted points
#
#' \code{line.unsorted}: Draw a curve from unsorted points
#' 
#' @param x The (unsorted) x coordinates of points to plot.
#' @param y The correponding y coordinates of points to plot.
#' @param type Line type. Default is "l"."
#' @param ... Additional graphical parameters
#' }
#
#' @examples{\dontrun{
#' plot.new();
#' plot.window(c(min(x),max(x)),c(min(y),max(y)));
#' Display$line.unsorted(x,y);
#' }
#
#' @seealso{Display}
#' @author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("line.unsorted","Display",function (static,x, y, type = "l", ...) {
	lines(sort(x),y[order(x)],type=type,...);
},static=TRUE)



###########################################################################/**
#' 
#' Display a progress message
#
#' \code{progressMsg}
#' Display a message accompanied by the percentage of completion.
#' 
#
#' @param msg Message to be dysplayed.
#' @param i Counter of the progress.
#' @param total The maximum value for a counter corresponding to 100 percent completion.
#' @param ... Not used
#' }
#
#' \examples{\dontrun{
#' for (i in 1:100){
#'   Display$progressMsg("processing: ",i,100);
#' # do something ...
#' }
#' }}
#
#' @seealso{Display}
#' \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("progressMsg","Display",function(static,msg,i,total,...){
	msgFreq<-max(1,as.integer(total/100));
	if ((i %% msgFreq)==0){
		cat("                                                      \r");
		cat(msg,round(i*100/total),"%\r");
		flush.console();
	}
},static=TRUE)



###########################################################################/**
#' @RdocMethod create
#' 
#' @title "Open a new display device"
#
#' \description{@get "title".}
#' 
#' @synopsis
#
#' \arguments{
#' @param width Display device width in inches. Default is 7.
#' @param height Display device height in inches. Default is 7.
#' @param bg Device background. Default is 'transparent'.
#' @param ... Not used
#' }
#
#' \examples{\dontrun{
#' Display$create(width=10,height=7,bg="green");
#' }}
#
#' @seealso{Display}
#' \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("create","Display",function(static,width=7,height=7,bg="transparent",...){
	devopt<-getOption("device");
	if (is.character(devopt) && devopt=="JavaGD"){
		width<-width*72;
		height<-height*72;
	}
	dev.new(width=width,height=height);	
	par(bg=bg);	
},static=TRUE)



###########################################################################/**
#' @RdocMethod textBoundingBox
#' 
#' @title "Determine the size of the text bounding box"
#
#' \description{
#' Determine the size of the text bounding box
#' taking into account the text angle (srt).
#' }
#' 
#' \note{
#' The function should be called only on already created
#' graphical device, otherwise an error will be thrown.
#' }
#
#' @synopsis
#
#' \arguments{
#' @param stext The text which width and height to be determined.
#' @param units If 'user' (default) the aspect ratio is calculated based on 'usr' and 'pin'.
#' graphics parameters or kept equal to 1 otherwise.}
#' @param cex Text font size. Default is 1.
#' @param srt Text angle.Default is 0.
#' @param get.biggest If @TRUE (default), returns the biggest bounding box 
#' in case stest contains multiple strings.}
#' @param ... Not used
#' }
#
#' \value{
#' The @data.frame with two columns containing the width and height of the bounding box(es).
#' }
#
#' \examples{\dontrun{
#' bounding.box<-Display$textBoundingBox("text",cex=2,srt=30);
#' }}
#
#' @seealso{Display}
#' \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("textBoundingBox","Display",function(static,stext,units="user",cex=1,srt=0,get.biggest=TRUE,...) {
	w<-strwidth(stext,cex=cex,units=units);
	h<-strheight(stext,cex=cex,units=units);
	if(get.biggest){
		w<-max(w);
		h<-max(h);
	}

	usr<-par("usr");
	pin<-par("pin");

	if (units=="user"){
		dx<-usr[2]-usr[1];
		dy<-usr[4]-usr[3];
		asp=(dx*pin[2])/(dy*pin[1]);
	} else {
		asp=1;
	}
	
	res<-array(0,dim=c(length(w),2));
	colnames(res)<-c("width","height");

	angle<-srt/180*pi;

	res[,"width"]<-abs(w*cos(angle))+abs(h*sin(angle)*asp);
	res[,"height"]<-abs(w*sin(angle)/asp)+abs(h*cos(angle));

	return(as.data.frame(res,stringsAsFactors=FALSE));
},static=TRUE)



###########################################################################/**
#' @RdocMethod zoom.pars
#' 
#' @title "Zoom graphics parameters"
#
#' \description{@get "title".}
#' 
#' @synopsis
#
#' \arguments{
#' @param zoom Zoom factor.
#' @param par.names Graphics parameters to zoom.
#' @param ... Not used
#' }
#
#' \value{
#' Previous values of altered graphics parameters.
#' }
#
#' \examples{\dontrun{
#' Display$zoom.pars(zoom=3);
#' }}
#
#' @seealso{Display}
#' \author{Alex Lisovich, Roger Day}
#*/###########################################################################
 
setMethodS3("zoom.pars","Display",function(static,zoom,
	par.names=c("lwd","cex","cex.axis","cex.lab","cex.main","cex.sub","mai"),...){
	pars<-par(no.readonly=TRUE);
	params<-list();

	for (param in par.names){
		params[[param]]=pars[[param]]*zoom;

	}
	op<-par(params);
	return(op);
},static=TRUE)



###########################################################################/**
#' @RdocMethod copy
#' 
#' @title "Save current plot to the file"
#
#' \description{
#' @get "title".
#' }
#' 
#' @synopsis
#
#' \arguments{
#' @param filename The filename under which to save the plot.
#' @param zoom Copy zoom factor. Default is 1.
#' @param plotFun If NULL (default), copies the content of the current device to the file,
#' otherwise uses plotFun to plot into the file.}
#' @param plotArgs The @list of parameters for the poltFun. Default is an empty @list.
#' @param type The type of plot, PNG, JPEG, TIFF, or PDF. Default is PNG.
#' @param res DPI resolution. Default is 72dpi.
#' @param quality If type is JPEG, the 'quality' of the JPEG image, as a percentage. 
#' Smaller values will give more compression but also more degradation of the image.
#' Default is 100 (best quality).
#' @param compression If type is TIFF the type of compression to be used.
#' Could be 'none', 'rle', 'lzw', 'jpeg' or 'zip'. Default is 'none'.}
#' @param ... Not used
#
#' \value{
#' Name of the file the plot was saved to.
#' }
#
#' \examples{\dontrun{
#' plot(c(1:20),c(1:20));
#' Display$copy("plot.png");
#' }}
#
#' @seealso{Display}
#' \author{Alex Lisovich, Roger Day}
#*/###########################################################################
 
setMethodS3("copy","Display",function(static,filename,zoom=1,plotFun=NULL,plotArgs=list(),type=c("png","jpeg","jpg","tiff","bmp","pdf"),
		res=72,quality=100,compression=c("none", "rle", "lzw", "jpeg", "zip"),...){

	all_types<-c("png","jpeg","jpg","tiff","bmp","pdf");

	is.type.missing<-missing(type);

	type<-match.arg(type);
	fcomponents<-strsplit(filename,split=".",fixed=TRUE)[[1]];

	if(length(fcomponents)>1){
		extension<-fcomponents[length(fcomponents)];
		if (extension %in% all_types){
			if(is.type.missing){
				type<-extension;
			} else {
				filename<-paste(c(fcomponents[-length(fcomponents)],type),collapse=".");
			}	
		} else {
			filename<-paste(filename,type,sep=".");
		}
	} else {
		filename<-paste(filename,type,sep=".");
	}

	display.dev<-dev.cur();

	sz<-par("din")*zoom;

	devopt<-getOption("device");
	if (is.character(devopt) && devopt=="JavaGD"){
		sz<-sz*7/4;
	}

	switch(which(all_types == type),
		png(filename,width=sz[1],height=sz[2],units="in",res=res),
		jpeg(filename,width=sz[1],height=sz[2],units="in",res=res,quality=quality),
		jpeg(filename,width=sz[1],height=sz[2],units="in",res=res,quality=quality),
		tiff(filename,width=sz[1],height=sz[2],units="in",res=res,compression=match.arg(compression)),	
		bmp(filename,width=sz[1],height=sz[2],units="in",res=res),
		pdf(filename,width=sz[1],height=sz[2])
	);

	if (is.null(plotFun)){
		dev.set(display.dev);
		dev.copy(which=dev.next());
	} else {
		do.call(plotFun,plotArgs);
	}

	dev.off();
	gc();

	dev.set(display.dev);
	invisible(filename);
},static=TRUE)

