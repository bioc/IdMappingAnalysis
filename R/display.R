
###########################################################################/**
# @RdocClass Display
# \encoding{latin1}
#
# @title "The Display class"
#
# \description{
# @classhierarchy
# 
# Serves as a wrapper for a set of graphical functions 
# defined as static methods of Display class
# }
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("Display",function(){
	extend(Object(),"Display");
})



###########################################################################/**
# @RdocMethod line.loess
# 
# @title "Plot loess transformed data"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{The x coordinates of points to plot.}
# \item{y}{The y coordinates of points to be loess transformed for plotting.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{\dontrun{
# plot.new();
# plot.window(c(min(x),max(x)),c(min(y),max(y)));
# Display$line.loess(x,y);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("line.loess","Display",function(static,x, y, ...) {
	loess.out = loess(y ~ x)
	inds<-order(loess.out$x);
	lines(loess.out$x[inds], loess.out$fitted[inds], ...)
},static=TRUE)


###########################################################################/**
# @RdocMethod line.unsorted
# 
# @title "Draw a curve from unsorted points"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{x}{The (unsorted) x coordinates of points to plot.}
# \item{y}{The correponding y coordinates of points to plot.}
# \item{type}{Line type. Default is "l"."}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{\dontrun{
# plot.new();
# plot.window(c(min(x),max(x)),c(min(y),max(y)));
# Display$line.unsorted(x,y);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("line.unsorted","Display",function (static,x, y, type = "l", ...) {
	lines(sort(x),y[order(x)],type=type,...);
},static=TRUE)



###########################################################################/**
# @RdocMethod progressMsg
# 
# @title "Display a progress message"
#
# \description{
# Display a message accompanied by the percentage of completion.
# }
# 
# @synopsis
#
# \arguments{
# \item{msg}{Message to be dysplayed.}
# \item{i}{Counter of the progress.}
# \item{total}{The maximum value for a counter corresponding to 100 percent completion.}
# \item{...}{Not used}
# }
#
# \examples{\dontrun{
# for (i in 1:100){
#   Display$progressMsg("processing: ",i,100);
# # do something ...
# }
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
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
# @RdocMethod create
# 
# @title "Open a new display device"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{width}{Display device width in inches. Default is 7.}
# \item{height}{Display device height in inches. Default is 7.}
# \item{bg}{Device background. Default is 'transparent'.}
# \item{...}{Not used}
# }
#
# \examples{\dontrun{
# Display$create(width=10,height=7,bg="green");
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
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
# @RdocMethod textBoundingBox
# 
# @title "Determine the size of the text bounding box"
#
# \description{
# Determine the size of the text bounding box
# taking into account the text angle (srt).
# }
# 
# \note{
# The function should be called only on already created
# graphical device, otherwise an error will be thrown.
# }
#
# @synopsis
#
# \arguments{
# \item{stext}{The text which width and height to be determined.}
# \item{units}{If 'user' (default) the aspect ratio is calculated based on 'usr' and 'pin'.
# graphics parameters or kept equal to 1 otherwise.}
# \item{cex}{Text font size. Default is 1.}
# \item{srt}{Text angle.Default is 0.}
# \item{get.biggest}{If @TRUE (default), returns the biggest bounding box 
# in case stest contains multiple strings.}
# \item{...}{Not used}
# }
#
# \value{
# The @data.frame with two columns containing the width and height of the bounding box(es).
# }
#
# \examples{\dontrun{
# bounding.box<-Display$textBoundingBox("text",cex=2,srt=30);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
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
# @RdocMethod zoom.pars
# 
# @title "Zoom graphics parameters"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{zoom}{Zoom factor.}
# \item{par.names}{Graphics parameters to zoom.}
# \item{...}{Not used}
# }
#
# \value{
# Previous values of altered graphics parameters.
# }
#
# \examples{\dontrun{
# Display$zoom.pars(zoom=3);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
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
# @RdocMethod copy
# 
# @title "Save current plot to the file"
#
# \description{
# @get "title".
# }
# 
# @synopsis
#
# \arguments{
# \item{filename}{The filename under which to save the plot.}
# \item{zoom}{Copy zoom factor. Default is 1.}
# \item{plotFun}{If NULL (default), copies the content of the current device to the file,
# otherwise uses plotFun to plot into the file.}
# \item{plotArgs}{The @list of parameters for the poltFun. Default is an empty @list.}
# \item{type}{The type of plot, PNG, JPEG, TIFF, or PDF. Default is PNG.}
# \item{res}{DPI resolution. Default is 72dpi.}
# \item{quality}{If type is JPEG, the 'quality' of the JPEG image, as a percentage. 
# Smaller values will give more compression but also more degradation of the image.
# Default is 100 (best quality).}
# \item{compression}{If type is TIFF the type of compression to be used.
# Could be 'none', 'rle', 'lzw', 'jpeg' or 'zip'. Default is 'none'.}
# \item{...}{Not used}
# }
#
# \value{
# Name of the file the plot was saved to.
# }
#
# \examples{\dontrun{
# plot(c(1:20),c(1:20));
# Display$copy("plot.png");
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
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

