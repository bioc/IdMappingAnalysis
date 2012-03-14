


###########################################################################/**
# @set "class=JointUniquePairs"
# @RdocMethod interactive.plot
# 
# @title "General purpose JointUniquePairs interactive plot function"
#
# \description{
# This is an interactive wrapper for JointUniquePairs plot functions allowing to 
# subset the data on the UniquePairsMatch object and select customazable drawing parameters
# through the graphical selection dialog. 
# }
# 
# @synopsis
#
# \arguments{
# \item{data}{The data to be plotted.}
# \item{plotFun}{The plotting function.}
# \item{plotType}{Selection dialog caption}
# \item{toggle_keys}{The @list of @logical toggling custom keys controlling the drawing process. Once set, their value
# remains unchanged during subsequent drawing cycles ('loop' mode) until user toggles the key again.
# Internally, this list is automatically prepended by file.copy toggling key allowing to turn 
# saving current plot into the file on and off. Default is empty list.}
# \item{keys}{The @list of @logical custom keys controlling the drawing process. Unlike toggle_keys, their value should be set
# every time the drawing cycle repeats ('loop' mode). Internally, the list is automatically prepended by new.plot key 
# allowing to open a new graphic device for a plot. Default is empty list.}
# \item{idMapNames}{Either a @character vector of ID Map names, 'menu' or 'loop'. If ID Map names, the data for plotting are generated
# by subsetting the data on a set of ID pairs from the match corresponding to the given ID Map set. If 'menu' or 'loop',
# user can select ID Map set interactively from the graphical selection dialog simultaneously controlling plot destination 
# (same or new graphic device), wherther or not the plot should be saved in a file as well as additional control parameters defined
# by toggle_keys and keys. If 'loop', the interactive drawing cycle repeats untill the user cancells the process. Default is 'loop'.}
# \item{new.plot}{@logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is formed and used.
# In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{copy.zoom}{Zoom factor used when saving plot in a file. Default is 1.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters.}
# }
#
# \examples{\dontrun{
# #plot correlation densities choosing the DB set interactively
# examples$jointUniquePairs@interactive.plot(
#			examples$corr,examples$jointUniquePairs$corr.plot,"correlation plot",idMapNames="loop",
#			toggle_keys=list(plot.Union=TRUE,subsetting=TRUE),
#			keys=list(),
#			new.plot=FALSE,file.copy=FALSE,
#			lineColors=NULL,lineStyles=NULL,lineWidths=2,verbose=TRUE);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interactive.plot","JointUniquePairs",function(this,data,plotFun,plotType,toggle_keys=list(),keys=list(),
			idMapNames="loop",new.plot=FALSE,file.copy=FALSE,copy.zoom=1,verbose=TRUE,...){

	title<-paste(plotType,": choose DBs");
	space<-paste(rep(" ",1.8*nchar(title)),collapse="");

	if (idMapNames[1]=="menu")
		mode<-"menu"
	else if (idMapNames[1]=="loop")
		mode<-"loop"
	else
		mode<-"items";

	if(mode=="items"){
		mapNames<-idMapNames;
	} else {
		mapNames<-getMapNames(this);
	}


	toggle_keys<-c(list("Save Plot"=file.copy),toggle_keys);
	keys<-c(list("New plot"=new.plot),keys);

	repeat{
		toggle_key_selection<-NULL;
		for (toggle_key in names(toggle_keys)){
			decorated_key<-gsub("."," ",toggle_key,fixed=TRUE)
			key_name<-ifelse(toggle_keys[[toggle_key]],paste("Turn",decorated_key,"off"),paste("Turn",decorated_key,"on"))
			toggle_key_selection<-c(toggle_key_selection,key_name);
		}

		key_selection<-NULL;
		for (key in names(keys)){
			decorated_key<-gsub("."," ",key,fixed=TRUE)
			key_selection<-c(key_selection,decorated_key);
		}

		
		if(mode=="menu" || mode=="loop") {
			selection<-c(toggle_key_selection,key_selection,space,mapNames);
			items<-rselect.list(selection, multiple = TRUE,
			 	title = title);
			if(length(items)==0)
				return(invisible(NULL));

			for (i in 1:length(toggle_keys)){		
				toggle_keys[[i]]<-ifelse(toggle_key_selection[i] %in% items,!toggle_keys[[i]],toggle_keys[[i]]);
			}


			for(i in 1:length(keys)){
				keys[i]<-(key_selection[i] %in% items);
			}

			idMapNames<-items[!(items %in% c(toggle_key_selection,key_selection,space))];
		}
	

		if (length(idMapNames)>0){
			
			if(keys[[1]])
				Display$create();
				
			if("bg" %in% names(list(...)))
				par(bg=list(...)$bg);	

			do.call(plotFun,c(list(data,idMapNames,verbose=verbose),toggle_keys[-1],keys[-1],list(...)));

			if(toggle_keys[[1]]){
				if(is.logical(file.copy)){
					filename<-paste(paste(idMapNames,collapse="-"),gsub(" ",".",plotType,fixed=TRUE),sep=".");
					filename<-paste(filename,"png",sep=".");
				} else {
					filename<-file.copy;
				}
				Display$copy(filename,zoom=copy.zoom,plotFun=plotFun,
					plotArgs=c(list(data,idMapNames,par.zoom=copy.zoom,verbose=verbose),toggle_keys[-1],keys[-1],list(...)));
			}

		}

		
		if(mode!="loop"){break};
	}
})




###########################################################################/**
# @RdocMethod interactive.corr.plot
# 
# @title "Interactive plot of correlation densities"
#
# \description{
# Plots the density distributions for a set of correlation objects derived from JointUniquePairs and Corr objects
# with optional subsetting by a group of ID Maps. This is an interactive version of JointUniquePairs$corr.plot utilizing the 
# JointUniquePairs@interactive.plot wrapper functionality.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{groups}{Either ID Maps names, 'menu' or 'loop'. If ID Map names, the functionality is similar to UniquePairsMatch.corr.plot
# If 'menu' or 'loop', user can select the pair of ID Maps interactively from the graphical selection dialog simultaneously controlling
# the plot destination (same or new graphic device), wherther or not the plot should be saved in a file etc. If 'loop', the interactive
# drawing cycle repeats untill the user cancells the process.}
# \item{new.plot}{@logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical indicating if the plot should be saved in a file.
# In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{plot.Union}{If @TRUE (default), plots also the density of the correlation object corrsesponding
# to the union of a set of correlation objects. In interactive mode can be toggled graphically.}
# \item{subsetting}{If TRUE, subsets the  Corr on a group of ID Maps 
# or uses the original Corr otherwise.  In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{lineColors}{The @vector of line colors (recycled if necessary) 
# for plotting the distributions of different Corr objects.
# If @NULL (default), the predefined set of colors is used.}
# \item{lineStyles}{The @vector of line styles (recycled if necessary) 
# for plotting the distributions of different Corr objects.
# If @NULL (default), the predefined set of line styles is used.}
# \item{lineWidths}{The vector of line widths (recycled if necessary) 
# for plotting the distributions of different Corr objects. Default is 2.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters.}
# }
#
# \examples{
# #plot correlation densities for a given DB set
# #treating the given DB set as a full group (subsetting=TRUE)
# examples$jointUniquePairs$interactive.corr.plot(examples$corr,
#		groups=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
#		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,verbose=TRUE,
#		cex.lab=1.4,cex.legend=1.1);
# \dontrun{
# #plot correlation densities choosing the DB set interactively
# examples$jointUniquePairs$interactive.corr.plot(examples$corr,
#		copy.zoom=2,verbose=TRUE,cex.lab=1.4,cex.legend=1.1);
# }}
# 
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interactive.corr.plot","JointUniquePairs",function(this,corr,groups="loop",
	new.plot=FALSE,file.copy=FALSE,plot.Union=TRUE,subsetting=FALSE,lineColors=NULL,lineStyles=NULL,lineWidths=2,verbose=TRUE,...){

      keys<-list();
	toggle_keys<-list(plot.Union=plot.Union,subsetting=subsetting);

	interactive.plot(this,corr,this$corr.plot,"correlation plot",
			idMapNames=groups,toggle_keys=toggle_keys,keys=keys,new.plot=new.plot,file.copy=file.copy,
			lineColors=lineColors,lineStyles=lineStyles,lineWidths=lineWidths,verbose=verbose,...);
})



###########################################################################/**
# @RdocMethod interactive.mixture.plot
# 
# @title "Interactive plot of mixture model components"
#
# \description{
# Plots the correlation densities of the empirical fit, mixture fit and each of the mixture components
# for a mixture model derived from  JointUniquePairs and Corr objects with optional subsetting
# by a group of ID Maps. This is an interactive version of JointUniquePairs$mixture.plot unitilizing the 
# JointUniquePairs$interactive.plot wrapper functionality.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{groups}{either ID Map  names, 'menu' or 'loop'. If ID Map names, the functionality is similar to JointUniquePairs$mixture.plot
# If 'menu' or 'loop', user can select the pair of ID Maps interactively from the graphical selection dialog simultaneously controlling
# the plot destination (same or new graphic device) wherther or not the plot should be saved in a file etc. If 'loop', the interactive
# drawing cycle repeats untill the user cancells the process.}
# \item{new.plot}{@logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is formed and used.
# In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{subsetting}{If @TRUE, subsets the  Corr on a group of ID Maps to compute the mixture model 
# or uses the original Corr otherwise.  In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{G}{The number of mixture model components. If a numerical vector, the optimal mixture model is computed. Default is c(1:5).}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# #plot mixture densities for a given DB set
# #treating the given DB set as a full group (subsetting=TRUE)
# examples$jointUniquePairs$interactive.mixture.plot(examples$corr,
#		groups=c("NetAffx_Q","DAVID_Q","EnVision_Q"),
#		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,G=c(1:5),
#		cex.lab=1.4,cex.legend=1.2,verbose=TRUE);
#
# \dontrun{
# #plot mixture densities choosing the DB set interactively
# examples$jointUniquePairs$interactive.mixture.plot(examples$corr,
#		G=c(1:5),verbose=TRUE,cex.lab=1.4,cex.legend=1.1);
# }}
# 
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interactive.mixture.plot","JointUniquePairs",function(this,corr,groups="loop",
		new.plot=FALSE,file.copy=FALSE,subsetting=FALSE,G=c(1:5),verbose=TRUE,...){

      keys<-list();
	toggle_keys<-list(subsetting=subsetting);

	interactive.plot(this,corr,this$mixture.plot,"mixture plot",
			idMapNames=groups,toggle_keys=toggle_keys,keys=keys,new.plot=new.plot,file.copy=file.copy,
			G=G,verbose=verbose,...);

})




###########################################################################/**
# @RdocMethod interactive.corr.boxplot
# 
# @title "Interactive boxplot of correlations by match group"
#
# \description{
# Creates a boxplot of correlations derived from  JointUniquePairs and Corr objects with optional subsetting
# by a group of ID Maps.  This is an interactive version of JointUniquePairs@corr.boxplot unitilizing the 
# JointUniquePairs@interactive.plot wrapper functionality.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{groups}{Either ID Map  names, 'menu' or 'loop'. If ID Map names, the functionality is similar to JointUniquePairs$corr.boxplot
# If 'menu' or 'loop', user can select the ID Map set interactively from the graphical selection dialog simultaneously controlling
# the plot destination (same or new graphic device) wherther or not the plot should be saved in a file etc. If 'loop', the interactive
# drawing cycle repeats untill the user cancells the process.}
# \item{show.None}{If @TRUE, includes a plot of subset of values not included into any other match group 
# with 'None' label. Default is @FALSE.}
# \item{new.plot}{@logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is formed and used.
# In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{subsetting}{If @TRUE, interprets Corr subset as a full group
# or uses the original Corr as a full group otherwise.
# In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{srt}{Match group label orientation. The default is 0.}
# \item{adj}{@numeric in a range (0..1) determening how close to the x-axis the group labels are,
# 1 being the closest and 0 being farthest. The default is 0.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# #boxplot of correlations by match group for a given DB set (using short names)
# #treating the given DB set as a full group (subsetting=TRUE)
#  DBs=list("NetAffx_Q"="AffQ","DAVID_Q"="DQ","EnVision_Q"="EnV");
#
# examples$jointUniquePairs$interactive.corr.boxplot(examples$corr,
#		groups=DBs,
#		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,
#		srt=30,adj=0,cex=1,cex.lab=1.4,cex.axis=1.2,cex.main=1.4,verbose=TRUE);
#
# \dontrun{
# #boxplot of correlations by match group choosing the DB set interactively
# examples$jointUniquePairs$interactive.corr.boxplot(examples$corr,multiline=TRUE,
#		srt=90,adj=0,cex=1,cex.lab=1,cex.axis=1,cex.main=1.2,verbose=TRUE);
#
# }}
# 
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interactive.corr.boxplot","JointUniquePairs",function(this,corr,groups="loop",show.None=FALSE,
					new.plot=FALSE,file.copy=FALSE,subsetting=FALSE,verbose=TRUE,srt=0,adj=0,...){

      keys<-list(show.None=show.None);
	toggle_keys<-list(subsetting=subsetting);

	interactive.plot(this,corr,
			this$corr.boxplot,"correlation boxplot",
			idMapNames=groups,toggle_keys,keys,new.plot=new.plot,file.copy=file.copy,
			verbose=verbose,srt=srt,adj=adj,...);

})



###########################################################################/**
# @RdocMethod interactive.mixture.boxplot
# 
# @title "Interactive boxplot of mixture component probabilities by match group"
#
# \description{
# Creates a boxplot for a mixture model derived from  JointUniquePairs and Corr objects
# with optional subsetting by a group of ID Maps. This is an interactive version of JointUniquePairs@corr.boxplot
# utilizing the JointUniquePairs$interactive.plot wrapper functionality.
# }
# 
# @synopsis
#
# \arguments{
# \item{corr}{Corr object.}
# \item{groups}{Either ID Map  names, 'menu' or 'loop'. If ID Map names, the functionality is similar to JointPairsMatch$mixture.boxplot
# If 'menu' or 'loop', user can select the ID Map set interactively from the graphical selection dialog simultaneously controlling
# the plot destination (same or new graphic device) wherther or not the plot should be saved in a file etc. If 'loop', the interactive
# drawing cycle repeats untill the user cancells the process.}
# \item{G}{Number of components in mixture model. If G is a @vector, the optimal
# number of component is determined. G is a @vector C(1:5) by default.}
# \item{plot.G}{The mixture component to be used for plotting. If NULL, the highest-order component of the mixture model is used.}
# \item{show.None}{If not @NA, includes a plot of subset of values not included into any other match group 
# with 'None' label. Default is @NA.}
# \item{new.plot}{@logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is formed and used.
# In interactive mode can be toggled graphically. Default is @FALSE}.
# \item{subsetting}{If @TRUE, interprets Corr subset as a full group
# or uses the original Corr as a full group otherwise. Default is @FALSE.}
# \item{srt}{Match group label orientation. The default is 0.}
# \item{adj}{@numeric in a range (0..1) determening how close to the x-axis the group labels are,
# 1 being the closest and 0 being farthest. The default is 0.}
# \item{verbose}{If @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# #boxplot of correlations by match group for a given DB set (using short names)
# #treating the given DB set as a full group (subsetting=TRUE)
#  DBs=list("NetAffx_Q"="AffQ","DAVID_Q"="DQ","EnVision_Q"="EnV");
#
# examples$jointUniquePairs$interactive.mixture.boxplot(examples$corr,
#		groups=DBs,
#		subsetting=TRUE,new.plot=TRUE,file.copy=FALSE,G=c(1:5),plot.G=2,
#		srt=30,adj=0,cex=1,cex.lab=1.2,cex.axis=1.2,cex.main=1.2,verbose=TRUE);
#
# \dontrun{
# #boxplot of correlations by match group choosing the DB set interactively
# examples$jointUniquePairs$interactive.mixture.boxplot(examples$corr,multiline=TRUE,
#		G=c(1:5),plot.G=2,
#		srt=45,adj=0,cex=1,cex.lab=1,cex.axis=1,cex.main=1.2,verbose=TRUE);
# }}
# 
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interactive.mixture.boxplot","JointUniquePairs",function(this,corr,groups="loop",G=c(1:5),plot.G=NULL,show.None=FALSE,
					new.plot=FALSE,file.copy=FALSE,subsetting=FALSE,verbose=TRUE,srt=0,adj=0,...){

      keys<-list(show.None=show.None);
	toggle_keys<-list(subsetting=subsetting);

	interactive.plot(this,corr,
			this$mixture.boxplot,"mixture boxplot",
			idMapNames=groups,toggle_keys,keys,new.plot=new.plot,file.copy=file.copy,
			verbose=verbose,G=G,plot.G=plot.G,srt=srt,adj=adj,...);

})

