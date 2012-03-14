

###########################################################################/**
# @RdocClass CorrData
# \encoding{latin1}
#
# @title "CorrData class"
#
# \description{
#  @classhierarchy
#
# CorrData object stores the pair of experiments on which the correlation related processing is performed
# (MS/MS and mRNA for example) in such a way that two experiments are aligned by experiment names 
# and by the primary keys ensuring the fast correlations. 
# Typically, the primary ID of the ID Map set under consideration is a primary key for a first experiment,
# and the secondary ID if the ID Map set is a primary key for a second experiment. 
# The alignment of two experiments by primary keys is guaranteed by using the unique pairs object
# to produce a matching pair of primary keys on which both experiments are ordered. 
# Represented by a list of two elements with names corresponding to the primary and secondary IDs 
# of the unique pairs ('acc' and 'probeset' for example), each element containing a data frame with primary
# or secondary IDs in the first column while the rest of columns contain the experiment data.
# The names of the data columns in both data frames are identical and correspond to the sample IDs. 
# The match of sample IDs and an alignment by primary/secondary IDs is ensured by the proper processing 
# during the object creation.
# }
# 
# @synopsis
#
# \arguments{
# \item{uniquePairs}{ UniquePairs object or a list of such objects
# on which a single or a list of CorrData objects is constructed.}
# \item{expSet1}{ a first ExperimentSet object with primary IDs corresponding (partially intersecting)
# with the content the first column of UniquePairs (uniquePairsData) object.}
# \item{expSet2}{ a second ExperimentSet object with primary IDs corresponding (partially intersecting)
# with the content the second column of UniquePairs (uniquePairsData) object.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \examples{
# corrData<-CorrData(examples$uniquePairs,
#		examples$msmsExperimentSet,examples$mrnaExperimentSet,verbose=TRUE);
# class(corrData);
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setConstructorS3("CorrData",function(uniquePairs=NULL,expSet1=NULL,expSet2=NULL,verbose=FALSE,...){
	if(is.null(uniquePairs) || is.null(expSet1) || is.null(expSet2)){
		extend(Object(),"CorrData",.data=NULL);
	} else {

		#coerce the input experiments into the data frames
		expSet1<-IdMapBase$as.data.frame(expSet1);
		expSet2<-IdMapBase$as.data.frame(expSet2);

		#check exp. set format correctness
		if((length(unique(expSet1[,1]))!=nrow(expSet1)) ||
			(length(unique(expSet2[,1]))!=nrow(expSet2))){
			warning("CorrData.create: non-unique experiment first column");
			return(NULL);
		}

		#set row names
		rownames(expSet1)<-expSet1[,1];
		rownames(expSet2)<-expSet2[,1];
			
		uniquePairs<-as.data.frame(uniquePairs);

		#synchronize unique pairs with both experimant primary IDs sets
		uniquePairs<-Subset$byColumn(uniquePairs,IdMapBase$primaryIDs(expSet1),column=IdMapBase$primaryKey(expSet1),verbose=verbose);
		uniquePairs<-Subset$byColumn(uniquePairs,IdMapBase$primaryIDs(expSet2),column=IdMapBase$primaryKey(expSet2),verbose=verbose);


	 	#restrict both experiment sets primary IDs to unique pairs only 
		expSet1<-Subset$byRowNames(expSet1,uniquePairs[,IdMapBase$primaryKey(expSet1)],verbose=verbose);
		expSet2<-Subset$byRowNames(expSet2,uniquePairs[,IdMapBase$primaryKey(expSet2)],verbose=verbose);

		#bring both experiments in synch by columns: same experiments and same experiment order (order is by first expSet1)
		expSet1<-Subset$byColNames(expSet1,colnames(expSet2),orderBySubset=FALSE,verbose=verbose);
		expSet2<-Subset$byColNames(expSet2,colnames(expSet1),verbose=verbose);

		#get data  for correlation
		analysisStructure<-CorrData$pack.experiments(list(expSet1,expSet2),uniquePairs,verbose=verbose);

		extend(Object(),"CorrData",
			.data=analysisStructure
		)
	}

})



###########################################################################/**
# @RdocMethod pack.experiments
# 
# @title "Convert aligned experiment data into the data structure incapsulated within the CorrData object"
#
# \description{@get "title".
# }
# 
# @synopsis
#
# \arguments{
# \item{experimentSetList}{ list of aligned ExperimentSet to be converted.}
# \item{uniqueTuples}{ set of unique pairs (triplets ets) each column of which corresponds to the primary key
# of the corresponding ExperimentSet object in the list.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# a @list of two or more elements with names corresponding to the primary IDs for each 
# experiment modality where each element contains a data frame with primary
# IDs of a given modality in the first column while the rest of columns contain the experiment data.
# The names of the data columns in both data frames are identical and correspond to the sample IDs.
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("pack.experiments","CorrData",function(static,experimentSetList,uniqueTuples,verbose=FALSE,...){

	if (verbose)
		cat("Creating data structure for correlations...\n");	
	res<-list();
	for (i in 1:ncol(uniqueTuples)) {
		res[[colnames(uniqueTuples)[i]]]<-experimentSetList[[i]][uniqueTuples[,i],];
		res[[colnames(uniqueTuples)[i]]][,1]<-as.character(res[[colnames(uniqueTuples)[i]]][,1]);
	}

	return(res);
},static=TRUE,protected=TRUE)

###########################################################################/**
# @RdocMethod primaryKey
# 
# @title "Retrieves a primary key for a given CorrData object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{A @character string representing primary key for given CorrData object}
#
# \examples{
# examples$corrData$primaryKey();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("primaryKey","CorrData",function(this,...){
	return(names(this$.data)[[1]]);
})


###########################################################################/**
# @RdocMethod secondaryKey
# 
# @title "Retrieves a secondary key for a given CorrData object"
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{\item{...}{Not used}}
# \value{A @character string representing secondary key for given CorrData object}
#
# \examples{
# examples$corrData$secondaryKey();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("secondaryKey","CorrData",function(this,...){
	return(names(this$.data)[[2]]);
})



###########################################################################/**
# @RdocMethod getUniquePairs
# 
# @title "Extract unique pairs from the CorrData object"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{UniquePairs object}
#
# \examples{
# uniquePairs<-examples$corrData$getUniquePairs();
# uniquePairs[1:10,];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getUniquePairs","CorrData",function(this,verbose=FALSE,...){
	corrData<-this$.data;
	pairs<-cbind(corrData[[1]][,1],corrData[[2]][,1]);
	colnames(pairs)<-names(corrData);
	uniquePairs<-UniquePairs(as.data.frame(pairs,stringsAsFactors=FALSE),secondaryKey=colnames(pairs)[2]);
	return(uniquePairs);
})



###########################################################################/**
# @RdocMethod getSampleNames
# 
# @title "Get experiment sample names"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used}
# }
#
# \value{
# @character vector of sample names encapsulated within the given object.
# }
#
# \examples{
# examples$corrData$getSampleNames();
# }
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getSampleNames","CorrData",function(this,...){
	return(colnames(this$.data[[1]])[-1]);
})



###########################################################################/**
# @RdocMethod getExperimentSet
# 
# @title "Get experiment set data frame for a given modality"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{modality}{Experiment modality for which data to be retrieved, i.e. 'Uniprot', 'Affy' etc.}
# \item{as}{@character string defining in what format to the experiment data should be returned.
# The possible values are either 'data.frame' (default) or 'ExpressionSet'.}
# \item{IDs}{Optional character vector of ID subset for which data to be retrieved,
# or @NULL (default) in which case data for all IDs returned.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \value{
# Depending on 'as' argument, either EspressionSet or @data.frame
# containing experiment data for a given modality.
# }
#
# \examples{
# #retrieve msms ('Uniprot') experiment data as a data frame
# expSetDF<-examples$corrData$getExperimentSet(modality="Uniprot",as="data.frame");
# expSetDF[1:10,1:5];
#
# #retrieve mrns ('Affy') experiment data as an ExpressionSet
# expSet<-examples$corrData$getExperimentSet(modality="Affy",as="ExpressionSet");
# print(expSet);
# expSetDF<-t(as(expSet,"data.frame"));
# expSetDF[1:10,1:5];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("getExperimentSet","CorrData",function(this,modality,as=c("data.frame","ExpressionSet"),IDs=NULL,verbose=FALSE,...){

	if (!(modality %in% names(this$.data))){
		throw("CorrData$getExperimentSet: invalid modality");
	}

	as<-match.arg(as);

	if(is.null(IDs))
		data<-this$.data[[modality]]
	else
		data<-Subset$byRowNames(this$.data[[modality]],IDs,orderBySubset=TRUE,na.value=NA,verbose=verbose);

	if(as=="ExpressionSet"){
		exprs<-as.matrix(data[,-1]);
		rownames(exprs)<-data[,1];
		return(new ("ExpressionSet",exprs=as.matrix(data[,-1]),annotation=modality));
	} else {
		return(data);
	}
})


###########################################################################/**
# @RdocMethod as.MultiSet
# 
# @title "Convert CorrData object into MultiSet object"
#
# \description{
# @get "title".# }
# 
# @synopsis
#
# \arguments{
# \item{...}{Not used}
# }
#
# \value{
# MultiSet object which assayData contain two matrices corresponding to the transposed data frames
# (with the mapping identifier column stripped out) encapsulated within the given CorrData object.
# Due to the nature of the MultiSet object, the sample names of the resulting MultiSet object should
# be accessed using featureNames() method, and the set of experiment identifiers should be accesed 
# through the call to colnames on the list returned by the assayData() method.
# }
#
# \examples{
# library(Biobase);
# #convert CorrData object to MultiSet
# mset<-as.MultiSet(examples$corrData);
# #get sample names
# samples<-featureNames(mset);
# print(samples);
# #retrieve primary and secondary keys
# annotation(mset);
# #retreive primary and secondary IDs
# uniprotIDs<-colnames(assayData(mset)[[1]]);
# affyIDs<-colnames(assayData(mset)[[2]]);
# uniprotIDs[1:20];
# affyIDs[1:20];
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("as.MultiSet","CorrData",function(this,...){
	data<-list();

	dat1<-as.matrix(this$.data[[1]][,-1]);
	dat2<-as.matrix(this$.data[[2]][,-1]);

	data[[primaryKey(this)]]<-t(dat1);
	data[[secondaryKey(this)]]<-t(dat2);

	res<-new("MultiSet",data,annotation=names(this$.data));

	return(res);
})

#' @export plot.CorrData
###########################################################################/**
# @RdocMethod plot
# 
# @title "Scatterplot of experiment data"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{input}{ character vector of primary IDs, or either vector or list of match pairs.}
# \item{outcomePairs}{ The pairs <sample ID, outcome> or @NULL (default). In the first case the scatterplot
# points are plotted with symbol corresponding to the first letter of the outcome keyword.
# In the second, if there are more than one pair is plotted the point set for each pair is marked as 1, 2, etc.
# and if there is only one pair is present the unfilled circles are used}
# \item{xlab}{ The X axis label. Default is 'protein count'.}
# \item{ylab}{ The Y axis label. Default is 'mRNA expression'.}
# \item{method}{ the method used to compute the correlation coefficient between X and Y data. Default is "spearman".}
# \item{proteinNames}{ extra comments in the plot main title. Default is @NULL (no extra comments).}
# \item{cols}{ the (recycled) vector of colors to plot each data series with for the particular match pair.
#  Default is RColorBrewer::brewer.pal(9,"Set1").}
# \item{cex}{ Plot font size. Default is 1.}
# \item{cex.main}{ Main title font size. Default is 1.2.}
# \item{cex.lab}{ X and Y titles font size. Default is 1.}
# \item{cex.axis}{ X and Y axis labels font size. Default is 1.}
# \item{font}{ data points  and axis labels font. Default is 2.}
# \item{font.main}{ main title font type. Default is 3.}
# \item{par.zoom}{ graphics parameters zoom factor. Scales the graphical parameters 
# like cex, lwd, mai etc. Default is 1.}
# \item{...}{Additional graphical parameters}
# }
#
# \examples{
# #scatterplot with outcome for Uniprot="P07355" (annexin 2), probe set ID="213503_x_at"
# examples$corrData$plot(input=list(c("P07355", "213503_x_at")), 
#	xlab="spectral count",
#	outcomePairs=examples$outcomeMap, proteinNames="ANXA2", 
#	cols=c("green", "red", "darkblue"));
# } 
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("plot","CorrData",function(x,input,outcomePairs=NULL,xlab="protein count",ylab="mRNA expression (log10)",
			method="spearman",proteinNames=NULL,cols=brewer.pal(9,"Set1"),
			cex=1,cex.main=1.2,cex.lab=1,cex.axis=1,font=1,font.main=3,par.zoom=1,...){

	corrData=x$.data;

	op2<-Display$zoom.pars(par.zoom);

  	model_pairs<-getUniquePairs(x);

	if(inherits(input,"data.frame")){# input are pairs
		matches<-getMatch(model_pairs,input);
	}else if (inherits(input,"list")){# input is a list of pairs
		input<-as.data.frame(input,stringsAsFactors=FALSE);
		input<-as.data.frame(t(input),stringsAsFactors=FALSE);
		matches<-getMatch(model_pairs,input);
	} else { # input are primary IDs
		matches<-(corrData[[1]][,1] %in% input);
	}


	sampleNames<-getSampleNames(x);

	if(is.null(outcomePairs)){
		outcomePairs<-data.frame(sampleNames,1,stringsAsFactors=FALSE);
		outcomes<-1;
  	}else{
		freqs<-sort(table(outcomePairs[,2]),decreasing=TRUE);
		outcomes<-names(freqs);
	}
  
 
      if(sum(matches)>0) {
		selected_pairs<-model_pairs[matches,];
	
		X<-as.numeric(as.matrix(corrData[[1]][matches,-1]));
		Y<-as.numeric(as.matrix(corrData[[2]][matches,-1]));

		xlim=range(X);
		ylim=range(Y);

	
		corrXY<-cor(X,Y,use="na.or.complete",method=method);

	
		if("bg" %in% names(list(...)))
		par(bg=list(...)$bg);	

		cnt<-0;
		groups<-which(matches==TRUE);

		line2<-NULL;
		for (i in 1:nrow(selected_pairs)){
			pair<-paste(selected_pairs[i,],collapse=" - ");
			if (!is.null(line2))
				line2<-paste(line2,pair,sep=", ")
			else
				line2<-pair;
		}

		line3<-paste("correlation(",method,")= ",round(corrXY,digits=3),sep="");
		main<-paste(line2,line3,sep="\n");
		if (!is.null(proteinNames))
			main<-paste(paste(proteinNames,collapse=", "),main,sep="\n");

		txtbox<-Display$textBoundingBox(main,cex=cex.main,units="inches");
		mai<-par("mai");
		mai[3]<-max(mai[3],txtbox$height+0.25*par.zoom);
		op<-par(mai=mai);



		for (outcome in outcomes){
			outcome_cols<-outcomePairs[outcomePairs[,2] == outcome,1];
			outcome_cols<-intersect(outcome_cols,colnames(corrData[[1]]));

			for (i in groups){
				if(is.numeric(outcome) && length(groups)>1)
					pch<-as.character(cnt+1)
				else
					pch<-outcome;

				X<-as.numeric(as.matrix(corrData[[1]][i,outcome_cols]));
				Y<-as.numeric(as.matrix(corrData[[2]][i,outcome_cols]));

				col<-cols[(cnt %% length(cols))+1];
				if(cnt==0){
					plot(X,Y, xlim=xlim,ylim=ylim,xlab="",ylab="",type="n",axes=FALSE,cex=0.1,...)
					box();
					axis(1,lwd=par("lwd"),cex.axis=cex.axis,...);
					axis(2,lwd=par("lwd"),cex.axis=cex.axis,...);
				}

				if(nchar(pch)>1){
					pchs<-unlist(strsplit(pch,split=""));
					txt=Display$textBoundingBox(pchs[1],cex=cex);
					nusym<-length(pchs);
					right<-as.integer(nusym/2)*0.5*txt$width;
					delts<-seq(from=-right,to=right,length.out=nusym);
					for(i in 1:nusym)
						points(X+delts[i],Y,pch=pchs[i],col=col,font=font,cex=cex)
				}else{
					points(X,Y,pch=pch,col=col,font=font,cex=cex)
				}
					
				cnt<-cnt+1;		

			} #for (i in groups){
		}#for (outcome in outcomes)


		title(main=main,xlab=xlab,ylab=ylab,cex.main=cex.main,font.main=font.main,cex.lab=cex.lab,...);

   	}
		
	par(op2);	
   	invisible();

},createGeneric=FALSE)


###########################################################################/**
# @RdocMethod interactive.plot
# 
# @title "Draw a scatterplot of experiment data interactively"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{input}{ character vector of primary IDs, vector of match pairs, "menu" or "loop". 
# In case of "menu" or "loop" the set if primary IDs is determined 
# interactively using graphical selection menu, and in case of "loop" the process of plotting repeates until user cancels it. 
# If data series for more than one match pair to be displayed (in case input is a vector of primary IDs for example), the set of match pairs
# also selected intactively. Default is "loop".}
# \item{new.plot}{ If @TRUE, then new graphic device is opened for plotting. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is used. Default is @FALSE.}
# \item{copy.zoom}{ Zoom factor used when saving plot in a file. Default is 1.}
# \item{...}{Additional graphical parameters including ones from plot.CorrData}
# }
#
# \examples{
# #non-interactive scatterplot plot with a single primary and secondary IDs pair and outcomes
# examples$corrData$interactive.plot(input=list(c("P07355","1568126_at")),
# 		outcomePairs=examples$outcomeMap,proteinNames="ANXA2",
# 		cols=c("green","red","darkblue"),cex=1.2,cex.main=1.2,font.main=4,cex.lab=1.2);
# 
# \dontrun{
# #interactive scatterplot with a single primary ID (uniprot) and outcomes
# examples$corrData$interactive.plot(c("P07355"), outcomePairs=examples$outcomeMap, proteinNames="ANXA2");
#
# #interactive scatterplot with multiple probeset IDs (uniprot) and without outcomes -  ANXA2 (annexin 2)
# examples$corrData$interactive.plot(c("P07355", "P07384", "P09382"));
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interactive.plot","CorrData",function(this,input="loop",new.plot=FALSE,file.copy=FALSE,copy.zoom=1,...){

  corrData<-this$.data;

  if (input[1]=="menu")
	mode<-"menu"
  else if (input[1]=="loop")
	mode<-"loop"
  else{
	mode<-"items";
  }

  filename<-NULL;
  model_pairs<-getUniquePairs(this);

  if (mode=="items"){
	if(inherits(input,"data.frame")){# input are pairs
		matches<-getMatch(model_pairs,input);
	}else if (inherits(input,"list")){# input is a list of pairs
		input<-as.data.frame(input,stringsAsFactors=FALSE);
		input<-as.data.frame(t(input),stringsAsFactors=FALSE);
		matches<-getMatch(model_pairs,input);
	} else { # input are primary IDs
		matches<-(corrData[[1]][,1] %in% input);
	}
	IDs<-sort(unique(model_pairs[matches,1]));
  } else {
	IDs<-sort(unique(model_pairs[,1]));
  }

  if (length(IDs)>1)
	mode="loop";

  sampleNames<-getSampleNames(this);

  can.select.IDs<-ifelse(length(IDs)>1,TRUE,FALSE);
  select.IDs<-can.select.IDs;
	
  repeat {
  	file.copy.name=ifelse(file.copy,"Turn Save Plot off","Turn Save Plot on");

	if(select.IDs && can.select.IDs && (mode=="menu" || mode=="loop")){
		input<-rselect.list(IDs,multiple=TRUE,title="Select IDs:");

		if (length(input)==0)
			return(invisible(filename));

		input<-input[!(input %in% c(" ","New plot",file.copy.name))];
	}


	if(inherits(input,"data.frame")){# input are pairs
		matches<-getMatch(model_pairs,input);
	} else { # input are primary IDs
		matches<-(corrData[[1]][,1] %in% input);
	}
	
	if (sum(matches)>1){
		mode="loop";
		if (can.select.IDs)
			options<-c(file.copy.name,"New plot","Select another ID"," ")
		else
			options<-c(file.copy.name,"New plot"," ");

		items<-rselect.list(c(options,paste(model_pairs[matches,1],model_pairs[matches,2])),
				multiple=TRUE,title="Select pairs:");

		if(length(items)==0)
			return(invisible(NULL));

		file.copy<-ifelse(file.copy.name %in% items,!file.copy,file.copy);
		new.plot<-("New plot" %in% items);
		select.IDs<-("Select another ID" %in% items);
		items<-items[!(items %in% c(" ","New plot","Select another ID",file.copy.name))];
		matches<-(paste(model_pairs[,1],model_pairs[,2]) %in% items);

	} else {
		select.IDs=TRUE;
	}

	if (sum(matches)>0){
		if(new.plot)
			Display$create();

		selected_pairs<-model_pairs[matches,];

		plot(this,selected_pairs,...);

		if (is.character(file.copy) || file.copy){
			if(is.logical(file.copy)){
				filename<-paste(selected_pairs[,1],selected_pairs[,2],sep="_",collapse="-");
				filename<-paste(filename,"scatterplot.png",sep=".");
			} else {
				file.name<-file.copy;
			}
			Display$copy(filename,zoom=copy.zoom,plotFun=this$plot,
				plotArgs=c(list(selected_pairs,par.zoom=copy.zoom),list(...)));
		}
	}


	if(mode!="loop" && sum(matches)<2){break};

   } # repeat
			
   invisible(filename);
})


