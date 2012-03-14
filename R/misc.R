###########################################################################/**
# @RdocClass Misc
# \encoding{latin1}
#
# @title "The Misc class"
#
# \description{
# @classhierarchy
# 
# Serves as a wrapper for various miscalleneous functions 
# used throughout the package defined as static methods of the Misc class.
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


setConstructorS3("Misc",function(){
	extend(Object(),"Misc");
})



###########################################################################/**
# @RdocMethod words
# 
# @title "Convert space delimited string to a vector of words"
#
# \description{
# @get "title".
# }
# 
# @synopsis
#
# \arguments{
# \item{string}{Space delimited character string.}
# \item{...}{Not used}
# }
#
# \value{
# Character vector of words extracted from space delimited string
# }
#
# \examples{
# IDs<-Misc$words("1007_s_at 207169_x_at 208779_x_at 210749_x_at");
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("words","Misc",function(static,string,...){
	res<-unlist(strsplit(string,split=" ",fixed=TRUE));
	res<-res[res!=""];
	return(res);
},static=TRUE)



###########################################################################/**
# @RdocMethod to.base
# 
# @title "Convert number to a numeric vector of a given base"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{n}{Number to convert.}
# \item{base}{Base to use for a conversion. Default is 2.}
# \item{r}{Order of the output nnumbers If r=TRUE then big-endian 
# otherwise small-endian. Default is @TRUE.}
# \item{...}{Not used}
# }
#
# \value{
# @numeric vector of a given base.
# }
#
# \examples{
# b<-Misc$to.base(25);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("to.base","Misc",function(static,n,base=2,r=TRUE,...)
{
   out <- NULL;
   while(n > 0) {
     if(r) {
       out <- c(out , n%%base)
     } else {
       out <- c(n%%base , out)
     }
     n <- n %/% base
   }
   if (is.null(out))
	out<-0;
   return(out)
},static=TRUE)



###########################################################################/**
# @RdocMethod to.index.expr
# 
# @title "Convert expression into index expression for a given list or data frame object"
#
# \description{
# @get "title".
# Uses the input expression to generate the corresponding expression on a given
# list using the expression variables to address the list items, 
# i.e. for an object X and expression 'a+b' generate the expression 'X$a + X$b'
# }
# 
# @synopsis
#
# \arguments{
# \item{obj}{Input @list or 2data.frame -derived object on which the expression to be constructed.}
# \item{expr}{@expression or @character string from which the indexed 
# expression on the input object to be constructed.}
# \item{...}{Not used}
# }
#
# \value{
# Indexed @expression on a given @list or @data.frame object.
# }
#
# \examples{\dontrun{
# indexed.expression<-to.index.expr(G,"a | b&c");
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("to.index.expr","Misc",function(static,obj,expr,...){

	getTerm<-function(obj_name,term){
		term<-paste(obj_name,"$",term,sep="");
		return(term);
	}

	op_regex<-"[-!|\\+\\*\\(\\)\\&]";
	
	obj_name<-deparse(substitute(obj));
	if(is.character(expr))
		expr<-parse(text=expr,srcfile=NULL);

	sexpr<-as.character(expr);
	sexpr<-gsub(" ","",sexpr,fixed=TRUE);

	terms<-strsplit(sexpr,split=op_regex)[[1]];
	terms<-terms[terms!=""];

	symbols<-strsplit(sexpr,split="")[[1]];
	ops_pos<-gregexpr(op_regex,sexpr)[[1]];

	res<-NULL;
	term_i<-0;
	prev_pos<-0;

	for (pos in ops_pos){
		if((pos-prev_pos)>1){
			term_i<-term_i+1;
			term<-getTerm(obj_name,terms[term_i]);
			res<-c(res,term);
		} 
		res<-c(res,symbols[pos]);
		prev_pos<-pos;
	}

	if(tail(ops_pos,n=1L)!=nchar(sexpr)){
		term<-getTerm(obj_name,tail(terms,n=1L));
		res<-c(res,term);
	}


	res<-paste(res,collapse=" ");
	res<-gsub(" ","",res,fixed=TRUE);
	return(parse(text=res,srcfile=NULL));
},static=TRUE)



###########################################################################/**
# @RdocMethod to.binary.logical
# 
# @title "Convert number to a  vector of logicals"
#
# \description{
# Convert number to a  vector of logicals with predefine length
# }
# 
# @synopsis
#
# \arguments{
# \item{n}{Number to convert.}
# \item{digits}{The length of output vector. If digits is less than the length of the output vector,
# the actual length is used. Default is 0.}
# \item{r}{Order of the output nnumbers If r=TRUE then big-endian otherwise small-endian. Default is @TRUE.}
# \item{...}{Not used}
# }
#
# \value{
# The @logical @vector of 'digits' length. If 'digits' is less than the length of the output vector,
# the actual length is used.
# }
#
# \examples{
# b<-Misc$to.binary.logical(25,digits=10);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################
 
setMethodS3("to.binary.logical","Misc",function(static,n,digits=0,r=TRUE,...){
	res<-rep(FALSE,times=digits);
	val<-as.logical(Misc$to.base(n,base=2,r=r));
	res[1:length(val)]<-val;
	return(res);
},static=TRUE)




###########################################################################/**
# @RdocMethod interleave
# 
# @title "Interleave two matrixes by columns"
#
# \description{@get "title".}
# 
# @synopsis
#
# \arguments{
# \item{A}{First @matrix.}
# \item{B}{Second @matrix.}
# \item{...}{Not used}
# }
#
# \value{
#  @matrix composed of A and B merged by interleaving of their columns.
# }
#
# \examples{\dontrun{
# C<-Misc$interleave(A,B);
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("interleave","Misc",function(stati,A,B,...){
	if (sum(dim(A)!=dim(B))>0)
		stop("interleave: size mismatch");
	
	res<-cbind(A,B);
	interleave<-seq(from=1,to=ncol(res),by=2);
	res[,interleave]<-A;
	res[,interleave+1]<-B;
	colnames(res)[interleave]<-colnames(A);
	colnames(res)[interleave+1]<-colnames(B);
	return(res);
},static=TRUE)


# Merge two lists of character vectors
# 
# @name CsvList.merge
# @param s1 first list of character vectors
# @param s2 second list of character vectors
# @param mergeOp merging operation (intersect, setdiff etc.)
# @param asStrings convert the resulting list of character vectors
# into the list of comma separated strings. Default is TRUE.
# @return the list each element of which contains a character vector
# or comma separated srting representing the merge result.
# @examples
# \dontrun{
# mrg<-Misc$CsvList.merge(c("a","b"),c("d","f"),mergeOp=union);
# }
# @author Alex Lisovich, Roger Day

###########################################################################/**
# @RdocMethod CsvList.merge
# 
# @title "Pairwise merge of two string vectors"
#
# \description{
# @get "title".
# }
# 
# @synopsis
#
# \arguments{
# \item{s1}{First string vector.}
# \item{s2}{Second string vectors.}
# \item{mergeOp}{Merging operation (intersect, setdiff etc.)}
# \item{asStrings}{If TRUE (default), convert the resulting string vector
# into the vector of comma separated strings.}
# \item{...}{Not used}
# }
#
# \value{
# If asStrings=TRUE, the vector of comma separated strings representing the merging results,
# and if asStrings=FALSE, then result is a @matrix of strings where each row
# represents the pairwise merge.
# }
#
# \examples{
# mrg<-Misc$CsvList.merge(c("a","b"),c("d","f"),mergeOp=union);
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("CsvList.merge","Misc",function(static,s1,s2,mergeOp,asStrings=TRUE,...){
	if(length(s1)!=length(s2))
		stop("CsvList.merge: argument length mismatch");

	len<-length(s1);

	if (asStrings)
		res<-sapply(1:len,function (i) paste(mergeOp(s1[[i]],s2[[i]]),collapse=","))
	else
		res<-sapply(1:len,function (i) mergeOp(s1[[i]],s2[[i]]));

	return(res);

},static=TRUE)


