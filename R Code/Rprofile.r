# To source this file into an environment to avoid cluttering the global workspace, put this in Rprofile:
# my.env <- new.env(); sys.source("C:/PathTo/THIS_FILE.r", my.env); attach(my.env)

#-----------------------------------------------------------------------
#     Load packages, set options and cwd, set up database connection
#-----------------------------------------------------------------------

## Load packages
# require(ggplot2)  #plotting
# require(plyr)		#data manipulation
# require(reshape)	#data manipulation
# require(sqldf)	#manipulate data frame with SQL
# require(Hmisc)	#frank harrell's miscellaneous functions

## Sets the working directory to C:/R
# setwd("C:/Amy Whitehead/Dropbox")
working.directory <- "C:/Users/awhitehead/Documents"

## Don't show those silly significanct stars
options(show.signif.stars=FALSE)

## Do you want to automatically convert strings to factor variables in a data.frame?
options(stringsAsFactors=TRUE)

## Hard code the US repository for CRAN so it doesn't ask me every time.
r <- getOption("repos")             
r["CRAN"] <- "http://cran.stat.auckland.ac.nz/"
options(repos = r)
rm(r)

#-----------------------------------------------------------------------
#                             Functions
#-----------------------------------------------------------------------
# easy way to install packages as required
packages<-function(x){
   x<-as.character(match.call()[[2]])
   if (!require(x,character.only=TRUE)){
      install.packages(pkgs=x,repos="http://cran.r-project.org")
      require(x,character.only=TRUE)
   }
}

## play a sound when R script is finished (doesn't work in Windows 7)
  finished <- function(x){
    packages(sound)
    cat("\n","\n","I'm finished!")
    play(Sine(440,0.25))
  }
 
## center numeric data in dataframe
myCenter <- function(x) {
  if (is.numeric(x)) { return(scale(x, scale=FALSE)[,1]) }
  
  if (is.data.frame(x) || is.matrix(x)) {
    m= x
    for (i in 1:ncol(x)) {
      if(is.numeric(x[,i])) {
        m[,i]= myCenter(x[,i])
      } 
    }
    return(as.data.frame(m))
  }
}

## Convert selected columns of a data frame to factor variables
factorcols <- function(d, ...) lapply(d, function(x) factor(x, ...)) 

## Returns a logical vector TRUE for elements of X not in Y
"%nin%" <- function(x, y) !(x %in% y) 

## Returns names(df) in single column, numbered matrix format.
n <- function(df) matrix(names(df)) 

## Calculates standard error
se<-function(x){(sqrt(var(na.omit(x))/length(na.omit(x))))}

## Arc-sine square root transformation
arcsine.sqrt <- function(x){asin(sqrt(x))}

#error bar function from http://monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
stop("vectors must be same length")
arrows(x,y+upper, x, y-lower, angle=0, code=3, length=length, ...)
}

## put histogram on diagonal of pairs
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

## put (absolute) correlations on pairs plot, with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

## print results of linear model on ggplot2
lm_eqn = function(x,y){
  m = lm(y ~ x);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2*","~~italic(P)~"="~p, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3),
                        p = format(round(summary(m)$coefficients[8],3 ))))
  as.character(as.expression(eq));                 
}

# This function returns a logical vector, the elements of which 
# are FALSE, unless there are duplicated values in x, in which 
# case all but one elements are TRUE (for each set of duplicates). 
# The only difference between this function and the duplicated() 
# function is that rather than always returning FALSE for the first 
# instance of a duplicated value, the choice of instance is random. 
duplicated.random = function(x, incomparables = FALSE, ...) 
{ 
  if ( is.vector(x) ) 
  { 
    permutation = sample(length(x)) 
    x.perm      = x[permutation] 
    result.perm = duplicated(x.perm, incomparables, ...) 
    result      = result.perm[order(permutation)] 
    return(result) 
  } 
  else if ( is.matrix(x) ) 
  { 
    permutation = sample(nrow(x)) 
    x.perm      = x[permutation,] 
    result.perm = duplicated(x.perm, incomparables, ...) 
    result      = result.perm[order(permutation)] 
    return(result) 
  } 
  else 
  { 
    stop(paste("duplicated.random() only supports vectors", 
               "matrices for now.")) 
  } 
} 

# append a comment to the top of a csv file (http://www.quintuitive.com/2013/01/11/adding-comments-to-csv-files/)
write.csv.comments = function( input, fileName, comments=c() )
{
  require( quantmod, quietly=TRUE )
  
  # First write the comments
  append = FALSE
  for( comment in comments ) {
    line = paste( sep="", "# ", comment )
    write( line, file=fileName, append=append )
    append = TRUE
  }  
  
  # Write the series, but suppress warnings to avoid the following:
  #    Warning message:
  #    In write.table(dx, file = file, row.names = row.names, col.names = col.names,  :
  #    appending column names to file
  suppressWarnings( write.zoo( input, quote=F, row.names=F, sep=",", file=fileName, append=append ) )
}

## Single character shortcuts for summary(), head() and data.frame().
s <- base::summary
h <- utils::head
d <- base::data.frame
l <- base::length

## ht==headtail, i.e., show the first and last 10 items of an object
ht <- function(d) rbind(head(d,10),tail(d,10))

## Show the first 5 rows and first 5 columns of a data frame or matrix
hh <- function(d) d[1:5,1:5]

## Read data on clipboard.
read.cb <- function(...) read.table(file="clipboard", ...) 

## Paste without a separator.
glue <- function(...) paste(...,sep="")

## name("test.png") results in "C:/R/2010-04-20-test.png" if running this in C:/R on April 20 2010.
name <- function(filename="filename") glue(getwd(),"/",Sys.Date(),"-",filename)

## Takes a dataframe and a column name, and moves that column to the front of the DF.
moveColFront <- function(d=dataframe, colname="colname") {
	index <- match(colname, names(d))
	cbind(d[index],d[-index])
}

## Permutes a column in a data.frame, sets seed optionally
permute <- function (dataframe, columnToPermute="column", seed=NULL) {
	if (!is.null(seed)) set.seed(seed)
	colindex <- which(names(dataframe)==columnToPermute)
	permutedcol <- dataframe[ ,colindex][sample(1:nrow(dataframe))]
	dataframe[colindex] <- permutedcol
	return(dataframe)
}

## Summarize missing data in a data frame. Return a list (lpropmiss) or data frame (propmiss)
lpropmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x)))
propmiss <- function(dataframe) {
	m <- sapply(dataframe, function(x) {
		data.frame(
			nmiss=sum(is.na(x)), 
			n=length(x), 
			propmiss=sum(is.na(x))/length(x)
		)
	})
	d <- data.frame(t(m))
	d <- sapply(d, unlist)
	d <- as.data.frame(d)
	d$variable <- row.names(d)
	row.names(d) <- NULL
	d <- cbind(d[ncol(d)],d[-ncol(d)])
	return(d[order(d$propmiss), ])
}

## Make a pretty QQ plot of p-values
qq = function(pvector, ...) {
    if (!is.numeric(pvector)) stop("D'oh! P value vector is not numeric.")
	pvector <- pvector[!is.na(pvector) & pvector<1 & pvector>0]
    o = -log10(sort(pvector,decreasing=F))
	#e = -log10( 1:length(o)/length(o) )
	e = -log10( ppoints(length(pvector) ))
	plot(e,o,pch=19,cex=1, xlab=expression(Expected~~-log[10](italic(p))), ylab=expression(Observed~~-log[10](italic(p))), xlim=c(0,max(e)), ylim=c(0,max(o)), ...)
	abline(0,1,col="red")
}

## Correlation matrix with p-values. 
#pairs(YOUR_DATA_FRAME_HERE,lower.panel=panel.smooth, upper.panel=panel.cor)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y,use="na.or.complete"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)

    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))

    text(0.5, 0.5, txt, cex = cex * r)
    text(.8, .8, Signif, cex=cex, col=2)
}

## Lists correlations greater than a given threshold
correlations <- function(data, threshold=0.7){
  MyCor <- cor(data, use="pairwise.complete.obs")
  myCorrelates <- apply(MyCor, 2, function(x) names(which(abs(x)>threshold)))
  for(n in 1:length(myCorrelates)) myCorrelates[[n]] <- myCorrelates[[n]][-which(myCorrelates[[n]] == names(myCorrelates)[n])]
  myCorrelates
 }

## Identifies outliers in a vector
is.outlier = function (x) {
     # See: Davies, P.L. and Gather, U. (1993).
     # "The identification of multiple outliers" (with discussion)
     # J. Amer. Statist. Assoc., 88, 782-801.
 
     x <- na.omit(x)
     lims <- median(x) + c(-1, 1) * 5.2 * mad(x, constant = 1)
     x < lims[1] | x > lims[2]
 }

## Function for arranging ggplots. use png(); arrange(p1, p2, ncol=1); dev.off() to save.
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
	dots <- list(...)
	n <- length(dots)
	if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
	if(is.null(nrow)) { nrow = ceiling(n/ncol)}
	if(is.null(ncol)) { ncol = ceiling(n/nrow)}
        ## NOTE see n2mfrow in grDevices for possible alternative
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
	ii.p <- 1
	for(ii.row in seq(1, nrow)){
	ii.table.row <- ii.row	
	if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
		for(ii.col in seq(1, ncol)){
			ii.table <- ii.p
			if(ii.p > n) break
			print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
			ii.p <- ii.p + 1
		}
	}
}

## Imputes the median value of a vector, matrix, or data frame.
## Stolen from na.roughfix function in the randomForest package.
medianImpute <- function (object=NULL, ...) {
	if (class(object) == "data.frame") {
		isfac <- sapply(object, is.factor)
		isnum <- sapply(object, is.numeric)
		if (any(!(isfac | isnum))) 
			stop("dfMedianImpute only works for numeric or factor")
		roughfix <- function(x) {
			if (any(is.na(x))) {
				if (is.factor(x)) {
					freq <- table(x)
					x[is.na(x)] <- names(freq)[which.max(freq)]
				}
				else {
					x[is.na(x)] <- median(x, na.rm = TRUE)
				}
			}
			x
		}
		object[] <- lapply(object, roughfix)
		return(object)
	}
	else if(is.atomic(object)) {
		d <- dim(object)
		if (length(d) > 2) 
			stop("vectorMedianImpute can't handle objects with more than two dimensions")
		if (all(!is.na(object))) 
			return(object)
		if (!is.numeric(object)) 
			stop("vectorMedianImpute can only deal with numeric data.")
		if (length(d) == 2) {
			hasNA <- which(apply(object, 2, function(x) any(is.na(x))))
			for (j in hasNA) object[is.na(object[, j]), j] <- median(object[, 
				j], na.rm = TRUE)
		}
		else {
			object[is.na(object)] <- median(object, na.rm = TRUE)
		}
		return(object)
	}
	else stop("Object is not a data frame or atomic vector")
}

## produce a piechart and list of matching colours based on the text provided and then add nearby colours
col.wheel <- function(str, nearby=3, cex=0.75) {
    cols <- colors()
  hsvs <- rgb2hsv(col2rgb(cols))
	srt <- order(hsvs[1,], hsvs[2,], hsvs[3,])
	cols <- cols[srt]
	ind <- grep(str, cols)
	if (length(ind) <1) stop("no colour matches found",
		call.=FALSE)
	s.ind <- ind
	if (nearby>1) for (i in 1:nearby) {
		s.ind <- c(s.ind, ind+i, ind-i)
	}
	ind <- sort(unique(s.ind))
	ind <- ind[ind <= length(cols)]
	cols <- cols[ind]
	pie(rep(1, length(cols)), labels=cols, col=cols, cex=cex)
	cols
}

# change text to proper case
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# multiplot function for ggplot2
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}


# Did you make it this far?
message("\n******************************\nSuccessfully loaded Rprofile.r\n******************************")