## TODO: Add comment
## 
## Author: jeroen
################################################################################
#
#
#recent.packages.html <- function(number=10){
#	
#	#html is malformed
#	maxlines <- number*2 + 11
#	mytemp <- tempfile()
#	if(getOption("repos") == "@CRAN@"){
#		repo <- "http://cran.r-project.org"
#	} else {
#		repo <- getOption("repos");
#	}
#	newurl <- paste(repo,"/web/packages/available_packages_by_date.html", sep="");
#	download.file(newurl, mytemp);
#	datastring <- readLines(mytemp, n=maxlines)[12:maxlines];
#	
#	#we only find packages from after 2010-01-01
#	myexpr1 <- '201[0-9]-[0-9]{2}-[0-9]{2} </td> <td> <a href="../../web/packages/[a-zA-Z0-9\\.]{2,}/'
#	myexpr2 <- '^201[0-9]-[0-9]{2}-[0-9]{2}'
#	myexpr3 <- '[a-zA-Z0-9\\.]{2,}/$'
#	newpackages <- unlist(regmatches(datastring, gregexpr(myexpr1, datastring)));
#	newdates <- unlist(regmatches(newpackages, gregexpr(myexpr2, newpackages)));
#	newnames <- unlist(regmatches(newpackages, gregexpr(myexpr3, newpackages)));
#	
#	newdates <- as.Date(newdates);
#	newnames <- substring(newnames, 1, nchar(newnames)-1);
#	returndata <- data.frame(name=newnames, date=newdates);
#	return(head(returndata, number));
#}
#
#recent.packages.ftp <- function(){
#	olddir <- getwd();
#	setwd(tempdir())
#	download.file("ftp://cran.r-project.org/pub/R/src/contrib/", destfile=tempfile(), method="wget", extra="--no-htmlify");
#	
#	#because of --no-htmlify the destfile argument does not work
#	datastring <- readLines(".listing");
#	unlink(".listing");
#	
#	myexpr1 <- "(?<date>[A-Z][a-z]{2} [0-9]{2} [0-9]{2}:[0-9]{2}) (?<name>[a-zA-Z0-9\\.]{2,})_(?<version>[0-9\\.-]*).tar.gz$"
#	matches <- gregexpr(myexpr1, datastring, perl=TRUE);
#	packagelines <- as.logical(sapply(regmatches(datastring, matches), length));
#	
#	#subset proper lines
#	matches <- matches[packagelines];
#	datastring <- datastring[packagelines];
#	N <- length(matches)
#	
#	#from the ?regexpr manual		
#	parse.one <- function(res, result) {
#		m <- do.call(rbind, lapply(seq_along(res), function(i) {
#			if(result[i] == -1) return("")
#			st <- attr(result, "capture.start")[i, ]
#			substring(res[i], st, st + attr(result, "capture.length")[i, ] - 1)
#		}))
#		colnames(m) <- attr(result, "capture.names")
#		m
#	}
#	
#	#parse all records
#	mydf <- data.frame(date=rep(NA, N), name=rep(NA, N), version=rep(NA,N))
#	for(i in 1:N){
#		mydf[i,] <- parse.one(datastring[i], matches[[i]]);
#	}
#	row.names(mydf) <- NULL;
#	#convert dates
#	mydf$date <- strptime(mydf$date, format="%b %d %H:%M");
#	
#	#So linux only displays dates for packages of less then six months old. 
#	#However strptime will assume the current year for packages that don't have a timestamp
#	#Therefore for dates that are in the future, we subtract a year. We can use some margin for timezones. 
#	infuture <- (mydf$date > Sys.time() + 31*24*60*60);
#	mydf$date[infuture] <- mydf$date[infuture] - 365*24*60*60;
#	
#	#sort and return
#	mydf <- mydf[order(mydf$date),];
#	row.names(mydf) <- NULL;
#	
#	#return
#	setwd(olddir)
#	return(mydf);
#}

recent.packages.rds <- function(){
	mytemp <- tempfile();
	download.file("http://cran.r-project.org/web/packages/packages.rds", mytemp);
	mydata <- as.data.frame(readRDS(mytemp), row.names=NA, optional=TRUE, stringsAsFactors=FALSE);
	mydata <- as.data.frame(lapply(mydata, unname), optional=TRUE, stringsAsFactors=FALSE);
	mydata$Published <- as.Date(mydata$Published);
	
	#sort and get the fields you like:
	mydata <- mydata[order(mydata$Published, decreasing=TRUE),c("Package", "Version", "Published")];
	row.names(mydata) <- NULL;
	return(mydata);
}

install.recent.packages <- function(repos="http://cran.r-project.org", libpath="", maxdays = 3, writecsv=TRUE, dbfile="~/.opencpu.cran.recent", Ncpus){
	
	#detect
	if(missing(Ncpus)){
		Ncpus <- max(1, parallel::detectCores()-2);
	}	
	
	#set ncpus
	options("Ncpus" = Ncpus);
	
	#set libpath
	.libPaths(libpath);	
	
	#set mirror
	options(repos=repos)
	options(warn=1);
	today <- as.Date(Sys.time());	
	now <- format(Sys.time(), "%Y-%m-%d_%H:%M");
	
	#check which we already tried
	if(!file.exists(dbfile)){
		warning("DB file not found! Installing all new packages!");
		alreadytried <- vector();
	} else {
		alreadytried <- readRDS(dbfile);
	}

	#what we already have
	currentpackages <- as.data.frame(installed.packages());
	current.string <- paste(currentpackages$Package, currentpackages$Version);	
	
	#see what is new
	newpackages <- recent.packages.rds();

	#Remove old stuff. Note that CRAN has different timezone than we do.
	newpackages <- newpackages[newpackages$Published >= today - maxdays,];

	#get a unique string for the package
	new.string <- paste(newpackages$Package, newpackages$Version, sep="_");		
	
	#Remove packages that are already installed
	newpackages <- newpackages[!(new.string %in% current.string),];
	
	#Remove packages that we tried before.
	newpackages <- newpackages[!(new.string %in% alreadytried),];
	
	#check if there are new packages
	if(nrow(newpackages) == 0){
		message("No new packages found.")
		return();
	}
	
	#install
	install.packages(newpackages$Package);
	
	#check if it worked
	installed.string <- paste(newpackages$Package, newpackages$Version, sep="_");
	updated.currentpackages <- as.data.frame(installed.packages());
	updated.current.string <- paste(updated.currentpackages$Package, updated.currentpackages$Version, sep="_");		
	newpackages$success <- installed.string %in% updated.current.string; 
			
	#dump csv
	if(writecsv){
		write.csv(newpackages, paste(now,"_recent.csv", sep=""), row.names=F);		
	}	

	#save dbfile
	saveRDS(new.string, file=dbfile);	
	
	#return the table
	return(newpackages);	
}



