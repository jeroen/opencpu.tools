# TODO: Add comment
# 
# Author: jeroen
###############################################################################


makefunction <- function(text){
	myfun <- runcode(text);
	if(!is.function(myfun)){
		stop("code is not a function.")
	}
	environment(myfun) <- globalenv();
	return(myfun);
}
