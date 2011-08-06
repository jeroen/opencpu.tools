# TODO: Add comment
# 
# Author: jeroen
###############################################################################


demo.lm <- function(formula, data){
	
	#make a scatterplot
	plot(data);
	
	#fit the model
	mylm <- lm(formula, data);
	
	#some more plots
	plot(mylm);
	
	#write a file
	write.table(data, "mydata.tab");
	
	#return R object with relevant data
	output <- list();
	output$anova <- apply(as.data.frame(anova(mylm)), 1, as.list);
	output$coef <- apply(as.data.frame(summary(mylm)$coefficients), 1, as.list);
	return(output);
}
