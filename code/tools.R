

#--------------------------------------------------------
# Tools: collection of useful functions
#-------------------------------------------------------- 

load_libs <- function(requiredPackages) {
  # load all required libs, if they are not installed try to install them automatically
  #
  # Args:
  #  requiredPackages: List of strings with package names that are about to be loaded 
  #                    If they are not installed automated installation is attempted
  missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
  if (length(missingPackages)) {
    install.packages(missingPackages, dependencies = TRUE)
  }
  
  for (package in requiredPackages) {
    library(package, character.only = TRUE)
  }
}


ml_pred <- function(train, test, target, task="regr", lrn){
  # machine learning prediction
  #
  # Args:
  #  train:  training data
  #  test:   test data
  #  target: target var
  #  task:   regr or classif
  #  lrn:    learner obj of mlr package
  # Return:
  #   prediction value on test data set
  
  if(task=="regr"){  # regression
    # define task
    task.reg <- makeRegrTask(data=train, target=target)
    # training model
    mod.reg  <- train(lrn, task.reg)
    # predict
    pred <- predict(mod.reg, newdata=test)$data$response
    
  } else {  # classification
    # define task
    task.classif <- makeClassifTask(data=train, target=target, positive = "1")
    # training model
    mod.classif  <- train(lrn, task.classif)
    # predict
    pred <- predict(mod.classif, newdata=test)
  }
  
  return(pred)
  
}


plot_HistDensity <- function(x, title){
  # plot histogram with density curve overlaid
  # Arg:
  #   x: numeric vector 
  # Return:
  #   plot of histogram with density curve overlaid
  x <- x[!is.na(x)]  # rm NA records
  hist(x, prob=TRUE, col="grey", nclass=50, main=NA) # prob=TRUE for probabilities not counts
  lines(density(x, adjust=1), col="blue", lty="solid", lwd=2) # add a density est
  title(main=title)
  
  dev.copy(png,paste0(title,"_hist.png"))
  dev.off()
}


plot_Box <- function(d, x="1", y, title=""){
  # plot boxplot
  # Arg:        
  #   d: data frame
  #   x: catergorical var 
  #   y: numerical var
  # return: 
  #   box plot of y catergoried by x
  p <- d %>%  
    filter_(!is.na(y)) %>%  # rm NA records
    ggplot(., aes_string(x=x, y=y)) +
    geom_boxplot(aes_string(fill=x)) + 
    ggtitle(title)
  
  if(x=="1"){ # categorical var with 1 level
    p + theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.position="none")
  } else { # categorical var with multiple level
    p + theme(plot.title = element_text(hjust = 0.5))
              #axis.text.x = element_text(angle = 90, hjust = 1))
  }
  ggsave(paste0(title,"_box.png"))
}


plot_StackingProp <- function(d, x="1", y, title=""){
  # stacking proportion plot 
  # Arg:        
  #   d: data frame
  #   x: catergorical var 
  #   y: catergorical var for count (proportion)
  # return: 
  #   stacking proportion plot of y for different x catergory
  ggplot(data = d) + 
    geom_bar(mapping = aes_string(x = x, fill = y), position = "fill") +
    guides(fill=guide_legend(title=title))
  ggsave(paste0(title,"_stackingProp.png"))
}


plot_Scatter <- function(d, x, y, title=""){
  # scatter plot with smooth trend line
  # Arg:        
  #   d: data frame
  #   x: num var 
  #   y: num var 
  # return: 
  #   scatter plot of two num vars with smooth trend line

  ggplot(data = d, mapping = aes_string(x = x, y = y)) + 
    geom_point() + 
    geom_smooth()
  ggsave(paste0(title,"_scatter.png"))
}



plot_BarCount <- function(d, x="1", y, title=""){
  # Side by side Bar count plot category on x and y
  # Arg:        
  #   d: data frame
  #   x: main catergorical var 
  #   y: catergorical var within x 
  # return: 
  #   Side by side Bar count plot category on x and y
  ggplot(data = d) + 
    geom_bar(mapping = aes_string(x = x, fill = y), position = "dodge")
  ggsave(paste0(title,"_barcount.png"))
}


plotRFVarImp <- function(rf.mod){
  # Plot variable importance of a RF model 
  #
  # Args:
  #   rf.mod: a rf model obj
  #
  # Returns:
  #   Two Plots: 1. based on pred accuracy 2. based on gini index 
  
  # Importance data
  dat <- data.frame(rownames(importance(rf.mod)), round(importance(rf.mod),2))
  
  names(dat)[c(1, ncol(dat)-1, ncol(dat))] <- c("Predictor","mda","mdg")
  rownames(dat) <- NULL
  
  pred.acc  <- select(dat, Predictor, mda)  # mean decrease in accuracy
  pred.gini <- select(dat, Predictor, mdg)  # mean decrease in gini
  
  # Var importance plot function
  importancePlot <- function(d,ylb,fontsize){
    fontsize <- as.numeric(fontsize)
    d <- d[order(d[,2],decreasing=T),]
    d$Predictor <- factor(as.character(d$Predictor),levels=rev(as.character(d$Predictor)))
    rownames(d) <- NULL
    
    d[,2] <- d[,2]/abs(max(d[,2])) * 100  # normalize relative to the variable with maximum score
    abs.min <- abs(min(d[,2]))
    
    g1 <- ggplot(data=d,aes_string(x="Predictor",y=ylb,group="Predictor")) + 
      geom_bar(stat="identity", colour="#d62d20", fill="#d62d20") + theme_grey(base_size=fontsize)
    #geom_bar(stat="identity", colour="#639f89", fill="#639f89") + theme_grey(base_size=fontsize)
    
    #if(ylb=="mda")      g1 <- g1 + labs(y="Mean decrease in accuracy") 
    #else if(ylb=="mdg") g1 <- g1 + labs(y="Mean decrease in Gini")
    g1 <- g1 + labs(y="Variable Importance") # Simplify for presentation purpose
    
    
    g1 <- g1 + theme(axis.title=element_text(size=25,face="bold"), 
                     axis.text.x=element_text(angle=0,hjust=1,vjust=0.4,colour='black'),
                     axis.text.y= element_text(colour='black', size=25)) + 
      geom_hline(yintercept=abs.min,linetype="dashed",colour="black") + coord_flip()
    print(g1)
  }
  
  importancePlot(d=pred.acc, ylb="mda", 20)
  importancePlot(d=pred.gini, ylb="mdg", 20)
}


combine_analyst <- function(d, char) {
  # Combine same analyst estimates
  #
  # Args:
  #   d: data frame with analyst est
  #   char: a char index duplicate analyst
  #
  # Returns:
  #   data frame combine same analyst estimate and drop col with char
  names <- names(d)
  index <- NULL  # index col match char
  for(i in 1:length(names)){
    
    if(grepl(char, names[i])){
      d[,i-1] <- ifelse(is.na(d[,i-1]), d[,i], d[,i-1])
      index <- c(index, i)
    }
    
  }
  return(d[,-index])
}

cal_corr <- function(dat, name) {
  # calculate corr and output plot 
  #
  # Args:
  #   d: data frame with analyst est
  #   name: a string for the fig name
  #
  # Returns:
  #   output 2 corr plot, one in circle one in numerical
  
  sum3 <- function(d){  # 3 number summary function
    c(min(d, na.rm=T), mean(d, na.rm=T), max(d, na.rm=T))
  }
  
  A <- t(apply(dat[,-c(1,2)], 1, sum3))
  colnames(A) <- c("Min", "Mean", "Max")
  A <- as.data.frame(A)
  B <-t(apply(dat[,-c(1,2)], 1, quantile, c(0.1,0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ), na.rm=T))
  out <- cbind(Actual=dat[,2], A, B)
  
  M <- cor(out, use="pairwise.complete.obs")
  
  png(paste0(name,"_circle.png"), width=650, height=650)
  corrplot(M, method="circle", type="upper", col=brewer.pal(n=11, name="RdYlBu"))
  dev.off()
  
  png(paste0(name,"_num.png"), width=650, height=650)
  corrplot(M, method="number", type="upper", col=brewer.pal(n=8, name="RdYlBu"))
  dev.off()
}



