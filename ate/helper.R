#########################
# SET WD TO LOCAL
#########################
#setwd("C:/Users/BrewJR/Documents/")
#setwd("./analystInstitute/Rtools")
setwd("/home/joebrew/ShinyApps/ate")

#########################
# LOAD NECESSARY PACKAGES
#########################
require(nnet)
#require(scales)
library(Hmisc) #note, not using ggplot

#########################
# SOURCE SCRIPTS FOR A FEW SUBFUNCTIONS
#########################
source("utils.R", echo=FALSE)
source("margins.R", echo=FALSE)


#########################
# USING THE test.data FUNCTION, CREATE A RANDOM DF NAMED df
#########################
#df <- test.data(ncols = 10, nobs=2000)

#########################
# WRITE FUNCTION FOR ONLY NON MULTINOMIAL
#########################
ate.plot.joe <- function(modform, 
                         data, 
                         level=0.95, 
                         method="logit", 
                         treatment="treat",
                         response="response",
                         color="darkgreen",
                         errbar.col="darkred",
                         alpha.text=0.6,
                         alpha.ci=0.3,
                         alpha.bar=0.4,
                         label.ci=FALSE,
                         signif.stars=TRUE,
                         print.data=FALSE,
                         subgroups=NULL, 
                         dims=NULL, 
                         file=NULL, 
                         title=NULL){
  #############################
  # ENSURE THAT RESPONSE AND TREAT ARE FACTORS
  #############################
  data[,treatment] <- as.factor(data[,treatment])
  data[,response] <- as.factor(data[,response])
  
  ##############################
  # NO SUBGROUPS
  ##############################
  if( is.null(subgroups) )
  {
    #########################
    ###LOGIT OR PROBIT
    #########################
    if( !is.null(method) && method %in% c("logit", "probit")){
      
      # CREATE J, WHICH WILL SERVE AS A PLACEHOLDER DATAFRAME FOR MARGINAL EFFECT AND C.I.'s
      j <- as.data.frame(vector(length = 1, mode="numeric"))
      
      # THE FIRST COLUMN IN J WILL BE MARGINAL EFFECT
      names(j) <- "m.e"
      
      # NAME MODEL
      mod <- glm(modform, data=data, family=binomial(link=method))
      
      # EXTRACT MARGINAL EFFECTS
      eff <- summary(mfxboot.glm(mod, data=data), conf.level = level)
      
      # COMBINE MARGINAL EFFECTS WITH C.I.'s
      effects <- cbind(eff$effects, eff$ci)
      
      # RENAME 
      names(effects)[-(2:(length(effects) - 2))] <- c("marginal.effect", "conf.lower", "conf.upper")
      
      # GIVE NAMES TO EACH ROW
      effects$var <- row.names(effects)
      
      # CREATE TITLE IF NOT SPECIFIED
      ut <- ifelse(is.null(title), paste("Treatment effects with ", sprintf("%d", 100*level), "% confidence intervals", sep=""), title)
      
      # PULL INTO MARGINAL EFFECT AND C.I. FROM EFFECTS
      j$var <- effects$var[which(effects$var == "treat1")]
      j$m.e <- effects$marginal.effect[which(row.names(effects) == "treat1")]
      j$conf.lower <- effects$conf.lower[which(effects$var == "treat1")]
      j$conf.upper <- effects$conf.upper[which(effects$var == "treat1")]
      
      
      #ASSIGN SOME LABELS (to be modified)
      bar.names <- " "
      myxlab <- " "
      
    }
    
    #########################
    ###  MLOGIT
    #########################
    else if( !is.null(method) && method == "mlogit"){
      stop("Haven't implemented multinomial yet")
    } 
    else
      stop( paste("unknown model type: ", method, sep="") )
  }
  else{
    
    
    ##############################
    # WITH SUBGROUPS
    ##############################
    
    #########################
    ###LOGIT OR PROBIT
    #########################
    if( !is.null(method) && method %in% c("logit", "probit")){
      
      # SPECIFY THE SUBGROUP
      sfactor <- as.factor(data[,subgroups])
      
      # SORT UNIQUE LIST OF THAT SUBGROUP'S LEVELS
      sfactor.unique <- unique(sort(sfactor)) 
      
      # ASSIGN NUMBER OF LEVELS 
      n.subgroups <- length(sfactor.unique)
      
      # CREATE J, WHICH WILL SERVE AS A PLACEHOLDER DATAFRAME FOR MARGINAL EFFECT AND C.I.'s
      j <- as.data.frame(vector(length = n.subgroups, mode="numeric"))
      
      # THE FIRST COLUMN IN J WILL BE MARGINAL EFFECT
      names(j) <- "m.e"
      
      # USING A FOR LOOP, SUBSET THE DATA TO ONLY INCLUDE OBSERVATIONS OF EACH SUBGROUP
      for (i in 1:n.subgroups){
        
        # NAME MODEL
        mod <- glm(modform, data=data[which(factor(data[,subgroups]) == sfactor.unique[i]),], family=binomial(link=method))
        
        # EXTRACT MARGINAL EFFECTS
        eff <- summary(mfxboot.glm(mod, data=data), conf.level = 0.9)
        
        # COMBINE MARGINAL EFFECTS WITH THEIR CI'S
        effects <- cbind(eff$effects, eff$ci)
        
        # RENAME
        names(effects)[-(2:(length(effects) - 2))] <- c("marginal.effect", "conf.lower", "conf.upper")
        
        # GIVE NAMES TO EACH ROW
        effects$var <- row.names(effects)
        
        # SPECIFY TITLE
        ut <- ifelse(is.null(title), paste("Treatment effects with ", sprintf("%d", 100*level), "% confidence intervals", sep=""), title)
        
        # POPULATE J WITH RESULTS OF EACH ITERATION OF THE LOOP
        j$var[i] <- effects$var[which(effects$var == "treat1")]
        j$m.e[i] <- effects$marginal.effect[which(row.names(effects) == "treat1")]
        j$conf.lower[i] <- effects$conf.lower[which(effects$var == "treat1")]
        j$conf.upper[i] <- effects$conf.upper[which(effects$var == "treat1")]
        
      } # END OF FOR LOOP
      
      # ASSIGN NAMES FOR BARPLOT
      bar.names <- sfactor.unique
      
      #ASSIGN SOME LABELS (to be modified)
      myxlab <- "Subgroups"
      
    }
  }
  
  #########################
  # PLOT CODE
  #########################
  bp <- barplot(j$m.e, 
                border=NA,
                col=adjustcolor(color, alpha.f=alpha.bar),
                names.arg=bar.names,
                ylim=c(min(j$conf.lower, na.rm=TRUE)*
                         ifelse(label.ci, 1.2, 1),
                       max(j$conf.upper, na.rm=TRUE)*
                         ifelse(label.ci, 1.2, 1)),
                ylab="Average treatment effect",
                xlab=myxlab,
                main=ut)
  
  
  # ADD TEXT FOR TREATMENT EFFECT AT BAR
  text(x=bp[,1],
       y=j$m.e,
       labels=round(j$m.e, digits=3),
       col=adjustcolor("black", alpha.f=alpha.text),
       pos=ifelse(j$m.e >= 0, 3, 1))
  
  # ADD TEXT FOR CONFIDENCE INTERVAL LABELS
  # TEXT conf.lower
  if(label.ci){
    text(x=bp[,1],
         y=j$conf.lower,
         labels=round(j$conf.lower, digits=3),
         col=adjustcolor("black", alpha.f=alpha.text),
         pos=1,
         cex=0.8)
    
    # TEXT conf.upper
    text(x=bp[,1],
         y=j$conf.upper,
         labels=round(j$conf.upper, digits=3),
         col=adjustcolor("black", alpha.f=alpha.text),
         pos=3,
         cex=0.8)
  }
  
  # ADD LINE AT X=0
  abline(h=0)
  
  #ADD ERROR BARS
  errbar(x=bp[,1],
         y=j$m.e,
         yplus = j$conf.upper,
         yminus = j$conf.lower,
         add=TRUE,
         pch=NA,
         errbar.col = adjustcolor(errbar.col, alpha.f=alpha.ci),
         lwd=2) 
  
  # PRINT OUT DATA IN CONSOLE IF SPECIFIED
  if(print.data){print(j)}
  
  
}

#######
# TEST
#######





