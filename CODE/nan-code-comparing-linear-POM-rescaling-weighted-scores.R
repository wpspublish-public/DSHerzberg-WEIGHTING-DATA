########################### DATA PREP SECTION ################################################################
# read the TOD data from David's Github acount
tod <- read.csv("https://raw.githubusercontent.com/wpspublish/DSHerzberg-TOD-R/master/INPUT-FILES/DATA-WEIGHTING/TODCgr1-12-unweighted.csv",header=TRUE)
tod <- tod[,c(3:ncol(tod))]
# remove the missing values
tod_complete <- tod[complete.cases(tod),]
# remove the cases for Gender = 3(others) and re-categorize the Ethnicity
tod_complete <- tod_complete[tod_complete$Gender != 3,]
tod_complete$Ethnicity <- ifelse(tod_complete$Ethnicity == 4 | 
                                   tod_complete$Ethnicity == 5 | 
                                   tod_complete$Ethnicity == 6, 4, 
                                 tod_complete$Ethnicity)
# assign the desired variables names
tod_complete$Gender <- ifelse(tod_complete$Gender == 1, "male", "female")
tod_complete$HighestEducation <- ifelse(tod_complete$HighestEducation == 1, "no_HS", 
                                        ifelse(tod_complete$HighestEducation == 2, "HS_grad",  
                                               ifelse(tod_complete$HighestEducation == 3, "some_college", "BA_plus")))

tod_complete$Ethnicity <- ifelse(tod_complete$Ethnicity == 1, "asian",
                                 ifelse(tod_complete$Ethnicity == 2, "black",
                                        ifelse(tod_complete$Ethnicity == 3, "white",
                                               ifelse(tod_complete$Ethnicity == 4, "other","hispanic"))))

tod_complete$region <- ifelse(tod_complete$region == 1,"northeast", 
                              ifelse(tod_complete$region == 2, "south",
                                     ifelse(tod_complete$region == 3,"midwest", "west")))

tod_complete$total <- apply(tod_complete[,c(7:ncol(tod_complete))], 1, sum)

# creat census target values
census_values=list(Gender=list('male'=.48,'female'=.52), 
                   HighestEducation=list('no_HS'=.12, 'HS_grad'=.26, 'some_college'=.30, 'BA_plus' = .32),
                   Ethnicity=list('hispanic'=.24, 'asian'=.04, 'black'=.14, 'white'=.52,'other'=.06),
                   region=list('northeast'=.16, 'south'=.38, 'midwest'=.22, 'west'=.24))

############################## WEIGHTING CALCULATION SECTION #####################################################
# Write a function to check the matchness (of level names) between TOD data and census target data
reorder_list <- function(x, reference_data){
  new_list = list()
  data_levels = levels(reference_data)
  for (level in levels(as.factor(reference_data)))
    new_list[[level]] = x[[level]]
  return(new_list)
}
# function for raking calculation 
rake <- function(data, variable, census){
  weights = rep(1, nrow(data))
  reordered_margins = list()
  for (n in variable){
    original_margins = census[[n]]
    reordered_margins[[n]] = reorder_list(original_margins, data[,n])
  }
  design_matrices = list()
  for (m in variable){
    design_matrices[[m]] = as.data.frame(model.matrix(~.+0, data=data[,m,drop=FALSE]))
    # the following codes tried to remove variable name from column name so only level names remain
    colnames(design_matrices[[m]]) = substr(colnames(design_matrices[[m]]), nchar(m)+1,
                                            nchar(design_matrices[[m]]))
  } 
  for (i in 1:300){ # set up the iteration number
    for (j in variable){
      inner_product = weights * design_matrices[[j]]
      weighted_sum = apply(inner_product,2,sum)
      level_weight = unlist(census[[j]][names(reordered_margins[[j]])])/
        (weighted_sum/nrow(inner_product))
      weight_output = rowSums(weights * mapply(`*`, design_matrices[[j]],level_weight))
      threshod = median(weight_output) + 6*IQR(weight_output)
      weight = pmin(weight_output, threshod)
      weight_final <- weight*length(weight)/sum(weight)
    }
  }
  return(weight_final)
}
# apply the rake function on the tod data
tod_weights = rake(tod_complete, c('Gender','HighestEducation','Ethnicity','region'), census_values)
# calcuate the Weighted score for eahc subjects and attach the results(Weighted_total)to the original data.
tod_complete$Weighted_total<- tod_weights *tod_complete$total

########################### SCORE TRANSFORMATION SECTION ##############################################################
# function for linear transformation
lm <- function(uwm,wm,uwsd,wsd,data){
  a = uwsd/wsd
  b = uwm - a*wm
  new = a*data + b
  return(new)
}
# attach the result (lms) to the origanl data
tod_complete$lms <- lm(mean(tod_complete$total),mean(tod_complete$Weighted_total),
                       sd(tod_complete$total),sd(tod_complete$Weighted_total),
                       tod_complete$Weighted_total)
# function for POM method
pt <- function(wd,rd){
  a = wd/max(wd)
  new = a*rd
  return(new)
}
#attach the result (pom) to the origanl data
tod_complete$pom <- pt(tod_complete$Weighted_total,tod_complete$total)

######################### CREATE A GRAPH for SCORE COMPARISONS #####################################################
# breakdown the socres based on ages
graph_data <- aggregate(cbind(total,Weighted_total,lms,pom) ~ Age, data = tod_complete, 
                        FUN = function(x) c(mean = mean(x)))
# find the maximum value among all scores
g_range <- range(0, graph_data[,2], graph_data[,3],graph_data[,4],graph_data[,5])
# plot the socres one by one 
plot(graph_data[,2], type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE); box()
lines(graph_data[,3], type="o", pch=22, lty=1, col="red")
lines(graph_data[,4], type="o", pch=23, lty=2, col="forestgreen")
lines(graph_data[,5], type="o", pch=24, lty=3, col="Black")
# add other graph labels 
label <- as.character(graph_data[,1])
axis(1, at=1:length(label), lab=label)
axis(2, las=1, at=4*0:g_range[2])
# add names of x-axis and y-axis and graph title
title(xlab="Age"); title(ylab="Score")
title(main="Socre comparisons (Unweight/Weighted/Linear/POM)", 
      col.main="black", font.main=2)
# add the referecen line
abline(h=44, col='coral2',lwd = 3)
# add the legend to make the graph self-evident
legend(1, 40, c("Unweight","Weighted","Liner","POM"), cex=0.8, 
       col=c("blue","red","forestgreen","Black"), 
       pch=21:22, lty=1:2)

