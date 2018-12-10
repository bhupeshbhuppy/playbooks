rm(list = ls())

######################################################################################################################
##### Required: 1. Dependent variable as a variable - dep                                                       ######
#####           2. Independent varibel as data frame - indep                                                    ######
##### Note: if only one variable is used still convert it to data frame                                         ######
#####                                                                                                           ######
######################################################################################################################
# importing library -------------------------------------------------------
library(plotly)
library(DescTools)

# import dependent and independent variable -------------------------------

dep<- rbinom(1000, 1, 0.3)
### should be a variable
indep<-data.frame(var1=sample(1:5,1000,replace=T),var2= runif(1000, min=2, max=10000) )
### indep should be a data frame
model<-c()


# create model (optional) -------------------------------------------------


if(length(dep) == nrow(indep)){
  modelling_data<-cbind(dep=dep,indep)
  model<-glm(dep~., modelling_data,family=binomial(link = "logit"))
}else{
  print("Length of dep and independent variable is not same.")
}


# performances metrics ----------------------------------------------------

if(length(model)>0){
  
  # 1. Variable contribution Test -------------------------------------------
  
  
  #########################################################################################################
  ####                            a. Event Rate vs Predictor Plot                                     #####
  #### We Plot the rate of the event predictedthe against each predictor. Tough only for dicrete      ##### 
  #### variables continousvariables can be predicted after binning etc. The important point to note   #####
  #### is a predictor variable in the model should show differentiation in the event rate based on its##### 
  #### own values.                                                                                    #####
  #########################################################################################################
  plt<-list()
  for(i in 1:length(indep)){
    if(class(indep[,i]) == "integer" || class(indep[,i]) == "factor" || class(indep[,i]) == "character"){
      data_freq<-as.data.frame(table(indep[,i],dep)/(as.data.frame(table(indep[,i]))[,2]))
      p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~dep, type="bar")%>%
        layout(title = paste0("Event Rate Chart- ",colnames(indep)[i]),
               xaxis = list(title = colnames(indep)[i],showgrid = T))
      plt[[i]]<-p
    }
  }
  plt
  
  
  #########################################################################################################
  ####                           b. Test for discriminatory power                                     #####
  #### We calculate Somers' D, Kolmogorov-Smirnov (KS) on ecah predictor variable. The aim is that    ##### 
  #### each predictor variable must exihibit  discriminatory power.                                   #####
  #### Need to explain a bit abot about interpratation of somers D and KS wrt variable discrimination
  ####
  #########################################################################################################
  somersD<-data.frame(var = c(colnames(indep)),somersD = rep(NA,length(indep)))
  for(i in 1: length(indep)){
    somersD[i,2]<-SomersDelta(dep,indep[,i])
  }
  
  
}else{
  print("Please create or import the model")
}









