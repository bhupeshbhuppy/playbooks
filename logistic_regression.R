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
library(reshape2)
library(perturb)
library(ROCR)
library(Metrics)
library(Hmisc)
# user defined functions --------------------------------------------------

woe<-function(dep,indep){
  woe<-dcast(as.data.frame(table(indep,dep)),indep~dep)
  woe$total<-rowSums(woe[,c(2,3)])
  woe$DB<-woe$`0`/length(indep)
  woe$DG<-woe$`1`/length(indep)  
  woe$WoE<-log(woe$DG/woe$DB)
  woe$DG_DB<-woe$DG - woe$DB
  iv<-sum(woe$DG_DB*woe$WoE)
  return(list("woe" = woe,"iv"=iv))
}

dropOutSwitchTest<-function(){
  #### To be filled after we have written discriminitrary power and predictive power testing
}

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
    j=1
    if(class(indep[,i]) == "integer" || class(indep[,i]) == "factor" || class(indep[,i]) == "character"){
      data_freq<-as.data.frame(table(indep[,i],dep)/(as.data.frame(table(indep[,i]))[,2]))
      p<-plot_ly(data_freq, x = ~Var1, y = ~Freq, color = ~dep, type="bar")%>%
        layout(title = paste0("Event Rate Chart- ",colnames(indep)[i]),
               xaxis = list(title = colnames(indep)[i],showgrid = T))
      plt[[j]]<-p
      j=j+1
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
  somersD
  ks<-c()
  #### KS = 1 is not from the same sample i.e. not a good predictors
  for(i in 1: length(indep)){
    if(class(indep[,i]) == "numeric" || class(indep[,i]) == "double"){
      ks<-as.data.frame(rbind(ks,data.frame(var= colnames(indep)[i], "DStat"= ks.test(model$fitted.values,indep[,i])$statistic[[1]])))
    }
  }
  ks
  
  
  #########################################################################################################
  ####                    c. Weight of Evidence (WoE) and Information Value                           #####
  #### Measures the strength of a set of binning across different values of the predictor variables   ##### 
  #### to seprate good and bad. High negative or positive values are an indication of the variables   #####
  ####                                                                                                #####
  #### Information Value:Asses overall power of a variable in assessing good and bad outcomes         #####
  #### Rule of Thumb: < 0.02	useless for prediction; 0.02 to 0.1	Weak predictor;                     #####
  ####                0.1 to 0.3	Medium predictor; 0.3 to 0.5	Strong predictor                      #####
  ####                >0.5	Suspicious or too good to be true                                         #####
  #########################################################################################################
  woe_iv<-list()
  for(i in 1:length(indep)){
    j<-1
    if(class(indep[,i]) == "integer" || class(indep[,i]) == "factor" || class(indep[,i]) == "character"){
      woe_iv[[j]]<-woe(dep,indep[,i])
      j=j+1
    }
  }
  #########################################################################################################
  ####                                    c. Dropout switch test                                      #####
  #########################################################################################################
  
  
  # 2. Multicollinearity -------------------------------------------
  
  #########################################################################################################
  ####                                    a. Correlation Assesment                                    #####
  #########################################################################################################
  
  
  #########################################################################################################
  ####                                    b. Coondition Index                                         #####
  ####  The largest conditional index in the conditional number. A large condition number indicates   #####
  ####  that the regression estimates may have considerable error due to multicollinearity.           #####
  ####  CI greater than 30 can be cause of concern and shoud be evaluated carefully.                  #####
  ####  https://academic.csuohio.edu/kneuendorf/c63113/hand26A.pdf                                    #####
  #########################################################################################################
  colldiag(model)
  
  #########################################################################################################
  ####                                    b. VIF (1/(1-R^2))                                          #####
  ####  VIF needs to be calculated for all the variables. Some high VIF are acceptable, especially    #####
  ####  those which can be proven to be important via the dropout test. FOr categorical variable      #####
  ####  VIF is calculated by using partial genealized R^2. Generaly VIF above 5 is regarded as bad    #####
  #########################################################################################################
  VIF(model)
  
  #########################################################################################################
  ####                                    c. Variable Dropout Test                                    #####
  ####  Removing one variable at a time and thenn assessing the VIF. Testing the reduced model by     #####
  ####  comparing the model performance with that of the original model. If possible the test should  #####
  ####  be performed with out of sample data                                                          #####
  #########################################################################################################
  if(length(indep)>2){
    for(i in 1: length(indep)){
      temp_model<-glm(glm(dep~., modelling_data[,-c(i+1)],family=binomial(link = "logit")))
      print(paste("After reoving the ",i,"th varible the model VIF is", VIF(temp_model)))
      
      #### Need to check the PM after they are all written
    }
  }
  
  # 3. DIscritiminatory Power Testing -------------------------------------------
  #########################################################################################################
  ####                                    a. ROC/ AUC                                                 #####
  #########################################################################################################
  
  AUC<-round(auc(dep, predict(model)),3)
  rocs <- performance(prediction(predict(model), dep), "tpr", "fpr")
  ROC_df<-data.frame("fpr"=rocs@x.values,"tpr"=rocs@y.values)
  colnames(ROC_df)<-c("fpr","tpr")
  
  plot_ly(ROC_df, y = ~tpr, x = ~fpr) %>% 
    
    add_lines(name = "Model",
              line = list(shape = "spline", color = "#737373", width = 7), 
              fill = "tozeroy", fillcolor = "#2A3356") %>% 
    
    
    add_segments(x = 0, y = 0, xend = 1, yend = 1, 
                 line = list(dash = "7px", color = "#F35B25", width = 4), 
                 name = "Random") %>% 
    
    add_segments(x = 0, y = 0, xend = 0, yend = 1, 
                 line = list(dash = "10px", color = "black", width = 4), 
                 showlegend = F) %>%
    
    add_segments(x = 0, y = 1, xend = 1, yend = 1, 
                 line = list(dash = "10px", color = "black", width = 4), 
                 showlegend = F) %>% 
    
    add_annotations(x = 0.8, y = 0.2, showarrow = F, 
                    text = paste0("Area Under Curve: ", AUC),
                    font = list(family = "serif", size = 18, color = "#E8E2E2")) %>%
    
    add_annotations(x = 0, y = 1, showarrow = F, xanchor = "left", 
                    xref = "paper", yref = "paper",
                    text = paste0("Receiver Operator Curve"),
                    font = list(family = "arial", size = 30, color = "#595959")) %>%
    
    add_annotations(x = 0, y = 0.95, showarrow = F, xanchor = "left", 
                    xref = "paper", yref = "paper",
                    text = paste0("Charts the percentage of correctly identified defaults (hits) against the percentage of non defaults incorrectly identifed as defaults (false alarms)"),
                    font = list(family = "serif", size = 14, color = "#999999")) %>% 
    
    
    layout(xaxis = list(range = c(0,1), zeroline = F, showgrid = F,
                        title = "Number of False Alarms"),
           yaxis = list(range = c(0,1), zeroline = F, showgrid = F,
                        domain = c(0, 0.9),
                        title = "Number of Hits"),
           plot_bgcolor = "#E8E2E2",
           height = 800, width = 1024)
  
  #########################################################################################################
  ####                                    b. SomersD'                                                 #####
  ####  It is calculated as the diference between the concordant and discordant pairs.                #####
  ####  SomersD'(Dxy) = (NCon - NDis)/(NCon + NDis + NTie). It is related to ROC/AUC as Dxy=2(AUC-0.5)##### 
  ####  A vale close to 0 indicates a random model. A lower value does not always mean that the model #####
  ####  should be rejected.                                                                           #####
  #########################################################################################################
  print(paste("Somers' D is: ",2*(AUC-0.5)))

  #########################################################################################################
  ####                                    C. Kolmogrov Smirnov(KS)                                    #####
  ####  It is used to test for difference between distribution function thereforecan be used to       #####
  ####  discriminate between the two binary responses. Higher value mean higher discriminatory power  ##### 
  ####  KS is not sensitive to the shape of distribution and should be complemented by other tests    #####
  ####  KS = Maximum difference between Cumulative % 1 and Cumulative % 0  along different Quartile   #####
  ####  https://www.listendata.com/2015/01/model-validation-in-logistic-regression.html               #####
  #########################################################################################################
  diff_TPRFPR=ROC_df$tpr-ROC_df$fpr 
  KS=max(diff_TPRFPR)
  cutoffAtKS=unlist(rocs@alpha.values)[which.max(diff_TPRFPR)]
  print(paste("The KS and corresponding cutoff is: ",c(KS, cutoffAtKS)))    
  
}else{
  print("Please create or import the model")
}








