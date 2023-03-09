
##DDHQ_Assignment Code

options(scipen = 100, digits = 4)

#Packages Used
library(tidyverse)
library(ggplot2)
library(usmap)
library(lubridate)
library(janitor)
library(tidymodels)
library(themis)
library(rpart)
library(yardstick)
library(vip)
library(rpart)
library(baguette)
library(plotly)

##LOADING AND SPLITTING DATA##
  
#Loading in data
election_results<-read.csv("DDHQ_Data_Exercise-1.csv")
election_results$R.D.Victory.Margin<-as.numeric(election_results$R.D.Victory.Margin)
results<-subset(election_results,Year!="2018")
election_results$R.D.Victory.Margin<-election_results$R.D.Victory.Margin/100

#Data Splitting
set.seed(20201020)
result_split<-initial_split(data=election_results,prop=0.75)

#Training data
result_train<-training(x=result_split)
result_train<-subset(result_train,Year!="2018")

#Testing data
result_test<-testing(x=result_split)

##EXPLORATORY DATA ANALYSIS##

#Boxplot of victory margin over time
boxplot(R.D.Victory.Margin*100~Year,
        data=result_test)

#Average R.D victory margin per state (2006-2016)
averages<-result_train%>%
  group_by(state=State)%>%
  summarize(Average_Margin=mean(R.D.Victory.Margin)*100)

#Stagnant Visualization
margin_plot<-
  plot_usmap(regions="state",
             data=averages,
             values="Average_Margin",
             color="black",
             size=0.001)+
  scale_fill_gradientn(colors=c("blue","white","red"),
                       breaks=c(-70,0,40),
                       labels=c("Min",0,"Max"))+
  labs(title="Average Vote Margin By State from 2006-2016")+
  theme(legend.position = "right",
        panel.background=element_rect(colour = "black", fill = "white"), 
        plot.title = element_text(face="bold"))

margin_plot

#Interactive Visualization
ggplotly(margin_plot)


##DATA PRE-PROCESSING##

#Pre-processing recipe
margin_recipe<-recipe(R.D.Victory.Margin~.,data=result_train)%>%
  step_rm(all_string_predictors())%>%
  step_rm("Congressional.District")%>%
  step_rm("Year")%>%
  prep()

#Baking the recipe
bake(margin_recipe,new_data=result_train)


##MODEL CREATION##

#Model
bag_mod<-
  bag_tree()%>%
  set_engine("rpart")%>%
  set_mode("regression")

#Workflow
bag_wf<-
  workflow()%>%
  add_model(bag_mod)%>%
  add_recipe(margin_recipe)

#Fitting model on training data
bag_fit<-bag_wf%>%fit(result_train)

#Evaluating model on testing data
margin_predicted<-bind_cols(result_test,
                            predict(object=bag_fit,
                                    new_data=result_test))

##IMPLEMENTING MODEL##

#Implementing model for prediction
implementation_predictions<-bind_cols(election_results,
                                      predict(object=bag_fit,
                                              new_data=election_results))

#Putting victory margin back into percent 
implementation_predictions$R.D.Victory.Margin<-implementation_predictions$R.D.Victory.Margin*100

implementation_predictions$.pred<-implementation_predictions$.pred*100

#Filtering implemented data to see results
implementation_predictions<-implementation_predictions%>%select(c("Race.ID", "Year","R.D.Victory.Margin",".pred"))

##MODEL EVALUATION AND INTERPRETATION##

#Metrics
implementation_predictions %>%
  filter(Year!="2018")%>%
  metrics(R.D.Victory.Margin, .pred)

#Filtering to see 2018 results
pred_2018<-filter(implementation_predictions,Year=="2018")

#Seeing breakdown of results
pred_2018%>%count(grepl("-",.pred))

##APPENDING ORIGINAL DATA SET##

#Appending original data set
results_final<-election_results%>%
  mutate(R.D.Victory.Margin=
           ifelse(is.na(R.D.Victory.Margin),
                  implementation_predictions$.pred,
                  R.D.Victory.Margin))
#Downloading as csv
write.csv(results_final,"results_final.csv",row.names=FALSE)






