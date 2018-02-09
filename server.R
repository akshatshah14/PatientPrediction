#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/


library(shiny)



#############################################################################################################
#No show arrival repeated visits
no_show_arrival_repeat = readRDS(file = "Arr - noshow RPV.rds")
myvars <- c("Outcome", "DistanceToClinic","SEX","AGE","Month","DURATION","REG.FSC.1","SCH.PROV.CATEGORY.1")
no_show_arrival_repeat <- no_show_arrival_repeat[myvars]
no_show_arrival_repeat$SEX = as.factor(no_show_arrival_repeat$SEX)
no_show_arrival_repeat$REG.FSC.1 = as.factor(no_show_arrival_repeat$REG.FSC.1)
no_show_arrival_repeat$SCH.PROV.CATEGORY.1 = as.factor(no_show_arrival_repeat$SCH.PROV.CATEGORY.1)
no_show_arrival_repeat$Outcome = as.factor(no_show_arrival_repeat$Outcome)
summary(no_show_arrival_repeat)


Records$DistanceToClinic = as.integer(Records$DistanceToClinic)
Records$Month = as.factor(Records$Month)
Records$SEX = as.factor(Records$SEX)
Records$REG.FSC.1 = as.factor(Records$REG.FSC.1)
Records$SCH.PROV.CATEGORY.1 = as.factor(Records$SCH.PROV.CATEGORY.1)
Records$Outcome = as.factor(Records$Outcome)

set.seed(5000)

samples_no_show_repeat  <- sample(nrow(no_show_arrival_repeat),as.integer(nrow(no_show_arrival_repeat)*0.75))
train.no_show_arrival_repeat = no_show_arrival_repeat[samples_no_show_repeat,]
test.no_show_arrival_repeat  = no_show_arrival_repeat[-samples_no_show_repeat,]


model_test_noshow_response = glm(Outcome ~ DistanceToClinic + SEX + DURATION + AGE + Month + REG.FSC.1 + SCH.PROV.CATEGORY.1, data = train.no_show_arrival_repeat , family = "binomial" )
pred1_noshow_Arr_Repeat = predict(model_test_noshow_response,test.no_show_arrival_repeat,type="response")
pred1_noshow_Arr_Repeat = ifelse(pred1_noshow_Arr_Repeat<0.5,0,1)
accuracy_75_noshow_arr_repeat = sum(diag(table(test.no_show_arrival_repeat$Outcome,pred1_noshow_Arr_Repeat)))/nrow(test.no_show_arrival_repeat)


#############################################################################################################

#Can arrival repeated visits
can_arrival_repeat = readRDS(file="Arr - CAN RPV.rds")
can_repeat_vars <- c("Outcome", "DistanceToClinic","AGE","MARITAL","DURATION","SCH.PROV.CATEGORY.1")
can_arrival_repeat <- can_arrival_repeat[can_repeat_vars]
can_arrival_repeat$SCH.PROV.CATEGORY.1 = as.factor(can_arrival_repeat$SCH.PROV.CATEGORY.1)
can_arrival_repeat$Outcome = as.factor(can_arrival_repeat$Outcome)
summary(can_arrival_repeat)

set.seed(5000)

samples_can_repeat  <- sample(nrow(can_arrival_repeat),as.integer(nrow(can_arrival_repeat)*0.75))
train.can_arrival_repeat = can_arrival_repeat[samples_can_repeat,]
test.can_arrival_repeat  = can_arrival_repeat[-samples_can_repeat,]


model_can_repeat = glm(Outcome ~ DistanceToClinic + DURATION + AGE + MARITAL + SCH.PROV.CATEGORY.1, data = train.can_arrival_repeat , family = "binomial" )
pred1_can_repeat = predict(model_can_repeat,test.can_arrival_repeat,type="response")
pred1_can_repeat = ifelse(pred1_can_repeat<0.5,0,1)
accuracy_75_can_repeat = sum(diag(table(test.can_arrival_repeat$Outcome,pred1_can_repeat)))/nrow(test.can_arrival_repeat)


#############################################################################################################
#Can arrival initial visits
can_arrival_initial = readRDS(file="Arr- Can IPV.rds")
can_initial_vars <- c("Outcome", "DistanceToClinic","AGE","Month","DURATION")
can_arrival_initial <- can_arrival_initial[can_initial_vars]
can_arrival_initial$Month = as.factor(can_arrival_initial$Month)
can_arrival_initial$Outcome = as.factor(can_arrival_initial$Outcome)
summary(can_arrival_initial)

set.seed(5000)

samples_can_initial  <- sample(nrow(can_arrival_initial),as.integer(nrow(can_arrival_initial)*0.75))
train.can_arrival_initial = can_arrival_initial[samples_can_initial,]
test.can_arrival_initial  = can_arrival_initial[-samples_can_initial,]


model_can_initial = glm(Outcome ~ DistanceToClinic + DURATION + AGE + Month, data = train.can_arrival_initial , family = "binomial" )
pred1_can_initial = predict(model_can_initial,test.can_arrival_initial,type="response")
pred1_can_initial = ifelse(pred1_can_initial<0.5,0,1)
accuracy_75_can_initial = sum(diag(table(test.can_arrival_initial$Outcome,pred1_can_initial)))/nrow(test.can_arrival_initial)


#############################################################################################################

noshow_arrival_initial = readRDS(file="Noshow- Arrival IPV.rds")
noshow_initial_vars <- c("Outcome","REG.FSC.1", "Month","DistanceToClinic","AGE","MARITAL","DURATION","SCH.PROV.CATEGORY.1","SEX")
noshow_arrival_initial <- noshow_arrival_initial[noshow_initial_vars]
noshow_arrival_initial$SCH.PROV.CATEGORY.1 = as.factor(noshow_arrival_initial$SCH.PROV.CATEGORY.1)
noshow_arrival_initial$Outcome = as.factor(noshow_arrival_initial$Outcome)
summary(noshow_arrival_initial)

set.seed(5000)

samples_noshow_arr_initial  <- sample(nrow(noshow_arrival_initial),as.integer(nrow(noshow_arrival_initial)*0.75))
train.noshow_arr_initial = noshow_arrival_initial[samples_noshow_arr_initial,]
test.noshow_arr_initial  = noshow_arrival_initial[-samples_noshow_arr_initial,]

model_noshow_arr_initial = glm(Outcome ~ DistanceToClinic + REG.FSC.1 + Month + SEX + DURATION + AGE + MARITAL + SCH.PROV.CATEGORY.1, data = train.noshow_arr_initial , family = "binomial" )
pred1_noshow_arr_initial = predict(model_noshow_arr_initial,test.noshow_arr_initial,type="response")
pred1_noshow_arr_initial = ifelse(pred1_noshow_arr_initial<0.5,0,1)
accuracy_75_noshow_arr_initial = sum(diag(table(test.noshow_arr_initial$Outcome,pred1_noshow_arr_initial)))/nrow(test.noshow_arr_initial)

############################################################################################################



can_arr_init <- function(can_ini) {
  pred = predict(model_can_initial,can_ini,type="response") 
  print(pred)
}

arr_noshow = function(no_show_repeat){
  pred = predict(model_test_noshow_response, no_show_repeat, type="response")
  print(pred)
}

can_arr_repeat = function(can_repeat){
  pred = predict(model_can_repeat, can_repeat, type="response")
  print(pred)
}
noshow_initial = function(noshow_ini){
  pred = predict(model_noshow_arr_initial, noshow_ini, type="response")
  print(pred)
}




server = function(input, output) {
  

observeEvent(input$no_Show_Repeat,{
  
  
  arr_no= data.frame(AGE= input$aGE,DistanceToClinic=input$distanceToclinic,SEX = input$sEX,Month = input$month,DURATION=input$dURATION,REG.FSC.1 = input$rEG_FSC_1,SCH.PROV.CATEGORY.1 = input$sCH_PROV_CATEGORY_1)
  if(arr_noshow(arr_no) < 0.5)
  {
    
    insertUI(
      selector = "#no_show_repeat",
      where = "afterEnd",
      ui = h2("Patient not showing it to the appointment")
      
    )
}else{
    insertUI(
      selector = "#no_show_repeat",
      where = "afterEnd",
      ui = h2("Patient will show up for the appointment")
      
      )
}
})



observeEvent(input$noshow_initial_initial,{
  
  
  noshow_ini= data.frame(AGE= input$noshow_initial_age, DistanceToClinic=input$noshow_initial_distancetoclinic, SEX = input$noshow_initial_sEX, Month = input$noshow_initial_month, DURATION=input$noshow_initial_dURATION, REG.FSC.1 = input$noshow_initial_rEG_FSC_1, SCH.PROV.CATEGORY.1 = input$noshow_initial_sCH_PROV_CATEGORY_1, MARITAL = input$noshow_initial_MARITAL)
  if(noshow_initial(noshow_ini) < 0.5)
  {
    
    insertUI(
      selector = "#noshow_arrival_initial",
      where = "afterEnd",
      ui = h2("Patient not showing it to the appointment")
     
    )
  }else{
    insertUI(
      selector = "#noshow_arrival_initial",
      where = "afterEnd",
      ui = h2("Patient will show up for the appointment")
     
    )
  }
  
})

output$noshow_arrival_initial_forecast= renderText({
  
})

observeEvent(input$can_repeat_Repeat,{
  
  
  can_rep= data.frame(AGE= input$can_repeat_age,DistanceToClinic=input$can_repeat_distanceToclinic,DURATION=input$can_repeat_dURATION,SCH.PROV.CATEGORY.1 = input$can_repeat_sCH_PROV_CATEGORY_1, MARITAL = input$can_repeat_MARITAL)
  if(can_arr_repeat(can_rep) < 0.5)
  {
    
    insertUI(
      selector = "#can_repeat",
      where = "afterEnd",
      ui = h2("Patient cancelling his appointment")
    )
  }else{
    insertUI(
      selector = "#can_repeat",
      where = "afterEnd",
      ui = h2("Patient will show up for the appointment")
    )
  }
})



output$can_repeat_forecast= renderText({
  
})

output$no_show_repeat_forecast= renderText({
  
})

output$can_initial_forecast= renderText({
  
})
observeEvent(input$can_initial,{
  
  
  can_ini= data.frame(AGE = input$can_initial_age, DistanceToClinic = input$can_initial_distancetoclinic, Month = input$can_initial_month, DURATION = input$can_initial_dURATION)
  if(can_arr_init(can_ini) < 0.5)
  {
    
    insertUI(
      selector = "#can_initial_init",
      where = "afterEnd",
      ui = h2("Patient not showing it to the appointment")
    )
  }else{
    insertUI(
      selector = "#can_initial_init",
      where = "afterEnd",
      ui = h2("Patient will show up for the appointment")
    )
  }
})



output$forecast = renderText({
  
})



##################################################
output$visualization = renderImage({
  if(input$visualization == "SEX"){
      return(list(
        src = "Gender.png",
        contentType = "image/png",
        alt = "Face"
      ))}
 
  if(input$visualization == "DistanceToClinic"){
    return(list(
      src = "Distance.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
  if(input$visualization == "Month"){
    return(list(
      src = "Month.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  if(input$visualization == "DURATION"){
    return(list(
      src = "Appointment.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
  if(input$visualization == "AGE"){
    return(list(
      src = "Age.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
  if(input$visualization == "REG.FSC.1"){
    return(list(
      src = "Reg.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  if(input$visualization == "SCH.PROV.CATEGORY.1"){
    return(list(
      src = "Schedule.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  if(input$visualization == "MARITAL"){
    return(list(
      src = "Relationship.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
}, deleteFile = FALSE)  

 
output$repeat_visualization = renderImage({
  if(input$repeat_visualization == "SEX"){
    return(list(
      src = "Gender_2.png",
      contentType = "image/png",
      alt = "Face"
    ))}
  
  if(input$repeat_visualization == "DistanceToClinic"){
    return(list(
      src = "Distance_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
  if(input$repeat_visualization == "Month"){
    return(list(
      src = "Month_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  if(input$repeat_visualization == "DURATION"){
    return(list(
      src = "Duration_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
  if(input$repeat_visualization == "AGE"){
    return(list(
      src = "Age_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
  if(input$repeat_visualization == "REG.FSC.1"){
    return(list(
      src = "Reg_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  if(input$repeat_visualization == "SCH.PROV.CATEGORY.1"){
    return(list(
      src = "Sch_Provider_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  if(input$repeat_visualization == "MARITAL"){
    return(list(
      src = "Relationship_2.png",
      contentType = "image/png",
      alt = "Face"
    ))
  }
  
}, deleteFile = FALSE)  



}

   



