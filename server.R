library(shiny)
library(shinydashboard)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(plotly)
library(Hmisc)
library(lme4)
library(markdown)
library(merTools)

# ############ Data Manipulation
# 
# business_raw <- stream_in(file("yelp_academic_dataset_business.json"))
# checkin_raw <- stream_in(file("yelp_academic_dataset_checkin.json"))
# 
# checkin_select <- data.frame(business_id=checkin_raw$business_id,
#                              work_lunch=checkin_raw$checkin_info$`11-3`,
#                              work_dinner=checkin_raw$checkin_info$`19-3`,
#                              weekend_lunch=checkin_raw$checkin_info$`11-6`,
#                              weeked_dinner=checkin_raw$checkin_info$`19-6`)
# 
# checkin_select[is.na(checkin_select)] <- 0
# 
# business_select <- business_raw[,c("business_id","full_address","categories","city","review_count","name","neighborhoods","longitude","state","stars","latitude")]
# 
# business_select <- data.frame(business_select,
#                               reservation=business_raw$attributes$`Takes Reservations`,
#                               parking_garage=business_raw$attributes$Parking$garage,
#                               parking_street=business_raw$attributes$Parking$street,
#                               parking_valid=business_raw$attributes$Parking$validated,
#                               parking_lot=business_raw$attributes$Parking$lot,
#                               parking_valet=business_raw$attributes$Parking$valet,
#                               price_range=business_raw$attributes$`Price Range`,
#                               good_for_lunch=business_raw$attributes$`Good For`$lunch,
#                               good_for_dinner=business_raw$attributes$`Good For`$dinner,
#                               good_for_brunch=business_raw$attributes$`Good For`$brunch,
#                               group=business_raw$attributes$`Good For Groups`)
# 
# poi_data <- inner_join(business_select,checkin_select,by=c("business_id"="business_id"))
# 
# ####exclude outside the mainland
 # poi_data <- poi_data[poi_data$longitude< -5,]
 # state_freq <- data.frame(table(poi_data$state))
 # poi_data <- poi_data[!poi_data$state %in% state_freq[state_freq$Freq < 10,"Var1"],]
 # poi_data <- na.omit(poi_data)
 # set.seed(1226)
 # sample = sample(1:nrow(poi_data),1000)
 # poi_data <- poi_data[sample,]


shinyServer(function(input, output) {
   output$execu_sum = renderText({ "Executive Summary" })
   
   output$title = renderText({"Potential Factors Affecting Yelp Check-Ins: How to Improve Yelp Check-Ins?"})
   
   output$summary = renderText({"The relationship between the number of Check-Ins per hour with business features - 
reviews, reservation, group friendly, star ratings and price range - was measured according to time using Poisson 
Generalized Linear Mixed Models. The effect of star ratings varies by states for lunchtime and dinnertime in workdays and
weekends. "})
   
   output$DesData = renderText({"Description of Data"})
   
   output$des_data_1 = renderText({"The data used in this analysis is from Yelp's database containing information 
about businesses and Check-Ins from 61049 businesses. In order to demonstrate the result in Shiny app, I randomly selected
1000 samples from the total dataset. Check-Ins data provides the number of Check-Ins in each hour of the day. Check-Ins 
in four types of mealtime were defined according to the following list:"})
   
   output$des_data_bullet = renderUI({
     HTML("<ul>
     <li>workday lunchtime: Wednesday 11:00-12:00</li>
     <li>workday dinnertime: Wednesday 19:00-20:00</li>
     <li>weekend lunchtime: Saturday 11:00-12:00</li>
     <li>weekend dinnertime: Saturday 19:00-20:00</li>
          </ul>") 
   }) 
  
   output$cmap_title = renderText({"Check-In Map"})
   
   output$checkin_map = renderPlotly({
     # checkin_state <- data.frame(state=unique(poi_data$state),work_lunch=tapply(poi_data$work_lunch,poi_data$state,mean),
     #                             work_dinner=tapply(poi_data$work_dinner,poi_data$state,mean),
     #                             weekend_lunch=tapply(poi_data$weekend_lunch,poi_data$state,mean),
     #                             weekend_dinner=tapply(poi_data$weekend_dinner,poi_data$state,mean))
     #  
     #  checkin_state <- gather(checkin_state,time,checkin,-state)
     #  stat_lon_lat <- read.csv("stat_lon_lat.csv")
     #  checkin_state <- left_join(checkin_state,stat_lon_lat[,c(1,2)],by=c("state"="abb"))
     #  checkin_state <- na.omit(checkin_state)
     #  colnames(checkin_state)[4] <- "region"
     #  checkin_state <- checkin_state[!checkin_state$region=="alaska",]
     #  
     #  timelist = list("work_lunch"="Workday Lunch","work_dinner"="Workday Dinner","weekend_lunch"="Weekend Lunch","weekend_dinner"="Weekend Dinner")
     #  
     #  checkin_state$checkin = round(checkin_state$checkin,4)
     #  checkin_state$num_busi = apply(checkin_state,1, function(x) length(poi_data$business_id[poi_data$state==x[1]]))
     #  checkin_state$hover <- apply(checkin_state,1,function(x) paste(capitalize(x[4]),'<br>',"Time:",timelist[[x[2]]],"<br>","Number of Businesses:",x[5],"<br>","Average Check-Ins:",x[3]))
     #  
     #  l <- list(color = toRGB("white"), width = 2)
     #  
     #  g <- list(
     #    scope = 'usa',
     #    projection = list(type = 'albers usa'),
     #    showlakes = TRUE,
     #    lakecolor = toRGB('white')
     #  )
     #    save(checkin_state,g,file = "checkin_map.rdata")
     load("checkin_map.rdata")
     
     p <- plot_geo(checkin_state[checkin_state$time==input$checkmap_time,], locationmode = 'USA-states') %>%
       add_trace(z = ~checkin, text = ~hover, locations = ~state,
         color = ~checkin, colors = c("grey","darkred")) %>%
       colorbar(title = "Average Check-Ins") %>%
       layout(geo = g)
     
     p

   })
   
   output$cscat_title <- renderText({"Check-In vs. Stars Jitter Plot"})
   output$scat_checkin <- renderPlotly({
     
      # poi_data$hover_cmap = apply(poi_data,1,function(x) paste(x[6],"<br>","Categories:",x[3][[1]][1],"<br>","City:",x[4],"<br>",
      #                                                             "Stars:",x[10],"<br>","Price:",x[18]))
      # 
      # work_lunch = ggplot(poi_data,aes(x=stars,y=work_lunch,size=price_range,color=price_range,text=hover_cmap)) +
      #   geom_jitter(alpha=0.5,width = 0.3) + scale_color_continuous(low='#D66D75', high='#6f0000', guide =  F)  +
      #   ggtitle(paste0("Check-Ins on Workday Lunchtime"))+
      #   guides(size=guide_legend(title="Price Range")) + ylab("Check-Ins")
      # 
      # work_dinner = ggplot(poi_data,aes(x=stars,y=work_dinner,size=price_range,color=price_range,text=hover_cmap)) +
      #   geom_jitter(alpha=0.5,width = 0.3) + scale_color_continuous(low='#D66D75', high='#6f0000', guide =  F)  +
      #   ggtitle(paste0("Check-Ins on Workday Lunchtime"))+
      #   guides(size=guide_legend(title="Price Range")) + ylab("Check-Ins")
      # 
      # weekend_lunch = ggplot(poi_data,aes(x=stars,y=weekend_lunch,size=price_range,color=price_range,text=hover_cmap)) +
      #   geom_jitter(alpha=0.5,width = 0.3) + scale_color_continuous(low='#D66D75', high='#6f0000', guide =  F)  +
      #   ggtitle(paste0("Check-Ins on Workday Lunchtime"))+
      #   guides(size=guide_legend(title="Price Range")) + ylab("Check-Ins")
      # 
      # weekend_dinner = ggplot(poi_data,aes(x=stars,y=weekend_dinner,size=price_range,color=price_range,text=hover_cmap)) +
      #   geom_jitter(alpha=0.5,width = 0.3) + scale_color_continuous(low='#D66D75', high='#6f0000', guide =  F)  +
      #   ggtitle(paste0("Check-Ins on Workday Lunchtime"))+
      #   guides(size=guide_legend(title="Price Range")) + ylab("Check-Ins")
      # 
      # j_list = list("work_lunch"=work_lunch,"work_dinner"=work_dinner,"weekend_lunch"=weekend_lunch,"weekend_dinner"=weekend_dinner)
      # save(j_list,file = "scat_checkin.rdata")
     load("scat_checkin.rdata")
     
     ggplotly(j_list[[input$scat_checkin_time]],tooltip='text')
     
     
   })
   output$sum_checkin_title <- renderText({"Summary: Check-Ins"})
   output$sum_checkin <- renderUI({
     HTML("In this sample, the data mostly comes from businesses in Arizona and Nevada.
 According to plots above, we can conclude that:
          <ul>
          <li>Customers in Wisconsin averagely check-in more than customers in other states.</li>
          <li>People are more likely to check-in at lunch than dinner in weekends, while it's opposite in weekdays.</li>
          <li>Four-star restaurants averagely have more Check-Ins than others.</li>
          <li>Since the size of points in jitter plots represents price range, we can see that popular businesses
              on workday lunchtime have different price ranges, while in other time hot places generally
              have the price range from 2 to 3.</li>
          </ul>") 
   }) 
   
   output$s_title <- renderText({"Bar Chart - Star Rating"})
   output$s_summary <- renderText({"Proportions of star rating are similar among states."})
   
   output$star <- renderPlotly({
     # star_state <- as.data.frame(table(poi_data$stars,poi_data$state))
     # star_state <- left_join(star_state,stat_lon_lat,by=c("Var2"="abb"))
     # star_state <- na.omit(star_state)
     # colnames(star_state)[c(1,2,4)] <- c("star","state","region")
     # star_state <- star_state[!star_state$region=="alaska",]
     # save(star_state,file = "star_state.rdata")
     load("star_state.rdata")
     
     ggplotly(ggplot(star_state) + aes(x=state,fill=star,y=Freq) + geom_bar(stat = "identity")+ scale_fill_brewer(palette = "Reds"))
     
     
   })
   
   output$p_title <- renderText({"Bar Chart - Price Range"})
   output$p_summary <- renderText({"Restaurants with price range $$ are the majority for every state in the dataset."})
   
   output$price <- renderPlotly({
     
     # price_state <- as.data.frame(table(poi_data$price_range,poi_data$state))
     # price_state <- left_join(price_state,stat_lon_lat,by=c("Var2"="abb"))
     # price_state <- na.omit(price_state)
     # colnames(price_state)[c(1,2,4)] <- c("price","state","region")
     # price_state <- price_state[!price_state$region=="alaska",]
     # save(price_state,file = "price_state.rdata")
     load("price_state.rdata")
     
     ggplotly(ggplot(price_state) + aes(x=state,fill=price,y=Freq) + geom_bar(stat = "identity")+ scale_fill_brewer(palette = "Reds"))
     
     
   })
   
   output$anova_title <- renderText({"ANOVA for Model Selection"})
   
   output$anova_text <- renderUI({
     HTML("I tried three Poisson Generalized Mixed Models and used ANOVA to select the best. Following variables were used in all models. <br>
          <b>Response variable:</b> Number of Check-Ins in 1 hour <br>
          <b>Independent variables:</b>
          <ul>
          <li>number of reviews</li>
          <li>indicator for accepting reservation</li>
          <li>indicator for group-friendly restaurants</li>
          <li>number of stars</li>
          <li>price range</li>
          </ul>
          <b>Models:</b>") 
   }) 
   
   output$model1 <- renderUI({
     withMathJax(sprintf('Model 1: Allowing slops of Stars to vary across states
                          $$Check-In=\\beta_0 + \\beta_1 ReviewCount + \\beta_2 Reservation +
                          \\beta_3 Group-Friendly + \\beta_{4[j]} Stars + \\beta_5 Price$$ \
                          Model 2: Allowing intercepts to vary across states
                          $$Check-In=\\beta_{0[j]} + \\beta_1 ReviewCount + \\beta_2 Reservation +
                          \\beta_3 Group-Friendly + \\beta_4 Stars + \\beta_5 Price$$ \
                          Model 3: Allowing both intercepts and slops of Stars to vary across states
                          $$Check-In=\\beta_{0[j]} + \\beta_1 ReviewCount + \\beta_2 Reservation +
                          \\beta_3 Group-Friendly + \\beta_{4[j]} Stars + \\beta_5 Price$$'))
   })
   
   output$anova_decision <- renderUI({
     HTML("
          According to results of Analysis of Variance, the best model should inculde both fixed effect and random effect of stars on Check-Ins 
          as well as different intercepts over states -- Model 3.
          <br>
          <br>")
   })
   
   output$anova_subtitle <- renderUI({
     if(input$anova_checkbox){
     HTML("<b>Results from ANOVA:</b>")}
   })
   
   output$anova1 <- renderPrint({
     if(input$anova_checkbox){
     # lmm_wol <- glmer(work_lunch ~ scale(review_count) + reservation + group + stars + price_range + (1 + stars |state),
     #                 data = poi_data,family = poisson, na.action=na.omit)
     # lmm_wol_1 <- glmer(work_lunch ~ scale(review_count) + reservation + group + price_range + (1 + stars |state),
     #                   data = poi_data,family = poisson, na.action=na.omit)
     # lmm_wol_2 <- glmer(work_lunch ~ scale(review_count) + reservation + group + stars + price_range + (0 + stars |state),
     #                    data = poi_data,family = poisson, na.action=na.omit)
     # 
     # anova_wol = anova(lmm_wol,lmm_wol_1,lmm_wol_2)
     # save(anova_wol,file = "anova1.rdata")
     
     load("anova1.rdata")
     anova_wol}
   })

   output$anova2 <- renderPrint({
     if(input$anova_checkbox){
      # lmm_wod <- glmer(work_dinner ~ scale(review_count) + reservation + group + stars + price_range + (1 + stars |state),
      #                   data = poi_data,family = poisson, na.action=na.omit)
      # lmm_wod_1 <- glmer(work_dinner ~ scale(review_count) + reservation + group + price_range + (1 + stars |state),
      #                     data = poi_data,family = poisson, na.action=na.omit)
      # lmm_wod_2 <- glmer(work_dinner ~ scale(review_count) + reservation + group + stars + price_range + (0 + stars |state),
      #                     data = poi_data,family = poisson, na.action=na.omit)
      # 
      # anova_wod = anova(lmm_wod,lmm_wod_1,lmm_wod_2)
      # save(anova_wod,file = "anova2.rdata")
      
     load("anova2.rdata")
     anova_wod}
   })
   output$anova3 <- renderPrint({
     if(input$anova_checkbox){
      # lmm_wel <- glmer(weekend_lunch ~ scale(review_count) + reservation + group + stars + price_range + (1 + stars |state),
      #                   data = poi_data,family = poisson, na.action=na.omit)
      # lmm_wel_1 <- glmer(weekend_lunch ~ scale(review_count) + reservation + group + price_range + (1 + stars |state),
      #                     data = poi_data,family = poisson, na.action=na.omit)
      # lmm_wel_2 <- glmer(weekend_lunch ~ scale(review_count) + reservation + group + stars + price_range + (0 + stars |state),
      #                     data = poi_data,family = poisson, na.action=na.omit)
      # 
      # anova_wel = anova(lmm_wel,lmm_wel_1,lmm_wel_2)
      # save(anova_wel,file = "anova3.rdata")
     
     load("anova3.rdata")
     anova_wel}
   })
   output$anova4 <- renderPrint({
     if(input$anova_checkbox){
      #lmm_wed <- glmer(weekend_dinner ~ scale(review_count) + reservation + group + stars + price_range + (1 + stars |state),
      #                  data = poi_data,family = poisson, na.action=na.omit)
      # lmm_wed_1 <- glmer(weekend_dinner ~ scale(review_count) + reservation + group + price_range + (1 + stars |state),
      #                     data = poi_data,family = poisson, na.action=na.omit)
      # lmm_wed_2 <- glmer(weekend_dinner ~ scale(review_count) + reservation + group + stars + price_range + (0 + stars |state),
      #                     data = poi_data,family = poisson, na.action=na.omit)
      # 
      # anova_wed = anova(lmm_wed,lmm_wed_1,lmm_wed_2)
      # save(anova_wed,file="anova4.rdata")
     
     load("anova4.rdata")
     anova_wed}
   })
   
   output$m_title <- renderText({"Modeling and Analysis"})
   
   output$coefp_subtitle <- renderText({"<ul style='list-style-type:square'><li>Result: Potential Factors - Coefficients Plot</li></ul>"})
   output$coefp <- renderPlotly({
     
     # pf_fun <- function(model,name){
     #   pf = data.frame(summary(model)$coefficient[-1,1:2],modelName=rep(name))
     #   pf$variable = rownames(pf)
     #   return(pf)
     # }
     # 
     # pf = rbind(pf_fun(lmm_wol,"Workday Lunch"),
     #            pf_fun(lmm_wod,"Workday Dinner"),
     #            pf_fun(lmm_wel,"Weekend Lunch"),
     #            pf_fun(lmm_wed,"Weekend Dinner"))
     # save(pf,file = "potfac.rdata")
     
     load("potfac.rdata")
     
     ggplotly(ggplot(pf, aes(colour = modelName)) + 
                geom_hline(yintercept = 0, colour = "red", lty = 2)+
                geom_linerange(aes(x = variable, ymin = Estimate - Std..Error*(-qnorm((1-0.9)/2)),
                                   ymax = Estimate + Std..Error*(-qnorm((1-0.9)/2))),
                               lwd = 1, position = position_dodge(width = 0.7))+
                geom_pointrange(aes(x = variable, y = Estimate, ymin = Estimate - Std..Error*(-qnorm((1-0.95)/2)),
                                    ymax = Estimate + Std..Error*(-qnorm((1-0.95)/2))),
                                lwd = 1/2, position = position_dodge(width = 0.7),
                                shape = 21, fill = "WHITE")+
                theme_bw()+ggtitle("Comparing Effects of Potential Factors on Check-Ins"),tooltip = "y")
     
   })
   
   output$m_subtitle <- renderUI({HTML("<ul style='list-style-type:square'> <li> Result Interpretation: Potential Factors Affecting Check-Ins</li></ul>")})
   
   output$m_text <- renderUI({
HTML("<ul> 

<li>  <b>Customers are more likely to Check-In when the restaurant is group friendly especially in the night.</b> 
Group-friendly restaurants have 29.00%, 46.55%, 36.20% and 41.58% more resepectively.</li>

<li>  <b>More expensive restaurants averagely have more Check-Ins except at workday lunch.</b> 
Restaurants that are one-level more expensive averagely receive 16.61% less, 32.54%, 19.22% and 21.78% more Check-Ins resepectively. </li>
                                   
<li>  <b>Restaurants accepting reservation have more Check-Ins in dinnertime while less in lunchtime.</b> 
They averagely received 4.72%(workday) and 29.22%(weekend) more Check-Ins at dinner, while they had 53.34%(workday) 
and 66.45%(weekend) less Check-Ins at lunch compared with those do not accept reservation.</li>
     
<li>  <b>Restaurants with more reviews averagely have more Check-Ins.</b> 
An increase of one standard deviation of review counts from the average number of reviews will lead to
29.75%, 20.44%, 33.94% and 19.90% increase respectively in Check-Ins.</li>

<li>  <b>Restaurants with more stars averagely have more Check-Ins.</b> 
Plots above shows the fixed effect of starts on Check-Ins. Increasing starts by 1 level (0.5 stars) lead 
to an increase of 29.75%, -8.27%, 52.70%, 1.94% Check-Ins respectively.</li>
                                   
</ul>")})
   
   
   output$me_coefp_subtitle <- renderText({"<ul style='list-style-type:square'><li>Result: Mixed Effects of Stars - Coefficients Plot</li></ul>"})
   
   output$me_coefp <- renderPlot({
     
     # random_fun = function(model,name){
     #   ran = as.data.frame(REsim(model))
     #   fix = summary(model)$coefficients[c(1,5),c(1,2)]
     #   eff = ran[,1:3]
     #   eff$modelName = rep(name)
     #   eff$mean = ifelse(eff$term=="stars",ran$mean + fix["stars",1],ran$mean + fix["(Intercept)",1])
     #   eff$sd = ifelse(eff$term=="stars",
     #                   sqrt((ran$sd)^2 + (fix["stars",2])^2),
     #                   sqrt((ran$sd)^2 + (fix["(Intercept)",2])^2))
     #   return(eff)
     # }
     # effect = rbind(random_fun(lmm_wol,"Workday Lunch"),
     #                random_fun(lmm_wod,"Workday Dinner"),
     #                random_fun(lmm_wel,"Weekend Lunch"),
     #                random_fun(lmm_wed,"Weekend Dinner"))
     # save(effect,file = "effect_plot.rdata")
     
     load("effect_plot.rdata")
     load("checkin_map.rdata")
     
     ggplot(effect[effect$term=="stars"&effect$groupID%in%checkin_state$state,], aes(colour = modelName)) + 
       geom_hline(yintercept = 0, colour = "red", lty = 2)+
       geom_linerange(aes(x = groupID, ymin = mean - sd*(-qnorm((1-0.9)/2)),
                          ymax = mean + sd*(-qnorm((1-0.9)/2))),
                      lwd = 1, position = position_dodge(width = 0.7))+
       geom_pointrange(aes(x = groupID, y = mean, ymin = mean - sd*(-qnorm((1-0.95)/2)),
                           ymax = mean + sd*(-qnorm((1-0.95)/2))),
                       lwd = 1/2, position = position_dodge(width = 0.7),
                       shape = 21, fill = "WHITE")+
       theme_bw()+ggtitle("Comparing Effects of Stars across States")
   })
   
   output$m_subtitle2 <- renderText({
     "<ul style='list-style-type:square'> <li>Result Interpretation: Effects of Stars across States</li></ul>"
   })
   
   output$m_text2 <- renderUI({
     HTML("Plots above shows the effects of stars on Check-Ins including fixed effects and random effects.
          <ul>
          <li>Generally, the effects of stars are significantly positive at lunch and largest at weekend lunch.</li>
          <li>The average effects at dinner are only positive in Arizona and Nevada but not statistically significant in other states.</li>
          </ul>")
   })
   
} )
