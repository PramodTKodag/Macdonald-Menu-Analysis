library(shiny)
library(shinydashboard)
library(plotrix)
library(ggthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(magrittr)
library(forecast)
library(ggplot2)
library(tidyr)
library(rlang)
library(repr)
require(PerformanceAnalytics)
library(reshape)
library(plyr)
library(dplyr)
require(data.table)
require(DT)
require(D3partitionR)

library(corrplot)

require(D3partitionR)
require(magrittr)
require(shinyWidgets)
require(ggplot2)
require(stringr)
library(rpart)
require(PerformanceAnalytics)
library(magrittr)
library(ggrepel)
library(stringr)

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(RColorBrewer))
suppressMessages(library(devtools))
suppressMessages(library(yarrr))
suppressMessages(library(tidyr))
suppressMessages(library(gridExtra))
suppressMessages(library(viridis))

library(plotly)
packageVersion('plotly')

dataMc=fread('menu.csv')


my_palette <- c("#00abbd", "#055499", "#132241", "#9bca3c", "#c3c3c3","#ff5a00", "#e91365", "#ff921f","#cc0000")







df1 <- data.frame(gp=gl(20,5), dt=seq(1:100))
ts1 <- ts((df1%>%filter(gp==2))$dt)

mc_menu = read.csv("C:/Users/pramod/Desktop/IIFF/BID_project/nutrition-facts-for-mcdonald-s-menu/menu.csv", header = T, sep = ",")

table(mc_menu$Category)
table(mc_menu$Item)
names(mc_menu)
str(mc_menu)

shinyServer(function(input,output){
  
  vals<-reactiveValues(switch_origin_cal=F)
  ###First tab, menu selection
  output$dish_type_selection<-renderUI(
    selectizeInput('dish_type_selection','Select dish type',c('All',unique(dataMc[['Category']])),multiple=T,selected='All')
  )
  
  output$dish_selection<-renderUI(
    selectizeInput('dish_selection','Add a dish',split(dataMc[Category%in%input$dish_type_selection | input$dish_type_selection=='All']$Item,dataMc[Category%in%input$dish_type_selection | input$dish_type_selection=='All',Category]),
                   multiple=T,selected=dataMc[Category%in%input$dish_type_selection | input$dish_type_selection=='All']$Item[sample(1:260,5)])
  )
  
  output$selected_items<-renderDataTable(
    {
      dataMc[Item%in%input$dish_selection,colnames(dataMc)[which(!colnames(dataMc)%like%'Daily Value')],with=F]
      
    },
    options=list(scrollX=T,dom='t'),rownames= T
    
  )
  
  total_fat = 0
  satu_fat = 0
  
  
  output$text2 <- renderText({
    
    test2 <- dataMc[Item%in%input$dish_selection,colnames(dataMc)[which(!colnames(dataMc)%like%'Daily Value')],with=F]
    print(test2)
    total_fat <- sum(test2[,"TotalFat"], na.rm = TRUE)
    satu_fat <- sum(test2[,"Saturated Fat"], na.rm = TRUE)
    coles <- sum(test2[,"Cholesterol"], na.rm = TRUE)
    sod <- sum(test2[,"Sodium"], na.rm = TRUE)
    carb <- sum(test2[,"Carbohydrates"], na.rm = TRUE)
    print(total_fat)
    print(satu_fat)
    print(coles)
    print(sod)
    print(carb)
    if(total_fat<65)
    {
      print("Total fat below level")
      ttl_fat= "Total Fat below level, "
    }
    else
    {
      print("Total fat Above level")
      ttl_fat= "Total Fat above level, "
    }
    
    if(satu_fat<20)
    {
      print("Total Saturated fat below level")
      sat_fat = "Saturated Fat below level, "
     
    }
    else
    {
      print("Total Saturated fat Above level")
      sat_fat = "Saturated Fat above level, "
      
    }
    
    
    if(sod<2400)
    {
      print("Total Sodium below level")
      sodi = "Sodium below level, "
      
    }
    else
    {
      print("Total Sodium Above level")
      sodi = "Sodium above level, "
      
    }
    
    if(coles<300)
    {
      print("Total Cholestrol below level")
      colest = "Cholestrol below level, "
      
    }
    else
    {
      print("Total Cholestrol Above level")
      colest = "Cholestrol above level, "
      
    }
    
    if(carb<300)
    {
      print("Total Cholestrol below level")
      carbo = "Carbohydrates below level, "
      
    }
    else
    {
      print("Total Carbohydrates Above level")
      carbo = "Carbohydrates above level, "
      
    }
    
    
    # if (total_fat<65 && satu_fat<20)
    # {
    #   print("Everything is under control")
    #   alert = "Everything is under control"
    # }
    # if(total_fat>65 || satu_fat>20)
    # {
    #   print("Dengerous")
    #   alert = "Dengerous <br>"
    # }
    paste(ttl_fat,sat_fat,colest,sodi, carbo)
  })
  
  
  
  output$barplot <- renderPlot({
   
    
    #######################################################
    # PIVORT TABLE OF CATEGORY AND SUM OF CALORIES
    aggregate(mc_menu$Calories, by=list(mc_menu$Category), sum)
    
    calories_cat = as.data.frame(aggregate(mc_menu$Calories, by=list(mc_menu$Category), sum))
    
    library(ggplot2)
    
    ggplot(calories_cat ) + geom_col(aes(Group.1, x, fill=rainbow(9))) + 
      geom_text(aes(x=Group.1 , y=x , label = x))+
      labs(title = "Each Category Containg number of Calories", x= "Categories", y= "Calories")+
      theme(
        plot.background = element_rect(fill="#F0F3F4"),
        panel.grid.major = element_line(colour = "#37474F"),
        panel.background = element_rect(fill="#F0F3F4"),
        axis.title.y = element_text(colour = "#3E2723", angle=90),
        axis.title.x = element_text(colour = "#3E2723", angle = 0),
        axis.text = element_text(colour = "#3E2723"),
        legend.position = "none")
    
    
    ##################################################################
    
    
  })
  
  output$barplot2 <- renderPlot({
    # FILTERED THE GRILLED AND CRISPY SANDWICHES FROM THE DATASET
    
    nv = mc_menu[grep("Crispy" , mc_menu$Item) , ]
    crispy_sandwich = nv[grep("Sandwich" , nv$Item) , ]
    crispy_sandwich$type = "Crispy Sandwich"
    
    nv1 = mc_menu[grep("Grilled" , mc_menu$Item) , ]
    Grilled_sandwich = nv1[grep("Sandwich" , nv1$Item) , ]
    Grilled_sandwich$type = "Grilled Sandwich"
    
    cris_gril_sand = rbind(crispy_sandwich, Grilled_sandwich)
    cris_gril_sand1 = cris_gril_sand[cris_gril_sand$Item!= "Southern Style Crispy Chicken Sandwich",]
    
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Premium Crispy Chicken Classic Sandwich"] = "Premium Chicken Classic Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Premium Crispy Chicken Club Sandwich"] = "Premium Chicken Club Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Premium Crispy Chicken Ranch BLT Sandwich"] = "Premium Chicken Ranch BLT Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Bacon Clubhouse Crispy Chicken Sandwich"] = "Bacon Clubhouse Chicken Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Premium Grilled Chicken Classic Sandwich"] = "Premium Chicken Classic Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Premium Grilled Chicken Club Sandwich"] = "Premium Chicken Club Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Premium Grilled Chicken Ranch BLT Sandwich"] = "Premium Chicken Ranch BLT Sandwich"
    levels(cris_gril_sand1$Item)[levels(cris_gril_sand1$Item)=="Bacon Clubhouse Grilled Chicken Sandwich"] = "Bacon Clubhouse Chicken Sandwich"
    
    
    
    
    ggplot(cris_gril_sand1 ) + geom_col(aes(Item, Calories, fill=rainbow(8))) + 
      geom_text(aes(x=Item , y=Calories , label = Calories))+
      labs(title = "Nutrition Facts for McDonald's Menu --- Calories in Grilled  VS Crispy Chicken Sandwiches", x= "ITEM", y= "CALORIES")+
      theme(
        plot.background = element_rect(fill="#F0F3F4"),
        panel.grid.major = element_line(colour = "#37474F"),
        panel.background = element_rect(fill="#F0F3F4"),
        axis.title.y = element_text(colour = "#3E2723", angle=90),
        axis.title.x = element_text(colour = "#3E2723", angle = 0),
        axis.text = element_text(colour = "#3E2723"),
        legend.position = "none") + facet_grid( type ~.)
  })
  

  
  output$pieplot <- renderPlot({
    
    mytable <- table(menu$Category)
    lbls <- paste(names(mytable), "\n", mytable, sep="")
    
    pie3D(table(d$Category),labels = lbls, explode=0.1, main = "Menu Items", col = c("Red","Green","Blue","Yellow","Orange","SkyBlue","Pink","LightBlue","Gray","Silver"))
  })
  
  
  output$plot <- renderPlot({
    plot(d$Calories, d$Item)
  })
  
  
  
  
  output$cond2 <- renderPlot({
    
    options(repr.plot.height=4, repr.plot.width=6)
    ggplot(menu, aes(x = Calories, fill = Category)) +
      geom_density(position = "fill") +
      scale_fill_brewer() +
      theme_pander() +
      labs(fill="")
    
    #hist(d$Calories, freq=F)
  })
  
  
  output$barchart2 <- renderPlotly({
    #Barchart - distribution of food categories
    new_col<-c("grey50", "blue","hotpink","Magenta","seagreen","violet","brown","maroon","navyblue")
    plot_ly(x = menu$Category, y=menu$Calories,color = menu$Category,colors =new_col , type = "box")%>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Calories"),
      showlegend=FALSE,
      autosize = T)
  })
  
  
  output$barchart3 <- renderPlotly({
    #Barchart - distribution of food categories
    plot_ly(x = menu$Category, y=menu$Protein,color = menu$Category,colors =new_col , type = "box")%>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      showlegend=FALSE,
      autosize = T)
  })
  
  output$barchart33 <- renderPlot(
    {
      menu %>% select(Category,Item,Protein,TotalFat)%>%arrange(desc(Protein))%>%filter(Category =="Chicken & Fish")%>%ggplot(aes(x=Item,y=Protein,col=Item))+geom_point(size=3)+theme(legend.position = "none",axis.title.x=element_blank(),
                                                                                                                                                                                        axis.text.x=element_blank(),
                                                                                                                                                                                        axis.ticks.x=element_blank())+geom_label_repel(aes(label=substr(Item,1,20)),size=2)+labs(title="High Protein/Fat Item in Chicken & Fish Category")+geom_bar(aes(y=TotalFat),alpha=0.5,stat="identity")
    }
  )
  
  output$barchart4 <- renderPlotly({
    #Barchart - distribution of food categories
    plot_ly(x = menu$Category, y=menu$Carbohydrates,color = menu$Category,colors =new_col , type = "box") %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      showlegend=FALSE,
      autosize = T)
  })
  
  output$barchart44 <- renderPlotly({
    p<-plot_ly(x=menu$Calories, y=menu$Carbohydrates, type="scatter", mode = "markers" , 
               marker=list( color=ifelse(menu$Calories>500,"red","blue") , opacity=0.5 , size=20) ) 
    p
    
  })
  
  output$barchart5 <- renderPlotly({
    #Barchart - distribution of food categories
    plot_ly(x = menu$Category, y=menu$TotalFat,color = menu$Category,colors =new_col , type = "box") %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      showlegend=FALSE,
      autosize = T)
  })
  
  output$barchart55 <- renderPlotly({
    m1<-menu%>%select(TotalFat,Calories.from.Fat,Saturated.Fat)
    m2<-as.matrix(m1)
    colnames(m2)<-NULL
    #m2
    p <- plot_ly(z=~m2) %>% add_surface()%>%layout(scene = list(xaxis = list(title = 'TotalFat'),
                                                                yaxis = list(title = 'Calories from Fat'),
                                                                zaxis = list(title = 'Saturated Fat')))
    p
    
  })
  
  output$barchart555 <- renderPlotly({
    m1<-menu%>%filter(Category %in% c("Beef & Pork","Chicken & Fish"))%>%arrange(desc(Total.Fat....Daily.Value.,Saturated.Fat....Daily.Value.,Trans.Fat))
    p <- plot_ly(m1, x = ~factor(Item,levels=Item), y = ~Total.Fat....Daily.Value., name = 'Total Fat DV', type = 'scatter', mode = 'lines+markers', width = 2,color = I('red')) %>%
      add_trace(y = ~Saturated.Fat....Daily.Value., name = 'Saturated Fat DV',color=I('blue')) %>%
      add_trace(y = ~Trans.Fat, name = 'Trans Fat',color=I("hotpink")) %>%
      layout(title = 'Camparing Fat in Items',
             xaxis = list(title = "",
                          showgrid = FALSE),
             yaxis = list(title = "value",
                          showgrid = FALSE),
             legend=list(orientation="r",xanchor="center"))
    p
    
  })
  
  output$barchart6 <- renderPlot({
    dat<-menu %>% select(Category,Sodium)%>% group_by(Category)%>%summarise(tsodium=sum(Sodium))
    dat
    dat$fraction = dat$tsodium / sum(dat$tsodium)
    dat = dat[order(dat$fraction), ]
    dat$ymax = cumsum(dat$fraction)
    dat$ymin = c(0, head(dat$ymax, n=-1))
    
    p1 = ggplot(dat, aes(fill=Category, ymax=ymax, ymin=ymin, xmax=9, xmin=3)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(0, 9)) +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      annotate("text", x = 0, y = 0, label = "Sodium") +
      labs(title="Sodium content in Category")
    p1
    
  })
  
  output$barchart7 <- renderPlotly({
    plot_ly(x = menu$Category, y=menu$Sugars,color = menu$Category,colors =new_col , type = "bar") %>% layout(title = "Sugars",                                                                                                           autosize = T)
    
  })
  
  output$barchart77 <- renderPlot({
   
    ss<-menu%>%select(Category,Item,Sugars,Serving.Size,Sugars)%>%filter(Category=="Smoothies & Shakes")
    ss$size<-NULL
    ss$size[str_detect(ss$Item,"Small")]<-"Small"
    ss$size[str_detect(ss$Item,"Medium")]<-"Medium"
    ss$size[str_detect(ss$Item,"Large")]<-"Large"
    ss$size[str_detect(ss$Item,"Snack")]<-"Snack"
    ss%>%filter(!size == "Snack")%>%arrange(desc(Sugars))%>%ggplot(aes(x=factor(Item,level=Item),y=Sugars,group=size,fill=size))+
      geom_bar(stat="identity",position="dodge",alpha=0.7)+theme(axis.text.x = element_text(angle=90))+coord_flip()+
      labs(x="Item",title="Sugar content in Smoothies & Shakes")
    
  })
  
  output$smooth2 <- renderPlotly({
    #Smooth density estimate (faceted) - calories by category
    p <- plot_ly(menu,x = ~Calories,
                 type = "histogram",
                 histnorm = "probability",name="Calorie",alpha=0.6)%>%
      add_histogram(x = ~Calories.from.Fat,name="Calorie From Fat",alpha=0.6) %>%
      layout(barmode = "overlay")
    p
  })
  
  output$boxplot2 <- renderPlot({
    Calories<-ggplot(data=menu,aes(x=Category,y=Calories,color=Calories)) +
      geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA,aes(color=Calories))+ggtitle("Calories per food category")
    Calories
  })
  
  output$boxplot3 <- renderPlot({
    Cholesterol<-ggplot(data=menu,aes(x=Category,y=Cholesterol,color=Cholesterol)) +
      geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)+ggtitle("Cholesterol per food category")
    
    Cholesterol
  })
  
  
  output$boxplot4 <- renderPlot({
    Sodium<-ggplot(data=menu,aes(x=Category,y=Sodium,color=Sodium)) +
      geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)+ggtitle("Sodium per food category")
    
    Sodium
  })
  
  output$boxplot5 <- renderPlot({
    Carbohydrates<-ggplot(data=menu,aes(x=Category,y=Carbohydrates,color=Carbohydrates)) +
      geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)+ggtitle("Carbohydrates per food category")
    
    Carbohydrates
  })
  
  
  output$boxplot6 <- renderPlot({
    Protein<-ggplot(data=menu,aes(x=Category,y=Protein,color=Protein)) +
      geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)+ggtitle("Protein per food category")
    
    Protein
  })
  
  output$boxplot <- renderPlot({
    d1<-menu%>%ggplot(aes(x=Sugars,y=Calories))+stat_density_2d(geom = "polygon", aes(fill = ..level..), h=c(100,1500) ,contour = TRUE)+scale_fill_viridis(option="magma")
    d2<-menu%>%ggplot(aes(x=Carbohydrates,y=Calories))+stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(100,1500) ,contour = FALSE)+scale_fill_viridis(option="magma")
    d3<-menu%>%ggplot(aes(x=Sodium,y=Calories))+stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(3000,1500) ,contour = FALSE)+scale_fill_viridis(option="magma")
    d4<-menu%>%ggplot(aes(x=Cholesterol,y=Calories))+stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(600,1500) ,contour = FALSE)+scale_fill_viridis(option="magma")
    d5<-menu%>%ggplot(aes(x=Dietary.Fiber,y=Protein))+scale_x_continuous(limits=c(0,8),expand=c(0,0))+stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(5,70) ,contour = FALSE)+scale_fill_viridis(option="magma")
    grid.arrange(d1,d2,d3,d4,d5,nrow=3,ncol=2)
  })
  
  
  output$bar3 <- renderPlot({
    
    choles2 <- input$choles
    h1(paste("Cholesterol content in ",choles2), align="center")
    options(repr.plot.height=4, repr.plot.width=6)
    menu %>%
      filter(.,Category==choles2) %>%
      ggplot(aes(x = reorder(Item, Cholesterol), y = Cholesterol)) +
      geom_bar(aes(fill=Calories<600), width=0.5, stat = "identity") +
      coord_flip() +
      theme_pander(base_size = 10) +
      theme(legend.position="none") +
      labs(x = NULL)+
      ggtitle(paste("Cholestrol content in ",choles2," items"))
  })

  
  
  
  
  
  # output$clustplot <- renderPlot({
  #   
  #   summDF <- 
  #     gatheredMenu %>% group_by(cluster, Nutrient) %>% 
  #     summarise(mean=mean(`Quantity per unit`)) 
  #   
  #   plot(ggplot(summDF, aes(x=Nutrient, y=mean, color=cluster, group=cluster)) + 
  #          geom_point() + geom_line() + 
  #          theme_bw() + theme(axis.text.x=element_text(angle=90)))
  # })
  
  # output$clustplot2 <- renderPlot({
  #   
  #   beverages <- menu[which(menu$Metric == "ml"),]
  #   food <- menu[which(menu$Metric == "g"),]
  #   
  #   corr <- cor(t(food[,5:ncol(food)]))
  #   corrplot::corrplot(corr, order = "hclust", tl.cex = 0.6, method = "color")
  # })
  
  # output$clustplot3 <- renderPlot({
  #   
  #   plot(ggplot(food, aes(x=`Calories`, y=Sodium,color=cluster)) + 
  #          geom_point() + 
  #          #Team theme_bw FTW
  #          theme_bw())
  # })
  
  # output$clustplot4 <- renderPlot({
  #   
  #   #Show correlation among beverage/liquid items
  #   corrplot(corr, order = "hclust", tl.cex = 0.6, method = "shade")
  # })
  
  output$pointplot <- renderPlot({
    
    menu %>%
      filter(!Category %in% c('Coffee & Tea', 'Beverages', 'Smoothies & Shakes')) %>%
      ggplot(aes(Serving.Size, Calories, color = Category)) + 
      geom_point()
  })
  
  
  # output$featureplot <- renderPlot({
  #   
  #   mean(menu$Calories)
  #   sd(menu$Calories)
  #   
  #   library(ggplot2);library(ISLR);library(lattice);library(caret);
  #   featurePlot(x=menu[c("TotalFat","Cholesterol")],y=menu$Calories,plot="pairs")
  # })
  
  output$analysis1 <- renderPlot({
    
    calories <- head(arrange(calories, desc(Calories)),20)
    
    ggplot(calories, aes(x=reorder(Item,Calories), y= Calories, fill= Category, label=Calories))+
      geom_bar(stat="identity")+
      coord_flip()+
      theme_bw()+
      scale_fill_manual(values = my_palette)+
      theme(legend.position = "top")+
      geom_text(hjust=1.3, colour = "white")+
      xlab("")+
      ggtitle("Top 20 products with the most calories")
    
  })

  
  unhealthy <- select(menu, c(1,2,7,12,14))
  unhealthy <- gather(unhealthy, Type, Value, -c(1:2))
  
  unhealthy_food <- filter(unhealthy, !(Category %in% c("Beverages", "Coffee & Tea", "Smoothies & Shakes")))
  
    
  output$unhealthy2 <- renderPlot({
   
    
    
    ggplot(unhealthy_food, aes(x=Item, y=Value, colour=Type))+
      geom_point(size=3)+
      coord_flip()+
      scale_color_manual(values = my_palette)+
      theme_bw()+
      theme(legend.position = "top")+
      geom_hline(yintercept=100, col="red", linetype=2)+
      geom_hline(yintercept=50, col="orange", linetype=2)+
      ylab("% Daily Value")+
      xlab("")+
      scale_y_continuous(position = "top")+
      ggtitle("Daily value of McDonald food items for Cholesterol, Sodium and Total Fat")
    
     
  })
  
  output$unhealthydrinks2 <- renderPlot({
    
    unhealthy_drink <- filter(unhealthy, Category %in% c("Beverages", "Coffee & Tea", "Smoothies & Shakes"))
    
    ggplot(unhealthy_drink, aes(x=Item, y=Value, colour=Type))+
      geom_point(size=3)+
      coord_flip()+
      scale_color_manual(values = my_palette)+
      theme_bw()+
      theme(legend.position = "top")+
      geom_hline(yintercept=100, col="red", linetype=2)+
      geom_hline(yintercept=50, col="orange", linetype=2)+
      ylab("% Daily Value")+
      xlab("")+
      scale_y_continuous(position = "top")+
      ggtitle("Daily value of McDonald drink items for Cholesterol, Sodium and Total Fat")
    
  })
  
  
  Daily_value <- select(menu, c(1,2,7,12, 14))
  
  #Daily_value <- select(datasource, c(1,2,7,12,14,16,18,21,22,23,24))
  Daily_value <- gather(Daily_value, key= Type, value= "Daily value", -c(1:2))
  Daily_value$Type <- gsub("\\(% Daily Value)", "", Daily_value$Type)
  
  
  output$dailyValue2 <- renderPlot({
    
    ggplot(Daily_value, aes(x=Category, y= `Daily value`, colour= Category))+
      geom_boxplot()+
      facet_grid(Type ~ .)+
      ggtitle("Cholesterol, Sodium and Total fat distribution", 
              subtitle = "Daily Value in % per category")+
      theme_bw()+
      ylab("Daily Value in %")+
      scale_color_manual(values = my_palette)+
      theme(legend.position = "none")
    
  })
  
  Daily_value <- select(menu, c(1,2,16,18, 21))
  
  #Daily_value <- select(datasource, c(1,2,7,12,14,16,18,21,22,23,24))
  Daily_value <- gather(Daily_value, key= Type, value= "Daily value", -c(1:2))
  Daily_value$Type <- gsub("\\(% Daily Value)", "", Daily_value$Type)
  
  output$dailyValue3 <- renderPlot({
    
    ggplot(Daily_value, aes(x=Category, y= `Daily value`, colour= Category))+
    geom_boxplot()+
      facet_grid(Type ~ .)+
      ggtitle("Carbohydrates, Dietary Fiber and Vitamin A distribution", 
              subtitle = "Daily Value in % per category")+
      theme_bw()+
      ylab("Daily Value in %")+
      scale_color_manual(values = my_palette)+
      theme(legend.position = "none")
    
  })
  
  Daily_value <- select(menu, c(1,2,22,23,24))
  
  #Daily_value <- select(datasource, c(1,2,7,12,14,16,18,21,22,23,24))
  Daily_value <- gather(Daily_value, key= Type, value= "Daily value", -c(1:2))
  Daily_value$Type <- gsub("\\(% Daily Value)", "", Daily_value$Type)
  
  output$dailyValue4 <- renderPlot({
    
    ggplot(Daily_value, aes(x=Category, y= `Daily value`, colour= Category))+
      geom_boxplot()+
      facet_grid(Type ~ .)+
      ggtitle("Vitamin C, Calcium and Iron distribution", 
              subtitle = "Daily Value in % per category")+
      theme_bw()+
      ylab("Daily Value in %")+
      scale_color_manual(values = my_palette)+
      theme(legend.position = "none")
    
  })
  
  output$salad2 <- renderPlot({
    bbr2 <- input$bbr
    calories <- head(arrange(calories, desc(Calories)),20)
    ggplot(subset(menu,Category==bbr2), aes(reorder(Item,Calories),Calories,fill=Calories)) + geom_bar(stat = "identity")  + theme(legend.position = "none")+ggtitle(paste(bbr2,"Menu Vs Calories")) + xlab("Item") + ylab("Calories")  + coord_flip()
    
  })
  
  output$carbo <- renderPlot({
    qplot(Calories, Carbohydrates, data=menu, color=Category,facets=~Category, size=I(1),xlab="Calories", ylab="Carbohydrates")
  })
  
  output$energy2 <- renderPlot({
    plot1 <- ggplot(menu, aes(x=Category, y=Calories))+
      geom_boxplot()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      ylab("energy content [kcal]")+xlab("Food Category")
    plot1
  })
  
  
  output$waterc <- renderPlot({
    myData4$Serving.Size <- as.numeric(myData4$Serving.Size)
    myData4 <- subset(myData4, !is.na(Serving.Size))
    myData4 <- ddply(myData4, c("Category","Item", "Serving.Size", "TotalFat", "Carbohydrates", "Protein", "Dietary.Fiber"),showlegend=FALSE,
                     autosize = T, summarize, water = (Serving.Size- TotalFat - Carbohydrates - Protein - Dietary.Fiber)/Serving.Size*100)
    
    myData$Category <- as.factor(myData$Category)
    flevel <- levels(myData$Category)
    
    plot4 <- ggplot(myData4, aes(x=Category, y=water, color=Category))+
      geom_point()+
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())+
      xlab("category")+ ylab("water content [%]")
    plot4
  })
  
  output$waterc2 <- renderPlot({
    plot5 <- ggplot(myData4, aes(x=(9*TotalFat+4*Carbohydrates+4*Protein)/Serving.Size,y=water, color=Category))+
      geom_point()+
      xlab("specific calory content [kcal/g]")+ylab("water [%]")+
      stat_smooth(method = "lm", col = "red")
    plot5
    
  })
  
  output$nutrious_meal2 <- renderPlot({
    library(dplyr)
    myBreakfast <- subset(myData, Category == "Breakfast")
    myLunchDinner <- subset(myData, Category == "Beef & Pork" | Category == "Chicken & Fish")
    mySides <- subset(myData, Category == "Snacks & Sides")
    mySalads <- subset(myData, Category == "Salads")
    myBeverages <- subset(myData, Category == "Beverages")
    myCoffees <- subset(myData, Category == "Coffee & Tea")
    myDesserts <- subset(myData, Category == "Desserts" | Category == "Smoothies & Shakes")
    
    myYear <- sample_n(myData,1)
    myYear$day <- 1
    myYear <- NULL
    
    for (i in 1:365){
      #pick a breakfast
      mySample <- sample_n(myBreakfast,1)
      mySample <- rbind(mySample, sample_n(myLunchDinner,2))
      mySample <- rbind(mySample, sample_n(mySides,2))
      mySample <- rbind(mySample, sample_n(myBeverages,2))
      mySample <- rbind(mySample, sample_n(myDesserts,1))
      mySample <- rbind(mySample, sample_n(myCoffees,2))
      mySample$day <- c(i,i,i,i,i,i,i,i,i,i)
      myYear <- rbind(myYear, mySample)
      mySample <- NULL
      
      
    }
    
    myYearAggregated <- ddply(myYear, c("day"), summarize, Iron = sum(Iron....Daily.Value.), VitaminA = sum(Vitamin.A....Daily.Value.), VitaminC = sum(Vitamin.C....Daily.Value.), Calcium = sum(Calcium....Daily.Value.), Fibers = sum(Dietary.Fiber....Daily.Value.))
    
    myMeltedYear <- melt(myYearAggregated, id.vars = "day")
    
    plot6 <- ggplot(myMeltedYear, aes(x=day, y=value, colour = value > 100))+
      facet_grid(. ~ variable)+
      geom_point()
    plot6
    
  })
  
  myData3 <- myData2
  myData3 <- subset(myData3, Calories != 0)
  myData3 <- subset(myData3, select = c("Category","Item", "myPercentFatCal", "myPercentProteinCal", "myPercentCarbCal"))
  
  myData3$Item <- factor(myData3$Item, levels=myData3$Item[order(myData3$myPercentFatCal,myData3$myPercentProteinCal,myData3$myPercentCarbCal)], ordered=TRUE)
  
  myData3 <- melt(myData3)
  
  output$fatcarbopro2 <- renderPlot({
    
    
    plot3 <- ggplot(myData3, aes(x=Item, y=value, fill = variable))+
      theme_bw()+
      geom_bar(stat = "identity",width=1)+
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())+
      xlab("Menu item")+ ylab("energy content [%]")+
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                        name="Food stuff",
                        breaks=c("myPercentFatCal", "myPercentProteinCal", "myPercentCarbCal"),
                        labels=c("Fat", "Protein", "Carbs"))
    plot3
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$msgOutput <- renderMenu({
    msgs <- apply(read.csv("messages.csv"),1, function(row){
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    dropdownMenu(type = "messages", .list = msgs)
  })
  output$approvedSales <-renderInfoBox({
    infoBox("Approval Sales","10,00,000", icon = icon("bar-chart-o"))
  })

  
  output$itemRequested <- renderValueBox({
    
    valueBox((aggregate(mc_menu$Calories, by=list(mc_menu$Category), sum))," Calories by category", icon = icon("fire"), color = "red")
  })
  output$project_code <- {(renderText(input$projcode)
    
  )}
  
  output$salary <- {(renderText(input$sal)
  )}
  
  output$pre <- renderPrint({
    # predict_class<-predict(model, Test, type="class")
    # confusionMatrix(predict_class, Test$Category)
    
    # str(mcd_n)
    mcd_n$Total.Fat....Daily.Value. <- NULL
    mcd_n$Sodium....Daily.Value. <- NULL
    mcd_n$Dietary.Fiber....Daily.Value. <- NULL
    mcd_n$Cholesterol....Daily.Value. <- NULL
    mcd_n$Saturated.Fat....Daily.Value. <- NULL
    mcd_n$Carbohydrates....Daily.Value. <- NULL
    mcd_n$Serving.Size <- NULL
    mcd_n$Item <- NULL
    set.seed(1)
    hanx<-createDataPartition(mcd_n$Category, p=.7, list = F)
    Train<-mcd_n[hanx,]
    Test<-mcd_n[-hanx,]
    
    model<-rpart(Category~., data = Train)
    predict_class<-predict(model, Test, type="class")
    confusionMatrix(predict_class, Test$Category)
    
  })
  
  output$destree <- renderPlot({
    prp(model, type=1, extra = 3, main="Decision Tree")
  })
  
  
  output$pre2 <- renderPrint({
    
    colnames(menu)
    menu$'Total.Fat....Daily.Value.' <- NULL
    menu$'Saturated.Fat....Daily.Value.' <- NULL
    menu$'Cholesterol....Daily.Value.' <- NULL
    menu$'Sodium....Daily.Value.' <- NULL
    menu$'Carbohydrates....Daily.Value.' <- NULL
    menu$'Dietary.Fiber....Daily.Value.' <- NULL
    menu$Item <- NULL
    menu$Serving.Size <- NULL
    
    set.seed(1)
    index <- createDataPartition(menu$Category, p=0.8, list=FALSE) 
    train <- menu[index,]
    test <- menu[-index,]
    
    model <- rpart(Category~., data=train)
    
    pred_rf <- predict(model_rf, test)                   
    confusionMatrix(pred_rf, test$Category)
    
  })
  
  # output$destree2 <- renderPlot({
  #   
  #   colnames(menu) 
  #   menu$'Total.Fat....Daily.Value.' <- NULL
  #   menu$'Saturated.Fat....Daily.Value.' <- NULL
  #   menu$'Cholesterol....Daily.Value.' <- NULL
  #   menu$'Sodium....Daily.Value.' <- NULL
  #   menu$'Carbohydrates....Daily.Value.' <- NULL
  #   menu$'Dietary.Fiber....Daily.Value.' <- NULL
  #   menu$Item <- NULL
  #   menu$Serving.Size <- NULL
  #   str(menu)
  #   levels(menu$Category)
  #   set.seed(1)
  #   index <- createDataPartition(menu$Category, p=0.8, list=FALSE) 
  #   train <- menu[index,]
  #   test <- menu[-index,]
  #   
  #   model <- rpart(Category~., data=train)
  #   prp(model, type=1, extra=2)
  #   
  # })
  
  output$model_rf <- renderPlot({
    model_rf <- randomForest(Category~., train, ntree=50)
    varImpPlot(model_rf)
    
  })
  
  
  
  
  
  output$depart <- {(renderText(input$dept))}
})