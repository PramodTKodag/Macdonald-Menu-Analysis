library(plotly)
library(shiny)
library(shinydashboard)
require(PerformanceAnalytics)
library(magrittr)
library(caret)

shinyUI(
  dashboardPage(title = "Macdonald's Menu Analysis", skin = "red",
    dashboardHeader(title = "Macdonald's Menu Analysis", dropdownMenuOutput("msgOutput"),
                    # dropdownMenu(type ="message",
                    #              messageItem(from = "finance Update", message="we are on threashold"),
                    #              messageItem(from = "Sales Update", message = "sales are 55%", icon = icon("bar-chart"), time = "22:00"),
                    #              messageItem(from = "Sales Update", message = "Sales metting at 6 PM on monday", icon = icon("handshake-o"), time = "03-25-2017")
                    #              )
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = "2 new tabs added to tthe dashboard",
                                   icon = icon("dashboard"),
                                   status = "success"
                                 ),
                                 notificationItem(
                                   text = "Server is currently running at at 95% load",
                                   icon = icon("warning"),
                                   status = "warning"
                                   )
                                 ),
                    dropdownMenu(type = "tasks",
                                 taskItem(
                                   value = 80,
                                   color = "aqua",
                                   "Shiny dashboard Eduction"
                                 ),
                                 taskItem(
                                   value = 10,
                                   color = "red",
                                   "Shiny dashboard Eduction 2"
                                 )
                                 
                                 
                                 )
                    
                    ),
    
    
    dashboardSidebar(
     
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        
        # textInput("projcode","Enter Your Project Code"),
        
        # sliderInput("sal","Enter ur Salary",0,100,value = c(10,60), step = 10),
        # 
        # 
         selectInput("dept", "What is your department", choices = c("marketing","Sales","finance"), multiple = T),
        
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Diet Calculation", tabName = "calcu", icon = icon("dashboard")),
      menuItem("Models", tabName = "predictions",
               menuSubItem("Decision Tree",tabName = "predict"),
               menuSubItem("Random Forest",tabName = "predict2")),
              
      menuItem("Graphs", tabName = "graphs", icon = icon("bar-chart-o"),
        menuSubItem("Calories in Food Category", tabName = "salad"),
        menuSubItem("Carbohydrate Analysis", tabName = "carb"),
       
        # menuSubItem("Cluster Nutrient", tabName = "clust"),
        # menuSubItem("Cluster Food Item", tabName = "clust2"),
        # menuSubItem("Sodium Calories Cluster", tabName = "clust3"),
        # menuSubItem("Clusters of Beverages", tabName = "clust4"),
        menuSubItem("Calories VS Serving Size", tabName = "point"),
        # menuSubItem("Scatter Plot Matrix", tabName = "feature"),
        menuSubItem("Menu Items Variety", tabName = "piechart2"),
        
        menuSubItem("Calories wise distribution", tabName = "bar2"),
        menuSubItem("Cholestrol Conten in Food Item", tabName = "bar3"),
        menuSubItem("Most Calories products (TOP 20)", tabName = "bar4"),
        menuSubItem("Unhealthy food items", tabName = "unhealthy"),
        menuSubItem("Unhealthy Drinks", tabName = "unhealthydrinks"),
        # menuSubItem(" daily value distribution", tabName = "dailyValue"),
       
        menuSubItem("Conditional Density estimate", tabName = "cond"),
        menuSubItem("Catogories and its Nutritional Value", tabName = "barchart"),
        menuSubItem("Distribution of Calorie", tabName = "smooth"),
        menuSubItem("Density plots for Calories Vs other Nutrients", tabName = "box2"),
        menuSubItem("Nutrition's per Category", tabName = "box3"),
        menuSubItem("Energy Content", tabName = "energy"),
        menuSubItem("fat, carbs and proteins Content", tabName = "fat_carbs_pro"),
        menuSubItem("Water Content", tabName = "waterc"),
        menuSubItem("Vitamins ,Iron, Fiber Cunsumption", tabName = "nutrious_meal"),
        menuSubItem("PLOT", tabName = "plot2"))
      # menuItem("Dashboard Finance", tabName = "finance", icon = icon("calendar")),
      # menuItem("Dashboard Sales" ,tabName = "sales"),
      # menuItem("Detailed Analysis", tabName = "analysis", badgeLabel = "new", badgeColor = "green"),
      # menuItem("Raw Data", tabName = "rawdata")
      
      # sliderInput("bins", "Number of breaks",1,100,50),
      # textInput("text_input", "Search Opportunties", value = "12346")
    )),
    dashboardBody(
    
      tabItems(
        tabItem(tabName = "dashboard",
                tags$img(src = "macd2.jpg", align = "left", width = "700", height="345"),
                fluidRow(
                  # avgFat<- mean(menu$TotalFat, na.rm=TRUE),
                  valueBox(mean(menu$TotalFat, na.rm=TRUE),"Average Fat", icon = icon("hourglass-3")),
                  valueBox(mean(menu$Cholesterol, na.rm=TRUE),"Average Cholestrol", icon = icon("hourglass-3")),
                  valueBoxOutput("itemRequested")
                 
                ),
                
                fluidRow(
                  column(width = 12,
                         #valueBox((aggregate(mc_menu$Calories, by=list(mc_menu$Category), sum))," Calories by category", icon = icon("fire"), color = "red")
                  #infoBox("Carbohydrate", (aggregate(menu$Carbohydrates, na.rm = TRUE, by=list(menu$Category), sum)), icon = icon("thumbs-up")),
                  infoBox("Mean Calories in Smoothies & Shakes", mean(calc6$Calories), icon = icon("thumbs-up")),
                  # infoBoxOutput("approvedSales")
                  
                  calc5 = menu%>%select(Category, Calories)%>%filter(Category == "Coffee & Tea"),
                  calc4 = menu%>%select(Category, Cholesterol)%>%filter(Category == "Beverages"),
                  calc6 = menu%>%select(Category, Calories)%>%filter(Category == "Smoothies & Shakes"),
                  
                  infoBox("Mean Cholestrol in Coffee and Tea", mean(calc4$Cholesterol), icon = icon("thumbs-up")),
                  infoBox("Mean Calories in Beverages", mean(calc5$Calories), icon = icon("thumbs-up")),
                  #infoBox("Average Calcium (% Daily Value)", mean(menu$Calcium_Daily_Value, na.rm=TRUE), icon = icon("thumbs-up")),
                  
                  calc = menu%>%select(Category, Calories)%>%filter(Category == "Breakfast"),
                  calc2 = menu%>%select(Category, Calories)%>%filter(Category == "Beef & Pork"),
                  calc3 = menu%>%select(Category, Calories)%>%filter(Category == "Chicken & Fish"),
                  
                  infoBox("Mean Calories in Beakfast",  mean(calc$Calories), icon = icon("thumbs-up")),
                  infoBox("Mean Calories in Beef & Pork",  mean(calc2$Calories), icon = icon("thumbs-up")),
                  infoBox("Mean Calories in Chicken & Fish",  mean(calc3$Calories), icon = icon("thumbs-up"))
                  
                  
                  )
                )
                ),
        tabItem(tabName = "calcu",
                tabPanel("Menu selection",
                         column(3,
                                uiOutput('dish_type_selection')),
                         column(5,
                                uiOutput('dish_selection')
                                
                                
                         ),
                         column(4,
                                h3(textOutput("text2"))),
                         column(12,dataTableOutput('selected_items')),
                         column(12,
                                h6("This nutrition information is derived from testing conducted in accredited laboratories, published resources and/or information provided by McDonald’s suppliers. The nutrition information is based on standard product formulations, serving sizes and average values for ingredients from McDonald’s suppliers and is rounded in accordance with applicable regulations/guidelines. Variations in serving sizes, preparation techniques, product testing and sources of supply, including regional and seasonal variations, may affect the nutrition values for each menu item. In addition, product formulations do change periodically. You should expect some variation in the nutrient content of products purchased in our restaurants. This information is current as of June 2015.")
                                )
                         
                         
                )
                
                
                ),
        
        # tabItem(tabName = "finance",
        #         h1("Finance Dashboard"),
        #         tabBox(
        #           tabPanel(title = "Most Calories", status = "primary", solidHeader = T, background = "aqua"),
        #           tabPanel(title = "Most Calories", status = "primary", solidHeader = T, background = "aqua"
        #                    
        #                    
        #                    )
        #         )
        #   
        # ),
        
        tabItem(tabName = "analysis",
                h1("Analysis")
                       
                     
           
        ),
        
        tabItem(tabName = "bar4",
                h1("Top 20 products With High Amount of Calories", align="center"),
                plotOutput("analysis1", width = 900, height = 700),
                h4("Chicken McNuggets (40 pieces)” is the most calories item, and by far."),
                h4("Breakfast products are pretty high on the list followed by a lot of smoothies & shakes. Suprisingly, there are not so many burgers, only the “double quarter pounder with cheese” and “bacon clubhouse burger")
                
                ),
        
        tabItem(tabName = "unhealthy",
                h1("Unhealthy food items", align="center"),
                br(),
                plotOutput("unhealthy2", height = 1000),
                p("We have few items with Cholesterol, Sodium or Total fast higher than the daily value. It is the case for the Chicken McNuggets (40 pieces), or some of the big breakfasts. If we check the items between 50% and 100%, we can see that the big breakfasts are often represented.")
          
        ),
        
        tabItem(tabName = "unhealthydrinks",
                h1("Unhealthy Drinks", align="center"),
                plotOutput("unhealthydrinks2", height = 1500),
                p("Although we have nothing more than 50% daily value, we have few high percentage: McFlurry with Reese’s Peanut Butter Cups (medium) and McFlurry with M&M’s candies (medium) have a 50% daily value total fat (!). The various Frappe also have pretty Total Fat / Cholesterol values. Sodium however, tends to be rather low in beverages when compared with the food products.")
                ),
        
        # tabItem(tabName = "dailyValue",
        #         plotOutput("dailyValue2"),
        #         br(),
        #         br(),
        #         plotOutput("dailyValue3"),
        #         br(),
        #         br(),
        #         plotOutput("dailyValue4")
        #         ),
        # 
        # tabItem(tabName = "sales",
        #         h1("Sales")),
        # 
        tabItem(tabName = "carb",
                h1("Carbohydrate Content in each food Category", align = "center"),
                plotOutput("carbo")
                ),
        
        tabItem(tabName = "energy",
                h1("Energy Content", align="center"),
                plotOutput("energy2", height = 600),
                p("Interestingly, the main components of a meal, be it the breakfast or the various burgers, have a mean energy content of about 500 kcal. Similarly, sides, desserts and salads (and the coffee mix drinks - which some would also count as desserts...) are around 250 kcal. ")
        ),
        
        tabItem(tabName = "waterc",
               
                h1("Water Content in food item (Category wise)", align="center"),
                plotOutput("waterc"),
                p("It seems Beef&Pork is a good value in terms of non-water content. For the Chicken & Fish category, it might be interesting to separate them and see whether the fish items are more watery than the chicken items. Salad seems to contain lots of water, way more than Smoothies & Shakes which one might not have guessed given the more liquid nature of the later compared to the former."),
                br(),
                br(),
                plotOutput("waterc2"),
                p("However, a high water content might be a good thing. Plotting calories per gram of food against water yields the insight that water contains no calories and is good when on a diet.BTW - the outlier with the highest energy content are cinnamon melts from the breakfast category.")
        ),
        
        tabItem(tabName = "nutrious_meal",
                h1("Vitamins, Iron, Fiber and Calcium cunsumption", align="center"),
                plotOutput("nutrious_meal2"),
                p("It looks like our hypothetical customer was pretty well covered for his vitamins (A and C) and calcium when randomly picking his/her meals for the day. However, his/her iron and fibers needs are not so well covered.")
                ),
        
        tabItem(tabName = "fat_carbs_pro",
                h1("Fat, Carbohydrate and Protein Content in Food Item", align="center"),
                plotOutput("fatcarbopro2", height = 500),
                p("From this plot one might conclude that food mainly contains fat and carbs.")
        ),
        
        
        tabItem(tabName = "rawdata",
                h1("Raw Data")),
        
        
        
        
        
        
        tabItem(tabName = "predict",
                        h1("Decision Tree"),
                        # dataTableOutput('prediction_1')
                        # h4("Decision Tree predicts the category of McDonalds food with 67%, while No Info Rate is 39%.")
                        # 
                        # textOutput("pre"),
                        verbatimTextOutput("pre"),
                        hr(),
                        hr(),
                        plotOutput("destree")
                
                      
                ),
        
        tabItem(tabName = "predict2",
                h1("Random Forest"),
                # dataTableOutput('prediction_2'),
                # h4("The Random Forest predicts the category with 82% accuracy with 39% No Info Rate.")
                verbatimTextOutput("pre2"),
                hr(),
                
                hr(),
                plotOutput("model_rf")
        ),
              
        
        
        
        tabItem(tabName = "salad",
                h1("Calories VS Food Category", align = "center"),
                selectInput("bbr", "check the Calories content in graph", choices = c("Breakfast","Beef & Pork","Chicken & Fish","Salads","Snacks & Sides","Desserts","Beverages","Coffee & Tea","Smoothies & Shakes","")),
                plotOutput("salad2")
        ),
        
        
        tabItem(tabName = "piechart2",
                plotOutput("pieplot", height = 500, width = 900),
                p("Mcdonald's Contain a most of the Items in the Coffee and Tea Category while the Salads and Desserts are in very less variations")
        ),
        
        
        tabItem(tabName = "bar2",
                h1("Calories wise distribution of McDonald's Menu Category", align="center"),
                plotOutput("barplot"),
               
                h4("Here we can see that the Coffee and tea categories contains high range of Calories, an average woman needs to eat about 2000 calories per day to maintain and an average man needs 2500 calories"),
                hr(),
                hr(),
                hr(),
                # h3("Confused between Chicken Grilled and Chicken Crispy Sandwiches ? This Chart will surely help u"),
                plotOutput("barplot2"),
                h4("This Graphs shows that Crispy Chicken Sandwiches has more Calories than Grilled Sandiches for every products. Depending on the gender, the calories which we will inatke will varies")
        ),
        
        tabItem(tabName = "cond",
                h1("Conditional Density Estimate", align="center"),
                plotOutput("cond2")
        ),
        
        tabItem(tabName = "barchart",
                
                tabsetPanel(type = "tabs",
                            tabPanel("Calories per Food Category", plotlyOutput("barchart2")
                                     
                            ),
                            tabPanel("Protein", plotlyOutput("barchart3"),
                                     plotOutput("barchart33")
                                     
                            ),
                            tabPanel("Carbohydrates", plotlyOutput("barchart4"),
                                     plotlyOutput("barchart44")
                                     
                            ),
                            tabPanel("Fat", plotlyOutput("barchart5"),
                                     plotlyOutput("barchart55", height = 800, width = 800),
                                     plotlyOutput("barchart555", height = 800, width = 800),
                                     h5("Chicken Nuggets takes the first place of having high fat, next comes Double Quarter Pounder with Cheese, Bacon Clubhouse Burger takes the 3rd position. 
Saturated Fat is high seems very high for these items, since saturated fat increases the blood cholestrol level.")
                            ),
                            
                            tabPanel("Sodium", plotOutput("barchart6")
                                     
                            ),
                            tabPanel("Sugar", plotlyOutput("barchart7"),
                                     plotOutput("barchart77")
                            )
                #plotlyOutput("barchart2")
        )
        ),
        
        tabItem(tabName = "bar3",
                h1("check the Cholestrol content", align="center"),
                selectInput("choles", "check the Cholestrol content", choices = c("Breakfast","Beef & Pork","Chicken & Fish","Salads","Snacks & Sides","Desserts","Beverages","Coffee & Tea","Smoothies & Shakes","")),
                
                plotOutput("bar3"),
                h5("This graphs shows that the cholestrol content in every product category wise. The RED line indicate that the food items which contains the Calories more than the 600 mg.")
                
        ),
        
        
        
        tabItem(tabName = "smooth",
                h1("Distribution Of Calories", align="center"),
                plotlyOutput("smooth2")
        ),
        
        tabItem(tabName = "box2",
                h1("Density plots for Calories Vs other Nutrients", align = "center"),
                plotOutput("boxplot", height = 600)
        ),
        
        
        
        # tabItem(tabName = "clust",
        #         plotOutput("clustplot")
        # ),
        
        # tabItem(tabName = "clust2",
        #         plotOutput("clustplot2")
        # ),
        
        # tabItem(tabName = "clust3",
        #         plotOutput("clustplot3")
        # ),
        
        # tabItem(tabName = "clust4",
        #         plotOutput("clustplot4")
        # ),
        
        tabItem(tabName = "point",
                h1("Caalories Depending on the Serving Size", align="center"),
                plotOutput("pointplot", width = 5000)
        ),
        
        # tabItem(tabName = "feature",
        #         plotOutput("featureplot")
        # ),
        
        
        tabItem(tabName = "box3",
                h1("Nutrition's per Food Category", align = "center"),
                tabsetPanel(type = "tabs",
                            
                           
                            
                            tabPanel("Calories per Food Category", plotOutput("boxplot2"),
                                     h6("Beverages have lowest amount of Calories while the Smoothies & Shakes contains highest amount of Calories.")
                                     ),
                            tabPanel("Cholestrol per Food Category", plotOutput("boxplot3"),
                                     h6("The amount of Cholestrol is much higher in Breakfast food category. Sometimes it is more than the daily cholestrol requirements. At the other point we can say that the beverages contains lesser amount of cholesterol content, in some food items sometimes there has no Cholesterol at all")
                                     ),
                            tabPanel("Sodium per Food Category", plotOutput("boxplot4"),
                                     h6("The amount of Sodium is much higher in Breakfast food category.  At the other Side we can say that the beverages, Coffee and Tea, Dessert contains lesser amount of Sodium content")
                                     ),
                            tabPanel("Carbohydrates per Food Category", plotOutput("boxplot5"),
                                     h6("Mean of Carbohydrates content in Smoothies & Shakes is high while the snacks and Sides have low in Carbohydrates")
                                     ),
                            tabPanel("Protein per Food Category", plotOutput("boxplot6"),
                                     h6("Chicken and fish is good in their Protein Content. Beverages have really less amount of proteins")
                                     )
                           
                            )
        ),
        
        
        tabItem(tabName = "plot2",
                plotOutput("plot")
        )
        
        
        
        
                  
          
        )
      )
      
    )
  )
