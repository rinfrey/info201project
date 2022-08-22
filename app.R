library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(maps)
library(DT)
library(fmsb)
library(dplyr)
library(shinythemes)
library(rsconnect)




happinessreport <- read.csv("data/happinessreport2022.csv")
happinessreport_2022 <- happinessreport
humancaptialindex <- read.csv("data/humancaptialindex.csv")
humancaptialindex_2020 <- humancaptialindex

#info for summary table 


#DATA USED For GRAPHING

humanindex <- humancaptialindex_2020
humanindex <- select(humanindex, X, WB.Code, Income.Group, HUMAN.CAPITAL.INDEX.2020, Learning.Adjusted.Years.of.School, Harmonized.Test.Scores, Adult.Survival.Rate)
humanindex <- rename(humanindex, country = X)
humanindex<- rename(humanindex, income_level =  Income.Group,
                    human_capital = HUMAN.CAPITAL.INDEX.2020,
                    avg_years_in_school = Learning.Adjusted.Years.of.School,
                    avg_test_scores = Harmonized.Test.Scores,
                    code = WB.Code,
                    adult_survival_rate = Adult.Survival.Rate)

score_df <- happinessreport_2022

new_score <- rename(score_df, GDP = Explained.by..GDP.per.capita)
new_score <- rename(new_score, Social_Support = Explained.by..Social.support)
new_score <- rename(new_score, Life_Expectancy = Explained.by..Healthy.life.expectancy)
new_score <- rename(new_score, Freedom = Explained.by..Freedom.to.make.life.choices)
new_score <- rename(new_score, Generosity = Explained.by..Generosity)
new_score <- rename(new_score, Corruption = Explained.by..Perceptions.of.corruption)
new_score <- rename(new_score, Happiness_Score = Happiness.score)

all_data <- select(new_score, -c(RANK, Whisker.low, Whisker.high, Dystopia..1.83....residual))

new_score <- select(new_score, -c(RANK, Happiness_Score, Whisker.low, Whisker.high, Dystopia..1.83....residual))
new_score <- new_score[!is.na(new_score$GDP), ]

capital <- humancaptialindex_2020
capital <- select(capital, Income.Group, Probability.of.Survival.to.Age.5, Expected.Years.of.School, Harmonized.Test.Scores, Learning.Adjusted.Years.of.School, Fraction.of.Children.Under.5.Not.Stunted, Adult.Survival.Rate)
capital <- rename(capital, Income_Group = Income.Group)
capital <- rename(capital, Probability_of_Survival_to_Age_5 = Probability.of.Survival.to.Age.5)
capital <- rename(capital, Expected_Years_Of_School = Expected.Years.of.School)
capital <- rename(capital, Test_Scores = Harmonized.Test.Scores)
capital <- rename(capital, Years_of_Schooling = Learning.Adjusted.Years.of.School)
capital <- rename(capital, Percent_Under_5_Not_Stunted = Fraction.of.Children.Under.5.Not.Stunted)
capital <- rename(capital, Adult_Survival_Rate = Adult.Survival.Rate)
capital <- capital[1:174, ]
capital <- select(capital, -c(Percent_Under_5_Not_Stunted))

capital <- group_by(capital, Income_Group)
capital <- summarise_all(capital, mean)
survival <- select(capital, Income_Group, Probability_of_Survival_to_Age_5)
expected_school <- select(capital, Income_Group, Expected_Years_Of_School)
test_scores <- select(capital, Income_Group, Test_Scores)
years_schooling <- select(capital, Income_Group, Years_of_Schooling)
adult_survival <- select(capital, Income_Group, Adult_Survival_Rate)


#PAGES AND LAYOUTS

intro_page <- tabPanel("Money as a Societal Indicator",
  tags$head(

    tags$style(HTML("
     @import url('https://fonts.googleapis.com/css2?family=Kanit&family=Press+Start+2P&display=swap');
     @import url('https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@500&display=swap');

      body {
        background-color:White;
        color:Black;
      }
      h2 {
        font-family: 'Press Start 2P', cursive;}
      p {  
      font-family: 'Roboto Mono', monospace; }
      
      .shiny-input-container {
        color: #474747;
      }"))), 
    
      
      

  

 
  titlePanel("How Does Money Impact our Societies, and our Daily Lives?"),
  
  
     h3(),
     p( "We chose to use data as a way to explore a few main questions:"),
     p(" - What societal factors does money impact, both in society at large and in our individual lives?"),
     p(" - Does having access to money impact happiness levels?"),

    
    p(""),
    p("These questions hold a wide range of infomation, but we chose to focus on a few key variables:"),
  p("- average income level of invdiuals"),
  p(" - GDP levels of countries"),
  p(" - Societal outcomes/perceptions (happiness of the culture, education levels, views on how society is run and life span of the indivduals within
      these communities)"),
  p(" - How does access to money affect our futures?"),
  
     p(" In order to narrow these question down even more, we pulled from two data sets, The Human Capital Index, and the World Happiness Report, in order to have correct range of information to evaluate the broad themes posed above."),
    p("We hope as you move through this evaluation you are able to gain a greater understanding of how humans and societies view happiness, money, the relationship between the two and how societal aspects shape these views."),
    p("In order to make this page accessible to a wide range of indivudals, we have chosen to use color blind safe colors, and when that is not an option - included an alternative way to view the data. "),
  

  column(12,
         align = "center",
  imageOutput("pic1")),
 
    )

graph_1 <- tabPanel(
  "Societal Dimensions and the Effect on Happiness Score",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = tags$span(style="color: Green;","Select a Country"),
        choices = new_score$Country
      ),
      selectInput(
        inputId = "char",
        label = tags$span(style="color: red;","Select a Second Country"),
        choices = new_score$Country
      ),
      checkboxInput(
        inputId = "choices",
        label = "Show Average of All Variables",
        value = FALSE,
        width = NULL
        
      )
    ),
    mainPanel(
      plotOutput(outputId = "radar"),
      tableOutput(outputId = "table2")
    )
  )

)

graph_2 <- tabPanel(
  
  "Potential for Human Capital",
  
  h2("Human Capital Outcomes"),
  plotlyOutput("graph2"),
  
  
  h2("What is Human Capital?"),
  p("HCL Definition as described by the World Bank:"),
  p("Ranging between 0 and 1, the index takes the value 1 only if a child born today can expect to 
  achieve full health (deﬁned as no stunting and survival up to at least age 60) and achieve her formal 
  education potential (deﬁned as 14 years of high-quality school by age 18)."),
  p("A country’s score is its distance to the “frontier” of complete education and full health. If it scores 0.70 in the Human Capital Index, 
  this indicates that the future earnings potential of children born today will be 70% of what they could have 
  been with complete education and full health."),
  p("https://www.worldbank.org/en/publication/human-capital/brief/the-human-capital-project-frequently-asked-questions?cid=GGH_e_hcpexternal_en_ext#HCI8)"),
  
  h2("What Variables Contribute?:"),
  column(4,
  selectInput("con",
              "Country",
              c("All", 
                unique(as.character(humanindex$country))))
),

column(4,
       selectInput("income",
                   "Income_Level",
                   c("All",
                     unique(as.character(humanindex$income_level))))
      
),
 DT::dataTableOutput("table")
)


graph_3 <- tabPanel(
  "How does Income Level Effect Different Aspects of Life? ",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "options",
        label = "Select Variable:",
        choices = list("Adult Survival Rate", "Years of Schooling", "Test Scores", "Expected Years of School", "Probability of Survival to Age 5"),
      ),
      checkboxInput(
        inputId = "xyz",
        label = "Show Exact Values for Each Income Group",
        value = FALSE,
        width = NULL
      ),
      verbatimTextOutput(outputId = "xxx")
    ),
    mainPanel(
      plotOutput(outputId = "bar"),
    )
  )
  
)


sum_page <- tabPanel(
  "Cumulative Findings",
  titlePanel("To Wrap Things Up..."),
  p("A Sample of Countries and their Perception on Societal Factors:"),
  tableOutput("nstable"),
  p("Above we can see a general trend that having a higher GDP leads higher outcomes in varying perceptions of societal aspects."),
  p("This is not a universal correlation, as we have found that GDP does not directly correlate to higher outcomes for viewing socital factors even if it is a trend."),
  p("We did find that countries with higher GDP's had greater chances for their citizens to create and hold human capital, with this corralation being shown below."),
  p("Human Capital Potenial/ The ability for indivdials to gain and hold wealth"),
  tableOutput("hcl"),
  p("Delving deeper into society, we evaluated how differing income groups had significantly chnaged outcomes in varying aspects."),
  p("Below is a sampling of how these income groups effect our daily lives:"),
  tableOutput("livetable"),
  tableOutput("schtable"),
  tableOutput("testt"),
p("Taking all of these findings into consideration we have determined that income and access to money can play a role on not only our present lives but our future."),
p("Even if happiness is an abstract topic, we have found that generally living in a society that has a higher GDP level increases societal outcomes - which may increase the happiness levels of those living within the society."),
  column(12,
         align = "center",
         imageOutput("pic2")),
)







# ui 
ui <- fluidPage(
  theme = shinytheme("flatly"),

  navbarPage(
    "b5// Evalution of Money's Impacts on Varying Socital Aspects",
    intro_page,
    graph_1,
    graph_2,
    graph_3,
    sum_page
  )
  )






server <- function(input, output){
  
#summary info table 
  
  table_hcl_hl <- data.frame(humancaptialindex_2020$X, 
                             humancaptialindex_2020$Income.Group,
                             humancaptialindex_2020$HUMAN.CAPITAL.INDEX.2020) 
  table_hcl_hl = rename(table_hcl_hl, country = humancaptialindex_2020.X, 
                        avg_income_level = humancaptialindex_2020.Income.Group,
                        potential_human_capital = humancaptialindex_2020.HUMAN.CAPITAL.INDEX.2020)
  table_hcl_hl <- na.omit(table_hcl_hl)
  
  table_hcl_hl <- table_hcl_hl %>%
    group_by(avg_income_level)%>%
    summarise(ability_for_captial = mean(potential_human_capital))
  
  table_ns <- data.frame(new_score)
  table_ns <- table_ns %>%
    slice_sample(n = 5) 
  
  
  survivaltable <- data.frame(adult_survival) %>%
    slice_sample(n = 3)
  schooltable <- data.frame(expected_school) %>%
    slice_sample(n = 3)
  testtable <- data.frame(test_scores)%>%
    slice_sample(n = 3)
  yearstable <- data.frame(years_schooling) %>%
    slice_sample(n = 3)
  
output$nstable <- renderTable({table_ns})  
output$hcl <- renderTable({table_hcl_hl})
output$livetable <- renderTable(survivaltable)
output$schtable <- renderTable(schooltable)
output$testt <- renderTable(testtable)

  
#image rendering 
  output$pic1 <- renderImage({
    filename <- normalizePath(file.path("www/newheader.jpg"))
    
    list(src = filename,
         alt = paste("pic1"))
  }, deleteFile = FALSE)
  

  output$pic2 <- renderImage({
    filename <- normalizePath(file.path("www/newheader.jpg"))
    
    list(src = filename,
         alt = paste("pic2"))
  }, deleteFile = FALSE)
  
 
  #graph 2 

  humanindex$hover <- with(humanindex, paste(country, "<br>", "HCL:", human_capital,
                                             '<br>', "Income level:", income_level, "<br>",
                                             "Average Test Scores:", avg_test_scores, "<br>",
                                             "Average Years in School", avg_years_in_school,
                                             "<br>",
                                             "Adult Survival Rate", adult_survival_rate))
  roundy <- list(
    projection = list(
      type = 'orthographic'
    ),
    showland = TRUE,
    landcolor = toRGB("#e5ecf6"))
  
  
  ahhhhhhh <- plot_geo(humanindex, locationmode = 'worldmap')
  ahhhhhhh <- ahhhhhhh %>% add_trace(
    z = ~humanindex$human_capital, text = ~hover, locations = ~humanindex$code,
    color = humanindex$human_capital, colors = "Purples")
  ahhhhhhh <- ahhhhhhh %>% layout(geo = roundy)
  ahhhhhhh <- ahhhhhhh %>% colorbar(title = "Human Capital Index")
  
  
  output$graph2 <- renderPlotly(ahhhhhhh)
  output$table <- DT::renderDataTable(DT::datatable({
    data <- humanindex
    if (input$con != "All") {
      data <- data[data$country == input$con,]
    }
if (input$income != "All") {
  data <- data[data$income_level == input$income,]
}
    data
  }))
  
  #graph 1
  make_radar_df <- function(country_name, name_country){
    rd_df <- select(new_score, -c(Country))
    rd_df <- rd_df[!is.na(rd_df$GDP), ]
    
    min_df <- summarise_all(rd_df, min)
    max_df <- summarise_all(rd_df, max)
    
    data_pt <- filter(new_score, Country == country_name)
    data_pt <- select(data_pt,-c(Country))
    
    data_pt2 <- filter(new_score, Country == name_country)
    data_pt2 <- select(data_pt2,-c(Country))
    
    
    #lets glue all three dataframes together 
    return(do.call("rbind", list(max_df, min_df, data_pt, data_pt2)) )
  }
  
  make_average <- function(country_name, name_country) {
    rd_df <- select(new_score, -c(Country))
    rd_df <- rd_df[!is.na(rd_df$GDP), ]
    
    min_df <- summarise_all(rd_df, min)
    max_df <- summarise_all(rd_df, max)
    
    data_pt <- filter(new_score, Country == country_name)
    data_pt <- select(data_pt,-c(Country))
    
    data_pt2 <- filter(new_score, Country == name_country)
    data_pt2 <- select(data_pt2,-c(Country))
    
    average <- summarise_all(rd_df, mean)
    
    return(do.call("rbind", list(max_df, min_df, data_pt, data_pt2, average)) )
  }
  
  make_table <- function(country_name, z) {
    country_one <- filter(all_data, Country == country_name)
    country_two <- filter(all_data, Country == z)
    
    total <- rbind(country_one, country_two)
    
    return(total)
  }
  
  make_average_table <- function(country_name, z) {
    country_one <- filter(all_data, Country == country_name)
    country_two <- filter(all_data, Country == z)
    
    average <- select(all_data, -c(Country))
    average <- average[!is.na(average$GDP), ]
    average <- summarise_all(average, mean)
    Country <- c("Average")
    
    average_all <- cbind(Country, average)
    
    total <- rbind(country_one, country_two, average_all)
    
    return(total)
  }
  
  output$table2 <- renderTable({
    if(input$choices == FALSE) {
      return(make_table(input$country, input$char))
      
    } else {
      return(make_average_table(input$country, input$char))
    }
  })
  
  output$radar <- renderPlot({
    if(input$choices == FALSE) {
      colors_border=c(rgb(0,0.4,0,0.9), rgb(1,0,0,0.9))
      
      radarchart(make_radar_df(input$country, input$char), pcol = colors_border, plty = 1)
      title(main = "Effect of Different Societal Aspects on Happiness Score")
      
    } else {
      colors_border=c(rgb(0,0.4,0,0.9), rgb(1,0,0,0.9), rgb(0,0,0,0.9))
      colors_in =c(rgb(0,0,0,0), rgb(0,0,0,0), rgb(0,0,0,0.3))
      
      radarchart(make_average(input$country, input$char), pcol = colors_border, pfcol = colors_in, plty = 1)
      title(main = "Effect of Different Societal Aspects on Happiness Score")
      
    }
  })
  
  make_radar_df <- function(country_name, name_country){
    rd_df <- select(new_score, -c(Country))
    rd_df <- rd_df[!is.na(rd_df$GDP), ]
    
    min_df <- summarise_all(rd_df, min)
    max_df <- summarise_all(rd_df, max)
    
    data_pt <- filter(new_score, Country == country_name)
    data_pt <- select(data_pt,-c(Country))
    
    data_pt2 <- filter(new_score, Country == name_country)
    data_pt2 <- select(data_pt2,-c(Country))
    
    
    #lets glue all three dataframes together 
    return(do.call("rbind", list(max_df, min_df, data_pt, data_pt2)) )
  }
  
  make_average <- function(country_name, name_country) {
    rd_df <- select(new_score, -c(Country))
    rd_df <- rd_df[!is.na(rd_df$GDP), ]
    
    min_df <- summarise_all(rd_df, min)
    max_df <- summarise_all(rd_df, max)
    
    data_pt <- filter(new_score, Country == country_name)
    data_pt <- select(data_pt,-c(Country))
    
    data_pt2 <- filter(new_score, Country == name_country)
    data_pt2 <- select(data_pt2,-c(Country))
    
    average <- summarise_all(rd_df, mean)
    
    return(do.call("rbind", list(max_df, min_df, data_pt, data_pt2, average)) )
  }
  
  make_table <- function(country_name, z) {
    country_one <- filter(all_data, Country == country_name)
    country_two <- filter(all_data, Country == z)
    
    total <- rbind(country_one, country_two)
    
    return(total)
  }
  
  make_average_table <- function(country_name, z) {
    country_one <- filter(all_data, Country == country_name)
    country_two <- filter(all_data, Country == z)
    
    average <- select(all_data, -c(Country))
    average <- average[!is.na(average$GDP), ]
    average <- summarise_all(average, mean)
    Country <- c("Average")
    
    average_all <- cbind(Country, average)
    
    total <- rbind(country_one, country_two, average_all)
    
    return(total)
  }
  
  output$table2 <- renderTable({
    if(input$choices == FALSE) {
      return(make_table(input$country, input$char))
      
    } else {
      return(make_average_table(input$country, input$char))
    }
  })
  
  output$radar <- renderPlot({
    if(input$choices == FALSE) {
      colors_border=c(rgb(0,0.4,0,0.9), rgb(1,0,0,0.9))
      
      radarchart(make_radar_df(input$country, input$char), pcol = colors_border, plty = 1)
      title(main = "Effect of Different Societal Aspects on Happiness Score")
      
    } else {
      colors_border=c(rgb(0,0.4,0,0.9), rgb(1,0,0,0.9), rgb(0,0,0,0.9))
      colors_in =c(rgb(0,0,0,0), rgb(0,0,0,0), rgb(0,0,0,0.3))
      
      radarchart(make_average(input$country, input$char), pcol = colors_border, pfcol = colors_in, plty = 1)
      title(main = "Effect of Different Societal Aspects on Happiness Score")
      
    }
  })
  
  output$xxx <- renderText({
    if(input$options == "Adult Survival Rate" && input$xyz == TRUE) {
      paste("High Income Adult Survival Rate: 0.92 ", "Upper Middle Income Adult Survival Rate: 0.8557 ", 
            "Lower Middle Income Adult Survival Rate: 0.804 ", "Low Income Adult Survival Rate: 0.746 ", sep="\n")
    } else if  (input$options == "Years of Schooling" && input$xyz == TRUE) {
      paste("High Income Years of Schooling: 10.29m ", "Upper Middle Income Years of Schooling: 7.80 ", 
            "Lower Middle Income Years of Schooling: 6.55 ", "Low Income Years of Schooling: 4.34 ", sep="\n")
      
    } else if (input$options == "Test Scores" && input$xyz == TRUE) {
      paste("High Income Test Scores: 486.92 ", "Upper Middle Income Test Scores: 410.7 ", 
            "Lower Middle Income Test Scores: 391.67 ", "Low Income Test Scores: 356.08 ", sep="\n")
      
    } else if (input$options == "Expected Years of School" && input$xyz == TRUE) {
      paste("High Income Expected Years of School: 13.16 ", "Upper Middle Income Expected Years of School: 11.82 ", 
            "Lower Middle Income Expected Years of School: 10.42 ", "Low Income Expected Years of School: 7.60 ", sep="\n")
      
    } else if (input$options == "Probability of Survival to Age 5" && input$xyz == TRUE) {
      paste("High Income Probability of Survival to Age 5: 0.994 ", "Upper Middle Income Probability of Survival to Age 5: 0.982 ", 
            "Lower Middle Income Probability of Survival to Age 5: 0.927 ", "Low Income Probability of Survival to Age 5: 0.958 ", sep="\n")
    }
    
  })
  
  output$bar <- renderPlot({
    if(input$options == "Adult Survival Rate") {
      ggplot(adult_survival, aes(x=Income_Group, y=Adult_Survival_Rate)) + 
        geom_bar(stat = "identity", fill = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"), colour = "black") +  
        labs(y = "Adult Survival Rate", x = "Income Group") +
        ggtitle("Adult Survival by Income Group") +
        theme(legend.position="none") 
    } else if  (input$options == "Years of Schooling") {
      ggplot(years_schooling, aes(x=Income_Group, y=Years_of_Schooling)) + 
        geom_bar(stat = "identity", fill = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"), colour = "black") + 
        labs(y = "Years of Schooling", x = "Income Group") +
        ggtitle("Years of Schooling by Income Group") +
        theme(legend.position="none") 
    } else if (input$options == "Test Scores") {
      ggplot(test_scores, aes(x=Income_Group, y=Test_Scores)) + 
        geom_bar(stat = "identity", fill = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"), colour = "black") +  
        labs(y = "Test Scores", x = "Income Group") +
        ggtitle("Test Scores by Income Group") +
        theme(legend.position="none") 
    } else if (input$options == "Expected Years of School") {
      ggplot(expected_school, aes(x=Income_Group, y=Expected_Years_Of_School)) + 
        geom_bar(stat = "identity", fill = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"), colour = "black") +
        labs(y = "Expected Years of School", x = "Income Group") +
        ggtitle("Expected Years of School by Income Group") +
        theme(legend.position="none") 
    } else if (input$options == "Probability of Survival to Age 5") {
      ggplot(survival, aes(Income_Group, Probability_of_Survival_to_Age_5)) + 
        geom_bar(stat = "identity", fill = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"), colour = "black") +
        labs(y = "Probability of Survival to Age 5", x = "Income Group") +
        ggtitle("Probability of Survival to Age 5 by Income Group") +
        theme(legend.position="none") 
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
