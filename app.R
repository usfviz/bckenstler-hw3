library(shiny)
library(ggplot2)
if(!"GGally" %in% installed.packages()) install.packages('GGally')
library(GGally)
library(MASS)

fb <- read.csv('data/dataset_Facebook.csv',sep = ';')
fb <- fb[fb$Total.Interactions!=6334,]

ui <- fluidPage(
  
  titlePanel("FB Post Interactions and Times"),
  
  fluidRow(
      mainPanel(tabsetPanel(
        tabPanel("Interactions vs. Post Times", plotOutput("scatterPlot")),
        tabPanel("Correlation Between Interactions/Time", plotOutput("scatterPlot_matrix")),
        tabPanel("Parallel Post Interactions and Time of Day", plotOutput("parcoords"))
      )
      )
    )
  ,
  fluidRow(
    column(width = 6,
           radioButtons(
             inputId = 'type',
             label = 'Type of Post',
             choices = c("All" = -1, 
                         "Photo" = "Photo", 
                         "Link" = "Link",
                         "Video" = "Video",
                         "Status" = "Status"
             )
           )
    ),
    column(width = 6,
           radioButtons(
             inputId = 'interaction',
             label = 'Type of Interaction',
             choices = c("Comment" = "comment", 
                         "Like" = "like",
                         "Share" = "share"
             )
           )
    )
  )
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    type <- input$type
    inter <- input$interaction
    if(type !=-1){
      indices <- fb$Type==type
    }    
    else indices <- !vector(mode = "logical", length = length(fb$Type))
    x <- fb$Post.Hour[indices]
    if(inter=="comment")   y <- fb$comment[indices]
    else if(inter=="like") y <- fb$like[indices]
    else y <- fb$share[indices]
    total.interactions <- fb$Total.Interactions[indices]
    post.type <- fb$Type[indices]
    qplot(x, y, xlab='Hour of the Day', ylab='Number of Interactions', size=total.interactions,
          color=post.type)

  })
  output$scatterPlot_matrix <- renderPlot({
    type <- input$type
    inter <- input$interaction
    if(type !=-1){
      indices <- fb$Type==type
    }    
    else indices <- !vector(mode = "logical", length = length(fb$Type))
    comments <- fb$comment[indices]
    likes <- fb$like[indices]
    shares <- fb$share[indices]
    hour.of.day <- fb$Post.Hour[indices]
    ggpairs(data.frame(comments, likes, shares, hour.of.day))
  })
  output$parcoords <- renderPlot({
    type <- input$type
    inter <- input$interaction
    if(type !=-1){
      indices <- fb$Type==type
    }    
    else indices <- !vector(mode = "logical", length = length(fb$Type))
    hour.of.day <- fb$Post.Hour[indices]
    comments <- fb$comment[indices]
    likes <- fb$like[indices]
    shares <- fb$share[indices]
    parcoord(data.frame(hour.of.day, comments, likes, shares), rainbow(length(comments)))
  })
}
shinyApp(ui = ui, server = server)

