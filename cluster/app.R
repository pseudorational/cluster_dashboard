#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Clustering with Iris"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("number_of_clusters",
                         "Number of clusters",
                         value=2,
                         min = 1,
                         max = 10,
                         step = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cluster_scatter")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
    output$cluster_scatter <- renderPlot({
        set.seed(1031)
        k_clusters = kmeans(iris[,3:4],centers = input$number_of_clusters,iter.max = 100,nstart = 25)
        clusters = k_clusters$cluster
        library(ggplot2)
        ggplot(data=cbind(iris[,3:5], clusters),aes(x=Petal.Length,y=Petal.Width,shape=Species, color=factor(clusters)))+
            geom_point()
    })
}    
# Run the application 
shinyApp(ui = ui, server = server)

