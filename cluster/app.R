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
    h1("Clustering with Iris"),
    numericInput("num_clust","Number of clusters",value=2,min = 1,max = 10,step = 1),
            selectInput('x','Select x',choices = names(iris[,1:4]),'Sepal.Length'),
            selectInput('y','Select y',choices = names(iris[,1:4]),'Sepal.Width'),
     mainPanel(
            plotOutput("cluster_scatter")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) { 
    output$cluster_scatter <- renderPlot({
        set.seed(1031)
        k_clusters = kmeans(iris[,c(input$x,input$y)],centers = input$num_clust,iter.max = 100,nstart = 25)
        clusters = k_clusters$cluster
        library(ggplot2)
        df = data.frame(x = iris[,input$x],y = iris[,input$y], clusters, Species = iris$Species)
        ggplot(df, aes(x,y,shape=Species, color=factor(clusters)))+
                   geom_point()
        #ggplot(data=cbind(iris, clusters),aes(x=noquote(input$x),y=Sepal.Width,shape=Species, color=factor(clusters)))+
        #    geom_point()
    })
}    
# Run the application 
shinyApp(ui = ui, server = server)

