library(ggplot2)
library(mclust)
library(shiny)
library(readxl)
ui <- fluidPage(
  h2('The uploaded file data'),
  dataTableOutput('mytable'),
  sidebarPanel(fileInput('file', 'Choose excel file to upload',
            accept = c(
              '.xlsx'
            )
  ),
  ################################################################
  
  actionButton("choice", "Show my data"),
  
  selectInput("EM", "Select Cluster Variable", choices = NULL), 
  
  selectInput("x_axis", "Select m/z Variable", choices = NULL), 
  
  selectInput("y_axis", "Select Intensity Variable", choices = NULL),
  
  numericInput("nthCluster", "Select One Cluster", 1,min=1,max=NA),
  
  actionButton("Plot","Click to plot")
  
),
mainPanel(plotOutput("plot1"),tableOutput("table1"))
)


server <- function(input, output,session){ info <- eventReactive(input$choice, {
  inFile <- input$file
  req(inFile)
  
  f <- as.data.frame(read_excel(inFile$datapath))
  f <- f[complete.cases(f),]
  vars <- names(f)

  updateSelectInput(session, "EM", "Select Cluster Variable", choices = vars)
  updateSelectInput(session, "x_axis", "Select m/z Variable", choices = vars)
  updateSelectInput(session, "y_axis", "Select Intensity Variable", choices = vars)
  
  
  f

})

info1 <- eventReactive(input$Plot,{mod1 <- Mclust(subset(info(),select=input$EM))
cluster <- mod1$classification
g <- subset(info(),select = c(input$x_axis,input$y_axis))
e <- cbind(cluster,g)
w <- subset(e,cluster==input$nthCluster)
w})



output$table1 <- renderTable(info())


output$plot1<-renderPlot({
  ggplot(info1(),aes(x=info1()[,2],y=info1()[,3]))+geom_bar(stat="identity",width=0.2,position="dodge")+
    theme_classic()+labs(x="m/z",y="Intensity")
  })
}
shinyApp(ui, server)

