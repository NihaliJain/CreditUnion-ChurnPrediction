
install.packages("ggpubr")
install.packages("polycor")
install.packages("Hmisc")
install.packages("plotly")
install.packages("shinydashboard")
library("ggpubr")
library(ggm)
library(ggplot2)
library(Hmisc)
library(plyr)
library(corrplot)
library(plotly)
library(shinydashboard)
library(data.table)
library(cluster)
library(factoextra)
library(NbClust)

#Data Loading and Clustering based on RMT score

Cust_data = fread("C:\\Users\\nihuj\\Desktop\\Predictive analysis in finance\\CU_EDA_variables.csv")
#Using only July metrics for clustering as some of them have churned in the subsequent months

norm_func = function(coln){
val = (coln - mean(coln))/(sd(coln))
return(val)
}

Cust_data[,norm_jul_tr:= norm_func(July.Trans)]
Cust_data[,norm_jul_sav:= norm_func(Savings_jul)]
Cust_data[,norm_jul_ln:= norm_func(Loan_jul)]
Cust_data[,norm_months:= norm_func(loyalty_months)]
Cust_data[,RMT_score := norm_jul_tr+norm_jul_sav+norm_jul_ln+norm_months]

clust = kmeans(Cust_data[,.(RMT_score)],4)
Cust_data[,Cust_clust := clust$cluster]



#Vizualization on R Shiny Dashboard

header <- dashboardHeader(title = "R Shiny Dashboard")  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
  )
)



frow2 <-fluidRow(
  box(
    title = "Plot 1"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("ByAge", height = "300px")
    
  )
  ,box(
    title = "Plot 2"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("ByType", height = "300px")
  ) 
)
# combine the two fluid rows to make the body
body <- dashboardBody(frow2)

Cluster_name=as.character(Cust_data2$Cust_clust)
Cluster_name1=as.character(Cust_data$Cust_clust)

ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

server <- function(input, output) 
{
  
  output$ByAge <- renderPlot({
    
    qplot(seq_along(Cust_data$RMT_score), Cust_data$RMT_score)+ 
      geom_point(data = Cust_data, 
                 aes(color=Cluster_name1) )+ ylab("RMT_score") + 
      xlab("Cluster") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Clusters by RMT_score")})
  
  output$ByType<- renderPlot({
    qplot(seq_along(Cust_data2$RMT_score), Cust_data2$RMT_score)+ 
      geom_point(data = Cust_data2, 
                 aes(color=Cluster_name) )+ ylab("RMT_score") + 
      xlab("Cluster") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Clusters by RMT_score removing outliers")})
  
  
  
}
shinyApp(ui, server)

