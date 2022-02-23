rm(list = ls()) 
gc() 

#-----------------------------------------
# Part 1: Load data and library packages
#-----------------------------------------

library(shiny)
library(tidyverse)
library(tools)
library(shinythemes)


S.J<-read.csv("standard.csv",header = T)
A.J<-read.csv("advanced.csv",header = T)
b.J<-read.csv("more bb.csv",header = T)
Pitch.J<-read.csv("pitch type J.csv",header = T)
Plate.J<-read.csv("plate discipline J.csv",header = T)
dash<-read.csv("Dashboard.csv",header = T)
sd<-read.csv("sd2.csv",header = T)
adv<-read.csv("adv2.csv",header = T)
pv<-read.csv("pv2.csv",header = T)
pt<-read.csv("pt2.csv",header = T)
pd<-read.csv("pd2.csv",header = T)
bb<-read.csv("bb2.csv",header = T)

PA.J<-S.J$PA
one<-rep(1,154)
ball.J<-b.J$Pitches
Avg<-function(x,pa,d)
{
  stat.ds<-numeric(length(x))
  stat.ds[1]<-x[1]
  for (i in 2:d-1) 
  {
    stat.ds[i]<-sum(x[1:i]*pa[1:i])/sum(pa[1:i])  
  }
  for (i in d:length(x)) 
  {
    stat.ds[i]<-sum(x[(i-d+1):i]*pa[(i-d+1):i])/sum(pa[(i-d+1):i])  
  }
  stat.ds
}
oswing<-Plate.J$O.Swing.
contact<-Plate.J$Contact.
SL<-Pitch.J$SL.
wOBA.J<-S.J$wOBA
Kper.J<-S.J$K
colnames(dash)<-paste(c("Season","Player","BB%","K%","AVG","OBP","SLG","wOBA","wRC+","SL%","wSL/c","O-Swing%","Contact%"))
colnames(adv)<-paste(c("Season","Type","Player","Tean","G","PA","HR","RBI","BB","SB","BB%","K%","ISO","BABIP","AVG","OBP","SLG","wOBA","wRC+","BsR","Off","Def","WAR"))
colnames(bb)<-paste(c("Season","Type","Player","Team","GB/FB","LD%","GB%","FB%","IFFB%","HR/FB","IFH%","BUH%","Pull%","Cent%","Oppo%","Soft%","Med%","Hard%"))
colnames(pt)<-paste(c("Season","Type","Player","Team","FB%","SL%","CT%","CB%","CH%","SF%","Others%"))
colnames(pv)<-paste(c("Season","Type","Player","Team","wFB","wSL","wCT","wCB","wCH","wSF","wFB/c","wSL/c","wCT/c","wCB/c","wCH/c","wSF/c"))
colnames(pd)<-paste(c("Season","Type","Player","Team","O-Swing%","Z-swing%","Swing%","O-Contact%","Z-Contact%","Contact%","Zone%","F-Strike%","SwStri%"))
cols <- c("A" = "black", "B" = "#55BF3B", "C" = "#f15c80")
#---------------------
# Part 2: UI
#---------------------
# User interface: Controls the layout and appearance of app


ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Enter text for Game number
      textInput(inputId = "d", 
                label = "Average by Games(1~154)", 
                placeholder = "Input games number(1~154)",
                
                value = 15),
      
      # Set wOBA level
      sliderInput(inputId = "wOBA", 
                  label = "Transparency of wOBA:", 
                  min = 0, max = 1, 
                  value = 1),
      # Set K% level
      sliderInput(inputId = "K", 
                  label = "Transparency of K%:", 
                  min = 0, max = 1, 
                  value = 0),
      # Set Slider% level
      sliderInput(inputId = "SL", 
                  label = "Transparency of SL%:", 
                  min = 0, max = 1, 
                  value = 0),
      # Set Contact% level
      sliderInput(inputId = "Con", 
                  label = "Transparency of Contact%:", 
                  min = 0, max = 1, 
                  value = 0),
      # Set O-swing% level
      sliderInput(inputId = "osw", 
                  label = "Transparency of O-swing%:", 
                  min = 0, max = 1, 
                  value = 0),
      # Show Reference line or not
      radioButtons(inputId = "radio",
                   label = "Reference line",
                   choices = list("Season Average"=1,
                                  "MLB average"=2,
                                  "None"=3), 
                   selected =3),
      # Show data table or not
      checkboxInput(inputId = "summary",
                    label = "Show Season Summary",
                    value = TRUE),
      
      conditionalPanel(
        condition = "input.summary == true",
        
        selectInput(inputId = "stat",
                    choices = c("Simplified Dashboard"=1,
                                "Full Statistics"=2),
                    label = "Summary Type",
                    selected = 1)
        
      )),
    
    
    mainPanel(
      
      # Show scatterplot
      plotOutput(outputId = "lineplot"),
      # Show data table1
      DT::dataTableOutput(outputId = "dashboard"),
      # Show data table2
      DT::dataTableOutput(outputId = "sd"),
      # Show data table3
      DT::dataTableOutput(outputId = "bb"),
      # Show data table4
      DT::dataTableOutput(outputId = "pt"),
      # Show data table5
      DT::dataTableOutput(outputId = "pv"),
      # Show data table6
      DT::dataTableOutput(outputId = "pd")
    )  
  )
)
#---------------------
# Part 3: server
#---------------------
# Contains instructions needed to build app

server <- function(input, output) {
  
  title<-reactive({paste("2017 Season Aaron Judge's",as.numeric(input$d),"Games Rolling Average")})
  p<-reactive({ggplot()+
      labs(title = title())+
      xlab("Games")+
      ylab("wOBA")+
      scale_y_continuous(sec.axis = sec_axis(~.*100 ,name = "Contact/ K / O-swing / Slider[%]"))+
      geom_line(data=NULL,aes(x=1:154,y=Avg(wOBA.J,PA.J,as.numeric(input$d)),colour="wOBA"),alpha=input$wOBA,size=1,show.legend =TRUE)+
      geom_line(data=NULL,aes(x=1:154,y=Avg(Kper.J,PA.J,as.numeric(input$d)),colour="K%"),alpha=input$K,size=1,show.legend =TRUE)+
      geom_line(data=NULL,aes(x=1:154,y=Avg(SL,ball.J,as.numeric(input$d)),colour="SL%"),alpha=input$SL,size=1,show.legend =TRUE)+
      geom_line(data=NULL,aes(x=1:154,y=Avg(contact,ball.J,as.numeric(input$d)),colour="Contact%"),alpha=input$Con,size=1,show.legend =TRUE)+
      geom_line(data=NULL,aes(x=1:154,y=Avg(oswing,ball.J,as.numeric(input$d)),colour="O-swing%"),alpha=input$osw,size=1,show.legend =TRUE)+
      scale_colour_manual(name=" ",
                          values=c("wOBA"= "black", "K%"="red","SL%"="blue","Contact%"="orange","O-swing%"="purple"))
    
    
  })
  p.s<-reactive({p()+
      
      geom_hline(yintercept=0.430, linetype="dashed", color = "black",size=1,alpha=input$wOBA)+
      geom_hline(yintercept=0.307, linetype="dashed", color = "red",size=1,alpha=input$K)+
      geom_hline(yintercept=0.225, linetype="dashed", color = "blue",size=1,alpha=input$SL)+
      geom_hline(yintercept=0.676, linetype="dashed", color = "orange",size=1,alpha=input$Con)+
      geom_hline(yintercept=0.247, linetype="dashed", color = "purple",size=1,alpha=input$osw)
  })
  p.m<-reactive({p()+
      
      geom_hline(yintercept=0.321, linetype="dashed", color = "black",size=1,alpha=input$wOBA)+
      geom_hline(yintercept=0.216, linetype="dashed", color = "red",size=1,alpha=input$K)+
      geom_hline(yintercept=0.163, linetype="dashed", color = "blue",size=1,alpha=input$SL)+
      geom_hline(yintercept=0.775, linetype="dashed", color = "orange",size=1,alpha=input$Con)+
      geom_hline(yintercept=0.299, linetype="dashed", color = "purple",size=1,alpha=input$osw)
  })
  
  
  
  
  output$lineplot<-renderPlot({
    if(input$radio==1){
      p.s()
    }else if(input$radio==2){
      p.m()
    }else {p()}  
  })
  # Create data table
  output$dashboard <- DT::renderDataTable({
    if(input$summary){
      if(input$stat==1){
        
        DT::datatable(data = dash,rownames = FALSE)}
      else if (input$stat==2){
        DT::datatable(data = adv,rownames = FALSE)
      }
    }
  })
  output$sd <- DT::renderDataTable({
    if(input$summary){
      if(input$stat==2){
        
        DT::datatable(data = sd,rownames = FALSE)}
    }
  })
  output$bb <- DT::renderDataTable({
    if(input$summary){
      if(input$stat==2){
        DT::datatable(data = bb,rownames = FALSE)}
    }
  })
  output$pt <- DT::renderDataTable({
    if(input$summary){
      if(input$stat==2){
        
        DT::datatable(data = pt,rownames = FALSE)}
    }
  })
  output$pv <- DT::renderDataTable({
    if(input$summary){
      if(input$stat==2){
        
        DT::datatable(data = pv,rownames = FALSE)}
    }
  })
  output$pd <- DT::renderDataTable({
    if(input$summary){
      if(input$stat==2){
        
        DT::datatable(data = pd,rownames = FALSE)}
    }
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

