
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(exact2x2)
library(shinythemes)
library(DT)





fet <- function(a,b,c,d) {round(fisher.test(matrix(c(a,b,c,d), byrow=T, nrow=2))$p.value,3)}

n1<-n2<-16

matrix_out<- matrix("NA",n1+1,n2+1)
for (i in 0:n1){
        for (j in 0:n2){
                matrix_out[i+1,j+1] <- fet(i,n1-i,j,n2-j)
                
        }
}
matrix_out<-as.data.frame(matrix_out)





ui <- fluidPage(
        
        theme = shinytheme("united"),
        
        titlePanel("Power and Sample Size Calculation",windowTitle = "Power/SampleSize"),
        hr(),
        h4("This is a simplified calculator; for now we will assume 2 groups only. 
           Also, we will come back to time-to-event"),
        br(),br(),
        h5("If you have a set sample size and want to know the power, 
           choose 'Power' tab, otherwise choose 'SampleSize' tab"),
        
        sidebarLayout(
                
                sidebarPanel(width = 5,
                             
                             wellPanel(
                                     h5("Set your expected proportion of outcome (yes; infected; dead)"),
                                     br(),
                                     h5("in the Control Group:"),
                                     numericInput(inputId = "p1",
                                                  label = "p1",
                                                  value=0.2,
                                                  min=0.0001,max=1),
                                     
                                     br(),
                                     h5("in the Experimental Group:"),
                                     numericInput(inputId = "p2",
                                                  label = "p2",
                                                  value=0.8,
                                                  min=0.0001,max=1)
                             ),
                             hr(),
                             hr(),
                             
                             conditionalPanel("input.tabselected=='Power'",
                                              h5("Set sample size in each group (enter an integer between 0 and 16):"),
                                              br(),
                                              h5("in the Control Group:"),
                                              numericInput(inputId = "n1",
                                                           label = "n1",
                                                           value=5,
                                                           min=1,max=16),
                                              
                                              br(),
                                              h5("in the Experimental Group:"),
                                              numericInput(inputId = "n2",
                                                           label = "n2",
                                                           value=5,
                                                           min=1,max=16)
                             ),
                             
                             conditionalPanel("input.tabselected=='SampleSize'",
                                              h5("Set the power (enter a number between 0 and 1):"),
                                              br(),
                                              numericInput(inputId = "power",
                                                           label = "Power",
                                                           value=0.9,
                                                           min=0, max=1)
                             ),
                             conditionalPanel("input.tabselected=='P-value Table'",
                                              h5("Set sample size in each group (enter an integer between 0 and 16):"),
                                              br(),
                                              h5("in the Control Group:"),
                                              numericInput(inputId = "n1_",
                                                           label = "n1",
                                                           value=2,
                                                           min=1,max=16),
                                              
                                              br(),
                                              h5("in the Experimental Group:"),
                                              numericInput(inputId = "n2_",
                                                           label = "n2",
                                                           value=2,
                                                           min=1,max=16)
                             )
                ),
                
                mainPanel( width=7,
                        tabsetPanel(
                                id = "tabselected",
                                tabPanel("Power",
                                         br(),
                                         br(),
                                         textOutput("output_power")),
                                tabPanel("SampleSize",
                                         br(),
                                         textOutput("output_samplesize"),
                                         br(),
                                         br(),
                                         plotOutput("output_hist")),
                                tabPanel("P-value Table",
                                         DT::dataTableOutput("output_table"),
                                         br(),
                                         br(),
                                         textOutput("des"))
                                
                                )
                        )
                )
        )





server <- function(input, output) {
        
        ### Power tab
        output$output_power<-renderText({
                paste("Power =",round(power2x2(input$p1,input$p2,input$n1,input$n2)$power,3))
        })
        
        
        ### SampleSize tab and histogram
        output$output_samplesize<-renderText({
                paste("Total Sample Size per Group =",ss2x2(input$p1,input$p2,power=input$power)$n0)
        })
        myTab<-reactive({
                out <- matrix("NA",input$n1+1,input$n2+1)
                for (i in 0:input$n1){
                        for (j in 0:input$n2){
                                out[i+1,j+1] <- fet(i,input$n1-i,j,input$n2-j)
                                
                        }
                }
                n <- 10000
                temp <- rep(out, round(n*t(t(dbinom(0:input$n1,input$n1,input$p1))) %*% dbinom(0:input$n2,input$n2,input$p2)))
        })
        output$output_hist<-renderPlot({
                hist(as.numeric(myTab()),breaks=20, xlab="Distribution of P-values", probability=T,yaxt="n", 
                     main="This is a histogram of the distributino of \n p-values you might get \n the line shows p=.05",ylab="")
                abline(v=.05, col="red")
                mtext(paste(round(mean(myTab()<=.05),3)," | ",round(mean(myTab()>.05),3)), side=3,adj=0,col="red")
                mtext("0.05", side=1,at=.05, col="red")
        })


        ### P-value table tab
        output$output_table<-DT::renderDataTable({

                matrix_out_subset<-matrix_out[1:(input$n1_+1),1:(input$n2_+1)]
                colnames(matrix_out_subset) <- paste(0:input$n2_,"/",input$n2_," (",round(dbinom(0:input$n2_,input$n2_,input$p2),3),")",sep="")
                rownames(matrix_out_subset) <- paste(0:input$n1_,"/",input$n1_," (",round(dbinom(0:input$n1_,input$n1_,input$p1),3),")",sep="")

                DT::datatable(data=matrix_out_subset,options=list(pageLength =20))

        })
        output$des<-renderText({
                paste("The table below shows the possible outcomes for group 1 as the rows (n1=",input$n1_,"),and for group 2 as the columns (n2=",input$n2_,". For each row/comlumn,
                      the probability of that outcome based on the probabilities that were specified
                      are also given in the row/column headings. The data in the table represent
                      the p-values that you would get for that combination of outcomes given by the row and column total.
                      ")
        })

        
}


shinyApp(ui = ui, server = server)