library(shiny)
library(shinyjs)
library(Epicalculator)
library(ggplot2)
ui <-  navbarPage(useShinyjs(),
                  #hide the error message in the app 
                  hidden(tags$style
                         (type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                  )),
                  
                  #Title of the app
                  title = "Epi Wizard",
                  
                  #introduction panel for description of the app
                  tabPanel("Introduction",
                           verbatimTextOutput("introduction")),
                  
 #creating 4 navgation panel for rate data, risk data, others and graphs
 #inside each navigation panel we will have tabpanel for different measures 
  #i.e.RR and RD
 #inside the tab we will use sidebarlayout with sideparpanel for input 
   #and mainpanel for output
 
 
 ###########################################################################################################
 #panel 1 for risk ratio and difference
                  navbarMenu("Risk data",
                             #risk ratio tab
                             tabPanel("Risk Ratio",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput('crr', 'Enter crude data (separated by comma)'),
                                          submitButton("Show Crude Risk Ratio"),
                                          br(),#adding blank space
                                          textInput('srr', 'Enter summary data (separated by comma)'),
                                          submitButton("Show Summary Risk Ratio"),
                                          br(), #adding blank space
                                          textInput('rr', 'Show Risk Ratio Plot (enter YES or NO)'),
                                          #Download button for the plot
                                          downloadButton('downRR', "Download the Plot")
                                                    ),
                                        mainPanel(
                                          htmlOutput("crr"), #crude risk ratio output
                                          br(),
                                          br(),
                                          htmlOutput("srr"),#ummary risk ratio output
                                          br(),
                                          br(),
                                          plotOutput("rr", height = "200px")) #risk ratio plot
                                      )),
                             
                              #risk ratio tab
                              tabPanel("Risk Difference",
                                        sidebarLayout(
                                         sidebarPanel(
                                           textInput('crd', 'Enter crude data (separated by comma)'),
                                           submitButton("Show Crude Risk Difference"),
                                           br(),#adding blank space
                                           textInput('srd', 'Enter summary data (separated by comma)'),
                                           submitButton("Show Summary Risk Difference"),
                                           br(), #adding blank space
                                           textInput('rd', 'Show Risk Difference Plot (enter YES or NO)'),
                                           #Download button for the plot
                                           downloadButton('downRD', "Download the Plot")
                                                       ),
                                         mainPanel(
                                           htmlOutput("crd"), #crude risk difference output
                                           br(),
                                           br(),
                                           htmlOutput("srd"),#summary risk difference output
                                           br(),
                                           br(),
                                           plotOutput("rd", height = "200px"))#risk difference plot
                                        ))
                  ),
 
 
################################################################################################################
#panel 2 for rate ratio and difference
                  navbarMenu("Person-Time data",
                        #rate ratio tab
                        tabPanel("Rate Ratio",
                          sidebarLayout(
                              sidebarPanel(
                                  textInput("a1", "exposed people with disease"),
                                  textInput("b1", "unexposed people with disease"),
                                  textInput("c1", "exposed person time"),
                                  textInput("d1", "unexposed person time"),
                                  submitButton("Show Rate Ratio")
                                  ),
                                   mainPanel=(htmlOutput("irr"))
                                )),
                        #rate Difference tab
                        tabPanel("Rate Difference",
                          sidebarLayout(
                              sidebarPanel(
                                    textInput("a2", "exposed people with disease"),
                                    textInput("b2", "unexposed people with disease"),
                                    textInput("c2", "exposed person time"),
                                    textInput("d2", "unexposed person time"),
                                    submitButton("Show Rate Difference")
                                    ),
                                    mainPanel=(htmlOutput("ird"))
                                ))
                  ),

############################################################################################################# 
#panel 3 for other calculations
                  navbarMenu("Others",
                             #odds ratio tab 
                             tabPanel(title = "Odds Ratio",
                                sidebarLayout(
                                  sidebarPanel(
                                    textInput("a5", "exposed people with disease"),
                                    textInput("b5", "unexposed people with disease"),
                                    textInput("c5", "exposed person time"),
                                    textInput("d5", "unexposed person time"),
                                    submitButton("Show Odds Ratio")
                                  ),
                                  mainPanel = (htmlOutput("or"))
                                )),
                             
                             #AR tab
                             tabPanel(title = "AR",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a9", "exposed people with disease"),
                                          textInput("b9", "unexposed people with disease"),
                                          textInput("c9", "exposed person time"),
                                          textInput("d9", "unexposed person time"),
                                          submitButton("Show AR")
                                        ),
                                        mainPanel = (htmlOutput("ar"))
                                      )),
                             
                             #AR% tab
                             tabPanel(title = "AR%",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a6", "exposed people with disease"),
                                          textInput("b6", "unexposed people with disease"),
                                          textInput("c6", "exposed person time"),
                                          textInput("d6", "unexposed person time"),
                                          submitButton("Show AR%")  ),
                                      mainPanel = (htmlOutput("arp"))
                                      )),
                             
                             #PAR tab
                             tabPanel(title = "PAR",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a7", "exposed people with disease"),
                                          textInput("b7", "unexposed people with disease"),
                                          textInput("c7", "exposed person time"),
                                          textInput("d7", "unexposed person time"),
                                          submitButton("Show PAR")
                                        ),
                                      mainPanel = (htmlOutput("par"))
                                      )),
                             
                             #PAR% tab
                             tabPanel(title = "PAR%",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a8", "exposed people with disease"),
                                          textInput("b8", "unexposed people with disease"),
                                          textInput("c8", "exposed person time"),
                                          textInput("d8", "unexposed person time"),
                                          submitButton("Show PAR%")
                                        ),
                                      mainPanel = (htmlOutput("parp"))
                                      ))
                             
                  ),

#################################################################################################
                  #Graphs tab
                  tabPanel(title = "Graphs")
                  
)

#end of ui function
#////////////////////////////////


################################################################################################
################################################################################################
################################################################################################







server <- function(input, output) ({
  
#introduction
  output$introduction<-renderText({
    "This app was created to contain tools designed for epidemiological data 
    analysis. The tools can show calculations risk data, person-time data, 
    odds ratio and other common measures i.e. AR, PAR. For risk calculations, 
    both crude and stratified data can be entered as text separated by comma. 
    The app is able to show the estimates with CI and the comparison of crude 
    and summary estimates. Other calculations are currently designed for crude 
    data only. More functions such as function to perform chi-square hypothesis 
    testing of homogeneity are available in the R package 'Epicalculator'. "
    })

####################################################################################  

#Risk ratio calculations:
#-----------------------
  #crude risk ratio
  output$crr <- renderUI({
    d3 <- as.numeric(unlist(strsplit(input$crr,","))) #make a vector from text input
    a3 <- risk(d3, measure = "RR", ci = 95, estimate = "crude") #risk finction from Epicalculator
    rr <- a3[[1]][[2]]    #extracting crude risk ratio for output
    l.ci3 <- a3[[2]][[1]] #extracting lower ci for output
    u.ci3 <- a3[[2]][[2]] #extracting upper ci for output
    rr <- paste("Crude Risk Ratio:", round(as.numeric(rr),2), " ")
    ci3 <- paste("95%CI: ", "(", round(l.ci3,2), " to ",
                 round(u.ci3,2), ")", sep="")
    HTML(paste(rr,'<br/>', ci3)) #final html output (<br> adds blank space)
  })
  
  
#summary risk ratio
  output$srr<-renderUI({
    s.d3 <- as.numeric(unlist(strsplit(input$srr,","))) #make a vector from text input
    s.a3 <- risk(s.d3, measure = "RR", ci = 95, estimate = "summary") #risk finction from Epicalculator
    s.rr <- s.a3[[1]][[2]]    #extracting summary risk ratiofor output
    s.l.ci3 <- s.a3[[2]][[1]] #extracting lower ci for output
    s.u.ci3 <- s.a3[[2]][[2]] #extracting upper ci for output
    s.rr <- paste("Summary Risk Ratio:", round(as.numeric(s.rr),2), " ")
    s.ci3 <- paste("95%CI: ", "(", round(s.l.ci3,2), " to ",
                   round(s.u.ci3,2), ")", sep="")
    HTML(paste(s.rr,'<br/>', s.ci3)) #final html output
  })
 
  
  #risk ratio plot 
  rrPlot<-reactive({ #save the plot as a reactive object that can be used later 
    if (input$rr == "YES"){ #if user entered YES to show risk ratio plot
      d3 <- as.numeric(unlist(strsplit(input$crr,","))) #use input for crude RR
      a3 <- risk(d3, measure = "RR", ci = 95, estimate = "crude")
      rr <- round(as.numeric(a3[[1]][[2]]),2) #crude RR
      l.ci3 <- round(a3[[2]][[1]],2)          #lower CI of crude RR
      u.ci3 <- round(a3[[2]][[2]],2)          #upper CI of crude RR
      s.d3 <- as.numeric(unlist(strsplit(input$srr,","))) #use input for summary RR
      s.a3 <- risk(s.d3, measure = "RR", ci = 95, estimate = "summary")
      s.rr <- round(as.numeric(s.a3[[1]][[2]]),2) #summary RR
      s.l.ci3 <- round(s.a3[[2]][[1]],2)          #lower CI of summary RR
      s.u.ci3 <- round(s.a3[[2]][[2]],2)          #upper CI of summary RR
      label <- c("Crude", "Summary") #combine crude and summary rr estimates (label)
      mean  <- c(rr, s.rr)           #combine crude and summary rr estimates (RR)
      lower <- c(l.ci3, s.l.ci3)     #combine crude and summary rr estimates (lower CI)
      upper <- c(u.ci3, s.u.ci3)     #combine crude and summary rr estimates (upper CI)
      df <- data.frame(label, mean, lower, upper) #data frame of crude and summary estimates
      # reverses the factor level ordering for labels after coord_flip()
      df$label <- factor(df$label, levels=rev(df$label))
      
      #Plot of the estimates
      ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper, color=label, fill=label)) +
        geom_pointrange(shape=22, lwd = 2, size = 10) +
        geom_hline(aes(yintercept = 1), lty=4) +  # add a dotted line at x=1 after flip
        coord_flip() +  # flip coordinates (puts labels on y axis)
        xlab("") +
        ylab("Risk Ratio") +
        ggtitle("Crude and Summary Estimates") +
        theme(text = element_text(size=20),
              plot.title = element_text(hjust = 0.5))  
         }
  })

#output the risk ratio plot  
  output$rr<-renderPlot({
    rrPlot()
  })
  
  
## download the risk ratio plot
  output$downRR <- downloadHandler(
    filename = function() {
      paste('Risk-Ratio-Plot', rrPlot(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, rrPlot(), device = "png")
    }
  )
  


#risk difference calculations:
#-----------------------------
  #crude RD :
  output$crd <- renderUI({
    d4 <- as.numeric(unlist(strsplit(input$crd,","))) #vector from the text input
    a4 <- risk(d4, measure = "RD", ci = 95, estimate = "crude") #RD caclulations
    rd <- a4[[1]][[2]]     #extracting RD for output
    l.ci4 <- a4[[2]][[1]]  #extracting lower CI for output
    u.ci4 <- a4[[2]][[2]]  #extracting upper CI for output
    rd <- paste("Crude Risk Difference:", round(as.numeric(rd),2), " ")
    ci4 <- paste("95%CI: ", "(", round(l.ci4,2), " to ",
                 round(u.ci4,2), ")", sep="")
    HTML(paste(rd,'<br/>', ci4)) #final html output
  })
  
  #summary RD:
  output$srd<-renderUI({
    s.d4 <- as.numeric(unlist(strsplit(input$srd,","))) #vector from text input
    s.a4 <- risk(s.d4, measure = "RD", ci = 95, estimate = "summary") #RD calculations
    s.rd <- s.a4[[1]][[2]]    #extracting RD for output
    s.l.ci4 <- s.a4[[2]][[1]] #extracting lower CI for output
    s.u.ci4 <- s.a4[[2]][[2]] #extracting upper CI for output
    s.rd <- paste("Summary Risk Difference:", round(as.numeric(s.rd),2), " ")
    s.ci4 <- paste("95%CI: ", "(", round(s.l.ci4,2), " to ",
                   round(s.u.ci4,2), ")", sep="")
    HTML(paste(s.rd,'<br/>', s.ci4)) #final output
  })
  
  
  #RD plot as a reactive object that can be used later
  rdPlot<-reactive({
    if (input$rd == "YES"){ #if user entered YES to show RD plot
      d4 <- as.numeric(unlist(strsplit(input$crd,","))) #input from crude data 
      a4 <- risk(d4, measure = "RD", ci = 95, estimate = "crude") #crude RD
      rd <- round(as.numeric(a4[[1]][[2]]),2)
      l.ci4 <- round(a4[[2]][[1]],2)
      u.ci4 <- round(a4[[2]][[2]],2)
      s.d4 <- as.numeric(unlist(strsplit(input$srd,","))) #input from stratified data
      s.a4 <- risk(s.d4, measure = "RD", ci = 95, estimate = "summary") #summary RD
      s.rd <- round(as.numeric(s.a4[[1]][[2]]),2)
      s.l.ci4 <- round(s.a4[[2]][[1]],2)
      s.u.ci4 <- round(s.a4[[2]][[2]],2)
      #adding crude and summary RD for plotting
      label <- c("Crude", "Summary")
      mean  <- c(rd, s.rd)
      lower <- c(l.ci4,s.l.ci4)
      upper <- c(u.ci4,s.u.ci4)
      df <- data.frame(label, mean, lower, upper)
      # reverses the factor level ordering for labels after coord_flip()
      df$label <- factor(df$label, levels=rev(df$label))
      #plot of crude and summary RD
      ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper, color=label, fill=label)) +
        geom_pointrange(shape=22, lwd = 2, size = 10) +
        geom_hline(aes(yintercept = 1), lty=4) +  # add a dotted line at x=1 after flip
        coord_flip() +  # flip coordinates (puts labels on y axis)
        xlab("") +
        ylab("Risk Difference") +
        ggtitle("Crude and Summary Estimates") +
        theme(text = element_text(size=20),
              plot.title = element_text(hjust = 0.5))  
      }
    })

#output plot for risk difference  
  output$rd<-renderPlot({
    rdPlot()
  })
  
## download the risk difference plot
  output$downRD <- downloadHandler(
    filename = function() {
      paste('Risk-Difference-Plot', rdPlot(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, rdPlot(), device = "png")
    }
  )
  
#####################################################################################
#Person-time data ouputs
#-----------------------
  
#rate ratio caclulations  
  output$irr<-renderUI({
    t1 <- crude.table(as.numeric(input$a1),as.numeric(input$b1),
                      as.numeric(input$c1),as.numeric(input$d1))
    r1 <- crude.rate(t1, measure = "IRR", ci=95)
    HTML(r1)
  })
  

  #rate difference caclulations   
  output$ird<-renderUI({
    t2 <- crude.table(as.numeric(input$a2),as.numeric(input$b2),
                      as.numeric(input$c2),as.numeric(input$d2))
    r2 <- crude.rate(t1, measure = "IRD", ci=95)
    HTML(r2)
  })
  
##################################################################################################
#Others output
#-------------
  
#odds ratio  
  output$or<-renderUI({
    t5 <- tablex(as.numeric(input$a5),as.numeric(input$b5),
                 as.numeric(input$c5),as.numeric(input$d5))
    r5 <- OR(t5)
    HTML(r5)
  })
  
# AR% 
  output$arp<-renderUI({
    t6 <- tablex(as.numeric(input$a6),as.numeric(input$b6),
                 as.numeric(input$c6),as.numeric(input$d6))
    r6 <- ARpercent(t6)
    HTML(r6)
  })
 
#PAR   
  output$par<-renderUI({
    t7 <- tablex(as.numeric(input$a7),as.numeric(input$b7),
                 as.numeric(input$c7),as.numeric(input$d7))
    r7 <- PAR(t7)
    HTML(r7)
  })

#PAR%    
  output$parp<-renderUI({
    t8 <- tablex(as.numeric(input$a8),as.numeric(input$b8),
                 as.numeric(input$c8),as.numeric(input$d8))
    r8 <- PARpercent(t8)
    HTML(r8)
  })
    

#AR 
     output$ar<-renderUI({
      t9 <- tablex(as.numeric(input$a9),as.numeric(input$b9),
                   as.numeric(input$c9),as.numeric(input$d9))
      r9 <- AR(t9)
      HTML(r9)
    })
 
}) #end of server function


#Final app
shinyApp(ui = ui, server = server)
