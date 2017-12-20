#install Epicalculator package first by using following code
devtools::install_github("PHP2560-Statistical-Programming-R/r-package-episquad-2-0")
library(Epicalculator)


#check, install and load the required packages if already not installed
check_package <- function(names){
  for(name in names)
  {
    if (!(name %in% installed.packages()))
      install.packages(name, repos="http://cran.us.r-project.org")

    library(name, character.only=TRUE)
  }
}
check_package(c("shiny", "shinyjs", "shinythemes", "ggplot2"))

ui <-  navbarPage(useShinyjs(),
                  theme = shinytheme("united"),
                  
                  #hide the error message in the app
                  hidden(tags$style
                         (type="text/css",
                           ".title {font-size: 20px; font-style: bold} ",
                           ".navbar {font-size: 18px} ",
                           ".shiny-html-output { font-size: 14px;
                                                 font-style: bold; 
                                                 color: red; 
                                                 text-align:center; }",
                           "#introduction{font-size: 14px; text-align:left; color: black;}",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                         )),
                  
                  #Title of the app
                  title = "Epi Wizard",

                  #introduction panel for description of the app
                  tabPanel("Introduction",
                           uiOutput("introduction")),
                  
                  #show some example data
                  navbarMenu("Example Data",
                             tabPanel("Risk data",
                                      HTML('<center><img src="img4.jpg" height = "600" width = "900"></center>')
                            ),
                             tabPanel("Person-Time data",
                                     HTML('<center><img src="img5.jpg" height = "600" width = "900"></center>')
                            ),
                             tabPanel("Others Data",
                                      HTML('<center><img src="img4.jpg" height = "600" width = "900"></center>')
                            )),

                  #creating 4 navgation panel for rate data, risk data, others and graphs
                  #inside each navigation panel we will have tabpanel for different measures
                  #i.e.RR and RD
                  #inside the tab we will use sidebarlayout with sideparpanel for input
                  #and mainpanel for output


                  ###########################################################################################################
                  #panel 1 for risk ratio and difference
                  navbarMenu("Risk Data",
                             #risk ratio tab
                             tabPanel("Risk Ratio",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput('crr', 'Enter crude data (a, b, c, d)'),
                                          checkboxInput("intcrr", "Show interpretation"),
                                          submitButton("Show Crude Risk Ratio"),
                                          br(),#adding blank space
                                          textInput('srr', 'Enter summary data (a1, b1, c1, d1...)'),
                                          checkboxInput("intsrr", "Show interpretation"),
                                          checkboxInput('rr', 'Show Risk Ratio Comparsion Plot', FALSE), #option for plot
                                          submitButton("Show Summary Risk Ratio"),
                                          br(), #adding blank space

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

                             #risk difference tab
                             tabPanel("Risk Difference",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput('crd', 'Enter crude data (a, b, c, d)'),
                                          checkboxInput('intcrd','Show Interpretation', F),
                                          submitButton("Show Crude Risk Difference"),
                                          br(),#adding blank space
                                          textInput('srd', 'Enter summary data (a1, b1, c1, d1...)'),
                                          checkboxInput('intsrd','Show Interpretation', F),
                                          checkboxInput('rd', 'Show Risk Difference Comparsion Plot', F), #option for plot
                                          submitButton("Show Summary Risk Difference"),
                                          br(), #adding blank space

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
                  navbarMenu("Person-Time Data",
                             #rate ratio tab
                             tabPanel("Rate Ratio",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput('cirr', 'Enter crude data (a, b, c, d)'),
                                          checkboxInput("intcirr","Show Interpretation", F),
                                          submitButton("Show Crude Rate Ratio"),
                                          br(),#adding blank space
                                          textInput('sirr', 'Enter summary data (a1, b1, c1, d1...)'),
                                          checkboxInput("intsirr","Show Interpretation", F),
                                          checkboxInput('irr', 'Show Rate Ratio Comparsion Plot'), #option for plot
                                          submitButton("Show Summary Rate Ratio"),
                                          br(), #adding blank space

                                          #Download button for the plot
                                          downloadButton('downIRR', "Download the Plot")
                                        ),
                                        mainPanel(
                                          htmlOutput("cirr"), #crude rate ratio output
                                          br(),
                                          br(),
                                          htmlOutput("sirr"),#summary rate ratio output
                                          br(),
                                          br(),
                                          plotOutput("irr", height = "200px")) #rate ratio plot
                                      )),

                             #rate Difference tab
                             tabPanel("Rate Difference",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput('cird', 'Enter crude data (a, b, c, d)'),
                                          checkboxInput("intcird","Show Interpretation", F),
                                          submitButton("Show Crude Rate Difference"),
                                          br(),#adding blank space
                                          textInput('sird', 'Enter summary data (a1, b1, c1, d1...)'),
                                          checkboxInput("intsird","Show Interpretation", F),
                                          checkboxInput('ird', 'Show Rate Difference Comparsion Plot'), #option for plot
                                          submitButton("Show Summary Rate Difference"),
                                          br(), #adding blank space
                                          #Download button for the plot
                                          downloadButton('downIRD', "Download the Plot")
                                        ),
                                        mainPanel(
                                          htmlOutput("cird"), #crude rate difference output
                                          br(),
                                          br(),
                                          htmlOutput("sird"),#summary rate difference output
                                          br(),
                                          br(),
                                          plotOutput("ird", height = "200px")) #rate difference plot
                                      ))
                  ),

                  #############################################################################################################
                  #panel 3 for other calculations
                  navbarMenu("Others",
                             #odds ratio tab
                             tabPanel(title = "Odds Ratio",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a5", "exposed with disease"),
                                          textInput("b5", "unexposed with disease"),
                                          textInput("c5", "exposed without disease"),
                                          textInput("d5", "unexposed without disease"),
                                          checkboxInput("intor","Show Interpretation", F),
                                          checkboxInput('orp', 'Show Odds Ratio Plot'),

                                          submitButton("Show OR")

                                        ),
                                        mainPanel(
                                          htmlOutput('ord'), #crude odds ratio output
                                          br(),
                                          br(),
                                          plotOutput('orp', height = "200px")) #odds ratio plot
                                      )),


                             #AR tab
                             tabPanel(title = "AR",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a9", "exposed with disease"),
                                          textInput("b9", "unexposed with disease"),
                                          textInput("c9", "exposed without disease"),
                                          textInput("d9", "unexposed without disease"),
                                          checkboxInput("intar","Show Interpretation", F),
                                          checkboxInput('arplot', 'Show AR Plot'),

                                          submitButton("Show AR")
                                         ),
                                        mainPanel(
                                          htmlOutput('ardata'),
                                          br(),
                                          br(),
                                          plotOutput('arplot', height = "200px"))
                                      )),

                             #AR% tab
                             tabPanel(title = "AR%",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a6", "exposed with disease"),
                                          textInput("b6", "unexposed with disease"),
                                          textInput("c6", "exposed without disease"),
                                          textInput("d6", "unexposed without disease"),
                                          checkboxInput("intarp","Show Interpretation", F),
                                          checkboxInput('ard', 'Show AR% Plot'),
                                          submitButton("Show AR%")

                                        ),
                                        mainPanel(
                                          htmlOutput('arp'), #Attributal risk %
                                          br(),
                                          br(),
                                          plotOutput('ard', height = "200px"))
                                      )),

                             #PAR tab
                             tabPanel(title = "PAR",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a7", "exposed with disease"),
                                          textInput("b7", "unexposed with disease"),
                                          textInput("c7", "exposed without disease"),
                                          textInput("d7", "unexposed without disease"),
                                          checkboxInput("intpar","Show Interpretation", F),
                                          checkboxInput('parplot', 'Show PAR Plot'),
                                          submitButton("Show PAR")

                                        ),
                                        mainPanel(
                                          htmlOutput('pardata'), #Attributal risk %
                                          br(),
                                          br(),
                                          plotOutput('parplot', height = "200px"))
                                      )),

                             #PAR% tab
                             tabPanel(title = "PAR%",
                                      sidebarLayout(
                                        sidebarPanel(
                                          textInput("a8", "exposed with disease"),
                                          textInput("b8", "unexposed with disease"),
                                          textInput("c8", "exposed without disease"),
                                          textInput("d8", "unexposed without disease"),
                                          checkboxInput("intparp","Show Interpretation", F),
                                          checkboxInput('parpplot', 'Show PAR% Plot'),
                                          submitButton("Show PAR%")

                                        ),
                                        mainPanel(
                                          htmlOutput('parpdata'), #population attributal risk %
                                          br(),
                                          br(),
                                          plotOutput('parpplot', height = "200px"))
                                      ))

                  )

                  #################################################################################################

)

#end of ui function
#////////////////////////////////


################################################################################################
################################################################################################
################################################################################################







server <- function(input, output)({
  #introduction
  output$introduction<-renderUI({
     includeMarkdown("Introduction.Rmd")
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

    if(input$intcrr == TRUE){ #option for interpretation
      HTML(paste(rr,'<br/>',ci3, '<br/>','<br/>', "The risk of the disease in exposed group is", round(as.numeric(a3[[1]][[2]]),2),"times the risk of the disease in unexposed group.")) #final html output (<br> adds blank space)

    }
    else {HTML(paste(rr,'<br/>', ci3))} #final html output (<br> adds blank space)

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
    if(input$intsrr == TRUE){ #option for interpretation
      HTML(paste(s.rr,'<br/>', s.ci3, '<br/>', '<br/>',"The risk of the disease in exposed group is",round(as.numeric(s.a3[[1]][[2]]),2),"times the risk of the disease in unexposed group after stratification.")) #final html output (<br> adds blank space)

    }
    else {HTML(paste(s.rr,'<br/>', s.ci3))} #final html output (<br> adds blank space)


  })


  #risk ratio plot
  rrPlot<-reactive({ #save the plot as a reactive object that can be used later
   if(input$rr == TRUE){
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
        geom_pointrange(shape=22, lwd = 2, size = 10, fill="white") +
        geom_hline(aes(yintercept = 1), lty=4) +  # add a dotted line at x=1 after flip
        coord_flip() + # flip coordinates (puts labels on y axis)
        scale_color_manual(values=c("darkgoldenrod3", "coral")) +
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

    if(input$intcrd == TRUE){#option for interpretation
      HTML(paste(rd,'<br/>', ci4,'<br/>', '<br/>',
"If people in exposed group had been unexposed, the risk of disease would decrease(if RD is +)/increase(if RD is -)", abs(round(as.numeric(a4[[1]][[2]]),2)),".")) #final html output

    }
    else {HTML(paste(rd,'<br/>', ci4))}

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

    if(input$intsrd == TRUE){#option for interpretation
      HTML(paste(s.rd,'<br/>', s.ci4,'<br/>', '<br/>',
                 "If people in exposed group had been unexposed, the risk of disease would decrease(if RD is +)/increase(if RD is -)", abs(round(as.numeric(s.a4[[1]][[2]]),2))," after stratification.")) #final html output

    }
    else {HTML(paste(s.rd,'<br/>', s.ci4))}
  })


  #RD plot as a reactive object that can be used later
  rdPlot<-reactive({
    if (input$rd == TRUE){ #if user check to show RD plot
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
        geom_pointrange(shape=22, lwd = 2, size = 10, fill="white") +
        geom_hline(aes(yintercept = 1), lty=4) +  # add a dotted line at x=1 after flip
        coord_flip() + # flip coordinates (puts labels on y axis)
        scale_color_manual(values=c("darkgoldenrod3", "coral")) +
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

  #crude rate ratio caclulations

  output$cirr <- renderUI({
    d1 <- as.numeric(unlist(strsplit(input$cirr,","))) #make a vector from text input
    a1 <- crude.rate(crude.table(d1[[1]],d1[[2]],d1[[3]],d1[[4]]), 95, measure = "IRR") #crude.rate finction from Epicalculator
    irr <- as.numeric(sapply(strsplit(a1, " "), "[[", 2)) #crude IRR
    if(input$intcirr == TRUE){
      HTML(paste(a1,'<br/>','<br/>',"The incidence rate in the exposed group is", irr, "times the incidence rate in the unexposed group."))
    } else{
    HTML(a1)}
  })
  #summary rate ratio calculations
  output$sirr <- renderUI({
    s.d1 <- as.numeric(unlist(strsplit(input$sirr,","))) #make a vector from text input
    s.a1 <- summary.rate(stratified.table(s.d1), 95, measure = "IRR") #crude.rate finction from Epicalculator
    s.irr <- as.numeric(sapply(strsplit(s.a1, " "), "[[", 3)) #summary IRR
    if(input$intsirr == TRUE){
      HTML(paste(s.a1,'<br/>','<br/>',"The incidence rate in the exposed group is", s.irr, "times the incidence rate in the unexposed group after stratification."))
    } else{
      HTML(s.a1)}
  })


  #rate ratio plot
  irrPlot<-reactive({ #save the plot as a reactive object that can be used later
    if (input$irr == TRUE){ #if user check to show rate ratio plot
      d1 <- as.numeric(unlist(strsplit(input$cirr,","))) #use input for crude IRR
      a1 <- crude.rate(crude.table(d1[[1]],d1[[2]],d1[[3]],d1[[4]]), 95, measure = "IRR")
      irr <- as.numeric(sapply(strsplit(a1, " "), "[[", 2)) #crude IRR
      l.ci1 <- as.numeric(sapply(strsplit(a1, " "), "[[", 5)) #lower CI of crude IRR
      u.ci1 <- as.numeric(sapply(strsplit(a1, " "), "[[", 7)) #upper CI of crude IRR
      s.d1 <- as.numeric(unlist(strsplit(input$sirr,","))) #use input for summary IRR
      s.a1 <- summary.rate(stratified.table(s.d1), 95, measure = "IRR")
      s.irr <- as.numeric(sapply(strsplit(s.a1, " "), "[[", 3)) #summary IRR
      s.l.ci1 <-  as.numeric(sapply(strsplit(s.a1, " "), "[[", 6)) #lower CI of summary IRR
      s.u.ci1 <-  as.numeric(sapply(strsplit(s.a1, " "), "[[", 8))  #upper CI of summary IRR
      label <- c("Crude", "Summary") #combine crude and summary irr estimates (label)
      mean  <- c(irr, s.irr)           #combine crude and summary irr estimates (IRR)
      lower <- c(l.ci1, s.l.ci1)     #combine crude and summary irr estimates (lower CI)
      upper <- c(u.ci1, s.u.ci1)     #combine crude and summary irr estimates (upper CI)
      df <- data.frame(label, mean, lower, upper) #data frame of crude and summary estimates
      # reverses the factor level ordering for labels after coord_flip()
      df$label <- factor(df$label, levels=rev(df$label))

      #Plot of the estimates
      ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper, color=label, fill=label)) +
        geom_pointrange(shape=22, lwd = 2, size = 10, fill="white") +
        geom_hline(aes(yintercept = 1), lty=4) +  # add a dotted line at x=1 after flip
        coord_flip() +  # flip coordinates (puts labels on y axis)
        scale_color_manual(values=c("darkgoldenrod3", "coral")) +
        xlab("") +
        ylab("Rate Ratio") +
        ggtitle("Crude and Summary Estimates") +
        theme(text = element_text(size=20),
              plot.title = element_text(hjust = 0.5))
    }
  })

  #output the rate ratio plot
  output$irr<-renderPlot({
    irrPlot()
  })

  ## download the rate ratio plot
  output$downIRR <- downloadHandler(
    filename = function() {
      paste('Rate-Ratio-Plot', irrPlot(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, irrPlot(), device = "png")
    }
  )




  #crude rate difference caclulations

  output$cird <- renderUI({
    d2 <- as.numeric(unlist(strsplit(input$cird,","))) #make a vector from text input
    a2 <- crude.rate(crude.table(d2[[1]],d2[[2]],d2[[3]],d2[[4]]), 95, measure = "IRD") #crude.rate finction from Epicalculator
    ird <- as.numeric(sapply(strsplit(a2, " "), "[[", 2)) #crude IRD
    if(input$intcird == TRUE){
      HTML(paste(a2,'<br/>','<br/>',
                 "If people in exposed group had been unexposed, the incidence rate of disease would decrease(if RD is +)/increase(if RD is -)"
                 , ird, "."))
    }else{
      HTML(a2)
    }


  })
  #summary rate difference calculations
  output$sird <- renderUI({
    s.d2 <- as.numeric(unlist(strsplit(input$sird,","))) #make a vector from text input
    s.a2 <- summary.rate(stratified.table(s.d2), 95, measure = "IRD") #crude.rate finction from Epicalculator
    s.ird <- as.numeric(sapply(strsplit(s.a2, " "), "[[", 3)) #summary IRD
    HTML(s.a2)
    if(input$intsird == TRUE){
      HTML(paste(s.a2,'<br/>','<br/>',
                 "If people in exposed group had been unexposed, the incidence rate of disease would decrease(if RD is +)/increase(if RD is -)"
                 , s.ird, " after stratification."))
    }else{
      HTML(s.a2)
  }
})
  #rate difference plot
  irdPlot<-reactive({ #save the plot as a reactive object that can be used later
    if (input$ird == TRUE){ #if user check to show rate ratio plot
      d2 <- as.numeric(unlist(strsplit(input$cird,","))) #use input for crude IRD
      a2 <- crude.rate(crude.table(d2[[1]],d2[[2]],d2[[3]],d2[[4]]), 95, measure = "IRD")
      ird <- as.numeric(sapply(strsplit(a2, " "), "[[", 2)) #crude IRD
      l.ci2 <- as.numeric(sapply(strsplit(a2, " "), "[[", 5)) #lower CI of crude IRD
      u.ci2 <- as.numeric(sapply(strsplit(a2, " "), "[[", 7)) #upper CI of crude IRD
      s.d2 <- as.numeric(unlist(strsplit(input$sird,","))) #use input for summary IRD
      s.a2 <- summary.rate(stratified.table(s.d2), 95, measure = "IRD")
      s.ird <- as.numeric(sapply(strsplit(s.a2, " "), "[[", 3)) #summary IRD
      s.l.ci2 <-  as.numeric(sapply(strsplit(s.a2, " "), "[[", 6)) #lower CI of summary IRD
      s.u.ci2 <-  as.numeric(sapply(strsplit(s.a2, " "), "[[", 8))  #upper CI of summary IRD
      label <- c("Crude", "Summary") #combine crude and summary ird estimates (label)
      mean  <- c(ird, s.ird)           #combine crude and summary ird estimates (IRD)
      lower <- c(l.ci2, s.l.ci2)     #combine crude and summary ird estimates (lower CI)
      upper <- c(u.ci2, s.u.ci2)     #combine crude and summary ird estimates (upper CI)
      df <- data.frame(label, mean, lower, upper) #data frame of crude and summary estimates
      # reverses the factor level ordering for labels after coord_flip()
      df$label <- factor(df$label, levels=rev(df$label))

      #Plot of the estimates
      ggplot(data=df, aes(x=label, y=mean, ymin=lower, ymax=upper, color=label, fill=label)) +
        geom_pointrange(shape=22, lwd = 2, size = 10,fill="white") +
        geom_hline(aes(yintercept = 1), lty=4) +  # add a dotted line at x=1 after flip
        coord_flip() +  # flip coordinates (puts labels on y axis)
        scale_color_manual(values=c("darkgoldenrod3", "coral")) +
        xlab("") +
        ylab("Rate Difference") +
        ggtitle("Crude and Summary Estimates") +
        theme(text = element_text(size=20),
              plot.title = element_text(hjust = 0.5))
    }
  })

  #output the risk ratio plot
  output$ird<-renderPlot({
    irdPlot()
  })

  ## download the risk ratio plot
  output$downIRD <- downloadHandler(
    filename = function() {
      paste('Rate-Difference-Plot', irrPlot(), '.png', sep='')
    },
    content = function(file) {
      ggsave(file, irdPlot(), device = "png")
    }
  )


  ##################################################################################################
  #Others output
  #-------------

  #odds ratio
  output$ord<-renderUI({
    t5 <- tablex(as.numeric(input$a5),as.numeric(input$b5),
                 as.numeric(input$c5),as.numeric(input$d5))
    r5 <- OR(t5)
    oddsratio<- as.numeric(((as.numeric(input$a5)*as.numeric(input$d5))/(as.numeric(input$b5)*as.numeric(input$c5))))
    if(input$intor == TRUE){
      HTML(r5,'<br/>','<br/>', "The odds of having the disease in exposed group is", round(oddsratio,2),
           "times the odds of having the disease in unexposed group.")
    }else{
    HTML(r5)
    }
    })
  orpPlot<-reactive({ #save the plot as a reactive object that can be used later
    if (input$orp == TRUE){  #if user check to show plot
      x<- as.numeric(log((as.numeric(input$a5)*as.numeric(input$d5))/(as.numeric(input$b5)*as.numeric(input$c5))))
      oddsratio<- as.numeric(((as.numeric(input$a5)*as.numeric(input$d5))/(as.numeric(input$b5)*as.numeric(input$c5))))
      var<- as.numeric(((1/as.numeric(input$a5))+(1/as.numeric(input$b5))+(1/as.numeric(input$c5))+(1/as.numeric(input$d5))))
      se<- as.numeric(sqrt(var))
      z<- as.numeric(1-(.5*((100-95)/100)))
      upper <- as.numeric(exp(x + (qnorm(z) * se)))
      lower <- as.numeric(exp(x - (qnorm(z) * se)))
      label<- ""
      df5<- data.frame(label, oddsratio, lower, upper)
      ggplot(data=df5, aes(x=label, y= oddsratio)) +
        theme_minimal()+
        geom_errorbar(aes(ymax=upper, ymin= lower),width=0.15, size=1.1, color="cadetblue") +
        geom_point(size=10, shape=22, fill="white") +
        xlab("") +
        ylab("Odds Ratio") +
        ggtitle("Odds Ratio Estimate\nwith 95% Confidence Interval") +
        coord_flip()
      }
    })
  #output the odds ratio plot
  output$orp<-renderPlot({
    orpPlot()
    })
  # AR%
  output$arp<-renderUI({
    t6 <-  tablex(as.numeric(input$a6),as.numeric(input$b6),
                  as.numeric(input$c6),as.numeric(input$d6))
    r6 <- ARpercent(t6)
    AR<- as.numeric((as.numeric(input$a6)/(as.numeric(input$a6)+as.numeric(input$c6)))-
                      (as.numeric(input$b6)/(as.numeric(input$b6)+as.numeric(input$d6))))
    ARpercent<- as.numeric(AR/(as.numeric(input$a6)/(as.numeric(input$a6)+as.numeric(input$c6)))*100)
    if(input$intarp == TRUE){
      HTML(paste(r6,'<br/>','<br/>',
                 "Among exposed individuals,",round(ARpercent,2),"% of the disease cases are attributed to exposure."))
    }else{
    HTML(r6)}
  })
  # Plot
  ardPlot<-reactive({ #save the plot as a reactive object that can be used later
      if (input$ard == TRUE){  #if user check to show  plot
        AR<- as.numeric((as.numeric(input$a6)/(as.numeric(input$a6)+as.numeric(input$c6)))-
                          (as.numeric(input$b6)/(as.numeric(input$b6)+as.numeric(input$d6))))
        ARpercent<- as.numeric(AR/(as.numeric(input$a6)/(as.numeric(input$a6)+as.numeric(input$c6)))*100)
        se<- as.numeric(sqrt(((1/(as.numeric(input$a6)+as.numeric(input$c6)))+
                                (1/(as.numeric(input$b6)+as.numeric(input$d6))))*
                               (1-((as.numeric(input$a6)+as.numeric(input$b6))/
                                     (as.numeric(input$a6)+as.numeric(input$b6)+as.numeric(input$c6)+as.numeric(input$d6))))*
                               ((as.numeric(input$a6)+as.numeric(input$b6))/
                                  (as.numeric(input$a6)+as.numeric(input$b6)+as.numeric(input$c6)+as.numeric(input$d6)))))
        z<- as.numeric(1-(.5*((100-95)/100)))
        upper <- as.numeric(ARpercent + ARpercent*(qnorm(z)*se/AR))
        lower <- as.numeric(ARpercent - ARpercent*(qnorm(z)*se/AR))
        label<- ""
        df6<- data.frame(label, ARpercent, lower, upper)
        ggplot(data=df6, aes(x=label, y= ARpercent)) +
          theme_minimal()+
          geom_errorbar(aes(ymax=upper, ymin= lower),width=0.15, size=1, color="cadetblue") +
          geom_point(size=10, shape=22, fill="white") +
          xlab("") +
          ylab("Attributable Risk Percent") +
          ggtitle("Attributalbe Risk Percent Estimate\nwith 95% Confidence Interval") +
          coord_flip()
      }
  })

    #output the AR% plot
    output$ard<-renderPlot({
      ardPlot()
    })

    #PAR
    output$pardata<-renderUI({
      t7 <- tablex(as.numeric(input$a7),as.numeric(input$b7),
                   as.numeric(input$c7),as.numeric(input$d7))
      r7 <- PAR(t7)
      PAR<- as.numeric((as.numeric(input$a7)/(as.numeric(input$a7)+as.numeric(input$c7)))-
                         (as.numeric(input$b7)/(as.numeric(input$b7)+as.numeric(input$d7))))*
        ((as.numeric(input$a7)+as.numeric(input$c7))/((as.numeric(input$a7)+
                                                         as.numeric(input$b7)+
                                                         as.numeric(input$c7)+
                                                         as.numeric(input$d7))))
      if(input$intpar == TRUE){
        HTML(paste(r7,'<br/>','<br/>',
                   "If we could change the exposure to nonexposure in the study population, we might expect the risk of disease in the study population decreases(if PAR is +)/increases(if PAR is -) by",
                  abs(round(PAR,2)),"."))
      }
     else{ HTML(r7)}
    })
    parplotPlot<-reactive({ #save the plot as a reactive object that can be used later
      if (input$parplot == TRUE){  #if user check to show rate ratio plot
        PAR<- as.numeric((as.numeric(input$a7)/(as.numeric(input$a7)+as.numeric(input$c7)))-
                           (as.numeric(input$b7)/(as.numeric(input$b7)+as.numeric(input$d7))))*
          ((as.numeric(input$a7)+as.numeric(input$c7))/((as.numeric(input$a7)+
                                                           as.numeric(input$b7)+
                                                           as.numeric(input$c7)+
                                                           as.numeric(input$d7))))
        se<- sqrt((((as.numeric(input$c7)*(as.numeric(input$b7)+as.numeric(input$d7)))/
                      (as.numeric(input$d7)*(as.numeric(input$a7)+as.numeric(input$c7))))^2)*
                    ((as.numeric(input$a7)/
                        (as.numeric(input$c7)*(as.numeric(input$a7)+as.numeric(input$c7))))+
                       (as.numeric(input$b7)/(as.numeric(input$d7)*(as.numeric(input$b7)+as.numeric(input$d7))))))
        z<- as.numeric(1-(.5*((100-95)/100)))
        upper <- as.numeric(PAR + (qnorm(z)*se))
        lower <- as.numeric(PAR - (qnorm(z)*se))
        label<- ""
        df9<- data.frame(label, PAR, lower, upper)
        ggplot(data=df9, aes(x=label, y= PAR)) +
          theme_minimal()+
          geom_errorbar(aes(ymax=upper, ymin= lower),width=0.15, size=1, color="cadetblue") +
          geom_point(size=10, shape=22, fill="white") +
          xlab("") +
          ylab("Population Attributable Risk") +
          ggtitle("Population Attributable Risk Estimate\nwith 95% Confidence Interval") +
          coord_flip()
      }
    })
    #output the PARplot
    output$parplot<-renderPlot({
      parplotPlot()
    })
    #PAR%
    output$parpdata<-renderUI({
      t8 <- tablex(as.numeric(input$a8),as.numeric(input$b8),
                   as.numeric(input$c8),as.numeric(input$d8))
      r8 <- PARpercent(t8)
      PARper<- (((as.numeric(input$a8)+as.numeric(input$b8))/
                   (as.numeric(input$a8)+as.numeric(input$b8)+as.numeric(input$c8)+as.numeric(input$d8))-
                   (as.numeric(input$b8)/(as.numeric(input$b8)+as.numeric(input$d8))))/
                  ((as.numeric(input$a8)+as.numeric(input$b8))/
                     (as.numeric(input$a8)+as.numeric(input$b8)+as.numeric(input$c8)+as.numeric(input$d8)))*100)
      if (input$intparp == TRUE){
        HTML(paste(r8,'<br/>','<br/>', round(PARper,2),
                   "of the disease cases in the study population is attributable to the exposure."))
      }
      else{HTML(r8)}
    })
    parpplotPlot<-reactive({ #save the plot as a reactive object that can be used later
      if (input$parpplot == TRUE){  #if user check to show  plot
        PARper<- (((as.numeric(input$a8)+as.numeric(input$b8))/
                         (as.numeric(input$a8)+as.numeric(input$b8)+as.numeric(input$c8)+as.numeric(input$d8))-
                         (as.numeric(input$b8)/(as.numeric(input$b8)+as.numeric(input$d8))))/
                        ((as.numeric(input$a8)+as.numeric(input$b8))/
                           (as.numeric(input$a8)+as.numeric(input$b8)+as.numeric(input$c8)+as.numeric(input$d8)))*100)
        se<- sqrt((((as.numeric(input$c8)*(as.numeric(input$b8)+as.numeric(input$d8)))/
                      (as.numeric(input$d8)*(as.numeric(input$a8)+as.numeric(input$c8))))^2)*
                    ((as.numeric(input$a8)/
                        (as.numeric(input$c8)*(as.numeric(input$a8)+as.numeric(input$c8))))+
                       (as.numeric(input$b8)/(as.numeric(input$d8)*(as.numeric(input$b8)+as.numeric(input$d8))))))*100
        z<- as.numeric(1-(.5*((100-95)/100)))
        upper <- as.numeric(PARper + (qnorm(z)*se))
        lower <- as.numeric(PARper - (qnorm(z)*se))
        label<- ""
        df8<- data.frame(label, PARper, lower, upper)
        ggplot(data=df8, aes(x=label, y= PARper)) +
          theme_minimal()+
          geom_errorbar(aes(ymax=upper, ymin= lower),width=0.15, size=1, color="coral") +
          geom_point(size=10, shape=22, fill="white") +
          xlab("") +
          ylab("Population Attributable Risk Percent") +
          ggtitle("Population Attributable Risk Percent Estimate\nwith 95% Confidence Interval") +
          coord_flip()
      }
    })
    #output the ARplot
    output$parpplot<-renderPlot({
      parpplotPlot()
    })

    #AR
    output$ardata<-renderUI({
      t9 <- tablex(as.numeric(input$a9),as.numeric(input$b9),
                   as.numeric(input$c9),as.numeric(input$d9))
      r9 <- AR(t9)
      AR<- as.numeric((as.numeric(input$a9)/(as.numeric(input$a9)+as.numeric(input$c9)))-
                        (as.numeric(input$b9)/(as.numeric(input$b9)+as.numeric(input$d9))))
      if(input$intar){
        HTML(paste(r9,'<br/>','<br/>',
                   "If people in exposed group had been unexposed, the risk of disease would decrease(if AR is +)/increase(if AR is -)",
                   abs(round(AR,3)), "."))
      }
      else{
      HTML(r9)}
    })
    arplotPlot<-reactive({ #save the plot as a reactive object that can be used later
      if (input$arplot == TRUE){  #if user check to show rate ratio plot
        AR<- as.numeric((as.numeric(input$a9)/(as.numeric(input$a9)+as.numeric(input$c9)))-
                          (as.numeric(input$b9)/(as.numeric(input$b9)+as.numeric(input$d9))))
        se<- as.numeric(sqrt(((1/(as.numeric(input$a9)+as.numeric(input$c9)))+
                                (1/(as.numeric(input$b9)+as.numeric(input$d9))))*
                               (1-((as.numeric(input$a9)+as.numeric(input$b9))/
                                     (as.numeric(input$a9)+as.numeric(input$b9)+as.numeric(input$c9)+as.numeric(input$d9))))*
                               ((as.numeric(input$a9)+as.numeric(input$b9))/
                                  (as.numeric(input$a9)+as.numeric(input$b9)+as.numeric(input$c9)+as.numeric(input$d9)))))
        z<- as.numeric(1-(.5*((100-95)/100)))
        upper <- as.numeric(AR + (qnorm(z)*se))
        lower <- as.numeric(AR - (qnorm(z)*se))
        label<- ""
        df9<- data.frame(label, AR, lower, upper)
        ggplot(data=df9, aes(x=label, y= AR)) +
          theme_minimal()+
          geom_errorbar(aes(ymax=upper, ymin= lower),width=0.15, size=1, color="cadetblue") +
          geom_point(size=10, shape=22, fill="white") +
          xlab("") +
          ylab("Attributable Risk") +
          ggtitle("Attributable Risk Estimate\nwith 95% Confidence Interval") +
          coord_flip()
      }
    })
    #output the ARplot
    output$arplot<-renderPlot({
      arplotPlot()
    })
})

#Final app
shinyApp (ui = ui, server = server)
