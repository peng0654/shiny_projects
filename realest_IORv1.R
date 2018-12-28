# install.packages('shiny')

# install.packages('DT')

# install.packages('scales')





suppressMessages({
  
  library(readr)
  
  library(dplyr)
  
  library(tidyr)
  
  library(magrittr)
  
  library(forcats)
  
  library(lubridate)
  
  library(shiny)
  
  library(ggplot2)
  
  library(grid)
  
  library(DT)
  
  library(scales)
  
})





ui <- function(request) { fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3('Real Estate Investment Return Analysis'),
      
      uiOutput('file_stub_selector'),
      
      # hr(),
      
      # selectInput('show_what', 'Plot:', choices=c('Return Rate','Interest Paid','Balance Paid','Remaining Loan Balance'),
      
      #             selected=c('actual','forecast'),
      
      #             multiple=TRUE),
      
      hr(),
      
      helpText('All costs and incomes are montly based'),
      
      fluidRow(
        
        column(6,
               
               
               
               selectInput("vrooms", "Number of bedrooms:", choices=c(1,2,3,4,5,6), multiple=FALSE, selected='1'),
               
               numericInput("proom", "Rental price per room:", 350, min = 0, max = 100000000000000),
               
               numericInput('ccred', 'Creditline for downpay:', 0, min = 0, max = 1000000000),
               
               numericInput('cHOV', 'HOA fee:', 0, min = 0, max = 1000000000),
               
               sliderInput('voccp','Occupancy rate:',min=0,max=100,value=80,step=0.5,post='%'),
               
               offset=0),
        
        column(6,
               
               numericInput('cmgt', 'Property mgmt fee:', 0, min = 0, max = 100000000),
               
               numericInput('cyard', 'Yard,utility paid by owner:', 0, min = 0, max = 100000000),
               
               numericInput('cwarr', 'Termite bond, home warranty:', 0, min = 0, max = 100000000),
               
               
               
               
               
               numericInput('cinsure', 'Insurance:', 0, min = 0, max = 1000000000),
               
               #numericInput('cProtax', 'Property tax:', 0, min = 0, max = 1000000000),
               
               
               
               sliderInput('cProtax','Property tax rate:',min=0,max=10,value=1.5,step=0.01,post='%'),
               
               offset=0)
        
      ), # fluidRow
      
      
      
      
      hr(),
      
      helpText('Puchase Dates'),
      
      fluidRow(
        
        column(6,
               
               dateInput("Purchase_dt", "Purchase Date:", value = "2000-01-01"),
               
               offset=0),
        
        column(6,
               
               dateInput("First_payment_dt", "First payment Date:", value = "2000-02-01"),
               
               offset=0)
        
      ),
      
      
      
      hr(),
      
      helpText('Initial investment or cost'),
      
      numericInput("vvalue", 'Property purhase price:', 200000, min = 100, max = 10000000000000000000000),
      
      sliderInput('vdown','Downpay rate:',min=0,max=100,value=15,step=1,post='%'),
      
      conditionalPanel( condition = "input.ccred > 0",
                        
                        numericInput('credit_term', 'credit line term:', 0, min = 0, max = 50),
                        
                        sliderInput('credit_rate','credit line Interest rate:',min=0,max=10,value=0,step=0.01,post='%')),
      
      selectInput('ltype', 'Loan type:', choices=c('Fixed', 'ARM'), multiple=FALSE, selected='Fixed'),
      
      numericInput('lterm', 'Loan term (years):', 30, min = 1, max = 50),
      
      conditionalPanel( condition = "input.ltype == 'ARM'", numericInput('fixlterm', 'Fixed rate Loan term (ARM Only, years):', 0, min = 0, max = 50)),
      
      numericInput('lIR', 'Fix interest Rate (annually in %):', 3, min = 0, max = 50),
      
      numericInput('lclose', 'closing cost:', 0, min = 0, max = 1000000000000),
      
      numericInput('lrepair', 'Initial repair cost:', 0, min = 0, max = 1000000000000),
      
      
      
      
      
      
      
      hr(),
      
      helpText('Resale gain and cost'),
      
      numericInput("vappvalue", 'Property purhase appraisal value:', 200000, min = 100, max = 10000000000000000000000),
      
      numericInput("rROI", 'Alternative investment reture rate (annually in %) :', 0, min = 0, max = 500),
      
      numericInput("rap", 'Property de/appraiction rate (annually in %) :', 0, min = 0, max = 500),
      
      numericInput("rsale", 'sale after purchase (monthes) :', 0, min = 0, max = 500000000),
      
      
      
      div(style='text-align:right',
          
          helpText('Real Estate ROI v0.1'),
          
          helpText('Questions? contact ',a('Peng Ren',href='pren0654@hotmail.com'))),
      
      width=3),
    
    mainPanel(
      
      tabsetPanel(id='main_tabs',
                  
                  tabPanel("stats",value='tab_stats',
                           
                           tabsetPanel(id='stats_tabs',
                                       
                                       tabPanel("Investment summary"
                                                
                                                ,value = 'Monthly_sum'
                                                
                                                ,fluidRow(textOutput('Monthly_Rental_Income')
                                                          
                                                          ,textOutput('Monthly_MTG_Pmt')
                                                          
                                                          ,plotOutput("PaymentPlot")
                                                          
                                                          ,textOutput('Total_inital_invst')
                                                          
                                                          ,textOutput('sale_date')
                                                          
                                                          ,splitLayout(cellWidths = c("50%", "50%"), plotOutput("Cash_flow_plot"), plotOutput("cum_roa"))
                                                          
                                                )),
                                       
                                       tabPanel("Tax Repot"
                                                
                                                ,value='tab_muview_plot'
                                                
                                                ,downloadButton('download_muview_plot','Download this plot')
                                                
                                                ,fluidRow(plotOutput("muview_plot",width='100%',height='90%'))),
                                       
                                       tabPanel("table"
                                                
                                                ,value='tab_muview_table'
                                                
                                                ,fluidRow(DT::dataTableOutput("muview_table"))),
                                       
                                       tabPanel("Others"
                                                
                                                ,value='tab_muview_state_plot'
                                                
                                                ,fluidRow(plotOutput("muview_state_plot",width='100%',height='90%')))
                                       
                           ))
                  
      )
      
    ) # mainPanel
    
  ) # sidebarLayout
  
)}





server<-function(input, output){
  
  
  
  MTG_DSN<-reactive({
    
    
    
    M_PMT<-ifelse(input$lIR==0, input$vvalue*(1-input$vdown/100)/(input$lterm*12),
                  
                  (input$vvalue*(1-input$vdown/100)*input$lIR/1200)/(1-(1+input$lIR/1200)^(input$lterm*-12))
                  
    )
    
    
    
    credit_down_pmt<-ifelse(input$ccred==0, 0,
                            
                            (input$ccred/(input$credit_term*12)*input$credit_rate/1200)/(1-(1+input$credit_rate/1200)^(input$credit_rate*-12))
                            
    )
    
    
    
    MTG_table<-seq(input$Purchase_dt%m+%months(1), input$Purchase_dt%m+% months(input$lterm*12), by = "month")%>%data.frame()%>%mutate(M_PMT = M_PMT
                                                                                                                                       
    )
    
    
    
    
    
    colnames(MTG_table)<-c('Date', 'Monthly_Payment')
    
    r_temp<-c()
    
    rem_princ<-0
    
    prin_amt<-0
    
    for ( i in (1:(input$lterm*12))){
      
      
      
      if ( i ==1){
        
        rem_princ<-input$vvalue*(1-input$vdown/100)
        
        
        
      }else{
        
        
        
        rem_princ<-rem_princ - prin_amt
        
      }
      
      
      
      
      
      
      
      proprity_value = input$vappvalue*(1+input$rap/1200)^(i)
      
      credit_line_down_pmt<-ifelse (i<= input$credit_term*12 ,credit_down_pmt, 0)
      
      Int_amt <-rem_princ*input$lIR/1200
      
      prin_amt <- M_PMT -Int_amt
      
      r<-cbind(rem_princ,Int_amt, prin_amt,proprity_value,credit_line_down_pmt)
      
      r_temp<-rbind(r_temp,r)
      
    }
    
    
    
    MTG_table<-cbind(MTG_table,r_temp) %>%group_by(year(Date))%>%arrange(Date)%>%
      
      mutate(assessment_value=first(proprity_value))%>%ungroup()%>%
      
      mutate(loan_age = seq.int(nrow(.))-1,
             
             if_half_y=ifelse((month(Date) %in% c(6,12)),'Y','N'),
             
             proprity_tax=assessment_value*input$cProtax/1200,
             
             Monthly_Cost = Monthly_Payment + input$cmgt + input$cyard + input$cwarr
             
             + input$cinsure + proprity_tax +input$cHOV+credit_line_down_pmt,
             
             Monthly_Income = as.numeric(input$vrooms)*input$proom*input$voccp/100,
             
             Monthly_Net_rental_cash_Flow =  Monthly_Income-Monthly_Cost,
             
             Monthly_Net_cash_Flow_with_sale = Monthly_Net_rental_cash_Flow+proprity_value,
             
             Cum_Net_Income =cumsum(Monthly_Net_rental_cash_Flow),
             
             cum_Net_Income_withsale=Cum_Net_Income+proprity_value,
             
             presentvalue_cum_Net_Inome =Cum_Net_Income*(1+input$rROI/1200)^(-1*loan_age),
             
             presentvalue_cum_Net_Inome_withsale =cum_Net_Income_withsale*(1+input$rROI/1200)^(-1*loan_age),
             
             Rental_return_rate_as_of_today= (Cum_Net_Income + cumsum(prin_amt))*(1+input$rROI/1200)^(-1*loan_age)/(input$vvalue*input$vdown/100+input$lclose+input$lrepair-input$ccred)-1,
             
             Investment_return_rate_as_of_today = (presentvalue_cum_Net_Inome_withsale-rem_princ)/(input$vvalue*input$vdown/100+input$lclose+input$lrepair-input$ccred)-1
             
      )%>%rename(Principal=rem_princ, Interest_payment=Int_amt, principal_payment=prin_amt)
    
    
    
    MTG_table
    
  })
  
  
  
  
  
  
  
  
  
  
  
  MTG_plot<-reactive({
    
    
    
    blue <- rgb(0, 0, 1, alpha=0.5)
    
    red <- rgb(1, 0, 0, alpha=0.5)
    
    
    
    second_y_sacle<-max(MTG_DSN()$Principal/max(MTG_DSN()$Monthly_Payment))
    
    
    
    MTG_plot<-MTG_DSN()%>%filter(if_half_y=='Y')%>%mutate(Principal2=Principal/second_y_sacle) %>%
      
      tidyr::gather(key=metric,value=value, c('principal_payment', 'Interest_payment'))
    
    
    
    p_temp<-ggplot(MTG_plot,aes(x=Date,y=value))+geom_col(aes(fill=metric))+
      
      scale_fill_manual(values=c(blue, red))+
      
      geom_line(aes(x=Date, y=Principal2, color='principle_remaining'), color='black',size=1)+
      
      ylab("Monthly Payments in Dolllar")+scale_y_continuous(sec.axis = sec_axis(~.*(second_y_sacle/1000), name = "Outstanding Principal in K"))+
      
      ggtitle("Monthly Payments and Outstanding Balance") +
      
      theme(plot.title = element_text(hjust = 0.5,size =15, face='bold'),legend.position="bottom",legend.title=element_blank())
    
    p_temp
    
    
    
  })
  
  
  
  
  
  
  
  # p_temp<-ggplot(MTG_plot,aes(x=Date))+ ylab("Monthly Payments in Dolllar")+ geom_col(aes(x=Date, y=principal_payment, fill='red'),fill =red,show.legend=TRUE)+
  
  #   geom_col(aes(x=Date, y=Interest_payment, fill='blue'),fill =blue, show.legend=TRUE)+geom_line(aes(y=Principal/second_y_sacle, color='black'), color='black',size=1)+
  
  #   scale_y_continuous(sec.axis = sec_axis(~.*(second_y_sacle/1000), name = "Outstanding Principal in K"))+
  
  #   scale_fill_identity(name = 'the fill', guide = 'legend',labels = c('m1'))+
  
  #   scale_colour_manual(name = 'the colour',
  
  #                       values =c('black'='black','red'='red', 'blue=blue'), labels = c('c2','c1', 'c3'))
  
  #
  
  # p_temp<-p_temp+ ggtitle("Monthly Payments and Outstanding Balance") +
  
  #   theme(plot.title = element_text(hjust = 0.5,size =15, face='bold'),legend.position =c(1,1))
  
  
  
  cash_flow_plot1<-reactive({
    
    
    
    blue <- rgb(0, 0, 1, alpha=0.5)
    
    red <- rgb(1, 0, 0, alpha=0.5)
    
    
    
    cash_plot<-MTG_DSN()%>%filter(if_half_y=='Y')
    
    
    
    
    
    p_temp<-ggplot(cash_plot,aes(x=Date,y=Cum_Net_Income))+geom_line(aes(x=Date, y=Cum_Net_Income, color='Cum_Net_Income'),size =1)+
      
      geom_line(aes(x=Date, y=presentvalue_cum_Net_Inome, color='presentvalue_cum_Net_Inome'),size =1)+
      
      geom_line(aes(x=Date, y=cum_Net_Income_withsale, color='Cum_Net_Income_r'),size =1)+
      
      geom_line(aes(x=Date, y=presentvalue_cum_Net_Inome_withsale, color='presentvalue_cum_Net_Inome_r'),size =1)+
      
      ylab("Monthly cash flow in Dolllar")+ ggtitle("Cumulative Monthly Cash Flow")+
      
      scale_y_continuous(labels = dollar)+scale_colour_manual(values =c(red, blue, 'black', 'yellow'), labels = c('Cum Net Income','presentvalue cum Net Inome','Cum Net Income with sale','presentvalue cum Net Inome with sale'))+
      
      guides(color=guide_legend(nrow=2,byrow=TRUE))+
      
      theme(plot.title = element_text(hjust = 0.5,size =15, face='bold'),legend.position="bottom",legend.title=element_blank())
    
    
    
    
    
    
    
    p_temp
    
    
    
    
    
  })
  
  
  
  
  
  roa_plot1<-reactive({
    
    
    
    blue <- rgb(0, 0, 1, alpha=0.5)
    
    red <- rgb(1, 0, 0, alpha=0.5)
    
    
    
    cash_plot<-MTG_DSN()%>%filter(if_half_y=='Y')
    
    
    
    
    
    p_temp<-ggplot(cash_plot,aes(x=Date,y=Rental_return_rate_as_of_today))+geom_line(aes(x=Date, y=Rental_return_rate_as_of_today, color='Rental_return_rate_as_of_today'),size =1)+
      
      geom_line(aes(x=Date, y=Investment_return_rate_as_of_today, color='Investment_return_rate_as_of_today'),size =1)+
      
      ylab("Point in time return rate")+ ggtitle("Return Rate Chart")+
      
      scale_y_continuous(labels = percent)+scale_colour_manual(values =c(blue, red), labels = c('Investment_return_rate_as_of_today','Rental_return_rate_as_of_today'))+
      
      # guides(color=guide_legend(nrow=2,byrow=TRUE))+
      
      theme(plot.title = element_text(hjust = 0.5,size =15, face='bold'),legend.position="bottom",legend.title=element_blank())
    
    
    
    
    
    
    
    p_temp
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$Monthly_Rental_Income <-renderText(
    
    { paste0('The Monthly total rental Income is $', as.numeric(input$vrooms)*input$proom)
      
    })
  
  
  
  
  
  
  
  output$Monthly_MTG_Pmt <-renderText(
    
    paste0('The Monthly Mortgage payment $ ', round(as.numeric(MTG_DSN()[1,2]), digits = 2))
    
  )
  
  
  
  
  
  
  
  
  
  output$PaymentPlot<- renderPlot({
    
    
    
    ph<-  MTG_plot()
    
    
    
    print(ph) 
    
  })
  
  
  
  
  
  output$Total_inital_invst<-renderText(
    
    paste0('Total first time cash is: ',
           
           '$',formatC(input$vvalue*input$vdown/100+input$lclose+input$lrepair-input$ccred, format="f", digits=0, big.mark=","))
    
    
    
  )
  
  
  
  
  
  output$sale_date<-renderText(
    
    if(input$rsale >=1){
      
      paste0('If you sale the property ', input$rsale, ' mths after purchase, the sale price will be: ',
             
             '$',formatC(MTG_DSN()$proprity_value[input$rsale+1], format="f", digits=0, big.mark=","))
      
      
      
    }else{
      
      paste0('No sale happens during the loan period!')
      
    }
    
    
    
  )
  
  
  
  
  
  output$Cash_flow_plot<- renderPlot({
    
    
    
    ph<-  cash_flow_plot1()
    
    
    
    print(ph) 
    
  })
  
  
  
  
  
  output$cum_roa<- renderPlot({
    
    
    
    ph<-  roa_plot1()
    
    
    
    print(ph) 
    
  })
  

  
  output$muview_table = renderDT(
    
    MTG_DSN(), options = list(lengthChange = FALSE)
    
  )
  
  
  
  
  
  
  
}


shinyApp(ui=ui, server=server)