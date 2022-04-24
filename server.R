
library(ggplot2)
library(tidyverse)
library(tidyr)
library(readr)
library(lubridate)
library(plyr)
library(dplyr)
library(ggradar)
library(fmsb)
library(plotly)


#function

Yrstatus<-function(df, endYear, Status){
  data <-df %>% filter(status_published_year == endYear )%>%
    dplyr::filter(status_type%in%Status)
  return(data)
}

plot_all <- function(data){
  
  ggplot(data, aes(x=status_published_year, y=n,
                   group = 1, color='orange'))+
    geom_line(stat="identity")+ geom_point() +
    ggtitle("Number of Posts (2012-2018)")+
    labs(x="Year",y="Number of Posts")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'))+theme(legend.position="none")
}

# define output reactive expressions

my_server<- function(input, output, session) {
  
  # before year: input$before_year
  # after year: input$after_year
  # status_type : input$status
  
  output$reactPlot <- renderPlot({
    # select status_type in input$status
    
    adata <- post %>% 
      dplyr::filter(status_type%in%input$status)  %>% group_by(status_published_year) %>% dplyr::summarise(n = sum(n))

    plot_all(adata)

  })
  
  output$before <- renderPlotly({
    column = input$tabs
    #     'total_num_reactions'
    before_data <- Yrstatus(facebook_data, input$before_year, input$status)  %>% ungroup()%>%
      filter(Date <= '2016-03-01')%>%
      select(c('status_type', 'status_published_month', input$tabs))
    
    if (input$tabs == 'total_num_reactions') {
      a <- ggplot(data = before_data, aes(x = status_published_month,
                                          y = total_num_reactions,
                                          fill = status_type)) +
        geom_bar(stat="identity") +
        
        scale_fill_brewer(palette="Paired") +
        #input$tabs,'in',input$before_year,'lll',
        #ggtitle(paste(colnames(before_data),'lll', column)) +
        ggtitle("Num of Reactions Before Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 2500)
      
      ggplotly(a)
      
      
    } else if (input$tabs == 'total_num_comments'){
      a <- ggplot(data = before_data, aes(x = status_published_month,
                                          y = total_num_comments,
                                          fill = status_type)) +
        geom_bar(stat="identity") +
        
        scale_fill_brewer(palette="Paired") +
        #input$tabs,'in',input$before_year,'lll',
        #ggtitle(paste(colnames(before_data),'lll', column)) +
        ggtitle("Num of Comments Before Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 1000)
      ggplotly(a)
    } else if (input$tabs == 'total_num_shares'){
      a <- ggplot(data = before_data, aes(x = status_published_month, 
                                          y = total_num_shares,
                                          fill = status_type)) +
        geom_bar(stat="identity") +
        
        scale_fill_brewer(palette="Paired") +
        #input$tabs,'in',input$before_year,'lll',
        #ggtitle(paste(colnames(before_data),'lll', column)) +
        ggtitle("Num of Shares Before Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 300)
      ggplotly(a)
    } else {
      a <- ggplot(data = before_data, aes(x = status_published_month, 
                                          y = total_num_likes,
                                          fill = status_type)) +
        geom_bar(stat="identity") +
        
        scale_fill_brewer(palette="Paired") +
        #input$tabs,'in',input$before_year,'lll',
        #ggtitle(paste(colnames(before_data),'lll', column)) +
        ggtitle("Num of Likes Before Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 8000)
      ggplotly(a)
    }
    
  })
  
  
  output$after <- renderPlotly({
    column = input$tabs
    
    after_data <- Yrstatus(facebook_data, input$after_year, input$status)  %>% ungroup() %>%
      filter(Date > '2016-03-01')%>%
      select(c('status_type', 'status_published_month', input$tabs)) 
    
    if (input$tabs == 'total_num_reactions') {
      b <- ggplot(data = after_data, aes(x = status_published_month, 
                                         y = total_num_reactions,
                                         fill = status_type)) +
        geom_bar(stat="identity") +
        scale_fill_brewer(palette="Paired") +
        ggtitle("Num of Reactions After Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 2500)
      
      ggplotly(b)
      
      # ggplotly(b)
    } else if (input$tabs == 'total_num_comments'){
      b <- ggplot(data = after_data, aes(x = status_published_month, 
                                         y = total_num_comments,
                                         fill = status_type)) +
        geom_bar(stat="identity") +
        scale_fill_brewer(palette="Paired") +
        ggtitle("Num of Comments After Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 1000)
      
      ggplotly(b)
    } else if (input$tabs == 'total_num_shares'){
      
      b <- ggplot(data = after_data, aes(x = status_published_month, 
                                         y = total_num_shares,
                                         fill = status_type)) +
        geom_bar(stat="identity") +
        
        scale_fill_brewer(palette="Paired") +
        ggtitle("Num of Shares After Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 300)
      
      ggplotly(b)
    } else {
      b <- ggplot(data = after_data, aes(x = status_published_month, 
                                         y = total_num_likes,
                                         fill = status_type)) +
        geom_bar(stat="identity") +
        
        scale_fill_brewer(palette="Paired") +
        ggtitle("Num of Likes After Launch New Feature") +
        labs(x="Month",y=input$tabs) +
        theme_bw() +
        ylim(0, 8000)
      ggplotly(b)
    }
  })
  
  
  observeEvent(input$tabs, {
    if (input$tabs == 'total_num_reactions'){
      
      output$after_react <- renderPlot({
        after_data <- Yrstatus(facebook_data, input$after_year, input$status)
        agre <- after_data %>% group_by(status_type) %>% 
          dplyr::summarise(loves = sum(total_num_loves),
                           wows = sum(total_num_wows),
                           hahas = sum(total_num_hahas),
                           sads = sum(total_num_sads), 
                           angrys = sum(total_num_angrys)) 
        ggradar(agre, base.size=2, font.radar = "sans",
                grid.mid=250, 
                grid.max=500, group.line.width=1, group.point.size = 1.5,
                plot.title="Radar chart of Reaction types")
      
      })
      
    }
    else {
      output$after_react <-NULL
    }
    
  })
  
  
  
}


shinyServer(my_server)
