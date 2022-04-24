
# define widgets
#my_ui 

my_ui <- fluidPage(
  titlePanel("Facebook Live Selling Analysis"),
  sidebarPanel(
    width = 4,
    p("This dataset includes the customer behavior data from the Facebook pages of 10 
      Thai fashion and cosmetics retail sellers from March 2012 to June 2018"),
    sliderInput(
      inputId = 'before_year', 
      label = "Year Before Facebook Live", 
      min = 2012, 
      max = 2016, 
      value = 2014, 
      step = 1,
      ticks = TRUE), 
    
    sliderInput(
      inputId = 'after_year', 
      label = "Year After Facebook Live", 
      min = 2016, 
      max = 2018, 
      value = 2016, 
      step = 1,
      ticks = TRUE), 
    
    
    # a group of checkboxes
    checkboxGroupInput(
      inputId ="status", 
      label = "Status Type", 
      choices = list("Video" = "video", 
                     "Photo" = "photo", 
                     "Link" = "link",
                     "Status" = "status"),
      selected = c("video", "photo","link", "status")),

    
    plotOutput("reactPlot",
               width = "100%", 
               height = "200px"),
    
    actionButton(
      inputId = "reset",
      label = "Reset Data",
      icon = icon("refresh"),
      width = "100%"
    )
  ),
  
  mainPanel(
    fluidPage(
      img(src = 'fb.png', height = '100px', width = '100px', align = "right"),
      tabsetPanel(id = 'tabs',
        tabPanel(title ="# of reactions",
                 value = 'total_num_reactions'),
        tabPanel(title ="# of comments", 
                 value = 'total_num_comments'),
        tabPanel(title ="# of shares", 
                 value = 'total_num_shares'),
        tabPanel(title ="# of likes",
                 value = 'total_num_likes')
      ),
      plotlyOutput("before",
                 width = "80%", 
                 height = "300px"),
      plotlyOutput("after",
                 width = "80%", 
                 height = "300px"),
      plotOutput("after_react",
                 width = "80%", 
                 height = "300px")

  ))
  
  
)


#shinyUI(my_ui)

