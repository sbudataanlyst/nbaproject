library(shiny)
library(tidyverse)
library(nbastatR) # if needed install_github("abresler/nbastatR") if needed
library(ggplot2)
library(gganimate)
library(lubridate)
library(ggalt)
library(artyfarty)
library(dplyr)
library(stringr)


# Define UI for application that draws a histogram


# id for Devin Booker franchise scoring record vs Boston

phx <- read_csv("updated_shots2023.csv")
shots2023 <- read_csv("updated_shots2023.csv")


ui <- fluidPage(
    # Application title
    titlePanel("2023 NBA Playoffs: Shot Selection"),
    fluidRow(
      uiOutput("player"),
      uiOutput("game"),
      #tableOutput("table"),
      splitLayout(cellWidths = c("50%", "50%"),
                  imageOutput("plot",width = "100%"),
                  imageOutput("plot2", width = "100%")
      )
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$player <- renderUI({
    selectizeInput(
      inputId = "searchplayer", 
      label = "Player Name",
      multiple = FALSE,
      choices = c("Search Bar" = "", paste0(unique(shots2023$namePlayer))),
      options = list(
        create = FALSE,
        placeholder = "Search Me",
        maxItems = '1',
        onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
        onType = I("function (str) {if (str === \"\") {this.close();}}")
                    )
                  )
                            })
  
  data_filter <- reactive({
    shots2023 %>%
      filter(namePlayer == input$searchplayer)
                          })        
  
  output$game <- renderUI({
    selectizeInput(
      inputId = "searchgame", 
      label = "Game",
      multiple = FALSE,
      choices = c("Game" = "", paste0(unique(data_filter()$slugMatchupDate))),
      options = list(
        create = FALSE,
        placeholder = "Search Me",
        maxItems = '1',
        onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
        onType = I("function (str) {if (str === \"\") {this.close();}}")
      )
    )
  })
  
 data_filter2 <- reactive({
                              data_filter() %>%
                              filter(slugMatchupDate == input$searchgame)
 })
  
  
  #######################################################################################################################################

 #output$table <- renderTable({
  #data_filter2() %>% mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>% 
   # mutate(time_chron=case_when(
    #  numberPeriod==1~ms("12:00")-gtime,
     # numberPeriod==2~ms("24:00")-gtime,
      #numberPeriod==3~ms("36:00")-gtime,
      #numberPeriod==4~ms("48:00")-gtime,
      #numberPeriod==5~ms("52:00")-gtime)) %>% 
    #mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))
 #})
 
 
 output$plot <- renderImage({
   DBshots <- data_filter2() %>% 
     mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>% 
     mutate(time_chron=case_when(
       numberPeriod==1~ms("12:00")-gtime,
       numberPeriod==2~ms("24:00")-gtime,
       numberPeriod==3~ms("36:00")-gtime,
       numberPeriod==4~ms("48:00")-gtime,
       numberPeriod==5~ms("52:00")-gtime)) %>% 
     mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))
   
   anim <- 
     ggplot(DBshots)+
     geom_hline(yintercept = 23.9,linetype=2,color="gray")+
     annotate("text",label="from downtown!",x=700,26.5,size=5,alpha=0.5,color="grey70")+
     geom_vline(xintercept = as.numeric(ms("48:00")),linetype=3,color="red")+
     
     geom_lollipop(aes(x=time_chron,y=distTrans,
                       color=isShotMade))+
     labs(y="shot distance (feet) \n *excludes dunks and free throws",
          x="time (minutes)", 
          title = paste0(unique(DBshots$namePlayer), 
                         " ",
                         unique(DBshots$slugMatchupDate), 
                         sep =" ")) + 
     scale_x_time(breaks = ms(c("12:00","24:00","36:00","48:00")))+
     #scale_color_manual(values = c("#00529b","#cc4b4b"),labels=c("made","missed"))+
     scale_color_manual(values = pal("xmen"), labels = c("made","missed")) + 
     #theme_farty()+
     theme(text = element_text(size = 19),
           panel.grid.major.x = element_blank(),
           legend.position = "bottom",
           legend.title = element_blank(),
           legend.text = element_text(size=19))+
     guides(color = guide_legend(override.aes = list(size = 4)))+
     theme_farty()+
     transition_states(idEvent)+shadow_mark()
   
   
   anim_save("outfile.gif", animate(anim, height = 800, width = 800))
   list(src = "outfile.gif", contentType = "image/gif")
 },
 deleteFile = TRUE
 )
  ####################################################################################################################################################################
 ###################################################################################################################################################################
 ###################################################################################################################################################################
 ###################################################################################################################################################################
 
 output$plot2 <- renderImage({
   DBshots <- data_filter2() %>% 
     mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>% 
     mutate(time_chron=case_when(
       numberPeriod==1~ms("12:00")-gtime,
       numberPeriod==2~ms("24:00")-gtime,
       numberPeriod==3~ms("36:00")-gtime,
       numberPeriod==4~ms("48:00")-gtime,
       numberPeriod==5~ms("52:00")-gtime)) %>% 
     mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))
   
   # to plot a half court, using data from the ballR shiny app 
   source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
   source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")
   plot_court() # created the court_points object we need
   court_points <- court_points %>% mutate_if(is.numeric,~.*10)
     # y coordinates are shifted to account for the backboard+rim in the ballr data
     
  DBcourt <- 
     ggplot(DBshots, aes(x=locationX, y=locationY+45)) + 
     scale_fill_manual(#values = c("#00529b","#cc4b4b")
       values = pal("xmen"), guide=FALSE)+ 
     geom_path(data = court_points,
               aes(x = x, y = y, group = desc),
               color = "black")+
     coord_equal()+
     geom_point(aes(fill=isShotMade),pch=21,size=4,color="white") +
     xlim(-260, 260) +
     #theme_farty()+
     labs(title="Shot location",x="",
          y="" #,
          #caption = "by @LuisDVerde\nNBA data accessed with nbastatr"
          )+
     theme(text = element_text(size = 19),
           panel.grid = element_blank(),
           axis.text = element_blank(),
           plot.caption = element_text(color="white"))+
     theme_farty()+
     transition_states(idEvent)+shadow_mark()
  
   anim_save("outfile2.gif", animate(DBcourt,height=800,width=800))
   list(src = "outfile2.gif", contentType = "image/gif")
 },
 deleteFile = TRUE
 ) 
 
 ####################################################################################################################################################################
 
 
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)
