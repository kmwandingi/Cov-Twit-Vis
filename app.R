library(shiny)
library(shinyjs)
library(shticky)
library(sigmajs)
library(waypointer)
library(graphTweets)

source("./data/network.R")
source("functions.R")

OFFSET <- "80%"
ANIMATION <- "slideInUp"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.8.1/css/all.css",
      integrity = "sha384-50oBUHEmvpQ+1lW4y57PTFmhCaXp0ML5d60M1M7uH2+nqUivzIebhndOJK28anvf",
      crossorigin = "anonymous"
    )
  ),
  use_shticky(),
  use_waypointer(),
  shortdiv(id = "sm",
    fluidRow(
      column(3, p(
        tags$a(
          style="text-decoration:none",
          "info@ergoanalysticscc.com",
          target = "_blank",
          class = "sg",
          href = "mailto:info@ergoanalyticscc.com"
          )
        )
      ),
      column(4, p(
        tags$a(
          style="text-decoration:none",
          "Red Cross - Safety Tips",
          target = "_blank",
          class = "sg",
          href = "https://www.redcross.org/about-us/news-and-events/news/2020/coronavirus-safety-and-readiness-tips-for-you.html/"
        )
      )
    ),
      column(5, p(
        tags$a(
          style="text-decoration:none",
          "nCOV2019 - Live Data",
          target = "_blank",
          class = "sg",
          href = "https://ncov2019.live/data"
            )
          )
        )
      )
    ),
  div(
    id = "bg",
    div(
      id = "stick",
      style = "position:fixed;width:100%;",
      fluidRow(
        column(4),
        column(8, sigmajsOutput("graph", width = "100%", height = "100vh"))
      )
    ),
    longdiv(
      h1("Nine Days of #coronavirus", class = "title"),
      br(),
      br(),
      h1(
        class = "subtitle",
        "Each", tags$i(class = "fas fa-circle sg"), "node is a twitter user,",
        "and each", tags$i(class = "fas fa-slash sg"), "edge is one tweet or more."
      ),
      br(),
      p(
        style = "text-align:center;",
        "Using data obtained from twitter, geocoded to Namibia. Starting Monday, 9 March to Tuesday, 17 March", 
        tags$a(
          class = "sg",
          tags$i(class = "fas fa-external-link-alt"),
          target = "_blank",
          href = "https://twitter.com/"
        )
      ),
      br(),
      br(),
      br(),
      p(
        style = "text-align:center;",
        tags$i(class = "fas fa-chevron-down fa-3x")
      )
    ),
    longdiv(
      div(
        id = "m1",
        uiOutput("1"),
        #uiOutput("sun")
      )
    ),
    longdiv(
      div(
        id = "m2",
        uiOutput("2"),
        #uiOutput("mon")
      )
    ),
    longdiv(
      div(
        id = "m3",
        uiOutput("3"),
        uiOutput("tue")
      )
    ),
    longdiv(
      div(
        id = "m4",
        uiOutput("4"),
        uiOutput("wed")
      )
    ),
    longdiv(
      div(
        id = "m5",
        uiOutput("5"),
        uiOutput("thu")
      )
    ),
    longdiv(
      div(
        id = "m6",
        uiOutput("6"),
        uiOutput("fri")
      )
    ),
    longdiv(
      div(
        id = "m7",
        uiOutput("7"),
        uiOutput("sat")
      )
    ),
    longdiv(
      div(
        id = "m8",
        uiOutput("8"),
        uiOutput("sun-2")
      )
    ),
    longdiv(
      div(
        id = "m9",
        uiOutput("9"),
        uiOutput("Mon-2")
      )
    ),
    longdiv(
      id = "m10",
      class = "dark",
      style = "text-align:left;",
      h1("Thank You.", class = "title-last"),
      br(),
      p("Refresh page if you wish to see the network animation again.", class = "dark"),
      br(),
      p("Look after yourselves and take care not to expose those more at risk.", class = "dark"),
      br(),
      p("Useful links and contact email at the top of the page.", class = "sg")
     )
   )
)

server <- function(input, output, session) {

  w1 <- Waypoint$
    new("m1", offset = "50%", animate = TRUE, animation = ANIMATION)$
    start()
  w2 <- Waypoint$
    new("m2", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w3 <- Waypoint$
    new("m3", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w4 <- Waypoint$
    new("m4", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w5 <- Waypoint$
    new("m5", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w6 <- Waypoint$
    new("m6", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w7 <- Waypoint$
    new("m7", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w8 <- Waypoint$
    new("m8", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  w9 <- Waypoint$
    new("m9", offset = OFFSET, animate = TRUE, animation = ANIMATION)$
    start()
  
  w10 <- Waypoint$
    new("m10", offset = "100%", animate = TRUE, animation = ANIMATION)$
    start()

  output$`1` <- renderUI({
    req(w1$get_triggered())
    
    if(w1$get_triggered() == TRUE) render_day(9)
  })

  output$`2` <- renderUI({
    req(w2$get_triggered())
    if(w2$get_triggered() == TRUE) render_day(10)
  })
  
  output$`3` <- renderUI({
    req(w3$get_triggered())
    if(w3$get_triggered() == TRUE) render_day(11)
  })
  
  output$`4` <- renderUI({
    req(w4$get_triggered())
    if(w4$get_triggered() == TRUE) render_day(12)
  })
  
  output$`5` <- renderUI({
    req(w5$get_triggered())
    if(w5$get_triggered() == TRUE) render_day(13)
  })
  
  output$`6` <- renderUI({
    req(w6$get_triggered())
    if(w6$get_triggered() == TRUE) render_day(14)
  })
  
  output$`7` <- renderUI({
    req(w7$get_triggered())
    if(w7$get_triggered() == TRUE) render_day(15)
  })
  
  output$`8` <- renderUI({
    req(w8$get_triggered())
    if(w8$get_triggered() == TRUE) render_day(16)
  })
  
  output$`9` <- renderUI({
    req(w9$get_triggered())
    if(w9$get_triggered() == TRUE) render_day(17)
  })

  # Our sticky plot
  shtick <- Shtick$
    new("#stick")$
    shtick()

  output$graph <- renderSigmajs({
    sigmajs() %>% 
      sg_settings(
        edgeColor = "default",
        defaultEdgeColor = "#c3c3c3",
        font = "Raleway",
        fontStyle = "sans-serif",
        touchEnabled = FALSE,
        mouseWheelEnabled = FALSE,
        labelSize = "proportional",
        labelThreshold = 9999
      )
  })

  observeEvent(w1$get_direction(), {
    
    req(w1$get_direction())
    
    if(w1$get_direction() == "down") add_data(9)
    else
      ""
  })

  observeEvent(w2$get_direction(), {
    
    req(w2$get_direction())
    
    if(w2$get_direction() == "down") add_data(10)
    else
      ""
  })

  observeEvent(w3$get_direction(), {
    
    req(w3$get_direction())
    
    
    if(w3$get_direction() == "down") add_data(11)
    else
      ""
  })

  observeEvent(w4$get_direction(), {
    if(w4$get_direction() == "down") add_data(12)
  })

  observeEvent(w5$get_direction(), {
    if(w5$get_direction() == "down") add_data(13)
  })

  observeEvent(w6$get_direction(), {
    if(w6$get_direction() == "down") add_data(14)
  })

  observeEvent(w7$get_direction(), {
    if(w7$get_direction() == "down") add_data(15)
  })

  observeEvent(w8$get_direction(), {
    if(w8$get_direction() == "down") add_data(16)
  })
  

  observeEvent(w9$get_direction(), {
    if(w9$get_direction() == "down") add_data(17)
    sigmajsProxy("graph")
  })
  
  observeEvent(w10$get_direction(), {
    if(w10$get_direction() == "down") add_data(17)
    sigmajsProxy("graph")
  })
  
  
}

shinyApp(ui, server)