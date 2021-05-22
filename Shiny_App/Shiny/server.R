library(readr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(stringr)

shinyServer(function(input, output, session) {

  
#Intro--------------------------------------------------------------------------------- 
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("introtext.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "Virtueller Rundgang", icon = icon("info-circle"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  # show intro tour
  observeEvent(input$intro,
               introjs(session, options = list("nextLabel" = "Weiter",
                                               "prevLabel" = "Zurück",
                                               "doneLabel" = "Alles klar, Los geht's!")))
  
  
    
#Tabitem = Bedarfsvorhersage---------------------------------------------------------------------------------    
    output$dateBox <- renderInfoBox({
        infoBox(
            h4("Datum"), subtitle = h4(time_series[which(time_series$Date==as.character(input$Datum)),which(colnames(time_series)== "day")], br(), input$Datum), 
            icon = icon("calendar"), color = "green")})

    output$totalmealbox <- renderInfoBox({
        infoBox(
            h4("Totale Menüs"),icon = icon("utensils"),color = "green",
            
            if (input$Anpassung_Menu1_day == "0" && input$Anpassung_Menu2_day == "0" && input$Anpassung_Menu3_day == "0")
            
                {h4(paste0("Geschätzter Bedarf: ", sum(head(sort(predicted_values[,which(colnames(predicted_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3))),
                          br(),
                          "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))} 
            else if (!(input$Anpassung_Menu1_day == "0") && input$Anpassung_Menu2_day == "0" && input$Anpassung_Menu3_day == "0")
                {h4(paste0("Geschätzter Bedarf: ", (sum(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE)[c(2,3)])) +input$Anpassung_Menu1_day),
                    br(),
                    "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))} 
            
            else if (input$Anpassung_Menu1_day == "0" && !(input$Anpassung_Menu2_day == "0") && input$Anpassung_Menu3_day == "0")
            {h4(paste0("Geschätzter Bedarf: ", (sum(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE)[c(1,3)])) +input$Anpassung_Menu2_day),
                br(),
                "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))} 
            
            else if (input$Anpassung_Menu1_day == "0" && input$Anpassung_Menu2_day == "0" && !(input$Anpassung_Menu3_day == "0"))
            {h4(paste0("Geschätzter Bedarf: ", (sum(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE)[c(1,2)])) +input$Anpassung_Menu3_day),
                br(),
                "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))}
            
            else if (input$Anpassung_Menu1_day == "0" && !(input$Anpassung_Menu2_day == "0") && !(input$Anpassung_Menu3_day == "0"))
            {h4(paste0("Geschätzter Bedarf: ", (sum(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE)[1])) +input$Anpassung_Menu2_day +input$Anpassung_Menu3_day),
                br(),
                "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))}
            
            else if (!(input$Anpassung_Menu1_day == "0") && input$Anpassung_Menu2_day == "0" && !(input$Anpassung_Menu3_day == "0"))
            {h4(paste0("Geschätzter Bedarf: ", (sum(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE)[2])) +input$Anpassung_Menu1_day +input$Anpassung_Menu3_day),
                br(),
                "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))}
            
            else if (!(input$Anpassung_Menu1_day == "0") && !(input$Anpassung_Menu2_day == "0") && input$Anpassung_Menu3_day == "0")
            {h4(paste0("Geschätzter Bedarf: ", (sum(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE)[3])) +input$Anpassung_Menu1_day +input$Anpassung_Menu2_day),
                br(),
                "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))}
            
            else if (!(input$Anpassung_Menu1_day == "0") && !(input$Anpassung_Menu2_day == "0") && !(input$Anpassung_Menu3_day == "0"))
            {h4(paste0("Geschätzter Bedarf: ", input$Anpassung_Menu1_day +input$Anpassung_Menu2_day + input$Anpassung_Menu3_day),
                br(),
                "Bedarf Vergleichsperiode: ",sum(head(sort(actual_values[,which(colnames(actual_values) ==as.character(input$Datum))],decreasing=TRUE), n = 3)))}
            
        )})
    
    output$day_icons <- renderInfoBox({
        infoBox(color  = "green", h4("Beeinflussende Faktoren:"),                     
                column(width= 12, align="center",
                            div(style="display: inline-block;",imageOutput("occupation", height = 40)),
                            div(style="display: inline-block;",imageOutput("season", height = 40)),
                            div(style="display: inline-block;",imageOutput("weather", height = 40)),
                            div(style="display: inline-block;",imageOutput("terrace", height = 40)),
                            div(style="display: inline-block;",imageOutput("event", height = 40)))
        )})
  
    #Menu 1---------------------------------------------------------------------------------    
    output$mealbox1 <- renderValueBox({
        valueBox(
            p("Menü 1"), color  = "light-blue",
               if (input$Anpassung_Menu1_day == "0")
               {h4(paste0("Geschätzter Bedarf: ", predicted_values[which(rownames(predicted_values)== input$ChooseMenu),which(colnames(predicted_values)== input$Datum)]),
                   br(),
                   "Bedarf Vergleichsperiode: ",actual_values[which(rownames(actual_values)== input$ChooseMenu),which(colnames(actual_values)== input$Datum)])}
               else
               {h4(paste0("Geschätzter Bedarf: ", input$Anpassung_Menu1_day),
                   br(),
                   "Bedarf Vergleichsperiode: ",actual_values[which(rownames(actual_values)== input$ChooseMenu),which(colnames(actual_values)== input$Datum)])}
               )})
    
    output$menu1_alt <- renderValueBox({
        valueBox(p("Alternative Menüs", br(), h6("Basierend auf der ursprünglichen Empfehlung")),
                 h4("1:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][4,1], br(),
                    "2:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][5,1], br(),
                    "3:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][6,1],),
                 color  = "light-blue")})

    #Menu 2---------------------------------------------------------------------------------
    output$mealbox2 <- renderInfoBox({
        valueBox(
            p("Menü 2"), color  = "yellow",
            if (input$Anpassung_Menu2_day == "0")
            {h4(paste0("Geschätzter Bedarf: ", predicted_values[which(rownames(predicted_values)== input$ChooseMenu2),which(colnames(predicted_values)== input$Datum)]),
                br(),
                "Bedarf Vergleichsperiode: ",actual_values[which(rownames(actual_values)== input$ChooseMenu2),which(colnames(actual_values)== input$Datum)])}
            else
            {h4(paste0("Geschätzter Bedarf: ", input$Anpassung_Menu2_day),
                br(),
                "Bedarf Vergleichsperiode: ",actual_values[which(rownames(actual_values)== input$ChooseMenu2),which(colnames(actual_values)== input$Datum)])}
        )})
    
    output$menu2_alt <- renderInfoBox({
        valueBox(p("Alternative Menüs", br(), h6("Basierend auf der ursprünglichen Empfehlung")),
                 h4("1:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][7,1], br(),
                    "2:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][8,1], br(),
                    "3:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][9,1],),
                 color  = "yellow")})
 
    #Menu 3---------------------------------------------------------------------------------
    output$mealbox3 <- renderInfoBox({
        valueBox(
            p("Menü 3"), color  = "aqua",
            if (input$Anpassung_Menu3_day == "0")
            {h4(paste0("Geschätzter Bedarf: ", predicted_values[which(rownames(predicted_values)== input$ChooseMenu3),which(colnames(predicted_values)== input$Datum)]),
                br(),
                "Bedarf Vergleichsperiode: ",actual_values[which(rownames(actual_values)== input$ChooseMenu3),which(colnames(actual_values)== input$Datum)])}
            else
            {h4(paste0("Geschätzter Bedarf: ", input$Anpassung_Menu3_day),
                br(),
                "Bedarf Vergleichsperiode: ",actual_values[which(rownames(actual_values)== input$ChooseMenu3),which(colnames(actual_values)== input$Datum)])}
        )})
    
    output$menu3_alt <- renderInfoBox({
        valueBox(p("Alternative Menüs", br(), h6("Basierend auf der ursprünglichen Empfehlung")),
                 h4("1:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][10,1], br(),
                    "2:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][11,1], br(),
                    "3:",alt_options[order(-alt_options[which(colnames(alt_options) ==as.character(input$Datum))]),c(1,which(colnames(alt_options) ==as.character(input$Datum))), drop = FALSE][12,1],),
                 color  = "aqua")})
    
#Tabitem = Wochenübersicht---------------------------------------------------------------------------------
 
    output$dateBox_week <- renderInfoBox({
        infoBox(
            h4("Zeitraum"), h4(time_series[which(time_series$Date==as.character(input$Datum)),which(colnames(time_series)== "day")]," ", "bis", " ", time_series[which(time_series$Date==as.Date(input$Datum, format="%Y-%m-%d") + 4),which(colnames(time_series)== "day")]),
            h4(input$Datum, " ", "bis", " ", br(), as.Date(input$Datum, format="%Y-%m-%d") + 4), 
            icon = icon("calendar"), color = "green")})
    
    output$week_total_demand <- renderInfoBox({
      infoBox(
        h4("Totale Menüs"),h5("Bedarf geschätzt | Vergleichsperiode"),icon = icon("utensils"),color = "green",
        h5("Menü 1 ",input$ChooseMenu_M_1, ": ",predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== input$Datum)]+
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]+ 
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]+ 
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]+
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)],
           " | ",
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== input$Datum)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)],
           br(), 
           "Menü 2 ",input$ChooseMenu_M_2, ": ",predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== input$Datum)]+
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]+ 
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]+ 
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]+
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)],
           " | ",
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== input$Datum)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)],
           br(), 
           "Menü 3 ",input$ChooseMenu_M_3, ": ",predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== input$Datum)]+
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]+ 
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]+ 
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]+
             predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)],
           " | ",
           actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== input$Datum)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]+
             actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)],
        )
      )
    })
    output$week_icons <- renderInfoBox({
        infoBox(color  = "green", h4("Beeinflussende Faktoren:"),
                column(12,
                column(width = 3, 
                       h5(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d"))), 2]),
                       h5(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d")+1)), 2]),
                       h5(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d")+2)), 2])
                       ),
                
                column(width= 3, 
                       fluidRow(
                         div(style="display: inline-block;",imageOutput("occupation_week_d1", height = 10)),
                         div(style="display: inline-block;",imageOutput("season_week_d1", height = 10)),
                         div(style="display: inline-block;",imageOutput("weather_week_d1", height = 10)),
                         div(style="display: inline-block;",imageOutput("terrace_week_d1", height = 10)),
                         div(style="display: inline-block;",imageOutput("event_week_d1", height = 10)) ),
                       fluidRow(
                         div(style="display: inline-block;",imageOutput("occupation_week_d2", height = 10)),
                         div(style="display: inline-block;",imageOutput("season_week_d2", height = 10)),
                         div(style="display: inline-block;",imageOutput("weather_week_d2", height = 10)),
                         div(style="display: inline-block;",imageOutput("terrace_week_d2", height = 10)),
                         div(style="display: inline-block;",imageOutput("event_week_d2", height = 10)) ),
                       fluidRow(
                         div(style="display: inline-block;",imageOutput("occupation_week_d3", height = 10)),
                         div(style="display: inline-block;",imageOutput("season_week_d3", height = 10)),
                         div(style="display: inline-block;",imageOutput("weather_week_d3", height = 10)),
                         div(style="display: inline-block;",imageOutput("terrace_week_d3", height = 10)),
                         div(style="display: inline-block;",imageOutput("event_week_d3", height = 10)) )
                       ),
                
                column(width = 3,
                       h5(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d")+3)), 2]),
                       h5(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d")+4)), 2])
                       ),
                column(width= 3, 
                       fluidRow(
                         div(style="display: inline-block;",imageOutput("occupation_week_d4", height = 10)),
                         div(style="display: inline-block;",imageOutput("season_week_d4", height = 10)),
                         div(style="display: inline-block;",imageOutput("weather_week_d4", height = 10)),
                         div(style="display: inline-block;",imageOutput("terrace_week_d4", height = 10)),
                         div(style="display: inline-block;",imageOutput("event_week_d4", height = 10)) ),
                       fluidRow(
                         div(style="display: inline-block;",imageOutput("occupation_week_d5", height = 10)),
                         div(style="display: inline-block;",imageOutput("season_week_d5", height = 10)),
                         div(style="display: inline-block;",imageOutput("weather_week_d5", height = 10)),
                         div(style="display: inline-block;",imageOutput("terrace_week_d5", height = 10)),
                         div(style="display: inline-block;",imageOutput("event_week_d5", height = 10)) )
                       ),
              
                ),
        )})
    
    #menu 1 d1-d5---------------------------------------------------------------------------------
   #predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== input$Datum)]
     output$m1_d1 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d"))), 2]),
            # h4(input$Datum),
            h6("Geschätzter Bedarf", "|", br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== input$Datum)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== input$Datum)])), 
            icon = icon("food"), color  = "light-blue")})
    
    output$m1_d2 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 1)), 2]),
            h6("Geschätzter Bedarf","|", br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)])), 
            icon = icon("food"), color  = "light-blue")})
    
    output$m1_d3 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 2)), 2]),
            h6("Geschätzter Bedarf","|", br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)])), 
            icon = icon("food"), color  = "light-blue")})
    
    output$m1_d4 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 3)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)])), 
            icon = icon("food"), color  = "light-blue")})
    
    output$m1_d5 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 4)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_1),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_1),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)])), 
            icon = icon("food"), color  = "light-blue")})
    
    #menu 2 d1-d5---------------------------------------------------------------------------------    
    output$m2_d1 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d"))), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== input$Datum)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== input$Datum)])), 
            icon = icon("food"), color  = "yellow")})
    
    output$m2_d2 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 1)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)])), 
            icon = icon("food"), color  = "yellow")})
    
    output$m2_d3 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 2)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)])), 
            icon = icon("food"), color  = "yellow")})
    
    output$m2_d4 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 3)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)])), 
            icon = icon("food"), color  = "yellow")})
    
    output$m2_d5 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 4)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_2),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_2),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)])), 
            icon = icon("food"), color  = "yellow")})
    
    #menu 3 d1-d5---------------------------------------------------------------------------------    
    output$m3_d1 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d"))), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== input$Datum)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== input$Datum)])), 
            icon = icon("food"), color  = "aqua")})
    
    output$m3_d2 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 1)), 2]),
            h6("Geschätzter Bedarf", "|",br(), " ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 1)])), 
            icon = icon("food"), color  = "aqua")})
    
    output$m3_d3 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 2)), 2]),
            h6("Geschätzter Bedarf", "|",br()," ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 2)])), 
            icon = icon("food"), color  = "aqua")})
    
    output$m3_d4 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 3)), 2]),
            h6("Geschätzter Bedarf", "|",br(), " ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 3)])), 
            icon = icon("food"), color  = "aqua")})
    
    output$m3_d5 <- renderInfoBox({
        valueBox(
            h4(time_series[which(time_series$Date== as.character(as.Date(input$Datum, format="%Y-%m-%d") + 4)), 2]),
            h6("Geschätzter Bedarf", "|",br(), " ","Bedarf Vergleichsperiode", br(), h4(predicted_values[which(rownames(predicted_values)== input$ChooseMenu_M_3),which(colnames(predicted_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)]," ", "|"," ",actual_values[which(rownames(actual_values)== input$ChooseMenu_M_3),which(colnames(actual_values)== as.Date(input$Datum, format="%Y-%m-%d") + 4)])), 
            icon = icon("food"), color  = "aqua")})
    
  
#Icons for day---------------------------------------------------------------------------------

    output$occupation <- renderImage({
       
      img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "occupation")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 85,
             height = 55)
    }, deleteFile=FALSE)
    
    output$season <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "season")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 55,
             height = 55)
    }, deleteFile=FALSE)
    
    output$weather <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "weather")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 55,
             height = 55)
    }, deleteFile=FALSE)
    
    output$terrace <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "terrace")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 55,
             height = 55)
    }, deleteFile=FALSE)
    
    output$event <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "event")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 55,
             height = 55)
    }, deleteFile=FALSE)

#Icons for week---------------------------------------------------------------------------------
    #Icons for week_d1---------------------------------------------------------------------------------   
    output$occupation_week_d1 <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "occupation")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 25,
             height = 15)
    }, deleteFile=FALSE)
    
    output$season_week_d1 <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "season")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 15,
             height = 15)
    }, deleteFile=FALSE)
    
    output$weather_week_d1 <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "weather")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 15,
             height = 15)
    }, deleteFile=FALSE)
    
    output$terrace_week_d1 <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "terrace")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 15,
             height = 15)
    }, deleteFile=FALSE)
    
    output$event_week_d1 <- renderImage({
        img <- time_series[which((time_series$Date)==as.character(input$Datum)),which(colnames(time_series)== "event")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 15,
             height = 15)
    }, deleteFile=FALSE)

    #Icons for week_d2---------------------------------------------------------------------------------   
    output$occupation_week_d2 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+1)), which(colnames(time_series)== "occupation")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 25,
           height = 15)
    }, deleteFile=FALSE)
    
    output$season_week_d2 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+1)),which(colnames(time_series)== "season")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$weather_week_d2 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+1)),which(colnames(time_series)== "weather")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$terrace_week_d2 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+1)),which(colnames(time_series)== "terrace")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$event_week_d2 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+1)),which(colnames(time_series)== "event")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    #Icons for week_d3---------------------------------------------------------------------------------   
    output$occupation_week_d3 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+2)), which(colnames(time_series)== "occupation")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 25,
           height = 15)
    }, deleteFile=FALSE)
    
    output$season_week_d3 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+2)),which(colnames(time_series)== "season")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$weather_week_d3 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+2)),which(colnames(time_series)== "weather")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$terrace_week_d3 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+2)),which(colnames(time_series)== "terrace")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$event_week_d3 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+2)),which(colnames(time_series)== "event")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    #Icons for week_d4---------------------------------------------------------------------------------   
    output$occupation_week_d4 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+3)), which(colnames(time_series)== "occupation")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 25,
           height = 15)
    }, deleteFile=FALSE)
    
    output$season_week_d4 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+3)),which(colnames(time_series)== "season")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$weather_week_d4 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+3)),which(colnames(time_series)== "weather")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$terrace_week_d4 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+3)),which(colnames(time_series)== "terrace")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$event_week_d4 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+3)),which(colnames(time_series)== "event")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    #Icons for week_d5---------------------------------------------------------------------------------   
    output$occupation_week_d5 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+4)), which(colnames(time_series)== "occupation")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 25,
           height = 15)
    }, deleteFile=FALSE)
    
    output$season_week_d5 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+4)),which(colnames(time_series)== "season")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$weather_week_d5 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+4)),which(colnames(time_series)== "weather")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$terrace_week_d5 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+4)),which(colnames(time_series)== "terrace")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    output$event_week_d5 <- renderImage({
      img <- time_series[which((time_series$Date)==as.character(as.Date(input$Datum, format="%Y-%m-%d")+4)),which(colnames(time_series)== "event")]
      png <- ".png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 15,
           height = 15)
    }, deleteFile=FALSE)
    
    
#Tabitem = Bedarfsverlauf---------------------------------------------------------------------------------   

    output$total_product <- renderImage({
      
      if(input$Include_pred_1 == "TRUE"){ 
        if(input$Pick_Zeitraum == "Woche") {
          img <- "Total_Mean_weekly_w.png"
          path <- c(www,img)
          path <- str_c(path, collapse = "")
          list(src= path,
               width = 520,
               height = 400)
        }
        else if ( input$Pick_Zeitraum == "Monat"){
          img <- "Total_Mean_monthly_w.png"
          path <- c(www,img)
          path <- str_c(path, collapse = "")
          list(src= path,
               width = 520,
               height = 400) 
        }
      }
      else if(input$Include_pred_1 == "FALSE"){
        if(input$Pick_Zeitraum == "Woche") {
          img <- "Total_Mean_weekly.png"
          path <- c(www,img)
          path <- str_c(path, collapse = "")
          list(src= path,
               width = 520,
               height = 400)
        }
        else if ( input$Pick_Zeitraum == "Monat"){
          img <- "Total_Mean_monthly.png"
          path <- c(www,img)
          path <- str_c(path, collapse = "")
          list(src= path,
               width = 520,
               height = 400) 
        }
        
      }
    }, deleteFile=FALSE)
    
    output$menu_mean_week <- renderImage({
      if(input$Include_pred_2 == "TRUE"){ 
        img <- Items[which((Items$Item)==as.character(input$Pick_Menu)),which(colnames(Items)== "Item")]
        png <- ".png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 450,
             height = 350)
      }
      else if (input$Include_pred_2 == "FALSE"){
        img <- Items[which((Items$Item)==as.character(input$Pick_Menu)),which(colnames(Items)== "Item")]
        png <- "_w_o.png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 450,
             height = 350) 
      }
        
    }, deleteFile=FALSE)  
    
    output$menu_mean_month <- renderImage({
      if(input$Include_pred_2 == "TRUE"){
      img <- Items[which((Items$Item)==as.character(input$Pick_Menu)),which(colnames(Items)== "Item")]
      png <- "_monthly.png"
      path <- c(www,img,png)
      path <- str_c(path, collapse = "")
      list(src= path,
           width = 450,
           height = 350)
      }
      else if (input$Include_pred_2 == "FALSE"){
        img <- Items[which((Items$Item)==as.character(input$Pick_Menu)),which(colnames(Items)== "Item")]
        png <- "_monthly_w_o.png"
        path <- c(www,img,png)
        path <- str_c(path, collapse = "")
        list(src= path,
             width = 450,
             height = 350) 
      }
      }, deleteFile=FALSE) 

#Tabitem = Erklärungen--------------------------------------------------------------------------------- 
    
    # Funktionsweise-Erklärung------------------------------------------------------------- 
    output$exp_func <- renderValueBox({
        valueBox(
            width = 12, p("Erklärungen"), color  = "green",
            
            if (input$expl_func == "function.png") { 
                fluidRow(
                    column(width = 12, solidHeader = TRUE,
                           h4("Funktionsweise"), 
                           p("Die vorliegende Applikation berechnet den Bedarf von jeweils drei Menüs für eine Systemgastronomie oder Restaurant basierend auf historischen Daten.
                             Im seitlichen Menü finden Sie die Bedarfsvorhersage für einen speziellen Tag, eine Wochenübersicht für eine ausgewählte Woche und den Bedarfsverlauf, der die Nachfrage nach den verschiedenen Menüs visualisiert.
                             Die Eingabemaske dient als Eingabefeld, in der Sie das zu betrachtende Datum und den Bedarf an Menü 1, Menü 2 und Menü 3 anpassen können.
                             Um eine gewisse Glaubwürdigkeit der Vorhersage herzustellen, finden Sie die entsprechenden Vorjahreswerte neben dem vorhergesagten Bedarf. 
                             Ausserdem sehen Sie welche Faktoren die Vorhersage beeinflussen. Diese werden im Detail weiter unten erklärt.")
                    ) ) 
            }
            
            
            else if ( input$expl_func == "prediction.png") {
                fluidRow(
                    column(width = 12, solidHeader = TRUE,
                           h4("Bedarfsvorhersage"), 
                           p("In der Bedarfsvorhersage, sehen Sie oben das ausgewählte Datum, den totalen Bedarf an Menüs und die Faktoren, die die Vorhersage beeinflussen.
                             für die jeweiligen Menüs sehen Sie die drei vorgeschlagenen Menüs für diesen Tag und die vorausgesagten Werte. 
                             Als Menü-Empfehlung wird immer das Menü angezeigt, das in der Vorhersage den höchsten Wert an Kunden generierte. Die alternativen Varianten sind die drei Optionen auf der rechten Seite.
                             Gefällt Ihnen ein Menüvorschlag nicht, können Sie ihn im entsprechenden Dropdown Menü anpassen.
                             Der angezeigte Wert der Vergleichsperiode setzt sich aus dem Bedarf in den relevanten Vorjahren bzw. relevanten Zeitabschnitten zusammen.")
                    ) ) 
                
            }
            
            else if ( input$expl_func == "calendar.png") {
                fluidRow(
                    column(width = 12, solidHeader = TRUE,
                           h4("Wochenübersicht"), 
                           p("In der Wochenübersicht, sehen Sie einen Überblick für die ausgewählte Woche mit den jeweiligen drei vorgeschlagenen Menüs. Oben sehen Sie den ausgewählten Zeitrahmen sowie die entsprechenden Wochentage. Weiterhin sind die Faktoren ersichtlich, die die Vorhersage beeinflussen.
                             Gefällt Ihnen ein Menüvorschlag nicht, können Sie ihn im entsprechenden Dropdown Menü anpassen.")
                    ) ) 
                
            }
            
            else if ( input$expl_func == "demand.png") {
                fluidRow(
                    column(width = 12, solidHeader = TRUE,
                           h4("Bedarfsverlauf"), 
                           p("Im Bedarfsverlauf können Sie sich ein Bild vom Bedarf nach einem individuellen Produkt im wöchentlichen und monatlichen Verlauf machen. Dabei sind die Durchschnittswerte über die jeweilige Verteilung angezeigt.
                           Weiterhin ist der totale Bedarf an Menüs im Wochenschnitt und die Top10 Menüs aufgeführt. Über die Schaltflächen können Sie anpassen, welches Menü in welchem Zeitraum angezeigt werden soll.")
                    ) ) 
                
            }
            
            )
        
    })
    
    
    
    # Icon-Erklärung-------------------------------------------------------------   
    output$exp_icons <- renderValueBox({
        valueBox(
            width = 12, p("Erklärungen"), color  = "green",
        
            if (input$expl_icons == "high.png") { 
                     fluidRow(
                         column(width = 1, solidHeader = TRUE,
                                h4("Icons"),
                                img(src = "low.png", width = 35),
                                img(src = "middle.png", width = 35),
                                img(src = "high.png", width = 35)),
                         column(width = 4, solidHeader = TRUE,
                                h4("Bedeutung"), 
                                p("Gästeaufkommen: ", br(), 
                                  "Tief | Mittel | Hoch")
                         ),
                         column(width = 7, solidHeader = TRUE,
                                h4("Interpretation"),
                                p("Der Wert für das Gästeaufkommen basiert auf der Anzahl Transaktionen in der Vorperiode. 
                                  So werden z.B. als `Hoch` alle Tage eingestuft, die über dem durchschnittlichem Gästeaufkommen liegen.",br(),br(),
                                  "Tief: Vorhersagewert kleiner als Vorjahreswert.", br(), 
                                  "Mittel: Vorhersagewert kleiner als Vorjahreswert.", br(),
                                  "Hoch: Vorhersagewert grösser als Vorjahreswert.")) ) 
        }
        
        else if ( input$expl_icons == "warm.png") {
            
                fluidRow(
                    column(width = 1, solidHeader = TRUE,
                           h4("Icons"),
                           img(src = "cold.png", width = 35),
                           img(src = "warm.png", width = 35)),
                    column(width = 4, solidHeader = TRUE,
                           h4("Bedeutung"), 
                           p("Saison: ", br(), 
                             "Kalt | Warm")
                    ),
                    column(width = 7, solidHeader = TRUE,
                           h4("Interpretation"),
                           p("Die genaue Einteilung der Saison hängt von der Art und dem Verhaltensmuster der Gäste des Gastronomiebetriebs ab und kann bei Bedarf angepasst werden.",br(),br(),
                             "Kalt: Tendenziell gleich viele Gäste.", br(), 
                             "Warm: Tendenziell weniger Gäste, da Konsumation auch wo anders stattfinden kann.")) ) 
            
        }
        
        else if ( input$expl_icons == "sunny.png") {
            
            fluidRow(
                column(width = 1, solidHeader = TRUE,
                       h4("Icons"),
                       img(src = "sunny.png", width = 35),
                       img(src = "rainy.png", width = 35),
                       img(src = "cloudy.png", width = 35)),
                column(width = 4, solidHeader = TRUE,
                       h4("Bedeutung"), 
                       p("Wetter: ", br(), 
                         "Sonnig | Regnerisch | Bewölkt")
                ),
                column(width = 7, solidHeader = TRUE,
                       h4("Interpretation"),
                       p("Spezifische Wetterbedingungen haben je nach Art des Gastronomiebetriebs individuelle Auswirkungen auf das Verhaltensmuster der Gäste und kann bei Bedarf angepasst werden.", br(), br(),
                         "Sonnig: Tendenziell weniger Gäste.", br(), 
                         "Regnerisch: Tendenziell mehr viele Gäste.", br(),
                         "Bewölkt: Tendenziell gleich viele Gäste.")) ) 
            
        }
        
        else if ( input$expl_icons == "event.png") {
            
            fluidRow(
                column(width = 1, solidHeader = TRUE,
                       h4("Icons"),
                       img(src = "event.png", width = 35),
                       img(src = "no_event.png", width = 35)),
                column(width = 4, solidHeader = TRUE,
                       h4("Bedeutung"), 
                       p("Spezielles Event steht an: ", br(), 
                         "Ja | Nein")
                ),
                column(width = 7, solidHeader = TRUE,
                       h4("Interpretation"),
                       p("Je nach Art des Gastronomiebetriebs variieren die Events und die daraus resultierenden Auswirkungen auf das Verhaltensmuster der Gäste. Diese können bei Bedarf angepasst werden.", br(), br(),
                         "Spezielle Events erhöhen den Bedarf an Menüs.")) ) 
            
        }
        
        else if ( input$expl_icons == "terrace.png") {
            
            fluidRow(
                column(width = 1, solidHeader = TRUE,
                       h4("Icons"),
                       img(src = "terrace.png", width = 35),
                       img(src = "no_terrace.png", width = 35)),
                column(width = 4, solidHeader = TRUE,
                       h4("Bedeutung"), 
                       p("Terasse geöffnet? ", br(), 
                         "Ja | Nein")
                ),
                column(width = 7, solidHeader = TRUE,
                       h4("Interpretation"),
                       p("Je nach Art des Gastronomiebetriebs variieren die Möglichkeiten einer Outdoor-Bewirtungsfläche. Diese können bei Bedarf angepasst werden.", br(), br(),
                         "Ist die Terasse geöffnet, erhöht sich der Bedarf an Menüs.")) ) 
            
        }
        )
        })
    
    
    
    
    
    })

#Source: https://stackoverflow.com/questions/46635054/selectizeinput-with-image-icon
#Tour Example: https://shiny.rstudio.com/gallery/hospital-data-antimicrobial.html
#Tour sources: https://rdrr.io/cran/shinyjs/man/shinyjs.html
#Tour sources: https://rdrr.io/cran/rintrojs/man/introjs.html
