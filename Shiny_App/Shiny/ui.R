library(readr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(stringr)
library(shinyjs)
library(rintrojs)

# UI ------------------------------------------------------------------

ui <- dashboardPage(
    skin = "green",
    title = "Sales",
    
    # Dashboard Header ------------------------------------------------------------------
    
    dashboardHeader(
        title = span(img(src = "prediction.png", height = 35), "Bedarfsorakel"),
        titleWidth = 325,
        
        dropdownMenu( messageItem(
            from = "Support",
            message = "Do you have any Feedback? Contact Philipp.schmelzer@student.unisg.ch",
            icon = icon("life-ring"),
            time = "today"
        ),
        icon = icon('comment')
        )
    ),
    # Dashboard Sidebar------------------------------------------------------------------------ 
    dashboardSidebar(
        width =250,
        introBox(data.step = 1, data.intro = intro$text[1], #  intro tour
                 div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
                 
                 introBox(data.step = 2, data.intro = intro$text[2],
                          introBox(data.step = 5, data.intro = intro$text[5],
                                   
        sidebarMenu(
            id = 'sidebar',
            style = "position: relative; overflow: visible;",
            menuItem("Bedarfsvorhersage", tabName = "Bedarfsvorhersage", icon = icon("splotch")),
            
            menuItem("Wochenübersicht", tabName = "Wochenübersicht", icon = icon("calendar-week")),
            
            menuItem("Bedarfsverlauf", tabName = "Bedarfsverlauf", icon = icon("chart-line")),
            
            menuItem("Erklärungen", tabName = "Erklärungen", icon = icon("question")),
            
            #Content of Eingabemaske
            menuItem("Eingabemaske", tabName = "Eingabemaske", icon = icon("keyboard"),
                     dateInput("Datum", "Datum", value = min(time_series$Date), min = min(time_series$Date), max = max(time_series$Date)),
                     numericInput("Anpassung_Menu1_day","Anpassung Bedarf Menu 1", value=0, min=0, max=1000, step = 1, width= "100%"),
                     numericInput("Anpassung_Menu2_day","Anpassung Bedarf Menu 2", value=0, min=0, max=1000, step = 1, width= "100%"),
                     numericInput("Anpassung_Menu3_day","Anpassung Bedarf Menu 3", value=0, min=0, max=1000, step = 1, width= "100%")

            ) #menuItem Eingabemaske
 
        ) # Sidebarmenu Parenthesis
   )) )), #dashboard Sidebar Parenthesis
    # Dashboard Body------------------------------------------------------------------------    
    dashboardBody(
      # Tour--------------------------------------------------------------------------------- 
      useShinyjs(),
      introjsUI(),
        
        # CSS styles------------------------------------------------------------------------
        tags$head(tags$style(HTML('.info-box {min-height: 135px;} .info-box-icon {height: 135px; line-height: 135px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
        #https://stackoverflow.com/questions/37861234/adjust-the-height-of-infobox-in-shiny-dashboard

        
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }'
        )
        )
        ), #https://rstudio.github.io/shinydashboard/appearance.html
        
        tags$head(tags$style(HTML('
  .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:500px;
  }'
        )
        )
        ),
        # Dashboard tabItems------------------------------------------------------------------------ 
        tabItems(
            #tabItem Bedarfsvorhersage------------------------------------------------------------------------ 
            tabItem(tabName = "Bedarfsvorhersage", 
                    h1("Bedarfsvorhersage"),
                        fluidRow(introBox(data.step = 3, data.intro = intro$text[3], #  intro tour
                                          width = 12,
                                 infoBoxOutput("dateBox", width = 3),
                                 infoBoxOutput("totalmealbox", width = 4),
                                 infoBoxOutput("day_icons", width = 5)),
                    
                        box(width = 12, 
                          title = "Menu 1", status = "primary",solidHeader = TRUE,  height = 250, br(),
                          introBox(data.step = 4, data.intro = intro$text[4], #  intro tour
                            box(width = 2, solidHeader = TRUE,
                                pickerInput(inputId = "ChooseMenu",
                                    label = h4("Menüauswahl"), selected = "Coffee",
                                    choices = Items$Item, width = 125)),
                            box(width = 5, solidHeader = TRUE,
                                infoBoxOutput("mealbox1", width = 12)),
                            box(width = 5, solidHeader = TRUE,
                                infoBoxOutput("menu1_alt", width = 12)) )),
                        
                        box(title = "Menu 2", status = "warning",solidHeader = TRUE, width = 12, height = 250, br(),
                            box(width = 2, solidHeader = TRUE,
                                pickerInput(inputId = "ChooseMenu2",
                                            label = h4("Menüauswahl"), selected = "Bread", 
                                            choices = Items$Item, width = 125)),
                            box(width = 5, solidHeader = TRUE,
                                infoBoxOutput("mealbox2", width = 12)),
                            box(width = 5, solidHeader = TRUE,
                                infoBoxOutput("menu2_alt", width = 12)) ),
                        
                        box(title = "Menu 3", status = "info",solidHeader = TRUE, width = 12, height = 250, br(),
                            box(width = 2, solidHeader = TRUE,
                                pickerInput(inputId = "ChooseMenu3",selected = "Tea",
                                            label = h4("Menüauswahl"), 
                                            choices = Items$Item, width = 125)),
                            box(width = 5, solidHeader = TRUE,
                                infoBoxOutput("mealbox3", width = 12)),
                            box(width = 5, solidHeader = TRUE,
                                infoBoxOutput("menu3_alt", width = 12)) )
                    
                        )#fluidrowparenthesis
                    
                    ),#tabItem parenthesis
                    
            #tabItem Wochenübersicht------------------------------------------------------------------------
            tabItem(tabName = "Wochenübersicht", 
                    h1("Wochenübersicht"),
                    fluidRow(width = 12,
                        infoBoxOutput("dateBox_week", width = 3),
                        infoBoxOutput("week_total_demand", width = 3),
                        infoBoxOutput("week_icons", width = 6)),

                    #Menü1------------------------------------------------------------------------
                    fluidRow(
                        box(title = "Menu 1", status = "primary",solidHeader = TRUE, width = 12, height = 300, br(),
                            box(width = 2, solidHeader = TRUE,
                                pickerInput(inputId = "ChooseMenu_M_1",
                                            label = h4("Menüauswahl"), selected = "Coffee",
                                            choices = Items$Item, width = 125)),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m1_d1", width = 12)),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m1_d2", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m1_d3", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m1_d4", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m1_d5", width = 12 ))  
                            ),
                        #Menü2------------------------------------------------------------------------
                        box(title = "Menu 2", status = "warning",solidHeader = TRUE, width = 12, height = 300, br(),
                            box(width = 2, solidHeader = TRUE,
                                pickerInput(inputId = "ChooseMenu_M_2",
                                            label = h4("Menüauswahl"), selected = "Bread",
                                            choices = Items$Item, width = 125)),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m2_d1", width = 12)),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m2_d2", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m2_d3", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m2_d4", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m2_d5", width = 12 ))  
                        ),
                        #Menü3------------------------------------------------------------------------
                        box(title = "Menu 3", status = "info",solidHeader = TRUE, width = 12, height = 300, br(),
                            box(width = 2, solidHeader = TRUE,
                                pickerInput(inputId = "ChooseMenu_M_3",
                                            label = h4("Menüauswahl"), selected = "Tea",
                                            choices = Items$Item, width = 125)),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m3_d1", width = 12)),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m3_d2", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m3_d3", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m3_d4", width = 12 )),
                            box(width = 2, solidHeader = TRUE,
                                infoBoxOutput("m3_d5", width = 12 ))  
                        )
                        )#fluidrow parenthesis
            ),#tabItem parenthesis
            
                    
            #tabItem Bedarfsverlauf------------------------------------------------------------------------
            tabItem(tabName = "Bedarfsverlauf", 
                    h1("Bedarfsverlauf"),h4("Auf dieser Seite finden Sie Informationen zum historischen Verlauf der verschiedenen Produkte."),
                    fluidRow(

                        box(title = "Kummulierte Menüs", status = "primary",solidHeader = TRUE, width = 12, height = 550,
                            box(width = 4, solidHeader = TRUE,
                                column(width=12,
                                pickerInput(inputId = "Pick_Zeitraum",
                                            label = h5("Wählen Sie den Zeitraum:"), selected = "",
                                            choices = c("Woche", "Monat"), width =  250),
                                br(),
                                p("Wollen Sie die Vorhersage in die Grafiken integrieren?"),
                                br(),
                                prettyToggle(inputId = "Include_pred_1",label_on = "Vorhersage integriert!", label_off = "Nicht integriert!",
                                  outline = TRUE, plain = TRUE,
                                  icon_on = icon("thumbs-up"), 
                                  icon_off = icon("thumbs-down")
                                ))),

                            box(width = 8, solidHeader = TRUE, h3("Bedarf Gesamte Menüs"),
                                imageOutput("total_product", width = 400))),
                        
                        box(title = "Individuelle Menüs", status = "primary",solidHeader = TRUE, width = 12, height = 500,
                            fluidRow(
                            box(width = 2, solidHeader = TRUE, 
                                pickerInput(inputId = "Pick_Menu",
                                    label = h5("Wählen Sie ein Menü:"), selected = "",
                                    choices = Items$Item, width = 125),
                                p("Wollen Sie die Vorhersage in die Grafiken integrieren?"),
                                prettyToggle(inputId = "Include_pred_2",label_on = "Vorhersage integriert!", label_off = "Nicht integriert!",
                                             outline = TRUE, plain = TRUE,
                                             icon_on = icon("thumbs-up"), 
                                             icon_off = icon("thumbs-down")
                                )
                                ),
                            
                            box(width = 5, solidHeader = TRUE,
                                h3("Wöchentlicher Durchschnitt"),
                                imageOutput("menu_mean_week",height = 350 )),
                            box(width = 5, solidHeader = TRUE,
                                h3("Monatlicher Durchschnitt"),
                                imageOutput("menu_mean_month",height = 350 ))
                            
                            )
                            
                        ),
                        box(title = "Top Produkte", status = "primary",solidHeader = TRUE, width = 12, height = 550,
                            box(width = 12, solidHeader = TRUE,
                                h3("Top 10 Produkte, ganzer Zeitraum"),
                                img(src = "Top10_Products.png", width = 600, height =400)),
                        )

                    )#fluidrowparenthesis
                    
            ), #tabItem parenthesis
            
            #tabItem Erklärungen------------------------------------------------------------------------
            tabItem(tabName = "Erklärungen",
                    h1("Erklärungen"), 
                    h4("Hier finden Sie Erklärungen zu den Funktionsweisen und anderen Elementen in der Anwendung."),
                    fluidRow(
                        box(title = "Erklärungen Funktionsweise", status = "success",solidHeader = TRUE, width = 12, height = 350,
                        fluidRow(
                            box(width = 3, solidHeader = TRUE, 
                                selectizeInput(
                                    "expl_func", "Auswahl Element:",
                                    choices = c("Funktionsweise" = "function.png", "Bedarfsvorhersage" = "prediction.png","Wochenübersicht" = "calendar.png", "Bedarfsverlauf" ="demand.png"),
                                    selected = "Funktionsweise",
                                    options = list(
                                        render = I(
                                            "{
                        option: function(item, escape) {
                        return '<div><img src=\"' + item.value + '\" width = 20 />' + escape(item.label) + '</div>'
                        }
                        }")))
                            ),
                            box(width = 9, solidHeader = TRUE,
                                valueBoxOutput("exp_func", width = 12))
                        )
                    ),
                    
                    
                    
                    box(title = "Erklärung Icons", status = "success",solidHeader = TRUE, width = 12, height = 350,
                        fluidRow(
                            box(width = 3, solidHeader = TRUE, 
                                selectizeInput(
                                           "expl_icons", "Auswahl Icon",
                                           choices = c("Gästeaufkommen" = "high.png", "Saison" ="warm.png",
                                                       "Event" = "event.png", "Terasse" ="terrace.png","Wetter" = "sunny.png"),
                                           selected = "Gästeaufkommen",
                                           options = list(
                                               render = I(
                                                   "{
                        option: function(item, escape) {
                        return '<div><img src=\"' + item.value + '\" width = 20 />' + escape(item.label) + '</div>'
                        }
                        }")))
                                ),
                            box(width = 9, solidHeader = TRUE,
                                valueBoxOutput("exp_icons", width = 12))
                    )
                    )
                    )
                    
                    
                    
                    
                    )#TabItem  Parenthesis

            )#TabItem Master Parenthesis
        ) #DashboardBody( Parenthesis
) #UI function Parenthesis





