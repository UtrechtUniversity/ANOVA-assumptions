library(shiny)
library("shinydashboard")
# fix github link, so put stuff on github

ui <- dashboardPage(
  # Appearance I #####
  skin = "black",
  
  dashboardHeader(title = "Assumptions of ANOVA", titleWidth = 280),
  # Dashboard #####
  dashboardSidebar(width = 280,
    sidebarMenu(id = "sidebarmenu",
      # menuItem("", tabName = "home", icon = icon("home")),
      menuItem("Normality", tabName = "tab1"),
      menuItem("Outliers", tabName = "tab2"),
      menuItem("Homogeneity of variance", tabName = "tab3"),
      menuItem("Independence of observations", tabName = "tab4"),
      menuItem("Disclaimer", tabName = "Disclaimer"),
      HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br>"),
      div(" -  Shiny app by",
          a(href="https://www.uu.nl/staff/EAarts/0",
            target = "_blank",
            "Emmeke Aarts"),align="left", style = "font-size: 10pt"),
      
      div(" -  Base R code by",
          a(href="https://www.uu.nl/staff/EAarts/0",target="_blank",
            "Emmeke Aarts"),align="left", style = "font-size: 10pt"),
      
      div(" -  Base Layout by",
          a(href="https://www.uu.nl/medewerkers/KMLek/0",target="_blank",
            "Kimberley Lek"),align="left", style = "font-size: 10pt"),
      
      div(" -  Shiny source files:",
          a(href="https://github.com/EducationalShinyUU/ANOVA_assumptions_shiny",
            target="_blank","GitHub"),align="left", style = "font-size: 10pt"),
      
      HTML("<br><br><br>"),
      img(src = 'UU_logo.png', align = "left", style="width: 300px")
      ### you can easily add extra tabs by including an extra "menuItem("...", tabName = "")," before the disclaimer ###
      ### you can remove or add <br> statements in the HTML function (menuItem("Disclaimer")) to adjust the position of the UU logo (make sure it is approximately at the bottom of the screen when opened)
    )
  ),
  
  # Appearance II #####  
  dashboardBody(
    # CSS styles
    tags$style(HTML(".irs-bar {background: #EAC626}")),
    tags$style(HTML(".irs-bar {border-top: 1px solid black}")),
    tags$style(HTML(".irs-bar-edge {background: #EAC626}")),
    tags$style(HTML(".irs-bar-edge {border: 1px solid black}")),
    tags$style(HTML(".irs-single {background: #EAC626}")),
    tags$style(HTML(
      ".selectize-input {border-color: #EAC626}"
    )),
    tags$style(HTML(
      ".selectize-dropdown {border-color: #EAC626}"
    )),
    
    ### note that #EAC626 is the mustard yellow color used in the sidebar. ###
    ### If possible, you can use this color + different shades of grey (+ black & white) in your figures. ###
    
    tags$head(tags$style(
      HTML(
        '.skin-black .main-header .logo {
        background-color: #EAC626;
        }
        .skin-black .main-header .logo:hover {
        background-color: #EAC626;
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #EAC626;
        }
        
        /* navbar (rest of the header) */
        .skin-black .main-header .navbar {
        background-color: #EAC626;
        }
        
        /* toggle button when hovered  */
        .skin-black .main-header .navbar .sidebar-toggle:hover{
        background-color: #EAC626;
        }
        
        /* other links in the sidebarmenu when hovered */
        .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #EAC626;
        }
        /* other links in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu a{
        background-color: #EAC626;
        color: #000000;
        }
        
        /* active selected tab in the sidebarmenu */
        .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
        background-color: #000000;
        color: #FFFFFF;
        }
        
        .skin-black .main-sidebar {color: #000000; background-color: #EAC626;}
        
        '
      )
      )),
    
    # Tab layout #####
    tabItems(
      tabItem(tabName = "Disclaimer", 
              box(width = 12,
                  h5("Terms of Usage Utrecht Unversity Shiny Server", 
                     br(), br(), 
                     tags$ul(
                      tags$li("Purpose of the service “utrecht-university.shinyapps.io” is to provide a digital place for trying out, evaluating and/or comparing methods developed by researchers of Utrecht University for the scientific community worldwide. The app and its contents may not be preserved in such a way that it can be cited or can be referenced to. "), 
                      tags$li("The web application is provided ‘as is’ and ‘as available’ and is without any warranty. Your use of this web application is solely at your own risk."), 
                      tags$li("	You must ensure that you are lawfully entitled and have full authority to upload  data in the web application. The file data must not contain any  data which can raise issues relating to abuse, confidentiality, privacy,  data protection, licensing, and/or intellectual property. You shall not upload data with any confidential or proprietary information that you desire or are required to keep secret. "),
                      tags$li("By using this app you agree to be bound by the above terms.")
                      )
                  )
                )
        ),
      #tabItem(tabName = "home", box(
      #  width = 12,
      #  align = "center",
      #  h4("Welcome"),
      #  column(12, align = "left", h5("add app background info"))
      #)),

      
####################### NORMALITY ############

      ## Tab 1 Normality #####      
tabItem(
  tabName = "tab1",
  #### INPUT
  ################
  
  fluidRow(
    column(6, 
           h3("Normality specifications", style="color:brown"),
           selectInput(
             "skew", "Amount of skewness", choices = c("No skewness (= 0.0)", 
                                                       "Small skewness (= 1.0)", 
                                                       "Medium skewness (= 2.0)",
                                                       "Large skewness (= 4.0)")
           ),
           selectInput(
             "kurt", "Amount of kurtosis", choices = c("No kurtosis (= 0.0)", 
                                                       "Small positive kurtosis (= 1.0)",
                                                       "Medium positive kurtosis (= 2.0)",
                                                       "Large positive kurtosis (= 4.0)")
           )
    ),
    column(6, 
           h3("ANOVA specifications", style="color:brown"),
           selectInput(
             "EffSize1", "Effect size", choices = c("No effect", "Small", "Medium", "Large", "Manual")
           ),
           conditionalPanel(condition = "input.EffSize1 == 'Manual'",
                            wellPanel(
                              h5("Population means:"),
                              sliderInput("mu1NORM", label="Group 1", value=0, min=-5, max=5),
                              sliderInput("mu2NORM", label="Group 2", value=0, min=-5, max=5),
                              sliderInput("mu3NORM", label="Group 3", value=0, min=-5, max=5),
                              h5("Population standard deviation:"),
                              sliderInput("sigmaNORM", label="All groups", value=1, min=0.5, max=5, step = 0.5)
                            )                            
           ),
           sliderInput("groupsizeNORM", label="Number of observations per experimental group", value=20, min=5, max=100)
    )
  ),
  actionButton(inputId= "runNORM", label = "Sample"),
  hr(),
  
  ###################
  ###### OUTPUT
  tabsetPanel(
    tabPanel("Boxplot",
             plotOutput("boxplNORM")
    ),
    tabPanel("Density plot",
             plotOutput("normplNORM")
    ),
    tabPanel("Descriptives",
             br(),
             br(),
             fluidRow(
               column(6,
                      tableOutput("descriptives1NORM")
               )
             ),
             br(),
             br()
    ),
    tabPanel("ANOVA tables",
             br(),
             br(),
             fluidRow(
               column(6,
                      tableOutput("aovSummary1NORM")
               )
             ),
             br(),
             br()
    )
  ),
  fluidRow(
    column(6, 
           h3(textOutput("p_val1NORM"), style="color:brown")
    ),
    column(6, 
           h3( textOutput("p_val2NORM"), style="color:brown")
    )
  )
),
####################### OUTLIERS ############

# Tab 2 outliers #
tabItem(
  tabName = "tab2",
  fluidRow(
    column(6, 
           h3("Outlier specifications", style="color:brown"),
           selectInput(
             "TypeOutl", "Outlier?", choices = c("No Outlier", 
                                                 "Specify outlier")
           ),
           conditionalPanel(condition = "input.TypeOutl == 'Specify outlier'",
                            wellPanel(
                              sliderInput("outl.val", label="Outlier value", value=0, min=-5, max=15),
                              selectInput(
                                "group.outl", "Outlier in:", choices = c("Group 1", 
                                                                         "Group 2", "Group 3")
                              )
                            )
           )
    ),
    column(6, 
           h3("ANOVA specifications", style="color:brown"),
           selectInput(
             "EffSize2", "Effect size", choices = c("No effect", "Small", "Medium", "Large", "Manual")
           ),
           conditionalPanel(condition = "input.EffSize2 == 'Manual'",
                            wellPanel(
                              h5("Population means:"),
                              sliderInput("mu1OUTL", label="Group 1", value=0, min=-5, max=5),
                              sliderInput("mu2OUTL", label="Group 2", value=0, min=-5, max=5),
                              sliderInput("mu3OUTL", label="Group 3", value=0, min=-5, max=5),
                              h5("Population standard deviation:"),
                              sliderInput("sigmaOUTL", label="All groups", value=1, min=0.5, max=5, step = 0.5)
                            )                            
           ),
           sliderInput("groupsizeOUTL", label="Number of observations per experimental group", value=20, min=5, max=100)
    )
  ),
  actionButton(inputId= "runOUTL", label = "Sample"),
  hr(),
  
  ###################
  ###### OUTPUT
  tabsetPanel(
    tabPanel("Boxplot",
             plotOutput("boxplOUTL")
    ),
    tabPanel("Normal distributions",
             plotOutput("normplOUTL")
    ),
    tabPanel("Descriptives",
             br(),
             br(),
             fluidRow(
               column(6,
                      h4("With outliers"),
                      tableOutput("descriptives1OUTL")
               ),
               column(6,
                      h4("Without outliers"),
                      tableOutput("descriptives2OUTL")
               )
             ),
             br(),
             br()
    ),
    tabPanel("ANOVA tables",
             br(),
             br(),
             fluidRow(
               column(6,
                      h4("With outliers"),
                      tableOutput("aovSummary1OUTL")
               ),
               column(6,
                      h4("Without outliers"),
                      tableOutput("aovSummary2OUTL")
               )
             ),
             br(),
             br()
    )
  ),
  fluidRow(
    column(6, 
           h3(textOutput("p_val1OUTL"), style="color:brown")
    ),
    column(6, 
           h3( textOutput("p_val2OUTL"), style="color:brown")
    )
  ) 
  
),
####################### HOMOGENEITY OF VARIANCE ############


# Tab 3 Homogeniety of variance #       
tabItem(
  tabName = "tab3",
  fluidRow(
    column(6, 
           h3("Variance specifications", style="color:brown"),
           selectInput(
             "TypeVar", "Variance ratio", choices = c("1:1", 
                                                      "1:5", 
                                                      "1:10", 
                                                      "1:20")
           ), 
           br(),
           selectInput(
             "ssRat", "Sample size ratio", choices = c("1:1", 
                                                       "1:4", 
                                                       "1:8")
           ),
           br(),
           conditionalPanel(condition = "input.ssRat != '1:1'",
                            selectInput(
                              "varAloc", "Smallest variance belongs to", 
                              choices = c("Group with smallest sample size", 
                                          "Group with largest sample size")
                            )                            
           )
           
    ),
    column(6, 
           h3("ANOVA specifications", style="color:brown"),
           selectInput(
             "EffSize3", "Effect size", choices = c("No effect", "Small", "Medium", "Large", "Manual")
           ),
           conditionalPanel(condition = "input.EffSize3 == 'Manual'",
                            wellPanel(
                              h5("Population means:"),
                              sliderInput("mu1VAR", label="Group 1", value=0, min=-5, max=5),
                              sliderInput("mu2VAR", label="Group 2", value=0, min=-5, max=5),
                              sliderInput("mu3VAR", label="Group 3", value=0, min=-5, max=5),
                              h5("Average population standard deviation:"),
                              sliderInput("sigmaVAR", label="All groups", value=1, min=0.5, max=5, step = 0.5)
                            )                            
           ),
           sliderInput("groupsizeVAR", label="Average number of observations per experimental group", value=20, min=5, max=100)
    )
  ),
  actionButton(inputId= "runVAR", label = "Sample"),
  hr(),
  
  ###################
  ###### OUTPUT
  tabsetPanel(
    tabPanel("Boxplot",
             plotOutput("boxplVAR")
    ),
    tabPanel("Normal distributions",
             plotOutput("normplVAR")
    ),
    tabPanel("Descriptives",
             br(),
             br(),
             fluidRow(
               column(6,
                      tableOutput("descriptives1VAR")
               ),
               column(6,
                      h3("Variance ratio in the sample:"),
                      textOutput("true.rat")
               )      
             ),
             br(),
             br()
    ),
    tabPanel("ANOVA tables",
             br(),
             br(),
             fluidRow(
               column(6,
                      tableOutput("aovSummary1VAR")
               )
             ),
             br(),
             br()
    )
  ),
  fluidRow(
    column(6, 
           h3(textOutput("p_val1VAR"), style="color:brown")
    ),
    column(6, 
           h3( textOutput("p_val2VAR"), style="color:brown")
    )
  )
  
),


####################### iNDEPENDENCE ############

tabItem(
  tabName = "tab4",
  fluidRow(
    column(6, 
           h3("Dependency specifications", style="color:brown"),
           selectInput(
             "TypeDep", "Dependency structure", choices = c("None (ICC = .00)", "Classmates (ICC = .10)", "Siblings (ICC = .30)", "Twins (ICC = .50)", "Manual")
           ),
           p(
             "The ICC denotes the amount that observations within a cluster (e.g., siblings, or students within the 
             same class) are alike. The value 0 implies that observations within a cluster are not alike at all, 
             while the value 1 implies that observations from the same cluster are identical to eachother"
    ),
    conditionalPanel(condition = "input.TypeDep == 'Manual'",
                     wellPanel(
                       sliderInput(
                         "ICC", label="ICC (Amount of dependency)", value=0.1, min=0, max=0.99, step = 0.1
                       ),
                       sliderInput(
                         "Csize", label = "Number of people within a cluster", value=10, min=2, max=100, step = 1
                       )
                     )                            
    )        
           ),
    column(6, 
           h3("ANOVA specifications", style="color:brown"),
           selectInput(
             "EffSize4", "Effect size", choices = c("No effect", "Small", "Medium", "Large", "Manual")
           ),
           conditionalPanel(condition = "input.EffSize4 == 'Manual'",
                            wellPanel(
                              h5("Population means:"),
                              sliderInput("mu1IND", label="Group 1", value=5, min=0, max=10),
                              sliderInput("mu2IND", label="Group 2", value=5, min=0, max=10),
                              h5("Population standard deviation:"),
                              sliderInput("sigmaIND", label="All groups", value=1, min=0.5, max=5, step = 0.5)
                            )                            
           ),
           sliderInput("Nclusters", label="Number of clusters (i.e., twins, classes) per experimental group", value=10, min=5, max=50)
    )
  ),
  actionButton(inputId= "runIND", label = "Sample"),
  hr(),
  
  ###################
  ###### OUTPUT
  tabsetPanel(
    tabPanel("Dependency structure",
             plotOutput("scatterIND")
    ),
    tabPanel("Boxplot",
             plotOutput("boxplIND")
    ), 
    tabPanel("Normal distributions", 
             plotOutput("normplIND")),
    tabPanel("Descriptives",
             br(),
             br(),
             fluidRow(
               column(6,
                      tableOutput("descriptives1IND")
               ),
               column(6,
                      h3(paste("ICC in the sample:"), textOutput("true.icc"))
               )      
             ),
             br(),
             br()
    ),
    tabPanel("ANOVA tables",
             br(),
             br(),
             fluidRow(
               column(6,
                      h4("not accounting for dependency"),
                      tableOutput("aovSummary1IND")
               )
             ),
             br(),
             br()
    )
  ),
  fluidRow(
    column(6, 
           h3(textOutput("p_val1IND"), style="color:brown"),
           h4("(Not accounting for dependency)", style="color:brown")
    ),
    column(6, 
           h3( textOutput("p_val2IND"), style="color:brown"),
           h4("(Accounting for dependency)", style="color:brown")
    )
  )
  
  )






        

  )
)
)



