# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#




shinyUI(navbarPage("",
                   tabPanel("Classifier",
                            
                            # Application title
                            titlePanel("Classification of Heart Disease w/KNN"),
                            
                            # Sidebar with a slider input for number of bins
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("k",
                                            "number of neighbors",
                                            min = 1,
                                            max = 20,
                                            value = 5),
                                checkboxGroupInput("checkGroup", label = h3("Dataset Features"), 
                                                   choices = feature.list, inline = F,
                                                   selected = names(feature.list))
                                
                              ),
                              
                              # Display KNN results
                              mainPanel(
                                dataTableOutput('confusionMatrix'),
                                verbatimTextOutput("value"),
                                includeMarkdown("ShinyAppDescription.Rmd")
                                
                              )
                            )
                            
                   ), 
                   tabPanel("Visualize Features",
                            fluidRow(                           
                              column(4, selectInput("featureDisplay_x", 
                                                    label = h3("X-Axis Feature"), 
                                                    choices = feature.list,
                                                    selected = feature.list[1])),
                              column(4, selectInput("featureDisplay_y", 
                                                               label = h3("Y-Axis Feature"), 
                                                               choices = feature.list,
                                                               selected = feature.list[2]))
                              
                            ),
                            fluidRow(
                              column(4,
                                     graphOutput("distPlotA")
                                     ),                              
                              column(4,
                                     graphOutput("distPlotB")      
                              ),
                              column(4,
                                     graphOutput("ScatterPlot")
                              )
                            )
                                                  
                            
                   ),
                   
                   
                   tabPanel("Feature Descriptions",
                            fluidRow(
                              column(10,
                                     includeMarkdown("include.Rmd")
                              )
                            )
                            
                   ),
                   
                   tabPanel("References",
                            fluidRow(
                              column(10,
                                     includeMarkdown("references.Rmd")
                              )
                            )
                            
                   )
))