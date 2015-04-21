# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



gg2fig <- function(gg) {
  gg.fig <- gg2list(gg)
  gg.fig.layout <- gg.fig$kwargs$layout
  gg.fig.data <- gg.fig$""
  list(
    data=list(gg.fig.data),
    layout=gg.fig.layout
  )
}


shinyServer(function(input, output) {
  
  
  
  observe({
    set.seed(1)
    knn.pred <- knn(data.frame(train.X[,input$checkGroup]),
                    data.frame(test.X[,input$checkGroup]),
                    train.Y, k = input$k)
    
    
    output$value <- renderText({ paste("Classification Error = ",ce(test.Y,knn.pred)) })
    output$confusionMatrix <- renderDataTable({
      # modify this to show title - confusion matrix
      # /false positive/positive false negative/negative
      true.positive    <- sum(knn.pred == "positive" & test.Y == "positive")
      false.positive   <- sum(knn.pred == "negative" & test.Y == "positive")
      true.negative    <- sum(knn.pred == "negative" & test.Y == "negative")
      false.negative   <- sum(knn.pred == "positive" & test.Y == "negative")
      row.names <- c("Prediction - FALSE", "Prediction - TRUE" )
      col.names <- c("Reference - FALSE", "Reference - TRUE")
      cbind(Outcome = row.names, as.data.frame(matrix( 
        c(true.negative, false.negative, false.positive, true.positive) ,
        nrow = 2, ncol = 2, dimnames = list(row.names, col.names))))
    }, options = table.settings
    )
    
  })
  
  observe({
    input_feature_x <- as.symbol(input$featureDisplay_x)
    input_feature_y <- as.symbol(input$featureDisplay_y)
    
    output$distPlotA <- renderGraph({
      # plot distribution of selected feature
      ggdistPlotA <- ggplot(ds, aes_string(x = input$featureDisplay_x, 
                                             fill = "factor(num)")) +
        geom_histogram(position = "dodge")  +
        labs(x = input$featureDisplay_x,
             y = "Count") + fte_theme() +
        scale_fill_manual(guide = F,values=c("#7A99AC", "#E4002B")) 
      
      # convert plot details to list
      # for plotly
      fig <- gg2list(ggdistPlotA)
      data <- list()
      for(i in 1:(length(fig)-1)){data[[i]]<-fig[[i]]}
      layout <- fig$kwargs$layout
      
      layout$annotations <- NULL # Remove the existing annotations (the legend label)

      # hide legend
      layout$showlegend = FALSE
      list(
        list(
          id="distPlotA",
          task="newPlot",
          data=data,
          layout=layout
        )
      )
    })
    

    
    output$distPlotB <- renderGraph({
      ggdistPlotB <- ggplot(ds, aes_string(input$featureDisplay_y, 
                                             fill = "factor(num)")) + 
        geom_histogram(position = "dodge") +
        labs(x = input$featureDisplay_y,
             y = "Count") + fte_theme() +
        scale_fill_manual(guide = F,values=c("#7A99AC", "#E4002B")) 
      
      
      # convert plot details to list
      # for plotly
      fig <- gg2list(ggdistPlotB)
      data <- list()
      for(i in 1:(length(fig)-1)){data[[i]]<-fig[[i]]}
      layout <- fig$kwargs$layout
      
      layout$annotations <- NULL # Remove the existing annotations (the legend label)
      # hide legend      
      layout$showlegend = FALSE
      
      list(
        list(
          id="distPlotB",
          task="newPlot",
          data=data,
          layout=layout
        )
      )
    })
    


  output$ScatterPlot <- renderGraph({
    # plot selected features against one another
      ggscatter <- ggplot(ds, aes_string(x = input$featureDisplay_x, 
                          y = input$featureDisplay_y, 
                          color = "factor(num)")) + 
      geom_point(size = 8, position = position_jitter(w = 0.1, h = 0.1)) + 
      labs(x = input$featureDisplay_x,
           y = input$featureDisplay_y) +
      fte_theme() + 
      scale_color_manual(name = "Heart Disease",values=c("#7A99AC", "#E4002B")) 
  
      # convert plot details to list
      # for plotly
      fig <- gg2list(ggscatter)
      data <- list()
      for(i in 1:(length(fig)-1)){data[[i]]<-fig[[i]]}
      layout <- fig$kwargs$layout
      layout$annotations <- NULL # Remove the existing annotations (the legend label)
      
     
      
     
      # place legend to the right of the plot
      layout$legend$x <- 100
      layout$legend$y <- 1
      
      list(
        list(
          id="ScatterPlot",
          task="newPlot",
          data=data,
          layout=layout
        )
      )
    })


    
  })
  
})