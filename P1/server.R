#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
  
  
  output$EmCategTime <- renderPlot({
    
    
    DATA=filter(data, 
               data$Category==input$emission,
               data$Country %in% input$countries)
    
    lower_bound=0
    upper_bound=0
    
    if(nrow(DATA)>0){
      lower_bound=min(min(DATA$Index),0)
      upper_bound=max(max(DATA$Index),0)
    }
    
    
    
    ggplot(DATA)+
      geom_line(aes(x = Year, y = Index,
                    color=Country))+
      labs(x="Year", y="Index", 
           title="Selected Emission Index Over Time")+
      scale_y_continuous(expand = c(0,0), 
                         limits = c(lower_bound, upper_bound))+
      theme(legend.position="bottom")
    
  })
  
  output$QolCategTime <- renderPlot({
    
    
    DATA=filter(data, 
                data$Category==input$qol,
                data$Country %in% input$countries)
    
    lower_bound=0
    upper_bound=0
    
    if(nrow(DATA)>0){
      lower_bound=min(min(DATA$Index),0)
      upper_bound=max(max(DATA$Index),0)
    }
    
    
    
    ggplot(DATA)+
      geom_line(aes(x = Year, y = Index,
                    color=Country))+
      labs(x="Year", y="Index", 
           title="Selected QOL Index Over Time")+
      scale_y_continuous(expand = c(0,0), 
                         limits = c(lower_bound, upper_bound))+
      theme(legend.position="bottom")
    
  })
  
  output$BivarByYear <- renderPlot({
    
    year=input$year
    prep=inner_join(filter(data, Year==year,
                           Category==input$emission),
                    filter(data, Year==year,
                           Category==input$qol),
                    by="Country")
    
    prep=filter(prep, Country %in% input$countries)
    
    y_lower_bound=0
    y_upper_bound=0
    x_lower_bound=0
    x_upper_bound=0
    
    if(nrow(prep)>0){
      y_lower_bound=min(min(prep$Index.y),0)
      y_upper_bound=max(max(prep$Index.y),0)
      x_lower_bound=min(min(prep$Index.x),0)
      x_upper_bound=max(max(prep$Index.x),0)
    }
    
    earliestYear=max(min(filter(data, Category==input$emission)$Year),
                     min(filter(data, Category==input$qol)$Year))
    latestYear=min(max(filter(data, Category==input$emission)$Year),
                     max(filter(data, Category==input$qol)$Year))
    
    qolFilt=filter(data, Category==input$qol, Country %in% input$countries)
    emFilt=filter(data, Category==input$emission, Country %in% input$countries)
    
    y_lower_bound=min(qolFilt$Index)
    y_upper_bound=max(qolFilt$Index)
    x_lower_bound=min(emFilt$Index)
    x_upper_bound=max(emFilt$Index)
    
    p <- ggplot(prep,aes(x=Index.x, y=Index.y)) 
    
    p + geom_point(aes(color=Country),size=6,alpha=.5)+
      labs(x="Selected Emission Category Index",
           y="Selected QOL Category Index",
           title=paste("QOL Category Index by Emission\nCategory Index for year",year))+
      scale_y_continuous(limits=c(y_lower_bound, y_upper_bound))+
      scale_x_continuous(limits=c(x_lower_bound, x_upper_bound))+
      theme(legend.position="bottom")
    
  })
  
  output$BivarSizeByYear <- renderPlot({
    
    year=input$year
    prep=inner_join(filter(data, Year==year,
                           Category==input$emission),
                    filter(data, Year==year,
                           Category==input$qol),
                    by="Country")
    
    prep=filter(prep, Country %in% input$countries)
    
    qolFilt=filter(data, Category==input$qol, Country %in% input$countries)
    emFilt=filter(data, Category==input$emission, Country %in% input$countries)
    
    y_lower_bound=0
    y_upper_bound=0
    
    if(nrow(qolFilt)>0){
      y_lower_bound=min(qolFilt$Index)
      y_upper_bound=max(qolFilt$Index)
    }
    
    
    denom=1
    if(nrow(emFilt)>0){
      while(abs(max(emFilt$Index))/denom>16){
      denom = denom*1.25
      }
    }
    
    
    p <- ggplot(prep,aes(x=Year.x, y=Index.y)) 
    
    p + geom_point(aes(color=Country),size=(prep$Index.x/denom),alpha=.5)+
      labs(x="Year",
           y="Selected QOL Category Index",
           title="QOL Category Index by Emission\nCategory Index from 1990 to 2019")+
      scale_y_continuous(limits=c(y_lower_bound, y_upper_bound))+
      scale_x_continuous(limits=c(1990, 2019))+
      theme(legend.position="bottom")
    
  })
  
  output$plot <- renderPlot({
    plot(mtcars$wt, mtcars$mpg)
  }, res = 96)
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
})
