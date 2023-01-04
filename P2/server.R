#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    output$harvestBiVar <- renderPlot({
      data <- filter(coffeeFilt, Harvest.Year==input$YEAR)
      
      p <- ggplot(data,aes(x=altitude_mean_meters, y=Total.Cup.Points)) 
      
      p + geom_point(aes(color=Country.of.Origin),size=3,alpha=.75)+
        labs(x="Mean Harvest Altitude (m)",
             y="Score",
             title="Coffee Score by Mean Harvest Altitude\nfor Each Country at Different Years")+
        scale_x_continuous(limits=c(0, 4500))+
        scale_y_continuous(limits=c(60, 100))+
        theme(legend.position="bottom",
              plot.title=element_text(hjust=0.5))
    
    })
    
    output$badYearHist <- renderPlot({
      ggplot(coffeeFilt6)+
        geom_bar(aes(x=Harvest.Year, y=..count..),fill="#bdab98")+
        coord_flip()+
        labs(title="Unfiltered")+
        theme(plot.title=element_text(hjust=0.5))
      
    })
    output$goodYearHist <- renderPlot({
      n=length(unique(coffeeFilt5$Harvest.Year))
      ggplot(coffeeFilt5)+
        geom_bar(aes(x=Harvest.Year, y=..count..),fill="#bdab98")+
        scale_x_continuous(breaks = seq(2010,2018))+
        coord_flip()+
        labs(title="Filtered")+
        theme(plot.title=element_text(hjust=0.5))
    })
    output$badProcPie <- renderPlot({
      ggplot(coffeeFilt5)+
        geom_bar(aes(x="",y=..count..,fill=Processing.Method))+
        scale_fill_manual(values = c("#0a0705","#4c3525","#f5efeb",
                                     "#bc9579","#ddc8ba","#8d6346")) +
        coord_polar(theta = "y") +
        labs(title="Unfiltered")+
        theme_dark()+
        theme(plot.title=element_text(hjust=0.5),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position="bottom",
              legend.title = element_blank())+
        labs(x="", y="")
        
      
    })
    output$goodProcPie <- renderPlot({
      ggplot(coffeeFilt4)+
        geom_bar(aes(x="",y=..count..,fill=Processing.Method))+
        scale_fill_manual(values = c("#4c3525","#bc9579","#ddc8ba","#8d6346")) +
        coord_polar(theta = "y") +
        labs(title="Filtered")+
        theme_dark()+
        theme(plot.title=element_text(hjust=0.5),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position="bottom",
              legend.title = element_blank())+
        labs(x="", y="")
    })
    output$badAltBox <- renderPlot({
      ggplot(coffeeFilt4)+
        geom_boxplot(aes(y=altitude_mean_meters),fill="#967969")+
        labs(title="Unfiltered")+
        theme_minimal()+
        theme(plot.title=element_text(hjust=0.5),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
        
    })
    output$goodAltBox <- renderPlot({
      ggplot(coffeeFilt3)+
        geom_boxplot(aes(y=altitude_mean_meters),fill="#967969")+
        labs(title="Filtered")+
        theme_minimal()+
        theme(plot.title=element_text(hjust=0.5),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    })
    
    output$CategScores <- renderPlot({
        year <- input$year
        alt <- input$alt
        proc <- input$proc
        spec <- input$spec
        
        categs <- names(coffeeFilt[7:16])
        scores <-  c(min(aromaCalc(year,alt,proc),10),
                     min(flavorCalc(year,alt,proc),10),
                     min(aftertasteCalc(year,alt,proc),10),
                     min(acidityCalc(year,alt,proc),10),
                     min(bodyCalc(year,alt,proc),10),
                     min(balanceCalc(year,alt,proc),10),
                     min(uniformityCalc(year,alt,proc),10),
                     min(cleanCupCalc(year,alt,proc),10),
                     min(sweetnessCalc(year,alt,proc,spec),10),
                     min(cupperPointsCalc(year,alt,proc),10))
        data <- data.frame(categs, scores)
        
        g <- ggplot(data=data, aes(x=categs, y=scores, fill=categs)) +
          geom_bar(stat="identity")+
          scale_fill_manual(values=c("#0a0705","#2b1e15",
                                     "#4c3525", "#6d4c36", "#8d6346",
                                     "#ac7b58","#bc9579", "#ccae9a", 
                                     "#ddc8ba", "#ede2db"))+
          theme_classic()+
          theme(axis.text.x = element_text(vjust = 0.5, 
                                           hjust=0.5, size=20),
                axis.text.y = element_text(vjust = 0.5, 
                                           hjust=0, size=16),
                legend.position = "none")+
          labs(x="")
        
        g+coord_flip(ylim=c(6,10))

    })
    
    output$TotScores <- renderPlot({
      year <- input$year
      alt <- input$alt
      proc <- input$proc
      spec <- input$spec
      
      scores <-  sum(min(aromaCalc(year,alt,proc),10),
                     min(flavorCalc(year,alt,proc),10),
                     min(aftertasteCalc(year,alt,proc),10),
                     min(acidityCalc(year,alt,proc),10),
                     min(bodyCalc(year,alt,proc),10),
                     min(balanceCalc(year,alt,proc),10),
                     min(uniformityCalc(year,alt,proc),10),
                     min(cleanCupCalc(year,alt,proc),10),
                     min(sweetnessCalc(year,alt,proc,spec),10),
                     min(cupperPointsCalc(year,alt,proc),10))
      data <- data.frame(scores)
      
      s=scores
      t=seq(0,25,0.1)
      y=.5*sin(t)+s
      
      qplot(t,y,geom="path")+
        coord_cartesian(ylim=c(70,90))+
        geom_area(fill="#3B1C0A")+
        theme_void()+
        labs(y="", x="", title=paste("Grade: ",substr(s, 1,5),"%"))+
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.title = element_text(hjust = 0.5, size=42))+
        theme(legend.position = "none")
      
    })
})
