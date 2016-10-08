#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(splines2)
library(MASS)

# Define server logic 
shinyServer(function(input,output){
    
    output$Data<-renderTable({
        x<-as.numeric(USArrests[,as.numeric(input$var)]);
        Urban_Population<-as.numeric(USArrests$UrbanPop);
        if(as.numeric(input$var) == 1) 
        {Murder <- x; data.frame(Murder,Urban_Population)}
        else if(as.numeric(input$var) == 2) 
        {Assault <- x; data.frame(Assault,Urban_Population)}
        else if (as.numeric(input$var) == 4) 
        {Rape <- x; data.frame(Rape,Urban_Population)}
                            })
    
    output$Map<- renderPlot({
        us <- map_data("state")
        if(as.numeric(input$var) == 1)
        {Crimedata <- data.frame(state = tolower(rownames(USArrests)), USArrests)
        ggplot(Crimedata, aes(map_id = state, fill=Murder)) +
            geom_map(map = us, colour="black") +     
            scale_fill_gradientn(colours=c("cyan","thistle2","hotpink","red")) +
            expand_limits(x = us$long, y = us$lat) +
            coord_map("bonne", lat0 = 50) +                               
            theme(
                panel.border = element_rect(fill = NA, colour = "black"), 
                panel.grid.major = element_line(colour = "white", size = 1), 
                panel.grid.minor = element_line(colour = "white", size = 1), 
                axis.line = element_blank(),
                axis.ticks = element_blank(), 
                axis.text.y = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position = "bottom",
                legend.key.size = unit(1.25, "cm"),
                legend.title = element_text(size = rel(1), face = "bold", hjust = 0, colour = "black"),
                legend.text = element_text(size = 10, colour = "black", angle = 45),
                panel.margin =       unit(2, "lines"),
                panel.background = element_rect(fill = 'gray', colour = 'gray')
            )
        }
        else if (as.numeric(input$var) == 2)
        {Crimedata <- data.frame(state = tolower(rownames(USArrests)), USArrests)
        ggplot(Crimedata, aes(map_id = state, fill=Assault)) +
            geom_map(map = us, colour="black") +     
            scale_fill_gradientn(colours=c("cyan","thistle2","hotpink","red")) + 
            expand_limits(x = us$long, y = us$lat) +
            coord_map("bonne", lat0 = 50) +                               
            theme(
                panel.border = element_rect(fill = NA, colour = "black"), 
                panel.grid.major = element_line(colour = "white", size = 1), 
                panel.grid.minor = element_line(colour = "white", size = 1), 
                axis.line = element_blank(),
                axis.ticks = element_blank(), 
                axis.text.y = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position = "bottom",
                legend.key.size = unit(1.25, "cm"),
                legend.title = element_text(size = rel(1), face = "bold", hjust = 0, colour = "black"),
                legend.text = element_text(size = 10, colour = "black", angle = 45),
                panel.margin =       unit(2, "lines"),
                panel.background = element_rect(fill = 'gray', colour = 'gray')
            )
        }
        else if (as.numeric(input$var) == 4)
        {Crimedata <- data.frame(state = tolower(rownames(USArrests)), USArrests)
        ggplot(Crimedata, aes(map_id = state, fill=Rape)) +
            geom_map(map = us, colour="black") +     
            scale_fill_gradientn(colours=c("cyan","thistle2","hotpink","red")) +
            expand_limits(x = us$long, y = us$lat) +
            coord_map("bonne", lat0 = 50) +                               
            theme(
                panel.border = element_rect(fill = NA, colour = "black"), 
                panel.grid.major = element_line(colour = "white", size = 1), 
                panel.grid.minor = element_line(colour = "white", size = 1), 
                axis.line = element_blank(),
                axis.ticks = element_blank(), 
                axis.text.y = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position = "bottom",
                legend.key.size = unit(1.25, "cm"),
                legend.title = element_text(size = rel(1), face = "bold", hjust = 0, colour = "black"),
                legend.text = element_text(size = 10, colour = "black", angle = 45),
                panel.margin =       unit(2, "lines"),
                panel.background = element_rect(fill = 'gray', colour = 'gray')
            )
        }
    })
    
        output$Crimeplot<- renderPlot({
        x<-as.numeric(USArrests[,as.numeric(input$var)])
        y<-as.numeric(USArrests$UrbanPop)
        if(as.numeric(input$var) == 1)
        {c1 <- ggplot(USArrests, aes(Murder,UrbanPop)) + stat_smooth(method = "lm", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region 
        c2 <- ggplot(USArrests, aes(Murder,UrbanPop)) + stat_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region
        c3 <- ggplot(USArrests, aes(Murder,UrbanPop)) + stat_smooth(method = "rlm", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region
        c4 <- ggplot(USArrests, aes(Murder,UrbanPop)) + stat_smooth(method = "loess", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a loess smoothed fit curve with confidence region
        plot_grid(c1, c2, c3, c4, labels=c("Linear regression", "Cubic spline regression", "Robust regression", "Locally weighted linear regression"), ncol = 2, nrow = 2, rel_widths = c(1, 1, 1, 1), rel_heights = c(1, 1, 1, 1))
        }
        else if (as.numeric(input$var) == 2)
        {c1 <- ggplot(USArrests, aes(Assault,UrbanPop)) + stat_smooth(method = "lm", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region 
        c2 <- ggplot(USArrests, aes(Assault,UrbanPop)) + stat_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region
        c3 <- ggplot(USArrests, aes(Assault,UrbanPop)) + stat_smooth(method = "rlm", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region
        c4 <- ggplot(USArrests, aes(Assault,UrbanPop)) + stat_smooth(method = "loess", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a loess smoothed fit curve with confidence region
        plot_grid(c1, c2, c3, c4, labels=c("Linear regression", "Cubic spline regression", "Robust regression", "Locally weighted linear regression"), ncol = 2, nrow = 2, rel_widths = c(1, 1, 1, 1), rel_heights = c(1, 1, 1, 1))
        }
        else if (as.numeric(input$var) == 4)
        {c1 <- ggplot(USArrests, aes(Rape,UrbanPop)) + stat_smooth(method = "lm", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region 
        c2 <- ggplot(USArrests, aes(Rape,UrbanPop)) + stat_smooth(method = "lm", formula = y ~ splines::bs(x, 3), fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region
        c3 <- ggplot(USArrests, aes(Rape,UrbanPop)) + stat_smooth(method = "rlm", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a linear regression line with confidence region
        c4 <- ggplot(USArrests, aes(Rape,UrbanPop)) + stat_smooth(method = "loess", fill = "gray", size = 1.5, alpha = 1) + geom_point() # Adds a loess smoothed fit curve with confidence region
        plot_grid(c1, c2, c3, c4, labels=c("Linear regression", "Cubic spline regression", "Robust regression", "Locally weighted linear regression"), ncol = 2, nrow = 2, rel_widths = c(1, 1, 1, 1), rel_heights = c(1, 1, 1, 1))
        }
    })
        
        output$Diagnostics<- renderPlot({
            x<-as.numeric(USArrests[,as.numeric(input$var)])
            y<-as.numeric(USArrests$UrbanPop)
            par(mfrow = c(2,3))
            par(cex = 0.8)
            lm.out = lm(y ~ x, data=USArrests)
            plot(lm.out,col="blue")
            plot(cooks.distance(lm.out),col="blue")
    })
        
            output$Documentation <-renderText({ 
            paste("*This is a Shiny App based on USArrests data. \n
*This Shiny App has 2 parts, a drop-down box which is on the left panel and a main panel on the right.  \n
*The drop-down box facilitates the user to choose three different crime rates in the USA. \n 
*Based on the selection in the drop-down menu, it is possible to see the associated data, map, regression plot and linear regression diagnostics on the tabs in the main panel on the right hand side. \n
*The code associated with this app and the presentation slides can be found in ui.R, server.R and Rpresentation.Rpres in the following repository: https://github.com/sadiqiqbal/ShinyAppProject.git.              ")})
})