#######################################################################
### Joint Executive Committee Cartel 1880-1886                     ### 
### A review of the JEC cartel's ability to maintain cooperation  ###
####################################################################

### Contents #################
# 1. Setup
# 2. Load, Review, and Wrangle Data
# 3. Models
# 4. Visualizations
##########################


###################
### 1. Setup   ###
#################

rm(list=ls()) #clear workspace

#install.packages(c("foreign", "dplyr", "AER", "mfx", "ggplot2", "ggvis", "gridExtra", "stargazer")) 

#libraries
library(foreign) 
library(dplyr) 
library(AER)
library(mfx)
library(ggplot2) 
library(ggvis)  
library(gridExtra)
library(stargazer)
library(directlabels)

#################################
# 2. Load Data and Wrangle   ###
###############################

# Data source, Stock & Watson: http://wps.pearsoned.co.uk/ema_ge_stock_ie_3/193/49605/12699041.cw/content/index.html

JEC <- read.dta("JEC.dta") # read STATA .dta file into data frame
str(JEC) # quick review of the data
# factor columns
cols <- c("cartel", "seas1", "seas2", "seas3", "seas4", "seas5", "seas6", "seas7", "seas8", "seas9", "seas10", "seas11", "seas12", "ice") # select columns to factor
JEC[,cols] <- data.frame(apply(JEC[cols], 2, as.factor)) #factor
#provide names to factor levels
levels(JEC$ice)  <- c("Clear Shipping Lanes", "Ice")
levels(JEC$cartel) <- c("Competition", "Cartel") 
#review data
summary(JEC)
plot(JEC$quantity, JEC$price) # simple price and quantity graph
plot(JEC$week, JEC$price) # price by week
hist(JEC$price)
hist(JEC$quantity)

#Export to CSV file
write.csv(JEC, file="JEC.csv")


##################
# 3. Models   ###
################

## Demand Model
# ln(Q Grain Ton Shipped)= B0+ B1 ln(Price) + B2 Ice[0,1] + B3:15 Seasonal Variation in Demand [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] + error

#Simple OLS
Demand <- lm(log(quantity) ~ log(price) + cartel + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, JEC)
summary(Demand) #results
plot(Demand) #check residuals 

stargazer(Demand, type="html") #html regression output

#2SLS Model using cartel status as an instrument for price. 
Demand.2sls <- ivreg(log(quantity) ~  log(price) + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | cartel + week + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data= JEC) #note log(price)=cartel after | break. Cartel used as an instrument for the effect of supply on price.
summary(Demand.2sls, diagnostics=TRUE)
plot(Demand)

stargazer(Demand, Demand.2sls, 
          single.row = TRUE, 
          title="JEC Transport Demand Equations",
          covariate.labels = c("log(Price)", "Cartel", "Ice", "Season 1", "Season 2", "Season 3", "Season 4", "Season 5", "Season 6", "Season 7", "Season 8", "Season 9", "Season 10", "Season 11", "Season 12"),
          out="models.html"
) #modified regression table output

# Probit model, Cartel ~ Price Quantity Ice 
Model4.Probit <- glm(cartel ~ log(quantity) + log(price) + ice , data=JEC, family=binomial(link='probit'))
summary(Model4.Probit)
probitmfx(cartel ~ log(quantity) + log(price) + ice , data=JEC)

########################
# 4. Visualizations ###
######################

# Scatterplot demonstrating supply demand interaction
plot.dualCasuality <- ggplot(JEC, aes(x=quantity, y=price)) +
        geom_point() +
        theme(panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(size = 20),
              axis.title.y = element_text(angle = 0, vjust = 1), 
              axis.title.x = element_text(hjust = 1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.position ="right",
              axis.line = element_line(color = "light grey")) +
        xlab("Quantity") +
        ylab("Price") +
        ggtitle("JEC Grain Transport")
plot.dualCasuality #call plot

#Scatter plot with color breakdown by cartel status 
plot.dualCasuality.2 <- ggplot(JEC, aes(x = quantity, y = price, color = cartel, shape = cartel)) +
        geom_point(alpha = .5, size = 3, position = position_jitter(w = 0.0, h = 0.0005)) +
        geom_rug(sides = "lb", alpha = .3) +
        theme(panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(size = 20),
              axis.title.y = element_text(angle = 0, vjust = 1), 
              axis.title.x = element_text(hjust = 1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.position ="right", 
              axis.line = element_line(color = "light grey"),
              legend.key = element_blank(),
              legend.position = "top"
              ) +
        scale_shape_manual(values = c(15, 16)) +
        scale_color_manual(values = c("yellow", "black")) +
        xlab("Quantity") +
        ylab("Price") +
        ggtitle("JEC Grain Transport") + 
        guides(color = guide_legend(override.aes = list(linetype = 0))) 

plot.dualCasuality.2 #call plot


#Scatter plot with color breakdown by time 
plot.dualCasuality.3time <- ggplot(JEC, aes(x = quantity, y = price, color = week, shape = cartel)) +
        geom_point(alpha = .5, size = 3, position = position_jitter(w = 0.0, h = 0.0005)) +
        geom_rug(sides = "lb", alpha = .3) +
        theme(panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(size = 20),
              axis.title.y = element_text(angle = 0, vjust = 1), 
              axis.title.x = element_text(hjust = 1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.position ="right", 
              axis.line = element_line(color = "light grey"),
              legend.key = element_blank(),
              legend.position = "top"
        ) +
        scale_shape_manual(values = c(15, 16)) +
        scale_colour_gradientn(colours = heat.colors(6)) +
        xlab("Quantity") +
        ylab("Price") +
        ggtitle("JEC Grain Transport") + 
        guides(color = guide_legend(override.aes = list(linetype = 0, alpha = 1))) 

plot.dualCasuality.3time #call plot


# Price by Week line graph stacked on Quantity by Week bar graph
# Giants Orange: #FB5B1F
plot.cartel <- ggplot(JEC, aes(x = week, y = price)) +
        geom_line(aes(color = JEC$cartel, size = JEC$cartel, group = 1)) +
        geom_dl(aes(label = JEC$cartel, color = JEC$cartel), method="lines2") +
        xlab("") +
        ylab("Price") +
        scale_size_manual(values = c(1.5, 1),
                          name = "Industry Status",
                          breaks = c("False", "True"),
                          labels = c("Competition", "Cartel"),
                          guide = guide_legend(reverse=TRUE)
                          ) +
        scale_color_manual( values = c("#FB5B1F", "grey"), 
                            name = "Industry Status",
                            breaks = c("False", "True"),
                            labels = c("Competition", "Cartel"),
                            guide = guide_legend(reverse=TRUE)
                          ) +
        scale_y_continuous(breaks = seq(0, 0.5, by = 0.05)) +
        ggtitle(expression(atop("Difficulties Maintaining Cartel Cooperation", atop(italic("Pricing and Cartel Status for JEC 1880-1886, Midwest to Eastern Seaboard"), "")))) +
        theme(panel.border = element_blank(), 
              panel.background = element_blank(),
              plot.title = element_text(size = 20),
              axis.title.y = element_text(angle = 0, vjust = 1), 
              axis.title.x = element_text(hjust = 1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.position = "top")

#plot.cartel #review plot

#create quantity of shipping by week graph
plot.quantity <- ggplot(JEC, aes(x = week, y = quantity, fill = ice)) +
        geom_bar(stat = 'identity') +
        scale_fill_manual(values = c("blue", "grey"),
                          breaks = c("Clear Shipping Lanes", "Ice"),
                          labels = c("Clear", "Ice"),
                          guide = guide_legend(reverse=TRUE),
                          name = "Shipping Lanes Great Lakes"
        ) +
        xlab("Week") +
        ylab("Quantity \n Freight") +
        theme(panel.border = element_blank(), 
              panel.background = element_blank(), 
              axis.title.y = element_text(angle = 0, vjust = -.2), 
              axis.title.x = element_text(hjust = 1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.key=element_blank(),
              legend.key.size = unit(.25, "cm"),
              legend.position = "bottom") 

#plot.quantity #quick view of quantity plot

## arrange price and quantity plots
g1<-ggplotGrob(plot.cartel)
g2<-ggplotGrob(plot.quantity)
#Bind the tables
g<-gtable:::rbind_gtable(g1, g2, "first")
#Remove a row between the plots
g <- gtable_add_rows(g, unit(-1,"cm"), pos = nrow(g1))
#Adjust heights, 5 to 1 ratio top to bottom
panels <- g$layout$t[grep("panel", g$layout$name)]
g$heights[panels] <- lapply(c(5,1), unit, "null")
#Draw. Setup new page and draw gtable with graphs
grid.newpage()
grid.draw(g)


## Price by cartel using ggvis 
# Select data -> put into ggvis -> output scatterplot
JEC %>% 
        ggvis(~week, ~price) %>% 
                layer_points(fill = ~factor(cartel)) %>%
                layer_lines()
