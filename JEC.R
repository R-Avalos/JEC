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

#libraries ----
# install.packages(c("haven", "dplyr", "AER", "mfx", "ggplot2", "ggvis", "gridExtra", "stargazer", "devtools", "lubridate", "ggthemes", "ggExtra"))
#devtools::install_github("tdhock/animint", upgrade_dependencies=FALSE)
library(haven) 
library(dplyr)
library(lubridate)
library(AER)
#library(mfx)
library(ggplot2) 
library(ggthemes)
library(ggExtra)
library(ggvis)
library(animint)
#library(gridExtra)
#library(stargazer)
#library(directlabels)
# ----


#################################
# 2. Load Data and Wrangle   ###
###############################

# Data source, Stock & Watson: http://wps.pearsoned.co.uk/ema_ge_stock_ie_3/193/49605/12699041.cw/content/index.html

JEC <- read_dta("JEC.dta") # read STATA .dta file into data frame
head(JEC) # quick review of the data

# Format Data -----
JEC$cartel <- as.factor(JEC$cartel) # set as factor
JEC$cartel <- with(JEC, factor(cartel, levels = rev(levels(cartel))))
levels(JEC$cartel) <- c("Cartel", "Competition") # rename factor levels
JEC$ice <- as.factor(JEC$ice)
levels(JEC$ice)  <- c("Clear Shipping Lanes", "Ice")
Start_Date <- ymd("1880-1-1") # Set start date, Jan 1st, 1880
JEC$date <- Start_Date + weeks((JEC$week)-1) # Create date vector, 1 removed as first day is 1880-1-1

# Setup Trigger
JEC$Trigger <- ifelse(JEC$cartel=="Competition", TRUE, FALSE)    
summary(JEC$Trigger)




#Export to CSV file
# write.csv(JEC, file="JEC.csv")


##################
# 3. Models   ###
################

## Demand Model
# ln(Q Grain Ton Shipped)= B0+ B1 ln(Price) + B2 Ice[0,1] + B3:15 Seasonal Variation in Demand [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] + error

#Simple OLS
Demand <- lm(log(quantity) ~ log(price) + cartel + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, JEC)
summary(Demand) #results
plot(Demand) #check residuals 

# stargazer(Demand, type="html") #html regression output

#2SLS Model using cartel status as an instrument for price.
# Price is jointly determined by supply and demand
# Cartel status does not effect the demand for shipment, but does have an effect on supply.
Demand_2sls <- ivreg(log(quantity) ~  log(price) + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | cartel + week + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data= JEC) #note log(price)=cartel after | break. Cartel used as an instrument for the effect of supply on price.
summary(Demand_2sls, diagnostics=TRUE)

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

# Cournot Competition Plot
Plot_Cournot <- ggplot(JEC, aes(x=date, y=cartel, alpha=cartel, fill=cartel)) +
        geom_bar(stat="identity", width = 7) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ylab("") +
        xlab("Date") +
        ggtitle("JEC Competition") +
        scale_fill_manual(values = c("grey", "dodger blue2")) +
        scale_alpha_manual(values = c(0.25, 1)) +
        theme(
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                legend.position = "none",
                plot.title = element_text(color = "dodger blue2", size = 18)
        ) 
Plot_Cournot
ggsave(Plot_Cournot, file="cournot_plot.png", width =12, height = 4, dpi = 300)


## Plot, different prices for quantities given cartel status


# Scatterplot demonstrating supply demand interaction ----
plot.dualCasuality <- ggplot(JEC, aes(x=quantity, y=price)) +
        geom_point(alpha=0.25) +
        xlab("Quantity") +
        ylab("Price") +
        ggtitle("JEC Grain Transport") +
        #geom_smooth(method = "lm", formula = y~x, color = "grey", alpha = 0.25) +
        theme_tufte(ticks = FALSE)
ggMarginal(plot.dualCasuality, type = "histogram", fill = "light grey", color = "light grey") #call plot

## Create Simpler Graph

# ----

#Scatter plot with color breakdown by cartel status ---- 

Plot_PriceDiff_Cartel <- ggplot(JEC, 
                               aes(x = quantity, 
                                   y = price, 
                                   color = cartel)
                               ) +
        geom_point(alpha = .2) +
        scale_shape_manual(values = c(15, 16)) +
        scale_color_manual(values = c("black", "blue")) +
        xlab("Quantity") +
        ylab("Price") +
        ggtitle("JEC Grain Transport") + 
        geom_smooth(method = "lm", formula = y~x, show.legend = FALSE) +
        guides(color = guide_legend(override.aes = list(linetype = 0))) +
        theme_tufte() +
        theme(legend.title=element_blank(),
              legend.position="top",
              legend.key = element_rect(colour = "white")
        )

Plot_PriceDiff_Cartel #call plot

# Plot Cartel Only (grey out competition)
# Plot Competition Only (greyout cartel)
# Combine all three plots into a row


# ----

############ Animation ----
#####
# Simple plot, select/deselect catel status
p1 <- ggplot(JEC, aes(x = quantity, y = price, 
                      color = cartel, 
                      showSelected = cartel)) +
        geom_jitter() +
        scale_color_manual(values = c("Red", "Dodger Blue")) +
        theme_tufte()

p2 <- ggplot(JEC, aes(x = week, y = quantity,
                      color = ice,
                      clickSelects = cartel)) +
        geom_point()+
        scale_color_manual(values = c("Dark Blue", "Dark Grey")) +
        theme_tufte()

plots <- list(plot1 = p1, plot2 = p2)
structure(plots, class = "animint")


# ms, milliseconds of duration, 2000 is decent
# 
        
#animint2dir(list(plot = p), out.dir = "simple", open.browser = FALSE) # simple directory to compile to
#library(servr)
#servr::httd("simple") 

# ----

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
