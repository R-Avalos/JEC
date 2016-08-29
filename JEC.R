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
# install.packages(c("haven", "dplyr", "broom", "AER", "mfx", "ggplot2", "ggvis", "gridExtra", "stargazer", "devtools", "lubridate", "ggthemes", "ggExtra"))
#devtools::install_github("tdhock/animint", upgrade_dependencies=FALSE)


library(haven) 
library(dplyr)
library(lubridate)
library(tidyr)
library(broom)
library(AER)
#library(mfx)
library(ggplot2) 
library(ggthemes)
library(ggExtra)
library(gridExtra)
library(ggvis)
library(animint)
#library(gridExtra)
library(stargazer)
#library(directlabels)



#################################
# 2. Load Data and Wrangle   ###
###############################

# Data source, Stock & Watson: http://wps.pearsoned.co.uk/ema_ge_stock_ie_3/193/49605/12699041.cw/content/index.html
JEC <- read_dta("JEC.dta") # read STATA .dta file into data frame
head(JEC) # quick review of the data
summary(JEC)

# Format Data -----
JEC$cartel <- as.factor(JEC$cartel) # set as factor
levels(JEC$cartel) <- c( "Competition", "Cartel") # rename factor levels
JEC$ice <- as.factor(JEC$ice)
levels(JEC$ice)  <- c("Clear Shipping Lanes", "Ice")
Start_Date <- ymd("1880-1-1") # Set start date, Jan 1st, 1880
JEC$date <- Start_Date + weeks((JEC$week)-1) # Create date vector, 1 removed as first day is 1880-1-1
# JEC$Trigger <- ifelse(JEC$cartel=="Competition", TRUE, FALSE) # Setup Trigger

# Summary Stats, must contver to data frame for stargazer package
df_JEC <- as.data.frame(JEC)
# df_JEC <- subset(df_JEC, select = -c(Trigger))
df_JEC$ice <- as.numeric(df_JEC$ice) - 1  
df_JEC$cartel <- as.numeric(df_JEC$cartel) - 1
cols <- c(5:16) #select seasons
df_JEC[,cols] <- lapply(df_JEC[,cols], as.logical) # convert seasons to logical 
stargazer(df_JEC, 
          type = "html", 
          title = "JEC Summary Table", 
          digits = 2, 
          summary.logical = FALSE,  
          out = "JECsummary.html") #save html


##################
# 3. Models   ###
################

## Demand Models

#Simple OLS
Demand <- lm(log(quantity) ~ log(price) + cartel + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, JEC)
summary(Demand) #results
plot(Demand) # quick check residuals 

# Plots to output
plot_Demand_residfit <-ggplot(Demand, aes(.fitted, .resid)) + 
        geom_point(alpha = 0.5) + 
        stat_smooth(method="loess") + 
        geom_hline(yintercept=0, col="red", linetype="dashed") + 
        xlab("Fitted Values") + 
        ylab("Residuals") + ggtitle("Residual vs Fitted Plot") +
        theme_tufte()
plot_Demand_residfit

plot_Demand_qq <- ggplot(Demand, aes(sample = .stdresid)) +
        geom_point(na.rm = TRUE) + 
        geom_abline(aes(qqline(.stdresid))) + 
        xlab("Theoretical Quantiles") + 
        ylab("Standardized Residuals") + ggtitle("Normal Q-Q") +
        theme_tufte()
plot_Demand_qq # not quite working with aesthestic


stargazer(Demand, 
          single.row = TRUE, 
          title="JEC Transport Demand",
          covariate.labels = c("log(Price)", "Cartel", "Ice", "<font color = grey>Season 1</font>", "<font color = grey>Season 2</font>", "<font color = grey>Season 3</font>", "<font color = grey>Season 4</font>", "<font color = grey>Season 5</font>", "<font color = grey>Season 6</font>", "<font color = grey>Season 7</font>", "Season 8", "<font color = grey>Season 9</font>", "<font color = grey>Season 10</font>", "<font color = grey>Season 11</font>", "<font color = grey>Season 12</font>"),
          dep.var.labels = "log(Quantity)",
          column.labels = "OLS",
          type = "html",
          out="OLSmodel.html"
) #OLS output

plot_OLS_residfit <- ggplot(Demand, aes(.fitted, .resid)) +
        geom_point(alpha = 0.5) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        stat_smooth(method="loess") +
        xlab("Fitted Values") +
        ylab("Residuals") +
        ggtitle("OLS Residual vs Fitted") +
        theme_tufte()
plot_OLS_residfit
ggsave(plot_OLS_residfit, file="plot_OLS_residfit.png", width = 6, height = 4, dpi = 300)

plot_OLS_QQ <- ggplot(Demand, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
        geom_point(na.rm = TRUE) + 
        geom_abline(aes(qqline(.stdresid)), color = "red") +
        theme_tufte ()
plot_OLS_QQ


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

# create a facet wrap for summary data, scatter plots for each variable over time
# corrplot


# Cournot Competition Plot
Plot_Cournot <- ggplot(JEC, aes(x=date, y=cartel, alpha=cartel, fill=cartel)) +
        geom_bin2d(stat = "identity") +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        ylab("") +
        xlab("Date") +
        ggtitle("JEC Competition") +
        scale_fill_manual(values = c("dodger blue2", "grey")) +
        scale_alpha_manual(values = c(1, 0.25)) +
        theme(
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                legend.position = "none",
                plot.title = element_text(color = "dodger blue2", size = 18)
        ) 
Plot_Cournot #print plot
ggsave(Plot_Cournot, file="cournot_plot.png", width =12, height = 4, dpi = 300)


## Plot, different prices for quantities given cartel status
# Linear_equation <- function(dependent_var, OLS, digit_length){
#         y <- tidy(OLS)
#         y1 <- glance(OLS)
#         y1b <- format(round(y1, digits = 3))
#         y2 <- format(round(y[2], digits = digit_length))
#         z <- paste0(dependent_var, " = ", 
#                     y2[2,1], " ", 
#                     y$term[2], " + ", 
#                     y2[1,1], ", R^2 = ",
#                     y1b[1,1]
#                     )
#         print(z)
# }

Plot_PriceDiff_Cartel <- ggplot(JEC, aes(x = quantity, y = price, color = cartel)) +
        geom_point(alpha = .5, size = 2) +
        geom_rangeframe(color = "black") +
        geom_vline(xintercept = c(0, 20000, 40000, 60000), 
                   color = "grey", 
                   alpha = 0.15) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(breaks=seq(0, 70000, 20000)) +
        scale_y_continuous(breaks=seq(0, 1, 0.05)) +
        scale_shape_manual(values = c(15, 16)) +
        scale_color_manual(values = c("red", "dodger blue")) +
        xlab("Quantity") +
        ylab("Price") +
        ggtitle("Cartel vs Competition \nLinear Price Trends") + 
        geom_smooth(method = "lm", formula = y~x) +
        guides(color = FALSE) +
        theme_tufte(ticks = FALSE) +
        theme(legend.title=element_blank(),
              legend.key = element_rect(colour = "white"),
              plot.title =element_text(size = 8)
                )
# Plot Cartel Only (grey out competition) # Plot Competition Only (greyout cartel)
JEC_2 <- subset(JEC, select = -(cartel)) # remove cartel from data set to plot points
Plot_StatusPoint <- ggplot(data = JEC, aes(x = quantity, y = price, color = cartel)) +
        geom_point(data = JEC_2, 
                   color = "light grey", 
                   alpha = .15, 
                   size = 2) +
        geom_point(size = 2) +
        geom_rangeframe(color = "grey") +
        geom_vline(xintercept = c(0, 20000, 40000, 60000), 
                   color = "grey", 
                   alpha = 0.15) +
        scale_color_manual(values = c("red", "dodger blue")) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(breaks=seq(0, 70000, 20000)) +
        scale_y_continuous(breaks=seq(0, 1, 0.05)) +
        xlab("Quantity") +
        ylab("Price") +
        facet_wrap(~ cartel) +
        guides(color = FALSE) +
        theme_tufte(ticks = FALSE) +
        theme(axis.text.x = element_text(color = "grey"),
              axis.text.y = element_text(color = "grey"),
              axis.title.x = element_text(color = "grey"),
              axis.title.y = element_text(color = "grey")
              )
Plot_StatusPoint #view plot
# Combine all three plots into a row
grid.arrange(Plot_StatusPoint, Plot_PriceDiff_Cartel, 
             nrow= 1,
             widths = 2:1,
             top = "Price and Quantity According to Cartel Status") #view plot
g <- arrangeGrob(Plot_StatusPoint, Plot_PriceDiff_Cartel, 
             nrow= 1,
             widths = 2:1,
             top = "Price and Quantity According to Cartel Status")
ggsave(g, file="cartelstatus_plot.png", width = 12, height = 4, dpi = 300) #save plot

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
