## Railroad JEC
rm(list=ls()) #clear workspace
#library
library(mfx)
library(AER)
library(foreign)
library(ggplot2)
library(ggvis)
library(directlabels)
library(stargazer)
library(gridExtra)
library(dplyr)

#Data
JEC <- read.dta("JEC.dta") # read .dta file into data frame
str(JEC) #look at structure
cols <- c("cartel", "seas1", "seas2", "seas3", "seas4", "seas5", "seas6", "seas7", "seas8", "seas9", "seas10", "seas11", "seas12", "ice") # select columns to factor, factor = catergorial data (yes/no, low/medium/high)
JEC[,cols] <- data.frame(apply(JEC[cols], 2, as.factor)) # factor columns
head(JEC$cartel)
levels(JEC$ice)  <- c("Clear Shipping Lanes", "Ice") #give names to factor levels
levels(JEC$cartel) <- c("Competition", "Cartel")

summary(JEC)
plot(JEC$quantity, JEC$price)
plot(JEC$week, JEC$price)


#Graphs aes(size=JEC$cartel, group=1
# price by cartel graph
# Giants Orange: #FB5B1F
plot.cartel <- ggplot(JEC, aes(x=week, y=price)) +
        geom_line(aes(color=JEC$cartel, size=JEC$cartel, group=1)) +
        geom_dl(aes(label=JEC$cartel, color=JEC$cartel), method="lines2") +
        xlab("") +
        ylab("Price") +
        scale_size_manual(values=c(1.5, 1),
                          name="Industry Status",
                          breaks=c("False", "True"),
                          labels=c("Competition", "Cartel"),
                          guide = guide_legend(reverse=TRUE)
                          ) +
        scale_color_manual( values=c("#FB5B1F", "grey"), 
                            name="Industry Status",
                            breaks=c("False", "True"),
                            labels=c("Competition", "Cartel"),
                            guide = guide_legend(reverse=TRUE)
                          ) +
        scale_y_continuous(breaks=seq(0, 0.5, by = 0.05)) +
        ggtitle(expression(atop("Difficulties Maintaining Cartel Cooperation", atop(italic("Pricing and Cartel Status for JEC 1880-1886, Midwest to Eastern Seaboard"), "")))) +
        theme(panel.border=element_blank(), 
              panel.background=element_blank(),
              plot.title=element_text(size=20),
              axis.title.y=element_text(angle=0, vjust=1), 
              axis.title.x=element_text(hjust=1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.position="top")

plot.cartel

# geom_dl(aes(label=JEC$cartel, color=JEC$cartel), method="lines2") +
# legend.key=element_blank()

#create quantity shipping by week graph
plot.quantity <- ggplot(JEC, aes(x=week, y=quantity, fill=ice)) +
        geom_bar(stat='identity') +
        scale_fill_manual(values = c("blue", "grey"),
                          breaks = c("Clear Shipping Lanes", "Ice"),
                          labels = c("Clear", "Ice"),
                          guide = guide_legend(reverse=TRUE),
                          name = "Shipping Lanes Great Lakes"
        ) +
        xlab("Week") +
        ylab("Quantity \n Freight") +
        theme(panel.border=element_blank(), 
              panel.background=element_blank(), 
              axis.title.y=element_text(angle=0, vjust=-.2), 
              axis.title.x=element_text(hjust=1), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.key=element_blank(),
              legend.key.size = unit(.25, "cm"),
              legend.position="bottom") 

plot.quantity


## arrange plots
g1<-ggplotGrob(plot.cartel)
g2<-ggplotGrob(plot.quantity)
#Bind the tables
g<-gtable:::rbind_gtable(g1, g2, "first")
#Remove a row between the plots
g <- gtable_add_rows(g, unit(-1,"cm"), pos=nrow(g1))
#Adjust heights, 5 to 1 ratio top to bottom
panels <- g$layout$t[grep("panel", g$layout$name)]
g$heights[panels] <- lapply(c(5,1), unit, "null")
#Draw. Setup new page and draw gtable with graphs
grid.newpage()
grid.draw(g)


##P rice by cartel ggvis ##     Interactive Graph for HTML
#take data -> put into ggvis -> output scatterplot
JEC %>% 
        ggvis(~week, ~price) %>% 
                layer_points(fill = ~factor(cartel)) %>%
                layer_lines()


#######################################
##  Models                          ###
#######################################

#lnQ Grain Ton Shipped= B0+ B1 ln(Price) + B2 Ice[0,1] + Seasonal Variation in Demand [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] + error

#Simple OLS
Demand <- lm(log(quantity) ~ log(price) + cartel + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, JEC)
summary(Demand)

plot(Model1.OLS)
stargazer(Model1.OLS) #html output for stargazer
stargazer(Model1.OLS, type="html") #html output for stargazer

stargazer(Demand, Demand.2sls, 
          single.row = TRUE, 
          title="JEC Transport Demand Equations",
          covariate.labels = c("log(Price)", "Cartel", "Ice", "Season 1", "Season 2", "Season 3", "Season 4", "Season 5", "Season 6", "Season 7", "Season 8", "Season 9", "Season 10", "Season 11", "Season 12"),
          out="models.html"
          )

#2SLS
Demand.2sls <- ivreg(log(quantity) ~  log(price) + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | cartel + week + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data= JEC) #note log(price)=cartel after | break
summary(Demand.2sls, diagnostics=TRUE)
plot(Demand)

#inverse demand equations
InverseDemand <- lm(log(price) ~ log(quantity) + cartel + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, JEC)
summary(InverseDemand)
#2SLS
InverseDemand.2sls <- ivreg(log(price) ~  + log(quantity) + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12 | cartel + week + ice + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, data= JEC)
summary(InverseDemand.2sls)


#simplified model with insignificant variables removed ()
Model2.OLS <- lm(log(quantity) ~ log(price) + cartel + ice, JEC)
summary(Model2.OLS)
plot(Model2.OLS)

# OLS Inverse Demand
InverseDemand <- lm(log(price) ~ log(quantity) + cartel + ice  + seas1 + seas2 + seas3 + seas4 + seas5 + seas6 + seas7 + seas8 + seas9 + seas10 + seas11 + seas12, JEC)
summary(InverseDemand)
plot(InverseDemand)


# Probit model, Cartel ~ Price Quantity Ice 
Model4.Probit <- glm(cartel ~ log(quantity) + log(price) + ice , data=JEC, family=binomial(link='probit'))
summary(Model4.Probit)
probitmfx(cartel ~ log(quantity) + log(price) + ice , data=JEC)


# Probit model average marginal effects
ProbitScalar <- mean(dnorm(predict(Model4.Probit, type = "link")))
ProbitScalar * coef(Model4.Probit)
### interpretation.. more quantiy less likely to cheat. Higher price more likely to cheat. More ice less likely to cheat (less competition, harder to mask cheating, easier to guess cheating/harder to miss-guess cheating)