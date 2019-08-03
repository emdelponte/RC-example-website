
# --------------------------------------------------------
# R script for reproducing the analysis of the the paper 
# Linear modelling of # Soil temperature effects on root
# lesion nematode population densities in R. 
# Prepared by Adam H Sparks
# University of Southern Queensland
# Centre for Crop Health, Toowoomba, Qld, AU. 
# --------------------------------------------------------


# Remove comment next two lines to instal packages if not installed before
# install.packages("tidyverse", 
# repos = c(CRAN = "https://cloud.r-project.org/")
                 
# Load the package
library(tidyverse)

# Load and inspect the data
nema <- read_csv("Nematode_Data.csv")
nrow(nema)
head(nema)                 

# Reshape data from wide to long format using gather function
nema_long <- nema %>% gather(Variety, Log_pop, Unplanted:Suneca)
nema_long
nrow(nema_long)

# Visualize the data
ggplot(nema_long,aes(x = Degree_days, y = Log_pop,colour = Temperature, group = Variety)) +
  geom_point() +
  geom_smooth(colour = "grey",se = FALSE, alpha = 0.5 ) +
  ylab(expression(paste("ln(", italic("P. thornei"), "/kg soil) + 1"), sep = "" )) +
  xlab("Thermal Time (˚C Days Above 10˚C)") +
  facet_wrap(~ Variety, ncol = 2)
                 

# Fit a linear regression model to Unplanted check
linear_model <- function(df){
              lm(Log_pop ~ Degree_days,
              data = df
                   )}

unplanted_model <- nema_long %>%
                  filter(Variety == "Unplanted") %>%
                  linear_model()
                   
# plot and summary of model fit
par(mfrow = c(2, 2))
plot(unplanted_model)
summary(unplanted_model)
                   
# Plot model and save figure
nema_long %>%
group_by(Variety) %>%
filter(Variety == "Unplanted") %>%
ggplot(aes(x = Degree_days, y = Log_pop,colour = Temperature)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, size = 1, se = FALSE, colour = "grey", alpha = 0.5) +
  ylab(expression(paste("ln(", italic("P. thornei"), "/kg soil) + 1"), 
                  sep = "")) +
  xlab("Thermal Time ˚C Days Above 10 ˚C)") +
  ggtitle("Unplanted Linear Model")+
  ggsave("Fig1_linear_model.png")
                   
# Fit a quadratic model to data from Gatcher cultivar
quadratic_model <- function(df){
                    lm(Log_pop ~ Degree_days + I(Degree_days ^ 2), 
                       data = df)}
                     
s_model <- nema_long %>%
           filter(Variety == "Gatcher") %>%
           quadratic_model()
                     
par(mfrow = c(2, 2))
plot(s_model)
summary(s_model)
                     
                     
# Plot model and save figure                   
nema_long %>%
 group_by(Variety) %>%
 filter(Variety == "Gatcher") %>%
 ggplot(aes(x = Degree_days, y = Log_pop,colour = Temperature)) +
  geom_point() + 
  geom_smooth(method = "lm",formula = y ~ x + I(x ^ 2), size = 1,se = FALSE, colour = "grey", alpha = 0.5) +
  ylab(expression(paste("ln(", italic("P. thornei"),"/kg soil) + 1"), 
                  sep = "")) +
  xlab("Thermal Time (˚C Days Above 10˚C)") +
  ggtitle("Gatcher Quadratic Model")+
ggsave("Fig2_quadratic_model.png")
                     
                     
                     