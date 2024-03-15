# Source: https://www.r-bloggers.com/2019/06/logistic-regression-in-r-with-healthcare-data-vitamin-d-and-osteoporosis/



sum_function <- function() { # create a function with the name my_function
  print("Siema, jestem Lena")
}

my_function()


#install.packages('tidyverse')
#install.packages('RNHANES')
#install.packages('ggplot2')
#install.packages('pROC')
#install.packages('plotly')

library(tidyverse)
library(RNHANES)
library(ggplot2)
library(pROC)
library(plotly)

d07 = nhanes_load_data("DEMO_E", "2007-2008") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR) %>% 
  left_join(nhanes_load_data("VID_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS) %>% 
  left_join(nhanes_load_data("BIOPRO_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, LBXSCA) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA) %>% 
  left_join(nhanes_load_data("OSQ_E", "2007-2008"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, OSQ060) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, Osteop = OSQ060)

d09 = nhanes_load_data("DEMO_F", "2009-2010") %>%
  select(SEQN, cycle, RIAGENDR, RIDAGEYR) %>%
  transmute(SEQN=SEQN, wave=cycle, RIAGENDR, RIDAGEYR) %>% 
  left_join(nhanes_load_data("VID_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, LBXVIDMS) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD=LBXVIDMS) %>% 
  left_join(nhanes_load_data("BIOPRO_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD,  LBXSCA) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium = LBXSCA) %>% 
  left_join(nhanes_load_data("OSQ_F", "2009-2010"), by="SEQN") %>%
  select(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, OSQ060) %>% 
  transmute(SEQN, wave, RIAGENDR, RIDAGEYR, vitD, Calcium, Osteop = OSQ060)

dat = bind_rows(d07, d09) %>% as.data.frame()


dat1 = dat %>% 
  mutate(
    vitD_group = case_when(
      vitD < 30 ~ "Deficiency",
      vitD >= 30 & vitD < 50 ~ "Inadequacy",
      vitD >= 50 & vitD <= 125 ~ "Sufficiency"))

dat2 = dat1 %>% 
  filter(!is.na(vitD_group), !is.na(Calcium), !is.na(Osteop), Osteop!=9) %>% 
  mutate(Gender = recode_factor(RIAGENDR, 
                                `1` = "Men", 
                                `2` = "Women"),
         Osteop = recode_factor(Osteop, 
                                `1` = 1, 
                                `2` = 0))

head(dat2)


# Age Distribution
age_histogram <- plot_ly(dat2, x = ~RIDAGEYR, type = "histogram", marker = list(color = "skyblue")) %>%
  layout(title = "Age Distribution", xaxis = list(title = "Age"), yaxis = list(title = "Frequency"))

age_histogram

# Vitamin D Distribution
vitD_histogram <- plot_ly(dat2, x = ~vitD, type = "histogram", marker = list(color = "lightgreen")) %>%
  layout(title = "Vitamin D Distribution", xaxis = list(title = "Vitamin D Level"), yaxis = list(title = "Frequency"))

vitD_histogram

# Calcium Distribution
calcium_histogram <- plot_ly(dat2, x = ~Calcium, type = "histogram", marker = list(color = "orchid")) %>%
  layout(title = "Calcium Distribution", xaxis = list(title = "Calcium Level"), yaxis = list(title = "Frequency"))

calcium_histogram



# Gender Distribution
gender_bar <- plot_ly(dat2, x = ~Gender, type = "bar", marker = list(color = "salmon")) %>%
  layout(title = "Gender Distribution", xaxis = list(title = "Gender"), yaxis = list(title = "Count"))

gender_bar

# Vitamin D Group Distribution
vitD_group_bar <- plot_ly(dat2, x = ~vitD_group, type = "bar", marker = list(color = "gold")) %>%
  layout(title = "Vitamin D Group Distribution", xaxis = list(title = "Vitamin D Group"), yaxis = list(title = "Count"))

vitD_group_bar

# Scatterplot
scatterplot <- plot_ly(dat2, x = ~vitD, y = ~Calcium, type = "scatter", mode = "markers", marker = list(color = "blue")) %>%
  layout(title = "Scatterplot of Vitamin D vs Calcium", xaxis = list(title = "Vitamin D Level"), yaxis = list(title = "Calcium Level"))

scatterplot

# Age by Gender
boxplot_age_gender <- plot_ly(dat2, x = ~Gender, y = ~RIDAGEYR, type = "box", boxmean = "sd", marker = list(color = "green")) %>%
  layout(title = "Age by Gender", xaxis = list(title = "Gender"), yaxis = list(title = "Age"))

boxplot_age_gender


fit <- glm(Osteop ~ vitD_group + Calcium + Gender + RIDAGEYR, 
           data = dat2, 
           family = "binomial")

summary(fit)

round(exp(coef(fit)), 2)

dat2$PredictedProb <- 1 - predict(fit, type = "response")


# Create an interactive scatter plot
plot <- plot_ly(dat2, x = ~RIDAGEYR, y = ~PredictedProb, color = ~Gender, type = "scatter", mode = "markers") %>%
  layout(title = "Predicted Probabilities of Osteoporosis",
         xaxis = list(title = "Age"),
         yaxis = list(title = "Predicted Probability"),
         color = list(title = "Gender"))

# Show the interactive plot
plot


# model without vitamin D
fit1 <- glm(Osteop ~ Calcium + Gender + RIDAGEYR, 
            data = dat2, 
            family = "binomial")
# model with vitamin D
fit2 <- glm(Osteop ~ vitD_group + Calcium + Gender + RIDAGEYR, 
            data = dat2, 
            family = "binomial")

dat2$prob1=predict(fit1,type=c("response"))
dat2$prob2=predict(fit2,type=c("response"))

roc(Osteop ~ prob1, data = dat2)

roc(Osteop ~ prob2, data = dat2)
