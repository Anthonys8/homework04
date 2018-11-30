library(tidyverse)
library(readr)

# load the data
df <- read.csv("raw_data/murders.csv", na.strings = c("", "NA", "#N/A"))

# filter the data to year 1995 only and remove na
murders <- filter(df[complete.cases(df),], year == 1995)
summary(murders)

# Finding the correlation between variables

cor(murders$murders, murders$arrests)
cor(murders$popul, murders$arrests)
cor(murders$murders, murders$popul)
cor(murders$murdrate, murders$arrestrate)
cor(murders$murders, murders$percblack)

# First Model

firstModel <- lm(murders ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc,
                 data = murders)
summary(firstModel)

# Standardised residuals plot for first model

murders <- murders %>% 
  mutate(
    stand_res = rstandard(firstModel)
  )
murders %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")

# Residual vs Fitted Value for first model

murders$fitted <- firstModel$fitted.values
murders$residuals <- firstModel$residuals
murders %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()


# Second Model
secondModel <- lm(murdrate ~ perc1019 + perc2029 + percblack + percmale + rpcpersinc,
                  data = murders)
summary(secondModel)

# Standardised residuals plot for second model

murders <- murders %>% 
  mutate(
    stand_res = rstandard(secondModel)
  )
murders %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")

# Residual vs Fitted Value for second model

murders$fitted <- secondModel$fitted.values
murders$residuals <- secondModel$residuals
murders %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()

