# Loading important libraries

library(readr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(repr)

# Loading dataset
Data = read.csv("~/Documents/dsi_2022-02/individual-assignment-2-Jezahmoud/data/menu.csv", header=T) 
names(Data)
# Cleaning the columns names
Clean_names <- gsub(pattern = "*....Daily.Value.", replacement=".DV", names(Data)) 
names(Data) <- Clean_names
names(Data)


# Choose the fields that have the strings "fl oz" and "carton"
drinks.oz <- Data[str_detect(Data$Serving.Size, " fl oz.*"),]
drinks.ml <- Data[str_detect(Data$Serving.Size, 'carton'),]

#Convert the ounces to millilitres
#values are rounded to zero decimal places
drinks.oz$Serving.Size <- 
  round(as.numeric(gsub(" fl oz.*", "", drinks.oz$Serving.Size))*29.5735,0)
drinks.ml$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ ml).*", "\\1", drinks.ml$Serving.Size)),0)

# Choose fields that contain "g" string
# values are rounded to zero decimal places
food.g <- Data[str_detect(Data$Serving.Size, 'g'),] 
food.g$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ g).*", "\\1", food.g$Serving.Size)),0)

# Merge the rows of all those data tables into a single data table.
# Make a new column labelled "Type" and the values to be "drink" or "meal."
Data2 <- rbind(drinks.oz,drinks.ml)
Data2$Type <- rep("drinks.ml", nrow(Data2))
food.g$Type <- rep("food.g", nrow(food.g))
Data2 <- rbind(Data2,food.g)

# Food categories
options(repr.plot.height=3, repr.plot.width=6)
ggplot(Data2) +
  geom_bar(aes(x = reorder(Category, table(Category)[Category])), fill = "lightblue") +
  coord_flip() +
  theme_tufte(ticks=F) +
  labs(x = NULL)


# Exploring calories
options(repr.plot.height=3, repr.plot.width=6)
ggplot(Data2, aes(x = Calories)) +
  geom_histogram(aes(y = ..density..), fill = "yellow", binwidth = 40, color="gray") + 
  geom_density() +
  scale_x_continuous(breaks = seq(min(Data$Calories), max(Data$Calories), by = 200)) +
  theme_pander()


#Calories by category
options(repr.plot.height=4, repr.plot.width=6)
ggplot(Data2, aes(x = Calories)) + 
  geom_density(fill="lightblue") +
  facet_wrap( ~ Category) +
  geom_vline(aes(xintercept = median(Calories)), linetype = 2) +
  theme_pander(base_size=8)


#Calories by category
options(repr.plot.height=3, repr.plot.width=6)
ggplot(Data2, aes(x = reorder(Category, Calories), y = Calories)) +
  geom_boxplot() +
  coord_flip() +
  theme_pander() +
  labs(x = NULL)
