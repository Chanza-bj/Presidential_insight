getwd()
setwd("C:/Users/Tutu/Desktop/R")

library(readxl)
library(tidyverse)
library(dplyr)

install.packages("patchwork")
library(tidyr)

library(gtable)

library(ggplot2)
library(tidyr)
install.packages("janitor")
library(janitor)

#Load the data from an Excel file and clean the column names:

data <- read_excel('Presidential.xlsx')
data
View(data)
plot(data)

head(data)

clean<-clean_names(data)
colnames(clean)
clean
head(clean)

data <- clean %>%
  group_by(province_name) %>%
  summarise(across(c(female_count,male_count,total_count)))

data

work <- data %>% 
  group_by(province_name) %>% 
  summarise(female_count = sum(female_count),male_count = sum(male_count),total_count = sum(total_count))
work


# Load the plotrix package
library(plotrix)

province <- c(work$province_name)
province
gender <- c(work$male_count, work$female_count)
gender
pie(work$total_count, labels = province)
Provinces <- data.frame(Provinces = c(work$province_name))
Female <-  data.frame(Female = c(work$female_count))
Male <- data.frame(Male = c(work$male_count))
Total <- data.frame(Total = c(work$total_count))
Gender <- cbind(Female, Male)
Provinces
Male
Female
Gender
bar <- data.frame(Provinces,Male, Female, Total)
View(bar)
bar
sum(Total)





library(plotrix)

library(graphics)


pct <- round(100*work$total_count/sum(work$total_count))
pie(work$total_count,
    labels = paste(work$province_name, sep = " ", pct, "%"),
    col = rainbow(length(work$total_count)),
    main = "2021 Presidential Election Results by Province")
legend("topright",
       legend = work$province_name,
       col = rainbow(length(work$total_count)),
       pch = 15,
       pt.cex = 2,
       title = "Provinces")


#Second

data %>% 
  pivot_longer(cols = c("Male", "Female"), names_to = "Gen", values_to = "Count") %>% 
  group_by(Provinces) %>% 
  mutate(Pct = Count / sum(Count)) %>% 
  ggplot(aes(x = "", y = Pct, fill = Gen)) +
  geom_col(width = 1, position = "stack") +
  geom_text(aes(label = scales::percent(Pct)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_discrete(name = "Gender") +
  labs(title = "2021 Presidential Election Results by Gender in each Province",
       x = "",
       y = "Percentage of Votes") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face = "bold"))



#third


data <- structure(list(Provinces=c("CENTRAL","COPPERBELT","EASTERN",
                                   "LUAPULA","LUSAKA","MUCHINGA","NORTH-WESTERN","NORTHERN",
                                   "SOUTHERN","WESTERN"),Male=c(317037,507825,403632,254570,
                                                                598927,185458,180988,280756,359918,183348),Female=c(349563,
                                                                                                                    518072,492707,312433,644692,216200,205689,325590,422149,
                                                                                                                    263945),Total=c(666600,1025897,896339,567003,1243619,
                                                                                                                                    401658,386677,606346,782067,447293)),row.names=c(NA,10L
                                                                                                                                    ),class="data.frame") %>% as_tibble()


data %>% 
  pivot_longer(cols = c("Male", "Female"), names_to = "Gen", values_to = "Count") %>% 
  ggplot(aes(x = Provinces, y = Count, fill = Gen)) +
  geom_col(position = "dodge") +
  geom_errorbar(mapping = aes(ymin = Count - sd(Count), ymax = Count + sd(Count)), position = "dodge", width = 0.2) +
  stat_summary(fun = "mean", geom = "errorbar", width = 0.2) +
  scale_fill_discrete(name = "Gender") +
  labs(title = "2021 Presidential Election Results by Gender in each Province",
       x = "Provinces",
       y = "Count") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"))


