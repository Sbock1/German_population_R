library(readxl)
library(ggplot2)
library(tidyverse)

# Source from Destatis / Genesis Database / 12411-0008
# Foreigners (Ausländer) are persons with no german citizenship according to Art. 116 Abs. 1 deutsches Grundgesetz
# https://www-genesis.destatis.de/genesis/online?operation=statistic&levelindex=0&levelid=1704038020380&code=12411&option=table&info=on#abreadcrumb

# Data loading
df <- read_excel("Test_R_1/Destatis_Q42022_Alter_Geschlecht_Familienstand__Ausländer_Mengen__melted_v1.xlsx")

# Data munging
## Change of format of columns for further processing
df$Jahr <- as.Date(df$Jahr, format = "%d.%m.%Y")
df$Anzahl <- as.numeric(df$Anzahl)

## Preview dataset
View(df)

# Calculating additional values for later presentation
## Grouping to get total population
df_total_pop <- df %>%
  drop_na() %>%
  group_by(Jahr) %>%
  summarise(Total_Anzahl = sum(Anzahl, na.rm = TRUE))

View(df_total_pop)

## Grouping to get to conclusion: "Percentage of non-germans compared to germans per year
df_aggregiert <- df %>%
  drop_na() %>%
  group_by(Jahr, Nationalität) %>%
  summarise(Total_Anzahl = sum(Anzahl, na.rm = TRUE))


View(df_aggregiert)

## Grouping distribution foreigner to native population over the years
df_aggregiert1 <- df_aggregiert %>%
  group_by(Jahr) %>%
  mutate(Anteil_an_Bev = Total_Anzahl / sum(Total_Anzahl), Anteil_Rest_an_Bev = 1 - Anteil_an_Bev)


## Grouping age distribution foreigner
df_aggregiert2 <- df %>%
  drop_na() %>%
  group_by(Jahr, Alter) %>%
  filter(Nationalität == "Ausländer") %>%
  summarise(Total_Anzahl = sum(Anzahl, na.rm = TRUE))

View(df_aggregiert2)

# Visual Presentation
## Stacked bar chart 1: 100% reference without showing absolute population growth
ggplot(df_aggregiert1, aes(x = Jahr, y = Anteil_an_Bev, fill = factor(Nationalität))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.f%%", Anteil_an_Bev * 100)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Anteil der Deutschen und Nicht-Deutschen an der Gesamtbevölkerung",
       x = "Jahr", y = "Anteil (in Prozent)",
       fill = "Nationalität") +
  scale_y_continuous(labels = scales::percent_format(scale = 100))
  

## Stacked bar chart 2: Absolute reference with showing absolute population growth
ggplot(df_aggregiert, aes(x = Jahr, y = Total_Anzahl, fill = factor(Nationalität))) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = sprintf("%.f%%", df_aggregiert1$Anteil_an_Bev * 100)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Anteil der Deutschen und Nicht-Deutschen an der Gesamtbevölkerung",
       x = "Jahr", y = "Anteil (in Prozent)",
       fill = "Nationalität") +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = " Mio."))

## Line graph
ggplot(df_total_pop, aes(x = Jahr, y = Total_Anzahl)) + 
  geom_line(color = "blue", size = 2) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = " Mio."))