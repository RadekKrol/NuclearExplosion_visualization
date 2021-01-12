######################################################  
### Wykorzystane pakiety
######################################################

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(forcats)

######################################################  
### Baza danych
######################################################

nex <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")
str(nex)
head(nex, n = 4)
glimpse(nex)


######################################################  
### Oczyszczanie danych i zmiana typu dannych
######################################################

### Zmiana typu date_long na typ date
nex$date_long <- lubridate::ymd(nex$date_long)

### zmieniamy zmienne kategoryczne na typu factor
nex <- nex %>% 
  mutate(country = factor(country, 
                          levels = c("CHINA", "FRANCE", "INDIA", "PAKIST", "UK", "USA", "USSR"),
                          labels = c("China", "France", "India", "Pakistan", "UK", "USA", "USSR"))) 
nex<- nex %>%
  mutate_at(vars(c("region","purpose","name","source","type")), as.factor)

glimpse(nex) # wszystkie potrzebne zmienne są typu factor

######################################################  
### Transfromacja danych
######################################################

### dodajemy kolumnę month
nex <- nex %>% 
  mutate(date = date_long) %>%
  separate(col = date_long, into = c("year2", "month", "day"), convert = TRUE, sep = "-") %>%
  select(-c(year2, day))

nex$month <- as.factor(nex$month)

glimpse(nex)

### dodajemy kolumnę decade
nex <- nex %>%
  mutate(decade = factor(10 * (year %/% 10)))
glimpse(nex)

### Dodajemy informację o prezydentach US
nex <- nex %>% 
  mutate(president = factor(case_when(date < "1953-01-20" ~ "Truman",
                               date < "1961-01-20" ~ "Eisenhower",
                               date < "1963-11-22" ~ "Kennedy",
                               date < "1969-01-20" ~ "Johnson",
                               date < "1974-08-09" ~ "Nixon",
                               date < "1977-01-20" ~ "Ford",
                               date < "1981-01-20" ~ "Carter",
                               date < "1989-01-20" ~ "Reagan",
                               date < "1993-01-20" ~ "Bush Senior",
                               TRUE ~ "Clinton")))
glimpse(nex)

### Stworzenie ramki danych zawierającej początek i start kryzysu atomowego pomiędzy USSR a US
cuban_crisis <- data.frame(
  event <- "Kryzys Kubański",
  start = 1961,
  end = 1963
)

### Stworzenie pomocniczej tabeli do wykresu z eksplozjami podziemnymi i na powierzchni
nex_d <- nex %>%
  mutate(depth_type = case_when(
    depth<0 ~ 1,
    depth>=0 ~ -1),
    type_ar = factor(case_when(
      type == "SHAFT/GR" ~ "SHAFT",
      type == "SHAFT/LG" ~ "SHAFT",
      type == "GALLERY" ~ "TUNNEL",
      type == "CRATER" ~ "SURFACE",
      type == "SHIP" ~ "BARGE",
      type =="UG" ~ "OTHER",
      type =="ROCKET" ~ "OTHER",
      type =="UW" ~ "OTHER",
      type =="SPACE" ~ "OTHER",
      type =="MINE"~ "OTHER",
      type =="WATER SU" ~ "OTHER",
      type =="WATERSUR" ~ "OTHER",
      TRUE ~ as.character(type)))) %>%
  mutate(depth_type = as.numeric(case_when(
    type_ar == "AIRDROP" ~ "1",
    type_ar == "ATMOSPH" ~ "1",
    type_ar == "BALLOON" ~ "1",
    type_ar == "BARGE" ~ "1",
    type_ar == "SURFACE" ~ "1",
    type_ar == "TOWER" ~ "1",
    TRUE ~ as.character(depth_type)))) %>%
  select(country, depth, depth_type, type_ar)%>%
  group_by(country)%>%
  mutate(total = n()) %>%
  ungroup() %>%
  mutate(country=fct_reorder(country, total))%>%
  group_by(country, type_ar, depth_type) %>%
  summarize(count = n())%>%
  arrange(-count) %>%
  arrange(country) %>%
  mutate(count = ifelse(depth_type <0, (-1 * count), count)) %>%
  filter(country %in% c("USA","USSR","France","China","UK")) 


######################################################  
### Wizualizacja Danych
######################################################

### Eksplozje nuklearne w kolejnych latach
nex %>% 
  group_by(year, decade) %>%
  summarize(total = n())  %>% 
  ungroup() %>%
  ggplot(aes(x = year, y = total, fill = decade)) +
  geom_col(aes(fill = decade), col = "black")+
  scale_fill_brewer("Dekady", palette = "Set1")+
  scale_x_continuous(limits = c(1940, 2000), breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000))+
  scale_y_continuous(limits = c(0, 180), breaks = c(seq(0,200, by = 20))) +
  theme(legend.position = "bottom") + 
  labs(x= "",
      y = "Liczba eksplozji nuklearnych",
      title = "Eksplozje nuklearne w kolejnych latach w podziale na dekady")+
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1, linetype = 2),
        plot.background = element_rect(color = "black", size = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, face = "italic"),
        axis.title.x = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 12))

### Liczba testów nuklearnych dla poszczególnych państw
nex %>%
  count(country, sort = TRUE) %>%
  mutate(country = fct_reorder(country, n))  %>%
  ggplot(aes(country, n)) +
  geom_col(aes(fill = country), show.legend = FALSE) +
  geom_text(aes(label = n, color = country), show.legend = FALSE,
            nudge_y = 40) + 
  scale_y_continuous(breaks = c(seq(0, 1100, by=100)))+
  labs(x = "",
       y = "Liczba testów nuklearnych",
       title = "Testy nuklearne w latach 1945-1998") +
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1, linetype = 2),
        plot.background = element_rect(color = "black", size = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, face = "italic"),
        axis.title.x = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"))

#### Liczba testów nuklearnych USA dla każdego prezydenta
nex %>% 
  filter(country == "USA") %>%
  group_by(year, president) %>%
  summarize(total = n()) %>%
  ggplot(aes(x = year, y = total, fill = president))+
  geom_col(position = "stack", col = "black")+
  geom_smooth(aes(group = 1), se = FALSE, method = "loess", span = 0.4, col = "black", size = 1.5, show.legend = F)+
  scale_fill_brewer("Prezydenci", palette = "Set1")+
  scale_x_continuous(expand = c(0,0), limits = c(1940, 1995), breaks = c(seq(1940, 1995, by =5)))+
  scale_y_continuous(expand = c(0,1), limits = c(0, 100), breaks = c(seq(0,100, by = 20)))+
  labs(x = "",
       y = "Ilość eksplozji")+
  ggtitle("Testy nuklearne w USA")+
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1, linetype = 2),
        plot.background = element_rect(color = "black", size = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, face = "italic"),
        axis.title.x = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 12))
  

### Skumulowana ilość eksplozji + siła eksplozji
nex %>% 
  select(id_no, year, country, yield_lower, yield_upper) %>% 
  group_by(year, country) %>% 
  summarise(kum = n_distinct(id_no),
            max_yield = max(yield_upper, na.rm = TRUE)) %>% 
  group_by(country) %>% 
  mutate(kum = cumsum(kum)) %>%
  ggplot(aes(x = year,y = kum, col = country)) +
  geom_point(aes(size = max_yield), alpha = 0.8) + 
  geom_rect(data = cuban_crisis, aes(xmin = start, xmax = end, ymin = 0, ymax = +Inf), alpha = 0.3, inherit.aes = FALSE, show.legend = FALSE)+
  geom_text(data = cuban_crisis, aes(y = 950, x = 1962, label = event),  inherit.aes = FALSE, size = 3, colour = "black")+
  scale_color_brewer("Kraje", palette = "Set1")+
  guides(colour = guide_legend(override.aes = list(size=6)))+
  scale_size_continuous("Siła Wybuchu", range = c(1, 10))+
  scale_x_continuous(limits = c(1940, 2000), breaks = c(seq(1940, 2000, by = 10)))+
  scale_y_continuous(limits = c(0,1100), breaks = c(seq(0,1200, by = 100)))+
  labs(x= "",
       y = "Skumulowana ilość eksplozji",
       title = "Skumulowana ilość eksplozji w rozbiciu na kraje")+
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1, linetype = 2),
        plot.background = element_rect(color = "black", size = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, face = "italic"),
        axis.title.x = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 12))


### testy nuklearne przeprowadzone przez USA i USSR w poszczególnych miesiącach
nex %>%
  filter(country == "USA"| country == "USSR") %>%
  group_by(month, country) %>%
  summarize(total = n(),
            max_yield = max(yield_upper, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = total, size = max_yield, col = country)) + 
  geom_point(alpha = 0.7)+
  geom_smooth(aes(group = country), method = "lm", se = FALSE)+
  scale_y_continuous(limits = c(0,150), breaks = c(seq(0,160, by = 25)))+
  labs( x = "miesiące",
        y = "Ilość eksplozji",
        title = "Eksplozje w poszczególnych miesiącach dla USSR i USA")+
  guides(size = guide_legend(title = "Siła Wybuchu", override.aes = list(linetype = NA)))+
  facet_wrap(.~country, scale = "free")+
  scale_size_continuous(range = c(1,10))+
  scale_color_brewer("Kraje", palette = "Set1")+
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1, linetype = 2),
        plot.background = element_rect(color = "black", size = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, face = "italic"),
        axis.title.x = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size =9),
        strip.background = element_blank(),
        strip.text.x = element_blank()) # usunięcie nazw facetów


### Eksplozje naziemne i podziemne wg krajów
ggplot(nex_d, aes(fill = type_ar, x = country, y = count))+
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette="Set1")+ 
  scale_y_continuous(breaks = seq(-1000,250,50), labels = abs(seq(-1000, 250, 50)))+
  scale_x_discrete(limits=c("UK", "China", "France", "USSR", "USA"))+
  geom_hline(yintercept = 0, colour="black", size=1)+
  labs(title = "Eksplozje naziemne i podziemne wg typów eksplozji",
       x = "",
       y= "Liczba eksplozji",
       fill="Typ eksplozji") +
  annotate("text", x="China", y= 150, label='Na powierzchni', fontface=2)+ 
  annotate("text", x="China", y= -500, label='Podziemne', fontface=2 )+
  theme(panel.background = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.1, linetype = 2),
        plot.background = element_rect(color = "black", size = 1),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        strip.text = element_text(size = 16),
        axis.title.y = element_text(hjust = 0.5, face = "italic"),
        axis.title.x = element_text(hjust = 0.5, face = "italic"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size =9),
        strip.background = element_blank(),
        strip.text.x = element_blank())


