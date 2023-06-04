#Reetodeep Hazra
#IST 719
#Final Poster Code


library(tidyverse) 
library(magrittr)
library(DataExplorer)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(gridExtra)
library(factoextra)

library(readr)
#install.packages("factoextra")
options(scipen = 999)

fifa <- read_csv("/Users/meghabanerjee/Downloads/fifa.csv")
head(fifa)

(88 * 4) * (18208/100)
colnames(fifa)
str(fifa)
dim(fifa)
introduce(fifa)
plot_intro(fifa)
plot_missing(fifa)
fifa.na()
data_list <- list(fifa)
plot_str(data_list)

fifa <- drop_columns(fifa, "Loaned From")

bundesliga <- c(
  "1. FC Nürnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern München",
  "Borussia Dortmund", "Borussia Mönchengladbach", "Eintracht Frankfurt",
  "FC Augsburg", "FC Schalke 04", "Fortuna Düsseldorf", "Hannover 96",
  "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
  "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
)

premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
  
)

laliga <- c(
  "Athletic Club de Bilbao", "Atlético Madrid", "CD Leganés",
  "Deportivo Alavés", "FC Barcelona", "Getafe CF", "Girona FC", 
  "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
  "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
)

seriea <- c(
  "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
  "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
  "Torino","Udinese"
  
)

superlig <- c(
  "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol Başakşehir FK","BB Erzurumspor","Beşiktaş JK",
  "Bursaspor","Çaykur Rizespor","Fenerbahçe SK", "Galatasaray SK","Göztepe SK","Kasimpaşa SK",
  "Kayserispor","Atiker Konyaspor","MKE Ankaragücü", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
)

ligue1 <- c(
  "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Étienne", "Dijon FCO", "En Avant de Guingamp",
  "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "Nîmes Olympique", 
  "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
  "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
)

eredivisie <- c(
  "ADO Den Haag","Ajax", "AZ Alkmaar", "De Graafschap","Excelsior","FC Emmen","FC Groningen",
  "FC Utrecht", "Feyenoord","Fortuna Sittard", "Heracles Almelo","NAC Breda",
  "PEC Zwolle", "PSV","SC Heerenveen","Vitesse","VVV-Venlo","Willem II"
)

liganos <- c(
  "Os Belenenses", "Boavista FC", "CD Feirense", "CD Tondela", "CD Aves", "FC Porto",
  "CD Nacional", "GD Chaves", "Clube Sport Marítimo", "Moreirense FC", "Portimonense SC", "Rio Ave FC",
  "Santa Clara", "SC Braga", "SL Benfica", "Sporting CP", "Vitória Guimarães", "Vitória de Setúbal"
)


fifa %<>% mutate(
  League = case_when(
    Club %in% bundesliga ~ "Bundesliga",
    Club %in% premierLeague ~ "Premier League",
    Club %in% laliga ~ "La Liga",
    Club %in% seriea ~ "Serie A",
    Club %in% superlig ~ "Süper Lig",
    Club %in% ligue1 ~ "Ligue 1",
    Club %in% liganos ~ "Liga Nos",
    Club %in% eredivisie ~ "Eredivisie"
  ),
  Country = case_when(
    League == "Bundesliga" ~ "Germany",
    League == "Premier League" ~ "UK",
    League == "La Liga" ~ "Spain",
    League == "Serie A" ~ "Italy",
    League == "Süper Lig" ~ "Turkey",
    League == "Ligue 1" ~ "France",
    League == "Liga Nos" ~ "Portugal", 
    League == "Eredivisie" ~ "Netherlands"
  )) %>% filter(!is.na(League)) %>% mutate_if(is.factor, as.character)


rm(bundesliga, premierLeague, laliga, seriea, superlig, ligue1, eredivisie, liganos)


head(fifa$Value)

fifa$Values <- str_remove_all(fifa$Value,"€")
fifa$Values <- str_replace_all(fifa$Values,"K", "000")
fifa$Values <- str_remove_all(fifa$Values,"M")

fifa$Values <- as.numeric(fifa$Values)

# Player Wage
fifa$Wages <- str_remove_all(fifa$Wage,"€")
fifa$Wages <- str_replace_all(fifa$Wages,"K", "000")

fifa$Wages <- as.numeric(fifa$Wages)

df<- fifa %>% mutate(Values = if_else(fifa$Values < 1000 , Values * 1000000, Values))
unique(df$Position)

defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","CAM","LM","RM", "LAM", "RAM", "LCM", "RCM", "LDM", "RDM")

fifa$Wage
df %<>% mutate(Class = if_else(Position %in% "GK", "Goal Keeper",
                               if_else(Position %in% defence, "Defender",
                                       if_else(Position %in% midfielder, "Midfielder", "Forward"))))

rm(defence, midfielder)

df %<>%
  mutate(Height = round((as.numeric(str_sub(Height, start=1,end = 1))*30.48) + (as.numeric(str_sub(Height, start = 3, end = 5))* 2.54)),
         Weight = round(as.numeric(str_sub(Weight, start = 1, end = 3)) / 2.204623))

colnames(df)
library(dplyr)
df%>%
  rename(
    "Heading.Accuracy"= "HeadingAccuracy",
    "Short.Passing"= "ShortPassing",
    "FK.Accuracy" = "FKAccuracy",
    "Long.Passing"= "LongPassing",
    "Ball.Control"= "BallControl",
    "Sprint.Speed"= "SprintSpeed",
    "Shot.Power"= "ShotPower",
    "Long.Shots"= "LongShots",
    "Standing.Tackle"= "StandingTackle",
    "Sliding.Tackle"= "SlidingTackle",
    "GK.Diving"= "GKDiving",
    "GK.Handling"= "GKHandling",
    "GK.Kicking"= "GKKicking",
    "GK.Positioning"= "GKPositioning",
    "GK.Reflexes"= "GKReflexes")

df %<>% select(-ID, -Body.Type, -Real.Face, -Joined, -Loaned.From, -Release.Clause, -Photo, -Flag, -Special, -Work.Rate)

introduce(df)

plot_missing(df)
plot_bar(df)
View(df)
#Relation between age and wage

ggplot(df, aes(Age, Wages))+
  geom_hex()+
  facet_wrap(League~., scales = "free")+
  scale_fill_viridis_c()+
  theme_minimal()


## Position vs International Reputation

ggplot(df)+
  geom_jitter(aes(Position, International Reputation, color = Class))+
  theme_minimal()+
  theme(legend.position = "top")+
  labs(y = "International Reputation")

## Distribution & The Average Age
summ <- df %>% 
  group_by(League) %>% 
  summarise(age = mean(Age))

options(repr.plot.width = 12, repr.plot.height = 8)

ggplot()+
  geom_histogram(df, mapping = aes(Age, fill = League))+
  geom_vline(summ, mapping = aes(xintercept = age), color = "red", size = 1.5)+
  geom_text(summ, mapping = aes(x = age+3, y = 65, label = round(age,digits = 2)))+
  facet_wrap(League~.)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(y = "Frequency", title = "Distribution & The Average Age of The Players in each League")

##1. Number of players playing in Europe
View(df)
world_map <- map_data("world")
library("data.table")
setnames(df,old=c("Nationality"),new= c("region"))
str(df)
library("dplyr")
numofplayers <- world_map %>% 
  mutate(region = as.character(region)) %>% 
  left_join( df,by=c("region","region"), all.x=TRUE)
             
             
             
             mutate(region = as.character(region),
                           region = if_else(region %in% "England","UK", region)))) 

%>%count(region, name = "Number of Player")

ggplot(numofplayers, aes(long, lat, group = group))+
  geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = TRUE)+
  scale_fill_viridis_c(option = "C")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(fill = "Number of Player",
       title = "Players playing in Europe",
       subtitle = "Player distribution from all over the world",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")



#Distribution of the Position Class in every League

df %>% group_by(League) %>% count(Class) %>% 
  ggplot(aes(League, n, fill = Class)) +
  geom_col()+
  coord_polar()+
  scale_fill_ordinal()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "Leagues", y = "Frequency",
       title = "Position distribution in each league",
       subtitle = "Player positions in the top 8 leagues",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")
)


##According to Age, Comparison Average Potential & Overall

df %>% 
  group_by(League, Age) %>% 
  summarise(Overall = mean(Overall),
            Potential = mean(Potential)) %>% 
  ggplot()+
  geom_line(aes(Age, Potential, color = "Potential")) +
  geom_line(aes(Age, Overall, color = "Overall"), alpha = 0.5) +
  facet_wrap(League~.)+
  scale_color_manual(values = c("blue", "red"))+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(color = NULL,
       title = "Comparing Average Potential and Overall Rating with Age",
       subtitle = "Visualizing the changes of potential over age",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")

## Contract Valid

options(repr.plot.width = 8, repr.plot.height = 5)

df <- df %>% 
  mutate(Contract.Valid.Until = as.numeric(
    str_sub(Contract.Valid.Until, str_length(Contract.Valid.Until)-3, str_length(Contract.Valid.Until))
  )) 

df %>% 
  group_by(Contract.Valid.Until, League) %>% 
  count() %>%
  ungroup() %>% 
  ggplot(aes(Contract.Valid.Until, n, color = League))+
  geom_line(size = 1.2)+
  theme_light()+
  scale_color_manual(values = c("seagreen", "royalblue", "orchid", "orange", "gray", "tomato", "navy", "red"))

df %>% 
  group_by(Contract.Valid.Until, League) %>% 
  count() %>%
  ungroup() %>% 
  ggplot(aes(Contract.Valid.Until, n, color = League))+
  geom_line(size = 1.2)+
  theme_light()+
  scale_color_manual(values = c("seagreen", "royalblue", "orchid", "orange", "gray", "tomato", "navy", "red"))+
  facet_wrap(League~.)



##The Best Abilities of The Best Forward Players¶

df %>% 
  arrange(-Overall) %>% 
  filter(Class == "Forward") %>% 
  head(5) %>% 
  select(Name, Crossing:Sliding.Tackle) %>% 
  gather(variables, Exp, -Name) %>% 
  group_by(Name) %>%
  arrange(-Exp) %>% 
  do(head(., 5)) %>% 
  ungroup() %>% 
  mutate(variables = reorder_within(variables, Exp, Name)) %>% 
  ggplot(aes(variables, Exp, fill = Name))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = Exp), position = position_stack(vjust = 0.5), color = "gold")+
  facet_wrap(Name~., scales = "free_y")+
  scale_x_reordered()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#DA291C", "#DA291C","#DA291C", "#004170", "#004170","#004170","#004170", "#004170",
                               "#DA291C", "#DA291C","#DA291C", "#DA291C"))+
  labs(x = "Skills", y = "Frequency", title = "Forward Players with the best abilities",
       subtitle= "Best foward players with respect to skills and abilities", caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")
)



# Top 20 wonderkid
df %>% 
  filter(Age < 20, Potential > 69) %>% 
  arrange(-Potential)%>% 
  group_by(Age)%>%
  do(head(.,10))%>%
  ggplot(aes(reorder(paste0(paste(Name, Position, sep =", "), " (",Club, ")"), Potential), Potential, fill = as.factor(Age)))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  facet_wrap(Age~., scales = "free")+
  labs(x = NULL)

#The Most Powerful Clubs

powerful <- df %>% 
  group_by(Club) %>% 
  summarise(mean = mean(Overall)) %>% 
  arrange(-mean) %>% 
  head(5)


df %>% 
  group_by(Club, Class) %>% 
  summarise(mean = mean(Overall)) %>% 
  ungroup() %>% 
  filter(Club %in% powerful$Club) %>% 
  ggplot(aes(reorder(Club, mean), mean, fill = Class))+
  geom_col(position = "fill")+
  geom_text(aes(label = round(mean,digits = 1)), position = position_fill(0.5))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "Clubs" , y = "Frequency", title = "Most powerful clubs according to overall rating",
       subtitle= "Team Power for every position", caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")


###

potential <- df %>% 
  group_by(Club) %>% 
  summarise(mean = mean(Potential)) %>% 
  arrange(-mean) %>% 
  head(5)


df %>% 
  group_by(Club, Class) %>% 
  summarise(mean = mean(Potential)) %>% 
  ungroup() %>% 
  filter(Club %in% powerful$Club) %>% 
  ggplot(aes(reorder(Club, mean), mean, fill = Class))+
  geom_col(position = "fill")+
  geom_text(aes(label = round(mean,digits = 1)), position = position_fill(0.5))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "Clubs" , y = "Frequency", title = "Most powerful clubs according to potential",
       subtitle= "Team Power for every position", caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")







### FC Barcelona and Juventus

df %>% 
  filter(Club == "Napoli") %>% 
  select(Name, Overall, Potential) %>% 
  arrange(-Overall) %>% 
  head(5) %>% 
  gather(variable, Exp, -Name) %>% 
  ggplot(aes(Name, Exp, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_fill_manual(values = c("#DA291C", "#004170"))+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(fill = NULL, x = 'Players', y="Frequency", title = "Top 5 best players of Napoli",
       subtitle = "Listed according to overall rating and average potential",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")


df %>% 
  filter(Club == "Juventus") %>% 
  select(Name, Overall, Potential) %>% 
  arrange(-Overall) %>% 
  head(5) %>% 
  gather(variable, Exp, -Name) %>% 
  ggplot(aes(Name, Exp, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_fill_manual(values = c("#DA291C", "#004170"))+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(fill = NULL, x = 'Players',y="Frequency",title = "Top 5 best players of Juventus",
       subtitle = "Listed according to overall rating and average potential",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")


## Messi vs Ronaldo
players <- df %>% 
  filter(Name %in% c("Cristiano Ronaldo", "L. Messi")) %>% 
  # Unite Name & Club variables
  mutate(Name = paste0(Name, ", ", Club)) %>%
  # Selection abilities of the players
  select(Name,Crossing:Sliding.Tackle) %>% 
  # Correction of the punctuation
  rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
  # Tranform from Variable to Observation
  gather(Skill, Exp, Crossing:`Sliding Tackle`, -Name)
head(players  )

### Messi vs Ronaldo plot


ggplot(players, aes(Skill, Exp, fill = Name))+
  geom_col(show.legend = TRUE)+
  coord_flip()+
  facet_wrap(Name~.)+
  scale_fill_manual(values = c("#DA291C", "#004170"))+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(fill = NULL, x = 'Frequency',y="Skills",title = "C.Ronaldo vs L.Messi",
       subtitle = "Showing the best 2 players according to overall skills",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")

##Best midfielders

df %>% 
  arrange(-Overall) %>% 
  filter(Class == "Midfielder") %>% 
  head(5) %>% 
  select(Name, Crossing:Sliding.Tackle) %>% 
  gather(variables, Exp, -Name) %>% 
  group_by(Name) %>%
  arrange(-Exp) %>% 
  do(head(., 5)) %>% 
  ungroup() %>% 
  mutate(variables = reorder_within(variables, Exp, Name)) %>% 
  ggplot(aes(variables, Exp, fill = Name))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = Exp), position = position_stack(vjust = 0.5), color = "gold")+
  facet_wrap(Name~., scales = "free_y")+
  scale_x_reordered()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#DA291C", "#DA291C","#DA291C", "#004170", "#004170","#004170","#004170", "#004170",
                               "#DA291C", "#DA291C","#DA291C", "#DA291C"))+
  labs(x = "Skills", y = "Frequency", title = "Midfielders with the best abilities",
       subtitle= "Best midfield players with respect to skills and abilities", caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")
)


## Top 5 defenders

df %>% 
  arrange(-Overall) %>% 
  filter(Class == "Defender") %>% 
  head(5) %>% 
  select(Name, Crossing:Sliding.Tackle) %>% 
  gather(variables, Exp, -Name) %>% 
  group_by(Name) %>%
  arrange(-Exp) %>% 
  do(head(., 5)) %>% 
  ungroup() %>% 
  mutate(variables = reorder_within(variables, Exp, Name)) %>% 
  ggplot(aes(variables, Exp, fill = Name))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = Exp), position = position_stack(vjust = 0.5), color = "gold")+
  facet_wrap(Name~., scales = "free_y")+
  scale_x_reordered()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#DA291C", "#DA291C","#DA291C", "#004170", "#004170","#004170","#004170", "#004170",
                               "#DA291C", "#DA291C","#DA291C", "#DA291C"))+
  labs(x = "Skills", y = "Frequency", title = "Defenders with the best abilities",
       subtitle= "Best defence players with respect to skills and abilities", caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")
)

## Best Goalkeepers

df %>% 
  arrange(-Overall) %>% 
  filter(Class == "Goal Keeper") %>% 
  head(3) %>% 
  select(Name, Crossing:Sliding.Tackle) %>% 
  gather(variables, Exp, -Name) %>% 
  group_by(Name) %>%
  arrange(-Exp) %>% 
  do(head(., 5)) %>% 
  ungroup() %>% 
  mutate(variables = reorder_within(variables, Exp, Name)) %>% 
  ggplot(aes(variables, Exp, fill = Name))+
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = Exp), position = position_stack(vjust = 0.5), color = "gold")+
  facet_wrap(Name~., scales = "free_y")+
  scale_x_reordered()+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_fill_manual(values = c("#DA291C", "#DA291C","#DA291C", "#004170", "#004170","#004170","#004170", "#004170",
                               "#DA291C", "#DA291C","#DA291C", "#DA291C"))+
  labs(x = "Skills", y = "Frequency", title = "Goalkeepers with the best abilities",
       subtitle= "Best goalkeepers with respect to skills and abilities", caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")
)

## De Gea vs Handanovic
players <- df %>% 
  filter(Name %in% c("De Gea", "S.Handanovi?")) %>% 
  # Unite Name & Club variables
  mutate(Name = paste0(Name, ", ", Club)) %>%
  # Selection abilities of the players
  select(Name,Crossing:Sliding.Tackle) %>% 
  # Correction of the punctuation
  rename_all(funs(gsub("[[:punct:]]", " ", .))) %>% 
  # Tranform from Variable to Observation
  gather(Skill, Exp, Crossing:`Sliding Tackle`, -Name)
head(players  )

### De Gea best


ggplot(players, aes(Skill, Exp, fill = Name))+
  geom_col(show.legend = TRUE)+
  coord_flip()+
  facet_wrap(Name~.)+
  scale_fill_manual(values = c("#DA291C", "#004170"))+
  theme_minimal()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(fill = NULL, x = 'Frequency',y="Skills",title = "C.Ronaldo vs L.Messi",
       subtitle = "Showing the best 2 players according to overall skills",
       caption="Source: https://www.kaggle.com/datasets/javagarm/fifa-19-complete-player-dataset")


View(df)


    

