#Grahpics

#Barplot of results filled by home wins 

ggplot(nbabet, aes(result))+ 
  geom_bar(aes(fill=homewin)) + 
  theme_excel()+
  labs(title =" Handicap Results", x = "Result", y = "Count")+
  scale_fill_discrete(name = "Home Team",labels = c("Loss", "Win"))

#Barplot of results filled by home favourite /underdog

ggplot(nbabet, aes(result))+
  geom_bar(aes(fill=favdog)) + 
  theme_pander()+
  labs(title =" Handicap Results", x = "Result", y = "Count")+
  scale_fill_discrete(name = "Home",labels = c("Underdog", "Favourite"))

#Barplot of Home Team Results filled by Handicap results

ggplot(nbabet, aes(homewin)) + 
  geom_bar(aes(fill=result)) + 
  theme_hc()+
  labs(title =" Home Team Results", x = "Home", y = "Count")+
  scale_fill_discrete(name = "Handicap Result",labels = c("Loss", "Win"))


#Barplot of Home Team Results filled by home team favoourite/underdog

ggplot(nbabet, aes(homewin)) + 
  geom_bar(aes(fill=favdog)) + 
  theme_solarized()+
  labs(title =" Home Team Results", x = "Home", y = "Count")+
  scale_fill_discrete(name = "Home",labels = c("Underdog", "Favourite"))

#Barplot of Home Favourite/ Underdog filled by handicap results

ggplot(nbabet, aes(favdog))+ 
  geom_bar(aes(fill=result)) + 
  theme_foundation()+
  labs(title =" Home Fav/Dog", x = "Home Team", y = "Count")+
  scale_fill_discrete(name = "Handicap Results",labels = c("Loss", "Win"))

#Barplot of Home Favourite/ Underdog filled by home team results

ggplot(nbabet, aes(favdog))+ 
  geom_bar(aes(fill=homewin)) + 
  theme_stata()+
  labs(title =" Home Fav/Dog", x = "Home Team", y = "Count")+
  scale_fill_discrete(name = "Home Team",labels = c("Loss", "Win"))





#Density plot of betting line

ggplot(nbabet,aes(pro_line))+
  geom_density(fill="red")+
  theme_gdocs()+
  labs(title ="Pro Line", x = "Pro Line", y = "Density")

#Density plot of final line

ggplot(nbabet,aes(final_line))+
  geom_density(fill="blue")+ 
  theme_gdocs()+
  labs(title ="Final Line", x = "Pro Line", y = "Density")

# Scatterplot of pro vs final line(Spread) filled with handicap result

qplot(x=pro_line, y=final_line, data = nbabet, colour=result) + 
  theme_dark()+
  labs(title ="Distribution of Spread", x = "Pro Line", y = "Final Line")+
  scale_fill_discrete(name = "Handicap",labels = c("Loss", "Win"))


ggplot(nbabet, aes(x=pro_line, y=final_line))+
  geom_point(aes(color=result)) +
  theme_fivethirtyeight()+
  labs(title ="Distribution of Spread", x = "Pro Line", y = "Final Line")+
  scale_fill_discrete(name = "Handicap",labels = c("Loss", "Win"))



#GGpairs graphs
ggpairs(nbabet, mapping = aes(color= result)) + theme_solarized_2()


