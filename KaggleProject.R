#View data frames
library(readr)
kaggle_2009 <- read_csv("C:/Users/burke/Desktop/PORTFOLIO/Kaggle Project/kaggle_2009.csv")
View(kaggle_2009)
kaggle_2010 <- read_csv("C:/Users/burke/Desktop/PORTFOLIO/Kaggle Project/kaggle_2010.csv")
View(kaggle_2010)
kaggle_nba_2009_2011 <- read_csv("C:/Users/burke/Desktop/PORTFOLIO/Kaggle Project/kaggle_nba_2009_2011.csv")
View(kaggle_nba_2009_2011)

#Get column names
colnames(kaggle_2009)
colnames(kaggle_2010)
colnames(kaggle_nba_2009_2011)

#Check if colnames are similar 
colnames(kaggle_2009) == colnames(kaggle_2010)
colnames(kaggle_2009) == colnames(kaggle_nba_2009_2011)

##Feature Engineering
library(dplyr)
#Use pipe operators to select columns of interest


#Create new column for the (projected)betting line h_oPoints- r_oPoints and add it to data frame
head(mutate(kaggle_nba_2009_2011, pro_line = h_oPoints-r_oPoints))
#Create new column for the final score difference line h_FinalPoints- r_FinalPoints and add it to data frame
head(mutate(kaggle_nba_2009_2011, final_line = h_FinalPoints-r_FinalPoints))


#Create column for home favourite or home underdog based on for if h_Road>r_Road then print hfav ifelse print hdog
kaggle_nba_2009_2011$favdog <- with(kaggle_nba_2009_2011, ifelse(h_oPoints > r_oPoints, "hfav", "hdog"))
newkaggle_nba_2009_2011$favdog <-  with(newkaggle_nba_2009_2011, ifelse(pro_line > 0, "hfav", "hdog"))
newkaggle_nba_2009_2011$homewin <-  with(newkaggle_nba_2009_2011, ifelse(h_FinalPoints > r_FinalPoints, "Win", "Loss"))

View(newkaggle_nba_2009_2011$pro_line)
View(newkaggle_nba_2009_2011$favdog)

#Export file to excel to use in SQL
library(writexl)
write_xlsx(newkaggle_nba_2009_2011,"newkaggle_nba_2009_2011.xlsx")

#Import
library(readxl)
newkaggle_nba_2009_2011 <- read_excel("~/newkaggle_nba_2009_2011.xlsx")
View(newkaggle_nba_2009_2011)

#Betting results df
nbabet <- newkaggle_nba_2009_2011 %>% 
  select(pro_line,final_line,result,home_win, favdog, homewin)

#Points df
pointsnba <- newkaggle_nba_2009_2011 %>%
  select(h_1st, h_2nd, h_3rd,h_4th,h_5th,h_o1st, h_o2nd, h_o3rd,h_o4th,h_o5th,r_1st, r_2nd, r_3rd,r_4th,r_5th,r_o1st, r_o2nd, r_o3rd,r_o4th,r_o5th, h_PointsWin, r_PointsWin, pro_line, final_line, result)

#Barplot of hpwin, hplost
library(ggplot2)
library(ggthemes)

#NEED TO EDIT COLOURS FOR GRAPHS

#Barplot of results filled by home wins 
ggplot(nbabet, aes(result))+ geom_bar(aes(fill=homewin)) + theme_excel()+
  labs(title =" Handicap Results", x = "Result", y = "Count")+
  scale_fill_discrete(name = "Home Team",labels = c("Loss", "Win"))

#Barplot of results filled by home favourite /underdog
ggplot(nbabet, aes(result))+ geom_bar(aes(fill=favdog)) + theme_pander()+
  labs(title =" Handicap Results", x = "Result", y = "Count")+
  scale_fill_discrete(name = "Home",labels = c("Underdog", "Favourite"))

#Barplot of Home Team Results filled by Handicap results
ggplot(nbabet, aes(homewin)) + geom_bar(aes(fill=result)) + theme_hc()+
  labs(title =" Home Team Results", x = "Home", y = "Count")+
  scale_fill_discrete(name = "Handicap Result",labels = c("Loss", "Win"))


#Barplot of Home Team Results filled by home team favoourite/underdog
ggplot(nbabet, aes(homewin)) + geom_bar(aes(fill=favdog)) + theme_solarized()+
  labs(title =" Home Team Results", x = "Home", y = "Count")+
  scale_fill_discrete(name = "Home",labels = c("Underdog", "Favourite"))

#Barplot of Home Favourite/ Underdog filled by handicap results
ggplot(nbabet, aes(favdog))+ geom_bar(aes(fill=result)) + theme_foundation()+
  labs(title =" Home Fav/Dog", x = "Home Team", y = "Count")+
  scale_fill_discrete(name = "Handicap Results",labels = c("Loss", "Win"))

#Barplot of Home Favourite/ Underdog filled by home team results
ggplot(nbabet, aes(favdog))+ geom_bar(aes(fill=homewin)) + theme_stata()+
  labs(title =" Home Fav/Dog", x = "Home Team", y = "Count")+
  scale_fill_discrete(name = "Home Team",labels = c("Loss", "Win"))



#Actual count
count(nbabet, result)
count(nbabet, favdog)
count(nbabet, homewin)
count(home_win)
count(nbabet,result,favdog,homewin)



#NEED TO EDIT COLOURS AND LABELS FOR GRAPHS

#Plot density plot of pro_line and Final Line
#Density plot of betting line
ggplot(nbabet,aes(pro_line))+geom_density(fill="red")+theme_gdocs()+
  labs(title ="Pro Line", x = "Pro Line", y = "Density")

#Density plot of final line
ggplot(nbabet,aes(final_line))+geom_density(fill="blue")+ theme_gdocs()+
  labs(title ="Final Line", x = "Pro Line", y = "Density")


#Plot pro line and final line
# Scatterplot of pro vs final line(Spread) filled with handicap result
qplot(x=pro_line, y=final_line, data = nbabet, colour=result) + theme_dark()+
  labs(title ="Distribution of Spread", x = "Pro Line", y = "Final Line")+
  scale_fill_discrete(name = "Handicap",labels = c("Loss", "Win"))

#This plot does not have label axises
ggplot(nbabet, aes(x=pro_line, y=final_line))+geom_point(aes(color=result)) + theme_fivethirtyeight()+
  labs(title ="Distribution of Spread", x = "Pro Line", y = "Final Line")+
  scale_fill_discrete(name = "Handicap",labels = c("Loss", "Win"))

#Combined plots
library(GGally)
#Edit different graphs
ggpairs(nbabet, mapping = aes(color= result)) + theme_solarized_2()

str(ggpairs(nbabet))

#Structure of dfs
  str(newkaggle_nba_2009_2011)
str(nbabet)
str(pointsnba)

#newkaggle_nba_2009_2011 <- newkaggle_nba_2009_2011[,-1] # drop X1 column


#Density plots of the predictors
library(reshape2)
meltedpoints <- melt(pointsnba, id.vars = "result")
head(meltedpoints)
View(meltedpoints)

library(lattice)
densityplot(~value|variable,
            data = meltedpoints,
            ##Adjust each axis so that the measurement scale is
            ## different for each panel
            scales = list(x=list(relation="free"),
                          y=list(relation="free")),
            ##'adjust' smooths the curve out
            adjust = 1.25,
            ##change the symbol on the rug for each data point
            pch = "|",
            xlab = "Predictor")



library(reshape2)
meltednewkaggle <- melt(newkaggle_nba_2009_2011, id.vars = "result")
head(meltednewkaggle)
View(meltednewkaggle)

library(lattice)
densityplot(~value|variable,
            data = meltednewkaggle,
            ##Adjust each axis so that the measurement scale is
            ## different for each panel
            scales = list(x=list(relation="free"),
                          y=list(relation="free")),
            ##'adjust' smooths the curve out
            adjust = 1.25,
            ##change the symbol on the rug for each data point
            pch = "|",
            xlab = "Predictor")
#Generated 96 plots hard to see

#Determine skewness of each predictor
# The skewness function calculates the sample skewness statistic for each predictor
library(e1071)

#Use the predictors for using only numeric columns excluding the 5ths
pointsnba1 <- newkaggle_nba_2009_2011 %>%
  select(h_1st, h_2nd, h_3rd,h_4th,h_o1st, h_o2nd, h_o3rd,h_o4th,r_1st, r_2nd, r_3rd,r_4th,r_o1st, r_o2nd, r_o3rd,r_o4th, h_PointsWin, r_PointsWin, pro_line, final_line)

#Use the predictors for pre game, 
prenbapts <- newkaggle_nba_2009_2011 %>%
  select(h_1st, h_2nd, h_3rd,h_4th,r_1st, r_2nd, r_3rd,r_4th,pro_line)


#Use the predictors for post game, 
finalnbapts <- newkaggle_nba_2009_2011 %>%
  select(h_o1st, h_o2nd, h_o3rd,h_o4th,r_o1st, r_o2nd, r_o3rd,r_o4th, h_PointsWin, r_PointsWin,final_line)

#Home df
homepts <- newkaggle_nba_2009_2011 %>%
  select(h_1st, h_2nd, h_3rd,h_4th,h_o1st, h_o2nd, h_o3rd,h_o4th,h_PointsWin)

#Road df1
roadpts <- newkaggle_nba_2009_2011 %>%
  select(r_1st, r_2nd, r_3rd,r_4th,r_o1st, r_o2nd, r_o3rd,r_o4th, r_PointsWin)

#For two predictors
skewness(pointsnba1$r_5th)
skewness(pointsnba1$final_line)


# the apply function can be used across all columns
nbaskewValues <-apply(pointsnba1, 2, skewness)
head(nbaskewValues)
nbaskewValues
mean(nbaskewValues)

#Correlation of each predictor
#To filter on between-predictor correlations
#cor can calculate the correlations between predictor variables
correlations <- cor(pointsnba1)
dim(correlations)
correlations[1:20,1:20]

#corrplot to visualize correlation structure of the data
library(corrplot)
corrplot(correlations, order = "hclust")

library(ggcorrplot)
ggcorrplot(correlations, hc.order = TRUE, type = "lower",
           ggtheme = ggplot2::theme_gray,
           outline.col = "white", lab = TRUE,insig = "blank")

#Choose the a certain amount of predictors based on skewness/ correlation

splom(~prenbapts,pch = 16, col = rgb(.2, .2, .2, .4), cex = .7)
splom(~finalnbapts, pch= 16, col = rgb(.2, .2, .2, .4), cex = .7 )

