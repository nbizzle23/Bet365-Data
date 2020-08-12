#Feature Engineering 

#Import data set
newkaggle_nba_2009_2011 <- read_excel("C:/Users/burke/Desktop/Kaggle Project/NBA_data-with-bet365-2009-2011-/newkaggle_nba_2009_2011.xlsx")

#Create column for home favourite or home underdog based on for if h_Road>r_Road then print hfav ifelse print hdog
newkaggle_nba_2009_2011$favdog <-  with(newkaggle_nba_2009_2011, ifelse(pro_line > 0, "hfav", "hdog"))

#Create column for if the home team won
newkaggle_nba_2009_2011$homewin <-  with(newkaggle_nba_2009_2011, ifelse(h_FinalPoints > r_FinalPoints, "Win", "Loss"))

#Betting results data frame
nbabet <- newkaggle_nba_2009_2011 %>% 
  select(pro_line,final_line,result,home_win, favdog, homewin)

#Points df
pointsnba <- newkaggle_nba_2009_2011 %>%
  select(h_1st, h_2nd, h_3rd,h_4th,h_5th,h_o1st, h_o2nd, h_o3rd,h_o4th,h_o5th,r_1st, r_2nd, r_3rd,r_4th,r_5th,r_o1st, r_o2nd, r_o3rd,r_o4th,r_o5th, h_PointsWin, r_PointsWin, pro_line, final_line, result)



