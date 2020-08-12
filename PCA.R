#PCA

#Density plots of the predictors

meltedpoints <- melt(pointsnba, id.vars = "result")

#Density plots
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



meltednewkaggle <- melt(newkaggle_nba_2009_2011, id.vars = "result")

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

