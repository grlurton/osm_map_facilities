par(mfrow = c(1,1))
par(bg = "grey")
plot(NigeriaShp , col = 'white')
plot(MatchStratC5, col = 'red' , add = TRUE )
plot(MatchStratC4 , col = 'green' , add=TRUE)
plot(MatchStratC3 , col = 'blue' , add=TRUE)
plot(MatchStratC2  , add=TRUE)
legend(x = 12.5 , y = 7.3 , legend = c("Stage 1","Stage 2" , "Stage 3" , "Stage 4"), # puts text in the legend
       pch=3,
       col=c("black","blue","green","red") , title = 'Legend' , bg= 'white') 
title(main = 'Matching results by stage')


plot(MatchStratC5, col = factor(MatchStratC5@data$MatchingStage) , add = TRUE )
