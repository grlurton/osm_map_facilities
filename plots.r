par(mfrow = c(1,1))
plot(Match_complete , col = as.factor(Match_complete$MatchingStage))
legend('left' , legend = sort(unique(as.factor(Match_complete$MatchingStage))) , 
       col = 1:5 , pch = 3 ,
       cex = 0.7 , text.width = 1)



plot(MatchStratC5, col = factor(MatchStratC5@data$MatchingStage) , add = TRUE )



### Plotting Wards

##Function that gets all convex hull for all wards

#WardsCH <- function(data){
#  i <- 0
#  empty <- 'oui'
#  wardsIds <- unique(data$wardID)
#  
#  n <- length(wardsIds)
#  toPrint <- 0
#  
#  for (ID in wardsIds){
#    wards <- GetWardsConvexHull(data , ID)
#    if (!is.null(wards) & class(wards)[1] == "SpatialPolygons"){
#      wards@polygons[[1]]@ID <- ID
#      if(empty == 'non'){
#        out <- spRbind(out , wards)
#      }
#      if(empty == 'oui'){
#        wards
#        out <- wards
#        empty <- 'non'
#      }
#    }
#    perc <- round(100*i/n , 0)
#    if (perc != toPrint){
#      toPrint <- perc
#      print(paste0(toPrint , "%"))
#    }
#    i <- i+1
#  }
#  out
#}
#a <- WardsCH(MatchStratC5)
#plot(a , col = factor(a@plotOrder))


GetWardsConvexHull <- function(Data , WardID){
  data <- Data[Data@data$wardID == WardID ,]
  if (nrow(data@data) > 1){
    out <- gConvexHull(data)
    out
  }
}






par(mfrow = c(1,1))
plot(Match_complete , col = as.factor(Match_complete$MatchingStage))
legend('left' , legend = sort(unique(as.factor(Match_complete$MatchingStage))) , 
       col = 1:5 , pch = 3 ,
       cex = 0.7 , text.width = 1)

WardKano <- DHISFacilities$Level4ID[DHISFacilities$Level2 == 'kn Kano State']

DataToPlot <- Match_complete[Match_complete$wardID %in% WardKano  & !is.na(Match_complete$wardID ),]

plot(DataToPlot , col = as.factor(DataToPlot$MatchingStage))
legend('left' , legend = sort(unique(as.factor(DataToPlot$MatchingStage))) , 
       col = 1:5 , pch = 3 ,
       cex = 0.7 , text.width = 1)