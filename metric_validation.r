library(geosphere)

for (lga_name in unique(NigeriaShp@data$lga_name_matched)){
  zone <- NigeriaShp[NigeriaShp@data$lga_name_matched == lga_name , ]
  aire <- areaPolygon(zone)
  CompareSetComplete$lga_area[CompareSetComplete$LGA == lga_name] <- aire
  lga_osm <- over(zone , osm_to_match  , returnList = TRUE)
  n_features <- nrow(lga_osm[[1]])
  CompareSetComplete$n_features[CompareSetComplete$LGA == lga_name] <- n_features
}


out <- ddply(CompareSetComplete , .(LGA , matching_step) , function(data) data.frame(median = median(data$dist) , 
                                                                     aire = unique(data$lga_area) ,
                                                                     state  = unique(data$state) ,
                                                                     density = median(data$n_features / data$lga_area))
)

oo <- dcast(out, state + LGA ~  matching_step , value.var = c('median'))


ggplot(data = out , aes(density , median , col = matching_step)) +
  geom_point() +
  facet_wrap(~state , scales = 'free')


ggplot(data = oo , aes(`Stage 1` , `Stage 4` , col = state)) +
  geom_point() +
  facet_wrap(~state , scales = 'free')


library(lme4)

keep = c()
for(var in colnames(CompareSetComplete)){
  print(var)
  CompareSetComplete[,var] <- as.character(CompareSetComplete[,var])
  CompareSetComplete[is.na(CompareSetComplete[var]),var] <- 'No Data'
  print(length(unique(CompareSetComplete[,var])) )
  if (length(unique(CompareSetComplete[,var])) >= 2){
    keep <- c(keep , var)
  }
}
  


pred_df <- subset(CompareSetComplete, select = keep)

a <- lm(data = pred_df , log(as.numeric(dist)) ~ log(as.numeric(n_features) / as.numeric(lga_area))  + health_fac + 
          power_supp + admin_leve+ capital +  operator_t + state +
          matching_step )




summary(a)
plot(as.numeric(CompareSetComplete$dist) , exp(predict(a)) )
hist(exp(predict(a)) - as.numeric(CompareSetComplete$dist))

plot(density(as.numeric(CompareSetComplete$dist)))
lines(density(exp(predict(a))) , col='red' )


