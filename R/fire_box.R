#' Fire Emission Boxplot
#'
#' This function reads in a datafram of fire and emissions data from DOI and USFS and creates box plots by state.
#' @param Fires Dataframe containing DOI and USFS fire and emissions data.
#' @param i value of 1 "DOI" or 2 "USFS" for data source
#' @keywords Fire Emissions Histogram
#' @export
#' @examples
#' fire_box(Fires = x, i = 1)


fire_box <- function(Fires, i){

  # state.code <- c(4,6,8,16,30,32,35,41,49,53,56)
  state.abbr <- c("AZ","CA","CO","ID","MT","NV","NM","OR","UT","WA","WY")
  state.name <- c("Arizona", "California","Colorado","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
  # var.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2","wind", "tavg", "tmax", "tmin", "precip","PDSI")
  # threshold <- c(35, 150, .15, 35, 100, .07, 75)
  Source <- c("DOI","USFS")
  Fires <- subset(Fires, Source==Source[i])
  
  ### make multi-year box plots per state/pollutant combo
  #pdf(paste0(Source[i],"_AllStates.pdf"), height=6, width=6, paper='special')
  for(j in 1:11){ # states
    
    pdf(paste0(Source[i],"_",state.name[j],".pdf"), height=6, width=6, paper='special')
    temp <- subset(Fires, Fires$State==state.abbr[j])
    
    
    boxplot(temp$PM2.5~temp$Year, col="lightgrey",main=paste0(Source[i], " ",state.name[j], " PM2.5"),las=1, xlab="",  ylab = "", outline=FALSE)
    title(ylab = expression(paste(mu,'g/m'^3)), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    #mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext(expression(paste("Treshold = 35",mu,'g/m'^3)), side=3, line=-2,las=1, adj=.9,cex=.6)
    #mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=35, col="red", lty=2)
    
    boxplot(temp$PM10~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " PM10"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste(mu,'g/m'^3)), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    #mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext(expression(paste("Treshold = 150",mu,'g/m'^3)), side=3, line=-2,las=1, adj=.9,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=150, col="red", lty=2)
    
    boxplot(temp$Lead~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " Lead"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste(mu,'g/m'^3)), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    #mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext(expression(paste("Treshold = 0.15",mu,'g/m'^3)), side=3, line=-2,las=1, adj=.9,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=.15, col="red", lty=2)
    
    boxplot(temp$CO~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " CO"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppm", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext("Treshold = 35ppm", side=3, line=-2,las=1, adj=.9,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=35, col="red", lty=2)
    
    
    boxplot(temp$NO2~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " NO2"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppb", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext("Treshold = 100ppb", side=3, line=-2,las=1, adj=.9,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=100, col="red", lty=2)
    
    boxplot(temp$O3~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " O3"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppm", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    #mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext("Treshold = 0.07ppm", side=3, line=-2,las=1, adj=.9,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=.07, col="red", lty=2)
    
    boxplot(temp$SO2~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " SO2"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppb", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    mtext("Treshold = 75ppb", side=3, line=-2,las=1, adj=.9,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    abline(h=75, col="red", lty=2)
    
    boxplot(temp$tavg~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " tavg"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste("Temperature (",degree,"C)")), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    
    boxplot(temp$tmax~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " tmax"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste("Temperature (",degree,"C)")), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    #  mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    
    boxplot(temp$tmin~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " tmin"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste("Temperature (",degree,"C)")), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    
    boxplot(temp$precip~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " precip"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "Precipitation (mm)", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    
    boxplot(temp$wind~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " wind"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "Wind Speed (m/s)", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    #mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    
    boxplot(temp$PDSI~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " PDSI"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "Palmer Drought Severity Index", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    # mtext(paste0("n=",tapply(temp$PM2.5, temp$Year, length)),side=1,line=-1,at=1:8,cex=.6)
    # mtext(paste0("N=",nrow(temp)), side = 1, line = 2.5, adj= .9)
    
    
    
    dev.off()
    
  }
  # dev.off()
  # #########
  # graphics.off()
  
  # # if we don't need to label the outliers, go through each state, pollutant combo and choose a threshold
  # state.abbr <- c(      "AZ","CA", "CO", "ID", "MT","NV", "NM","OR","UT", "WA", "WY")
  # PM2.5_thresholds <- c(  17,  30,   15,   33,   22,  29,   14,  45,  16,   19,   11)
  # PM10_thresholds <-  c(  45,  80,   60,   60,   88,  60,   75,  45,  75,   59,   42)
  # Lead_thresholds <-  c(.065,.022, .042, .046, .056, .07, .023, .028, .07, .025, .026)
  # CO_thresholds <-    c( 1.2, 1.5,  1.1,  1.4,  1.4, 1.1,   .9, 1.5, 1.2,  1.4,   .6)
  # NO2_thresholds <-   c(  40,  30,   45,   27,   14,  36,   25,  30,  45,   30,    7)
  # # O3_thresholds
  # # SO2_thresholds
  # 
  # for(j in 1:11){
  #   
  #   temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$PM2.5 > PM2.5_thresholds[j])
  #   indexes <- list()
  #   indexes <- temp$X
  #   for(i in seq_along(indexes)){
  #     Fires[indexes[i],"PM2.5"] <-NA
  #   }
  #   temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$PM10 > PM10_thresholds[j])
  #   indexes <- list()
  #   indexes <- temp$X
  #   for(i in seq_along(indexes)){
  #     Fires[indexes[i],"PM10"] <-NA
  #   }
  #   temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$Lead > Lead_thresholds[j])
  #   indexes <- list()
  #   indexes <- temp$X
  #   for(i in seq_along(indexes)){
  #     Fires[indexes[i],"Lead"] <-NA
  #   }
  #   temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$CO > CO_thresholds[j])
  #   indexes <- list()
  #   indexes <- temp$X
  #   for(i in seq_along(indexes)){
  #     Fires[indexes[i],"CO"] <-NA
  #   }
  #   temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$NO2 > NO2_thresholds[j])
  #   indexes <- list()
  #   indexes <- temp$X
  #   for(i in seq_along(indexes)){
  #     Fires[indexes[i],"NO2"] <-NA
  #   }
  #   #temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$O3 > O3_thresholds[j])
  #   #indexes <- list()
  #   #indexes <- temp$X
  #   #for(i in seq_along(indexes)){
  #   #  Fires[indexes[i],"O3"] <-NA
  #   #}
  #   #temp <- subset(Fires, Fires$State==state.abbr[j] & Fires$SO2 > SO2_thresholds[j])
  #   #indexes <- list()
  #   #indexes <- temp$X
  #   #for(i in seq_along(indexes)){
  #   #  Fires[indexes[i],"SO2"] <-NA
  #   #}
  # 
  # }
  # 
}
