#' Fire Emission Boxplot
#'
#' This function reads in a dataframe of fire and emissions data from DOI and USFS and creates box plots by state.
#' @param Fires Dataframe containing DOI and USFS fire and emissions data.
#' @param i value of 1 "DOI" or 2 "USFS" for data source
#' @keywords Fire Emissions Histogram
#' @export
#' @examples
#' fire_box(Fires = x, i = 1)


fire_box <- function(Fires, i){


  #####
  ##Should the following variables be passed to the function and be dynamic?
  ##They are used for subsetting (state.abbr) and labels (state.name)
  state.abbr <- c("AZ","CA","CO","ID","MT","NV","NM","OR","UT","WA","WY")
  state.name <- c("Arizona", "California","Colorado","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")


  #####
  ##Should this be passed to the function??
  Source <- c("DOI","USFS")


  Fires <- subset(Fires, Source==Source[i])

  ### make multi-year box plots per state/pollutant combo
  for(j in 1:length(state.abbr)){ # states

    pdf(paste0(Source[i],"_",state.name[j],".pdf"), height=6, width=6, paper='special')
    temp <- subset(Fires, Fires$State==state.abbr[j])


    boxplot(temp$PM2.5~temp$Year, col="lightgrey",main=paste0(Source[i], " ",state.name[j], " PM2.5"),las=1, xlab="",  ylab = "", outline=FALSE)
    title(ylab = expression(paste(mu,'g/m'^3)), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext(expression(paste("Treshold = 35",mu,'g/m'^3)), side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=35, col="red", lty=2)

    boxplot(temp$PM10~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " PM10"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste(mu,'g/m'^3)), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext(expression(paste("Treshold = 150",mu,'g/m'^3)), side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=150, col="red", lty=2)

    boxplot(temp$Lead~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " Lead"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste(mu,'g/m'^3)), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext(expression(paste("Treshold = 0.15",mu,'g/m'^3)), side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=.15, col="red", lty=2)

    boxplot(temp$CO~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " CO"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppm", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext("Treshold = 35ppm", side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=35, col="red", lty=2)


    boxplot(temp$NO2~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " NO2"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppb", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext("Treshold = 100ppb", side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=100, col="red", lty=2)

    boxplot(temp$O3~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " O3"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppm", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext("Treshold = 0.07ppm", side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=.07, col="red", lty=2)

    boxplot(temp$SO2~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " SO2"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "ppb", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)
    mtext("Treshold = 75ppb", side=3, line=-2,las=1, adj=.9,cex=.6)
    abline(h=75, col="red", lty=2)

    boxplot(temp$tavg~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " tavg"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste("Temperature (",degree,"C)")), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)

    boxplot(temp$tmax~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " tmax"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste("Temperature (",degree,"C)")), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)

    boxplot(temp$tmin~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " tmin"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = expression(paste("Temperature (",degree,"C)")), line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)

    boxplot(temp$precip~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " precip"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "Precipitation (mm)", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)

    boxplot(temp$wind~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " wind"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "Wind Speed (m/s)", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)

    boxplot(temp$PDSI~temp$Year, col="lightgrey",main=paste0(Source[i], " ", state.name[j], " PDSI"),las=1, xlab="", ylab = "", outline=FALSE)
    title(ylab = "Palmer Drought Severity Index", line = 2.5)
    title(xlab = "Year Fires Occurred", line = 2.5)


    dev.off()
  }
}
