#' Fire Emission Histograms
#'
#' This function reads in a datafram of fire and emissions data from DOI and USFS and creates histograms by state.
#' @param fires Dataframe containing DOI and USFS fire and emissions data.
#' @param agency Column number for the agency column in the fires dataframe
#' @keywords Fire Emissions Histogram
#' @export
#' @examples fire_hist(fires = x, agency = 7)
#' @import ggplot2
#' @import gridExtra
#' @import Rmisc
#' @import ggpubr
#' @import cowplot



fire_hist <- function(fires, agency){

  require(ggplot2)
  # require(grid)
  require(gridExtra)
  require(Rmisc)
  require(ggpubr)
  require(cowplot)


  colnames(fires)[agency] <- "Agency"

  #####
  ##Should the following variables be passed to the function and be dynamic?
  ##Some are used for subsetting (var.name, state.abbr) and others are used for labels (state.name, x.labs)
  var.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2","wind", "tavg", "tmax", "tmin", "precip","PDSI")
  state.abbr <- c("AZ","CA","CO","ID","MT","NV","NM","OR","UT","WA","WY")
  state.name <- c("Arizona", "California","Colorado","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
  x.labs <- c(expression(paste("PM2.5 ",mu,'g/m'^3)),
              expression(paste("PM10 ",mu,'g/m'^{3})),
              expression(paste("Lead ",mu,'g/m'^{3})),
              "CO (ppm)",
              expression(paste("NO"[2]," (ppb)")),
              expression(paste("O"[3]," (ppb)")),
              expression(paste("SO"[2]," (ppb)")),
              "Wind Speed (m/s)",
              expression(paste("Temperature (",degree,"C)")),
              expression(paste("Temperature (",degree,"C)")),
              expression(paste("Temperature (",degree,"C)")),
              "Precipitation (mm)",
              "Palmer Drought Severity Index")

  # var.full.name <- c("PM2.5 Measurements","PM10 Measurements","Lead Measurements","CO Measurements","NO2 Measurements",
  #                    "O3 Measurements","SO2 Measurements","Daily Average Wind Speed",
  #                    "Daily Average Temperature", "Daily Maximum Temperature", "Daily Minimum Temperature",
  #                    "Daily Precipitation","Palmer Drought Severity Index")
  #


  for(j in 1:length(var.name)){
    plots <- list()

    for(i in 1:length(state.abbr)){
      state <- subset(fires, State==state.abbr[i])
      #make multi-plot
      a <- ggplot2::ggplot(state, aes(get(var.name[j]), fill=Agency)) +

        ggplot2::geom_histogram(bins=30)+
        ggplot2::scale_fill_manual(values=c("grey65", "grey50"), guide=FALSE) +
        ggplot2::ggtitle(state.name[i]) +
        ggplot2::xlab(x.labs[j]) +
        ggplot2::ylab("Frequency") +
        ggplot2::theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(size = 7))

      plots[[i]] <- a
    }


    b <- ggplot2::ggplot(state, aes(get(var.name[j]), fill=Agency)) +
      ggplot2::geom_histogram(bins=30)+
      ggplot2::scale_fill_manual(values=c("grey65", "grey50"))

    extractLegend <- function(gg) {
      grobs <- ggplot2::ggplot_gtable(ggplot_build(gg))
      foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
      grobs$grobs[[foo]]
    }
    legend <- extractLegend(b)

    plots[[12]] <- cowplot::ggdraw(legend)
    lay <- rbind(c(1,1,2,2,3,3,4,4),
                 c(5,5,6,6,7,7,8,8),
                 c(9,9,10,10,11,11,NA,12))

    grDevices::pdf(paste0(var.name[j],"_AgencyComparison.pdf"), width=10, height=7, onefile=FALSE)
    figure <- gridExtra::grid.arrange(grobs=plots,layout_matrix=lay)
    print(figure)
    grDevices::dev.off()
  }
}
