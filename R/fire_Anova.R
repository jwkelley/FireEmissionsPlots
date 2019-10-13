#' Fire Emissions and Weather Anova Charts
#'
#' This function reads in a dataframe of fire, emissions, and weather data from DOI and USFS and creates ANOVA Charts.
#' @param fires Dataframe containing DOI and USFS fire and emissions data.
#' @keywords Fire Emissions ANOVA
#' @export
#' @examples fire_ANOVA(fires = x)
#' @import dplyr
#' @import tidyr
#' @import ggpubr
#' @import gdata
#' @import ggplot2
#' @import reshape2
#' @import grid
#' @import gridExtra


library(dplyr)
library(tidyr)
library(ggpubr)
library(gdata)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
library(cowplot)

setwd("D:/R_Package_Cbone_FIRES/Code/TestPackage/")

Fires <- read.csv("Fires_AllCalcs.csv")


var.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2","wind", "tavg", "tmax", "tmin", "precip","PSDI")
state.abbr <- c("AZ","CA","CO","ID","MT","NV","NM","OR","UT","WA","WY")
state.num <- c(4,6,8,16,30,32,35,41,49,53,56)
state.name <- c("Arizona", "California","Colorado","Idaho","Montana","Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")

### divide Fires into USFS & DOI
DOI <- Fires[Fires$Source=="DOI",]
USFS <- Fires[Fires$Source=="USFS",]
plots <- list()
####################################################################
data.source <- DOI            
source <- data.source[1,"Source"]
variable <- data.source$tavg 
                 
##################################################################
aov <- aov(variable ~ State, data=data.source) 
tukey <- TukeyHSD(aov)
file <- as.data.frame(tukey$State)
file <- subset(file, select = c("p adj"))
file$States <- rownames(file)
file$`p adj` <- round(file$`p adj`,2)
file <- as.data.frame(file) %>% separate(States, into = c("State1","State2"))
rownames(file) <- c(seq(1:55))
file <- file[c(2,3,1)]


file$p <- cut(file$"p adj",breaks = c(0,0.01,0.05,0.10,1.001),right = FALSE)

plots[[5]] <- ggplot(file, aes(State1, State2)) + 
  geom_tile(aes(fill = p), color="white") +
  scale_y_discrete( position = "right") +
  scale_fill_manual(labels=c("0.00-0.01","0.01-0.05", "0.05-0.10", "0.10-1.00"),
                    values=c("#e31a1c","#fd8d3c","#fecc5c","#ffffb2")) +
  labs(fill = "P Value") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Average Temperature") +
  theme(legend.position="none")


##### store legend ###################################################################
b <- ggplot(file, aes(State1, State2)) + 
          geom_tile(aes(fill = p), color="white") +
          scale_y_discrete( position = "right") +
          scale_fill_manual(labels=c("0.00-0.01","0.01-0.05", "0.05-0.10", "0.10-1.00"),
            values=c("#e31a1c","#fd8d3c","#fecc5c","#ffffb2")) +
         labs(fill = "P Value") +
          xlab(NULL) +
          ylab(NULL) 

extractLegend <- function(gg) {
  grobs <- ggplot_gtable(ggplot_build(gg))
  foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
  grobs$grobs[[foo]]
}
legend <- extractLegend(b)
plots[[6]] <- ggdraw(legend)
############################################################################

# lay <- rbind(c(1,1,1,2,2,2,3,3,3,4,4,4),
             # c(5,5,5,6,6,6,7,7,7,NA,8,NA))
lay <- rbind(c(1,1,1,2,2,2,3,3,3),
             c(4,4,4,5,5,5,NA,6,NA))

# setwd("F:/Rx Fire/ANOVA/")
pdf("DOI_weather.pdf", width=10.5, height=7, onefile=FALSE)
figure <- grid.arrange(grobs=plots,layout_matrix=lay)
print(figure)
dev.off()

