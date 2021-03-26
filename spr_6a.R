
## Sprat APHIA ID = 126425
rm(list=ls())
#!/usr/bin/env Rscript
setwd("C:/Work/WoS_SPRAT")

aphia.code <- 126425


library(FLCore)
library(readxl)

data.files <- list.files("data")

landings <- read.csv("sprat landings.csv")

wos.spr <- FLStock()

wos.spr@catch <- wos.spr@landings <- 

FLQuant(
  landings$Landings,
  dim = c(1, 35, 1, 1, 1, 1),
  dimnames = list(quant = "Age", year = 1985:2019),
  quant = NULL,
  units = "t",
  iter = 1,
  fill.iter = TRUE
)

wos.spr@landings <- FLQuant(as.vector(landings$Landings), units = "Tonnes", dimnames=list(year = c(min(landings$Year):max(landings$Year))))
?FLQuant
read<-function(filename){
  cat(c("Loading data from ", filename, ".\n"))

    filename <- data.files[3]
    
    sample.data <- read_xlsx(paste("data/",filename, sep=""), sheet = "sample data") 
    length.data <- read_xlsx(paste("data/",filename, sep=""), sheet = "length data") 
       bio.data <- read_xlsx(paste("data/",filename, sep=""), sheet = "bio data") 

       raising.factor <- sample.data %>% 
         select (`sample_no`, `weight landed (kg)`, `weight sampled (kg)`) %>%
         mutate( raising_factor = `weight landed (kg)`/`weight sampled (kg)`)
       
       length.data <- left_join(length.data, raising.factor, by = NULL)
       

      length.data <- 
        length.data %>%
         mutate(raised_no = number * raising_factor)
       
}




### SPRAT SURVEY MAPS  ###

coast <- read.csv("C:/Work/europe_coast.csv")


hh <- read_xlsx("data/survey/SCOWCGFS_Q4_2011-2019.xlsx", sheet = "HH")
hh$unique.id <- paste(hh$HaulNo, hh$Year, sep="-")
hh <- data.frame(hh)
survey.years <- unique(hh$Year)

hl <- read_xlsx("data/survey/SCOWCGFS_Q4_2011-2019.xlsx", sheet = "HL")
hl <- data.frame(hl)
hl$unique.id <- paste(hl$HaulNo, hl$Year, sep="-")
hl <- hl[hl$SpecCode == aphia.code,]

temp <- tapply(hl$HLNoAtLngt, hl$unique.id, sum)

hh$SPR[match(names(temp), hh$unique.id)] <- temp

hh$SPR.cpue <- hh$SPR * (60/hh$HaulDur)


library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthhires")
library("scales")


world <- ne_countries(scale = "large", returnclass = "sf")

for(a in 1:length(survey.years)){

  plot.location <- paste0("results/SCOGFS_Q4_", survey.years[a],".png", sep="")
  plot.title    <- paste0("Sprat Abundance, SCOGFS - Q4 ", survey.years[a],".png", sep="")
  
temp  <- data.frame(x = hh$ShootLong[hh$Year==survey.years[a]], y = hh$ShootLat[hh$Year==survey.years[a]], CPUE = hh$SPR.cpue[hh$Year==survey.years[a]])
temp <- temp %>%
  arrange(CPUE) %>%
  mutate(CPUE=CPUE/1000) 
  
temp$present <- 21
temp$present[is.na(temp$CPUE)] <- 4
temp$col <- 1
temp$col[is.na(temp$CPUE)] <- 2
temp$alpha <- 0.75
temp$alpha[is.na(temp$CPUE)] <- 1
temp$CPUE <- temp$CPUE+1
temp$CPUE[is.na(temp$CPUE)] <- 0.5

p <-
  ggplot(data = world) +
     xlab("Longitude") + ylab("Latitude") +
     geom_sf(fill= "olivedrab4") +
     theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background =element_rect(fill = "aliceblue")) +
     coord_sf(xlim = c(-12, -4), ylim = c(55, 60), expand = FALSE) +
#    geom_point(data = temp.2, aes(x=x, y=y, colour = "red", label = "Zero Hauls")) +
     geom_point(data = temp, aes(x=x, y=y), size = temp$CPUE, colour = temp$col, shape = temp$present, fill= temp$col, alpha = temp$alpha) +
     theme(legend.position="top")  +
     ggtitle(plot.title)

     ggsave(plot.location, 
            p, 
            dpi = 300,
            pointsize = 4)
     }



spr.landings <- read.csv("SPR.csv")
lan.yrs <- unique(spr.landings$YEAR)

for(i in 1:length(lan.yrs)){

statsq.landings <-  tapply(spr.landings$LIVE_WEIGHT_EQUIV[spr.landings$YEAR == lan.yrs[i]], spr.landings$RECTANGLE[spr.landings$YEAR == lan.yrs[i]], sum, na.rm=T)/1000


names(statsq.landings) <- substr(names(statsq.landings), 6, 9)

statsq.landings <- statsq.landings[!is.na(statsq.landings)]

cnts <- ices.rect(names(statsq.landings))


statsq.landings <- data.frame(stat.rec = names(statsq.landings), tonnes = statsq.landings, cnts)

# p <-  
ggplot(data = world) +
    xlab("Longitude") + ylab("Latitude") +
    geom_tile(data = statsq.landings, aes(x=lon, y = lat, height = 0.5, width = 1, fill = tonnes))+
    scale_fill_gradientn(colours = rev(heat.colors(16)[3:14])) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.grid.minor.y = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background =element_rect(fill = "aliceblue"))  +  
    geom_hline(yintercept = seq(55, 60, by=0.5), color = gray(.5), linetype = "dashed", size = 0.5) +
    geom_sf(fill= "olivedrab4") +  
    coord_sf(xlim = c(-12, -4), ylim = c(55, 60), expand = FALSE) +
    ggtitle(paste("Sprat Landings, ", lan.yrs[i], sep=""))


  ggsave(paste("results/landings_", lan.yrs[i], ".jpg", sep=""), 
         p, 
         dpi = 300,
         pointsize = 4)
}
  
###################
##   Q1 Surveys  ##
##  2011 - 2019  ##
###################

hh <- read_xlsx("data/survey/SCOWCGFS_Q1_2011-2019.xlsx", sheet = "HH")
hh$unique.id <- paste(hh$HaulNo, hh$Year, sep="-")
hh <- data.frame(hh)
survey.years <- unique(hh$Year)

hl <- read_xlsx("data/survey/SCOWCGFS_Q1_2011-2019.xlsx", sheet = "HL")
hl <- data.frame(hl)
hl$unique.id <- paste(hl$HaulNo, hl$Year, sep="-")
hl <- hl[hl$SpecCode == aphia.code,]

temp <- tapply(hl$HLNoAtLngt, hl$unique.id, sum)

hh$SPR[match(names(temp), hh$unique.id)] <- temp

hh$SPR.cpue <- hh$SPR * (60/hh$HaulDur)


library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthhires")

world <- ne_countries(scale = "large", returnclass = "sf")

for(a in 1:length(survey.years)){
  
  plot.location <- paste0("results/SCOGFS_Q1_", survey.years[a],".png", sep="")
  plot.title    <- paste0("Sprat Abundance, SCOGFS - Q1 ", survey.years[a],".png", sep="")
  
  temp  <- data.frame(x = hh$ShootLong[hh$Year==survey.years[a]], y = hh$ShootLat[hh$Year==survey.years[a]], CPUE = hh$SPR.cpue[hh$Year==survey.years[a]])
  temp <- temp %>%
    arrange(CPUE) %>%
    mutate(CPUE=CPUE/1000) 
  
  temp$present <- 21
  temp$present[is.na(temp$CPUE)] <- 4
  temp$col <- 1
  temp$col[is.na(temp$CPUE)] <- 2
  temp$alpha <- 0.75
  temp$alpha[is.na(temp$CPUE)] <- 1
  temp$CPUE <- temp$CPUE+1
  temp$CPUE[is.na(temp$CPUE)] <- 0.5
  
  p <- ggplot(data = world) +
    xlab("Longitude") + ylab("Latitude") +
    geom_sf(fill= "olivedrab4") +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background =element_rect(fill = "aliceblue")) +
    coord_sf(xlim = c(-12, -4), ylim = c(55, 60), expand = FALSE) +
    #    geom_point(data = temp.2, aes(x=x, y=y, colour = "red", label = "Zero Hauls")) +
    geom_point(data = temp, aes(x=x, y=y), size = temp$CPUE, colour = temp$col, shape = temp$present, fill= temp$col, alpha = temp$alpha) +
    theme(legend.position="top")  +
    ggtitle(plot.title)
  
  ggsave(plot.location, 
         p, 
         dpi = 300,
         pointsize = 4)
}


###################
##   Q4 Surveys  ##
##  1992 - 2010  ##
###################

hh <- read_xlsx("data/survey/SCOIBTS_Q4_1990-2010.xlsx", sheet = "HH")
hh$unique.id <- paste(hh$HaulNo, hh$Year, sep="-")
hh <- data.frame(hh)
survey.years <- unique(hh$Year)

hl <- read_xlsx("data/survey/SCOIBTS_Q4_1990-2010.xlsx", sheet = "HL")
hl <- data.frame(hl)
hl$unique.id <- paste(hl$HaulNo, hl$Year, sep="-")
hl <- hl[hl$SpecCode == aphia.code,]

temp <- tapply(hl$HLNoAtLngt, hl$unique.id, sum)

hh$SPR[match(names(temp), hh$unique.id)] <- temp

hh$SPR.cpue <- hh$SPR * (60/hh$HaulDur)


library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthhires")

world <- ne_countries(scale = "large", returnclass = "sf")

for(a in 1:length(survey.years)){
  
  plot.location <- paste0("results/SCOIBTS_Q1_", survey.years[a],".png", sep="")
  plot.title    <- paste0("Sprat Abundance, SCOIBTS - Q4 ", survey.years[a],".png", sep="")
  
  temp  <- data.frame(x = hh$ShootLong[hh$Year==survey.years[a]], y = hh$ShootLat[hh$Year==survey.years[a]], CPUE = hh$SPR.cpue[hh$Year==survey.years[a]])
  temp <- temp %>%
    arrange(CPUE) %>%
    mutate(CPUE=CPUE/1000) 
  
  temp$present <- 21
  temp$present[is.na(temp$CPUE)] <- 4
  temp$col <- 1
  temp$col[is.na(temp$CPUE)] <- 2
  temp$alpha <- 0.75
  temp$alpha[is.na(temp$CPUE)] <- 1
  temp$CPUE <- temp$CPUE+1
  temp$CPUE[is.na(temp$CPUE)] <- 0.5
  
  p <- ggplot(data = world) +
    xlab("Longitude") + ylab("Latitude") +
    geom_sf(fill= "olivedrab4") +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background =element_rect(fill = "aliceblue")) +
    coord_sf(xlim = c(-12, -4), ylim = c(55, 60), expand = FALSE) +
    #    geom_point(data = temp.2, aes(x=x, y=y, colour = "red", label = "Zero Hauls")) +
    geom_point(data = temp, aes(x=x, y=y), size = temp$CPUE, colour = temp$col, shape = temp$present, fill= temp$col, alpha = temp$alpha) +
    theme(legend.position="top")  +
    ggtitle(plot.title)
  
  ggsave(plot.location, 
         p, 
         dpi = 300,
         pointsize = 4)
}

