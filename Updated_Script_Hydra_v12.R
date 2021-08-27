rm(list=ls())

###########################
## NEW DAILY REPORT CODE ##
##      FOR HYDRA        ##
###########################
#
#
# Make sure to create a folder with the current date in this format:
# YEAR-MM-DD (eg. 2021-01-20)
# 
# Data for rhino evidence should have at least the word 'Evidence' in the file name
# Data for rhino sightings should have at least the word 'Sighting' in the file name

##########################
## CHANGABLE PARAMETERS ##
##########################
Data.dir <- "C:/Users/Daikoro/Documents/R/Reporting/Hydra data"  ## Path to main folder containing all data and subfolders

TodaysDate <- as.Date(Sys.time())
TodaysDateTime <- Sys.time()

MaxAccuracy <- 25    ## Maximum accuracy for GPS-data of patrols
MinDuration <- 0.5     ## Patrols under 1 hour will be ignored
MinDistance <- 1     ## Patrols under 1 KM will be ignored
IntervalBetweenPatrols <- 2 ## time difference between patrols to be seen as different

KDE_value <- 1000    ## Can also be a number (in meters) or the word "href" 

## Active users
Phones <- c("PaulAllin", "Andy", "Barrie", "Nick", "Roan", "TA-Camp",
            "Jacques", "Craig", "Oscar2", "Oscar1", "Leonie", "Siboniso")

## Active vehicle trackers
Vehicles <- c("BarrieVehicle", "Beagle", "Dave", "Groot", "Happy", "JacquesVehicle")


##
################################################################################
## SCRIPT STARTS FROM HERE

## LOAD PACKAGES
#install.packages("pacman")
pacman::p_load(sf, tidyverse, adehabitatHR, raster, ggspatial, ggnewscale, paletteer, units, 
               grid, gridExtra, Cairo, cowplot, magick, lubridate, patchwork, ggpubr, patchwork)



###########################
## SET WORKING DIRECTORY ##
###########################
setwd(Data.dir)


#####################
## LOAD SHAPEFILES ##
#####################
OWNR.WGS <- st_read(dsn = "OWNRBufferzone.shp")
OWNR.UTM <- st_transform(OWNR.WGS, crs = 32736)

OWNR_buf <- st_buffer(x = OWNR.UTM, dist = 500)

Ekuthuleni.UTM <- st_read(dsn = "Ekuthuleni.shp")

BushRiver.WGS <- st_read(dsn = "BRL.shp")
BushRiver.UTM <- st_transform(BushRiver.WGS, crs = 32736)


#####################
## LOAD RHINO DATA ##
#####################

## Sightings
Sightings.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = "Sightings", full.names = TRUE)

Sightings <- read_delim(Sightings.file,delim=";")

Sightings.clean <- Sightings %>%
  filter(Animal == "White Rhino" | Animal == "Black Rhino")

Sightings.gps <- Sightings.clean[,c("longitude", "latitude", "dateOccurred")]


## Evidence
Evidence.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = "Evidence", full.names = TRUE)

Evidence <- read_delim(Evidence.file,delim=";")

Evidence.clean <- Evidence %>%
  filter(Animal == "White Rhino" | Animal == "Black Rhino")

Evidence.gps <- Evidence.clean[, c("longitude", "latitude", "dateOccurred")]


## Poaching evidence
Poaching.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = "Poaching", full.names = TRUE)

Poaching <- read_delim(Poaching.file,delim=";")

Poaching <- subset(Poaching, Region == "OWNR")

#Set shapes for poaching
shapes <- c(16, 14, 15, 18, 7, 10, 8, 6)
names(shapes) <- c("Gunshot", "Arrest", "Tracks", "Snare", "Suspicious object", "Suspicious person", "Poacher camp", "Border crossing")


######################
## LOAD CAMERA DATA ##
######################
CT_data <- read.csv("CT_locations.csv")

Camera.sf <- CT_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 32736)


######################
## LOAD PATROL DATA ##
######################
Patrol.files <- list.files(paste0(Data.dir, "/", TodaysDate, "/OWNR"), pattern = "tracking", full.names = TRUE)

Patrol.data <- lapply(Patrol.files, 
                      read_delim, delim = ";")

## Get names of users / vehicles
FileNames <- list.files(paste0(Data.dir, "/", TodaysDate, "/OWNR"), pattern = "tracking")
FileNames <- str_extract(FileNames, "[^_]+") ##Remove all characters after the first underscore (see file names)

N_Rows <- unlist(lapply(Patrol.data,
                        nrow)
)
NewTeamNames <- rep(FileNames, N_Rows)

Patrol.data <- bind_rows(Patrol.data)

Patrol.data$Team <- NewTeamNames

names(Patrol.data) <- c(
  "Timestamp", "Distance", "TimeDifference", "Accuracy", "Altitude", "DevideID", "Heading",
  "Latitude", "Longitude", "Speed", "CumulativeDist", "Team")


######################
## CLIP PATROL DATA ##
######################
Patrol.sf <- Patrol.data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 32736) %>%
  st_crop(OWNR_buf)


#######################
## CLEAN PATROL DATA ##
#######################
Patrol.stats <- Patrol.sf %>%
  mutate(
    Timestamp = with_tz(Timestamp, tzone = "Africa/Johannesburg")
  ) %>% 
  replace_na(list(Accuracy = 0, Speed = 0)) %>%
  subset(Accuracy < MaxAccuracy & !hour(Timestamp) %in% c(3:5)) %>% 
  arrange(Team, Timestamp) %>%
  group_by(Team) %>%
  mutate(
    TimeDifference = difftime(Timestamp, lag(Timestamp), units = "secs"),
    TimeDifference = replace_na(TimeDifference, replace = 0),
    Date = as.Date(Timestamp),
    Hour = cut(Timestamp, "hour"),
    gap = as.integer(TimeDifference >= (IntervalBetweenPatrols * 3600)),  
    Patrol = paste(Team, formatC(cumsum(gap)+1, width = 3, flag = "0"), sep=".")
  )

Patrol.sum <- Patrol.stats %>%
  ungroup() %>%
  group_by(Team, Patrol) %>%
  mutate(
    TimeDifference = difftime(Timestamp, lag(Timestamp), units = "hours"),
    TimeDifference = replace_na(TimeDifference, replace = 0)
  ) %>%
  summarise(
    Start = format(min(Timestamp), "%d-%b %H:%M"),
    End = format(max(Timestamp), "%H:%M"),
    "Distance" = sum(Distance),
    "Time on patrol" = sum(TimeDifference)
  ) %>%
  st_drop_geometry() %>%
  subset(Distance > MinDistance & `Time on patrol` > MinDuration) %>% 
  arrange(Start, Patrol)

#Store active devices for later
ActiveDevices <- unique(Patrol.sum$Team)

##Remove possible double vehicle / users
Barrie <- grep('Barrie', Patrol.sum$Team, value = TRUE)
if(n_distinct(Barrie) > 1) Patrol.stats <- Patrol.stats %>% 
  filter(Team != 'BarrieVehicle')

Jacques <- grep('Jacques', Patrol.sum$Team, value = TRUE)
if(n_distinct(Jacques) > 1) Patrol.stats <- Patrol.stats %>% 
  filter(Team != 'JacquesVehicle')

# AndyGroot <- grep("Andy|Groot", Patrol.sum$Team, value = TRUE)
# if(n_distinct(AndyGroot) > 1) Patrol.stats <- Patrol.stats %>%
#   filter(Team != 'Groot')

CampGroot <- grep("TA-Camp|Groot", Patrol.sum$Team, value = TRUE)
if(n_distinct(CampGroot) > 1) Patrol.stats <- Patrol.stats %>% 
  filter(Team != 'Groot')

PaulHappy <- grep("PaulAllin|Happy", Patrol.sum$Team, value = TRUE)
if(n_distinct(PaulHappy) > 1) Patrol.stats <- Patrol.stats %>% 
  filter(Team != 'Happy')

# LeonieLizzy <- grep("Leonie|Lizzy", Patrol.sum$Team, value = TRUE)
# if(n_distinct(NickLizzy) > 1) Patrol.stats <- Patrol.stats %>% 
#   filter(Team != 'Lizzy')

OscarDave <- grep("Oscar|Dave", Patrol.sum$Team, value = TRUE)
if(n_distinct(OscarDave) > 1) Patrol.stats <- Patrol.stats %>%
  filter(Team != 'Dave')

## Get the active patrol teams route
Patrol.sum <- Patrol.sum %>% 
  subset(Team %in% Patrol.stats$Team) %>%
  ungroup() %>% 
  dplyr::select(-Team)

Patrol.sf <- Patrol.stats %>%
  subset(Patrol %in% Patrol.sum$Patrol) 

Patrol.lines <- Patrol.sf %>% 
  group_by(Team, Patrol) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_jitter(Patrol.lines, amount = 60) #use jitter to avoid overlapping lines

MaxPatrolDate <- as.Date(max(Patrol.sf$Timestamp))
MinPatrolDate <- as.Date(min(Patrol.sf$Timestamp))

##############################
## PREPARE DATA FOR HEATMAP ##
##############################
Rhino.gps <- rbind(Sightings.gps, Evidence.gps)

Rhino.gps <- Rhino.gps %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32736) %>%
  mutate(dateOccurred = as.Date(dateOccurred)
  ) %>%
  subset(dateOccurred >=TodaysDate-9 & dateOccurred <= TodaysDate) %>%
  st_geometry() %>%
  as_Spatial()


####################
## CREATE HEATMAP ##
####################
Rhino.Map <- ggplot() +
  layer_spatial(data = OWNR.UTM, fill = "#90ee90", alpha = 0.2) +
  layer_spatial(data = Ekuthuleni.UTM, fill = "#90ee90", alpha = 0.2) +
  layer_spatial(data = BushRiver.UTM, fill = "#90ee90", alpha = 0.2) +
  ggtitle("Team activity and rhino distribution in Olifants West",
          subtitle = paste("Date:", 
                           format(MaxPatrolDate, "%d %b %y"), 
                           ". Not enough rhino data for heatmap.")
  ) +
  theme_void() +
  annotation_north_arrow(which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), location = "tr") +
  annotation_scale(height = unit(0.5, "cm"))

##Check if enough rhino points are available for heatmap
if(nrow(Rhino.gps@coords) >= 5){
  
  Rhino.kde <- kernelUD(Rhino.gps, h=KDE_value, grid=500, extent=1)
  Rhino.kde <- getvolumeUD(Rhino.kde)
  Rhino.r <- raster(as(Rhino.kde,"SpatialPixelsDataFrame"))
  Rhino.r[Rhino.r>90] <- NA
  Rhino.r <- trim(Rhino.r)
  Rhino.rp <- rasterToPoints(Rhino.r)
  Rhino.rp <- data.frame(Rhino.rp)
  colnames(Rhino.rp) <- c("Longitude", "Latitude", "Kernel")
  
  ## Create Rhino map
  Rhino.Map <- Rhino.Map +
    geom_raster(data=Rhino.rp, aes(x=Longitude, y=Latitude, fill=Kernel, alpha = Kernel)) +
    scale_fill_paletteer_c(name = "KDE", palette = "grDevices::heat.colors",
                           guide = guide_colourbar(
                             title.position = "top",
                             title.hjust = 0.5)
    ) + 
    scale_alpha(range = c(0.8, 0.3), guide = "none") +
    ggtitle(
      "Team activity and rhino distribution in Olifants West",
      subtitle = paste("Date:", 
                       format(MaxPatrolDate, "%d %b %y"), 
                       "- Rhino data from the past 10 days.")) +
    geom_contour(data=Rhino.rp, aes(x=Longitude, y=Latitude, z=Kernel), breaks=50, col='black', linetype=2, alpha=0.8)
}

#########################
## Add patrol coverage ##
#########################
RhinoPatrol <- Rhino.Map + 
  new_scale_color() + 
  geom_sf(data=Patrol.lines, aes(col=Team), size = 0.9) +
  scale_color_paletteer_d(name = "", "ggsci::category20_d3", 
                          guide = guide_legend(title.position = "top",
                                               title.hjust = 0.5,
                                               label.position = "top",
                                               label.hjust = 0.5 
                                               )) +
  theme(
    legend.spacing.y = unit(0.01, 'cm')
  )


##########################
## Add camera trap data ##
##########################
RhinoCam <- RhinoPatrol +
  new_scale("fill") +
  geom_sf(data = Camera.sf, aes(fill = str_wrap(Type, 6)), shape = 22, size = 3) +
  scale_fill_paletteer_d(name = "Camera", "ggthemes::colorblind", direction = -1,
                         guide = guide_legend(title.position = "top",
                                              title.hjust = 0.5,
                                              label.position = "bottom",
                                              label.hjust = 0.5,
                                              nrow = 2,  byrow = TRUE)) +
  guides(colour = guide_legend(override.aes = list(size=6))) 


######################################
## Add rhino and poaching data data ##
######################################
Poaching10Days <- Poaching %>%
  subset(dateOccurred >=TodaysDate-9 & as.Date(dateOccurred) <= TodaysDate) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32736)

PoachingToday <- subset(Poaching10Days, dateOccurred > (TodaysDateTime - (3600*24)))

Sightings10Days <- Sightings.clean %>%
  subset(dateOccurred >=TodaysDate-9 & as.Date(dateOccurred) <= TodaysDate) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32736) %>%
  dplyr::select(Animal, Identifier, dateOccurred) %>% 
  rename("Evidence" = Animal)

Evidence10Days <- Evidence.clean %>%
  subset(dateOccurred >=TodaysDate-9 & as.Date(dateOccurred) <= TodaysDate) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32736) %>%
  dplyr::select(Evidence, `submitted by (first name)`, dateOccurred) %>% 
  mutate(
    Evidence = "Tracks/Midden"
  ) %>% 
  rename("Identifier" = `submitted by (first name)`)

SightingsEvidence <- rbind(Sightings10Days, Evidence10Days)

SightingsEvidenceToday <- subset(SightingsEvidence, dateOccurred > (TodaysDateTime - (3600*24)))

RhinoFill <- c("black", "white", "grey")
names(RhinoFill) <- c("Black Rhino", "White Rhino", "Tracks/Midden")

FinalMap <- RhinoCam + 
  new_scale("fill") +
  geom_sf(data = SightingsEvidence, aes(fill = Evidence), shape = 24, size = 3) +
  scale_fill_manual("Rhino activity", values = RhinoFill, 
                    guide = guide_legend(title.position = "top",
                                         title.hjust = 0.5,
                                         label.position = "bottom",
                                         label.hjust = 0.5,
                                         nrow = 2,  byrow = TRUE),
  ) +
  geom_sf(data = Poaching10Days, aes(shape = str_wrap(Evidence, 7)), size = 4) +
  scale_shape_manual("Poaching", values = shapes,
                     guide = guide_legend(title.position = "top",
                                          title.hjust = 0.5,
                                          label.position = "bottom",
                                          label.hjust = 0.5,
                                          nrow = 2,  byrow = TRUE)) +
  theme(
    legend.title = element_text(face="bold"),
    legend.justification = "center",
    legend.text.align = 0.9,
    legend.spacing.x = unit(1.5, "mm"),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position = "bottom",
    legend.margin=margin(t = 0, b=0.5, l=1, unit='cm'),
    legend.text = element_text(
      margin = margin(r = 8, unit = "pt"))
  ) +
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              label.hjust = 0.5,
                              nrow = 2,  byrow = TRUE)
  )

if(nrow(SightingsEvidenceToday) > 0 ){
  FinalMap <- FinalMap +
    geom_sf(data = SightingsEvidenceToday, fill = NA, shape = 1, size = 6, stroke = 1.5, colour = "blue")
}

if(nrow(PoachingToday) > 0 ){
  FinalMap <- FinalMap +
    geom_sf(data = PoachingToday, fill = NA, shape = 1, size = 6, stroke = 1.5, colour = "red")
}
## Add TA logo
TA_logo <- image_read("TAimage.jpg")

FinalMap <- ggdraw() +
  draw_image(TA_logo, x = -0.28, y = 0.36, scale = .2) +
  draw_plot(FinalMap)


##################
## PATROL STATS ##
##################
Orders.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = "event", full.names = TRUE)

PatrolOrders <- read_delim(Orders.file,delim=";")

PatrolOrders <- PatrolOrders %>% 
  drop_na(description) %>% 
  distinct(description, .keep_all = TRUE) %>% 
  rename("Patrol orders" = description, "Patrol type" = `Survey Type`) %>% 
  mutate(
    "Time" = case_when(
      hour(dateOccurred) < 12 ~ "Morning",
      TRUE ~ "Night"
    ),
    Team = paste(`submitted by (first name)`, `submitted by (last name)`),
    Date = as_date(dateOccurred),
    `Patrol orders` = str_wrap(`Patrol orders`, width = 35)
  ) %>% 
  arrange(dateOccurred, Team) %>% 
  filter(`submitted by (first name)` == "Oscar") %>% 
  dplyr::select(Date, Team, Time, `Patrol type`, `Patrol orders`)

Order.table <- ggtexttable(PatrolOrders, rows = NULL, theme = ttheme("blank", base_size = 10)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

Order.grob <- Order.table %>% 
  tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Overview of patrol orders")

if(!exists("Order.grob")) {
  Order.grob <- "No patrol orders entered on c-more" %>% 
    ggtexttable(rows = NULL, 
                theme = ttheme("blank", base_size = 10)) %>%
    tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Overview of patrol orders")
}

## Patrol activity table
##Ensure the right units are used in the summary stats
units(Patrol.sum$Distance) <- with(ud_units, km)
Patrol.sum$Distance <- round(Patrol.sum$Distance, 1)
Patrol.sum$`Time on patrol` <- round(Patrol.sum$`Time on patrol`, 1)

##Create table for the pdf later.
Patrol.grob <- Patrol.sum %>% 
  ggtexttable(rows = NULL, theme = ttheme("blank", base_size = 10)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>% 
  tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Summary of team activity")


#####################################
## TABLE FOR POACHING OBSERVATIONS ##
#####################################
Poaching.table <- PoachingToday %>% 
  dplyr::select(dateOccurred, Identifier, Grid, Evidence, `Follow-up`) %>% 
  st_drop_geometry() %>% 
  mutate(
    dateOccurred = format(dateOccurred, format = "%Y-%m-%d %H:%M"),
    `Follow-up` = case_when(
      nchar(`Follow-up`) > 3 ~ "Yes",
      Evidence != "Tracks" & Evidence != "Suspicious person" ~ "NA",
      TRUE ~ "No"
    )
  ) %>%
  rename(c("Reported on" = dateOccurred, "Team" = Identifier, "Observation" = Evidence)) %>% 
  ggtexttable(rows = NULL, theme = ttheme("blank", base_size = 10)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>% 
  tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Reports of illegal activities")

if(!exists("Poaching.table")) {
  Poaching.table <- "No ilegal activities reported today" %>% 
    ggtexttable(rows = NULL, 
                theme = ttheme("blank", base_size = 10)) %>%
    tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Reports of illegal activities")
}

#####################
## PATROL ACTIVITY ##
#####################
PatrolHour <- Patrol.sf %>%
  mutate(Hour = as.POSIXct(Hour, tz = "Africa/Johannesburg")) %>%
  count(Team, Patrol, Hour) %>%
  group_by(Patrol) %>% 
  mutate(Activity = n / sum(n))

Timeline <- seq(from = as.POSIXct(paste(TodaysDate-1, "17:00:00"), tz = "Africa/Johannesburg"),
                to = as.POSIXct(paste(TodaysDate, "18:00:00"), tz = "Africa/Johannesburg"), by = "hours")

#create a new df with expand.grid to get all combinations of date/id
PatrolExpand <- expand.grid(Team = unique(PatrolHour$Team), Hour = Timeline)

#join original and newdf to have the frequency counts from original df
PatrolJoin <- left_join(PatrolExpand, PatrolHour, by=c("Team","Hour"))   

#replace all NA with 0 for rows which were not in original df
PatrolJoin$Activity[is.na(PatrolJoin$Activity)] <- 0

PatrolActivity <- ggplot(data=PatrolJoin, aes(x=Hour, y=Activity, colour = Team)) + 
  stat_smooth(method = 'loess', span = 0.2, se = FALSE, lwd = 0.9) +
  scale_color_paletteer_d("ggsci::category20_d3") +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  scale_y_continuous("Relative activity", expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) + 
  scale_x_datetime("Time", date_labels = "%d-%b\n%H:%M") +
  ggtitle("Team activity",
          subtitle = 
            if_else(MinPatrolDate != MaxPatrolDate,
                    paste0("Based on GPS-fixes between ", format(MinPatrolDate, "%d% %b"), " - ", format(MaxPatrolDate, "%d% %b '%y")),
                    paste0("Based on GPS-fixes from ", format(MinPatrolDate, "%d% %b '%y"))
            )
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(margin = margin(r = 10, unit = "pt", l = -1)), 
    legend.title=element_blank(),
    legend.spacing.y = unit(1, "mm"), 
    legend.spacing.x = unit(0.5, "mm"),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.margin=margin(t = 0, b=0.5, unit='cm'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  )

#################
## Rhino stats ##
#################
All10days <- st_drop_geometry(SightingsEvidence)

All10.sum <- All10days %>%
  mutate(Date = as.Date(dateOccurred)) %>%
  count(Identifier, Date)

# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

RhinoReports <- ggplot(data = All10.sum, aes(x=Date, y=n, fill=Identifier)) + 
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_date(name=NULL, date_labels = "%d\n%b", 
               breaks = seq(from = TodaysDate-9, to = TodaysDate, by = "1 day"), date_breaks = "day",
               limits = c(TodaysDate-9.5, TodaysDate+0.49)) +
  scale_y_continuous("N Reports", breaks = integer_breaks(), expand = expansion(mult = c(0, 0.1))) +
  ggtitle("Reports of Rhino Activity",
          subtitle = paste0("In the last 10 days (", format(TodaysDate-9, "%d% %b"), " -", format(TodaysDate, "%d% %b"), ")")) +
  scale_fill_paletteer_d(name = "Observer", "ggthemes::colorblind") +
  theme_bw() +
  theme(
    legend.text = element_text(margin = margin(r = 10, unit = "pt", l = -1)), 
    legend.title=element_blank(),
    legend.spacing.y = unit(1, "mm"), 
    legend.spacing.x = unit(0.5, "mm"),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.margin=margin(t = 0, b=0.5, unit='cm'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE, override.aes = list(size=6)))

### IDENTIFY NAMES WITHOUT TRACKING DATA TODAY
PhoneOverview <- tibble(
  Device = Phones,
  Status = NA
) %>%
  mutate(
    Status = case_when(
      Device %in% ActiveDevices & is.na(Status) ~ "Tracking working and active",
      Device %in% unique(Patrol.data$Team) & is.na(Status) ~ "Tracking working, but user inactive",
      TRUE ~ "Tracking off / app not working")
  ) %>%
  arrange(Status, Device)

VehicleOverview <- tibble(
  Device = Vehicles,
  Status = NA
) %>%
  mutate(
    Status = case_when(
      Device %in% ActiveDevices & is.na(Status) ~ "Tracking working and active",
      Device %in% unique(Patrol.data$Team) & is.na(Status) ~ "Tracking working, but user inactive",
      TRUE ~ "Device not working: Check batteries")
  ) %>%
  arrange(Status, Device)

DevicesOverview <- rbind(PhoneOverview, VehicleOverview)


Devices.table <- ggtexttable(DevicesOverview, rows = NULL, 
                             theme = ttheme("blank", base_size = 9)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

Devices.grob <- Devices.table %>% 
  tab_add_title(text = "Overview of tracking devices", face = "bold", padding = unit(0.3, "line"))


################
## CREATE PDF ##
################
SecPage <- list(PatrolActivity, RhinoReports)

layout <- "
ABB
ABB
ACC
DCC
"

Cairo::CairoPDF(file=paste0(Data.dir, "/", TodaysDate, "/OWNR_Daily Report_", TodaysDate, ".pdf"), 
                width=11, height=7)

FinalMap
grid.arrange(
  grobs = SecPage,
  ncol = 2,
  widths = 2:1,
  padding = unit(2, "line"),
  top = textGrob("Activity overview", gp=gpar(cex=1.5))
)
patchwork::wrap_elements(Devices.grob) +
  patchwork::wrap_elements(Order.grob) +
  patchwork::wrap_elements(Patrol.grob) +
  patchwork::wrap_elements(Poaching.table) +
  patchwork::plot_layout(design = layout, widths = unit(c(11,1), c('cm', 'null')))


dev.off()


####################
## Clean up the mess
rm(list=ls())

#################################################################################################################
##################################################################################################################
###########################
## NEW DAILY REPORT CODE ##
##      GRIETIJIE        ##
###########################
#
#
##########################
## CHANGABLE PARAMETERS ##
##########################
Data.dir <- "C:/Users/Daikoro/Documents/R/Reporting/Hydra Data"  ## Path to main folder containing all data and subfolders

TodaysDate <- as.Date(Sys.time())
TodaysDateTime <- Sys.time()

MaxAccuracy <- 25    ## Maximum accuracy for GPS-data of patrols
MinDuration <- 1     ## Patrols under 1 hour will be ignored
MinDistance <- 1     ## Patrols under 1 KM will be ignored
IntervalBetweenPatrols <- 2 ## min time (hours) for two patrols to be seen as two different

## Active users
Phones <- c("Golf1", "Golf2")

## Active vehicle trackers
Vehicles <- c("Shaya")


#####################
## LOAD SHAPEFILES ##
#####################
Maseke.UTM <- st_read(dsn = "GrietjieMaseke.shp")
Railway.UTM <- st_read(dsn = "Railway.shp")

########################
## LOAD POACHING DATA ##
########################

## Poaching evidence
Poaching.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = "Poaching", full.names = TRUE)

Poaching <- read_delim(Poaching.file,delim=";")

Poaching <- subset(Poaching, Region == "Grietjie")

#Set shapes for poaching
shapes <- c(16, 14, 15, 18, 7, 10, 8, 6)
names(shapes) <- c("Gunshot", "Arrest", "Tracks", "Snare", "Suspicious object", "Suspicious person", "Poacher camp", "Border crossing")

######################
## LOAD PATROL DATA ##
######################
Patrol.files <- list.files(paste0(Data.dir, "/", TodaysDate, "/Grietjie"), pattern = "tracking", full.names = TRUE)

Patrol.data <- lapply(Patrol.files, 
                      read_delim, delim = ";")

## Get names of users / vehicles
FileNames <- list.files(paste0(Data.dir, "/", TodaysDate, "/Grietjie"), pattern = "tracking")
FileNames <- str_extract(FileNames, "[^_]+") ##Remove all characters after the first underscore (see file names)

N_Rows <- unlist(lapply(Patrol.data,
                        nrow)
)
NewTeamNames <- rep(FileNames, N_Rows)

Patrol.data <- bind_rows(Patrol.data)

Patrol.data$Team <- NewTeamNames

names(Patrol.data) <- c(
  "Timestamp", "Distance", "TimeDifference", "Accuracy", "Altitude", "DevideID", "Heading",
  "Latitude", "Longitude", "Speed", "CumulativeDist", "Team")

######################
## CLIP PATROL DATA ##
######################
Patrol.sf <- Patrol.data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = 32736) %>%
  st_crop(Maseke.UTM)

Railway.UTM <- Railway.UTM %>% 
  st_crop(Maseke.UTM)

#######################
## CLEAN PATROL DATA ##
#######################
Patrol.stats <- Patrol.sf %>%
  mutate(
    Timestamp = with_tz(Timestamp, tzone = "Africa/Johannesburg")
  ) %>% 
  replace_na(list(Accuracy = 0, Speed = 0)) %>%
  subset(Accuracy < MaxAccuracy & !hour(Timestamp) %in% c(3:5)) %>% 
  arrange(Team, Timestamp) %>%
  group_by(Team) %>%
  mutate(
    TimeDifference = difftime(Timestamp, lag(Timestamp), units = "secs"),
    TimeDifference = replace_na(TimeDifference, replace = 0),
    Date = as.Date(Timestamp),
    Hour = cut(Timestamp, "hour"),
    gap = as.integer(TimeDifference >= (IntervalBetweenPatrols * 3600)),  
    Patrol = paste(Team, formatC(cumsum(gap)+1, width = 3, flag = "0"), sep=".")
  )

Patrol.sum <- Patrol.stats %>%
  ungroup() %>%
  group_by(Team, Patrol) %>%
  mutate(
    TimeDifference = difftime(Timestamp, lag(Timestamp), units = "hours"),
    TimeDifference = replace_na(TimeDifference, replace = 0)
  ) %>%
  summarise(
    Start = format(min(Timestamp), "%d-%b %H:%M"),
    End = format(max(Timestamp), "%H:%M"),
    "Distance" = sum(Distance),
    "Time on patrol" = sum(TimeDifference)
  ) %>%
  st_drop_geometry() %>%
  subset(Distance > MinDistance & `Time on patrol` > MinDuration) %>% 
  arrange(Start, Patrol)

#Store active devices for later
ActiveDevices <- unique(Patrol.sum$Team)

##Remove possible double vehicle / users
GolfShaya <- grep("Golf|Shaya", Patrol.sum$Team, value = TRUE)
if(n_distinct(GolfShaya) > 1) Patrol.stats <- Patrol.stats %>%
  filter(Team != 'Shaya')

Patrol.sum <- Patrol.sum %>% 
  subset(Team %in% Patrol.stats$Team) %>%
  ungroup() %>% 
  dplyr::select(-Team)

Patrol.sf <- Patrol.stats %>%
  subset(Patrol %in% Patrol.sum$Patrol) 

Patrol.lines <- Patrol.sf %>% 
  group_by(Team, Patrol) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  st_jitter(Patrol.lines, amount = 120) #use jitter to avoid overlapping lines

MaxPatrolDate <- as.Date(max(Patrol.sf$Timestamp))
MinPatrolDate <- as.Date(min(Patrol.sf$Timestamp))

####################
## CREATE BASEMAP ##
####################
Maseke.Map <- ggplot() +
  layer_spatial(data = Maseke.UTM, fill = "#90ee90", alpha = 0.2) +
  geom_sf(data = Railway.UTM, lwd = 1.1, lty = "dashed") +
  ggtitle("Team activity and poaching activity in Grietjie / Maseke",
          subtitle = paste("Date:", 
                           format(MaxPatrolDate, "%d %b %y"), 
                           "- Poaching data from the past 10 days.")
  ) +
  theme_void() +
  annotation_north_arrow(which_north = "true", height = unit(2, "cm"), width = unit(2, "cm"), location = "br", pad_y = unit(1.5, "cm")) +
  annotation_scale(height = unit(0.5, "cm"), location = "br")

#########################
## Add patrol coverage ##
#########################
MasekePatrol <- Maseke.Map + 
  geom_sf(data=Patrol.lines, aes(col=Team), size = 0.9) +
  scale_color_viridis_d(name = "Name", 
                        guide = guide_legend(title.position = "top",
                                             title.hjust = 0.5,
                                             label.position = "bottom",
                                             label.hjust = 0.5))

#######################
## Add poaching data ##
#######################
Poaching10Days <- Poaching %>%
  subset(dateOccurred >=TodaysDate-9 & as.Date(dateOccurred) <= TodaysDate) %>%
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>%
  st_transform(crs = 32736)

PoachingToday <- subset(Poaching10Days, dateOccurred > (TodaysDateTime - (3600*24)))

FinalMap <- MasekePatrol + 
  geom_sf(data = Poaching10Days, aes(shape = str_wrap(Evidence, 7)), colour = "black", size = 4) +
  scale_shape_manual("Poaching signs", values = shapes,
                     guide = guide_legend(title.position = "top",
                                          title.hjust = 0.5,
                                          label.position = "bottom",
                                          label.hjust = 0.5)) +
  theme(
    legend.title = element_text(face="bold"),
    legend.text.align = 0.5,
    legend.spacing.x = unit(1.5, "mm"),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.position = "bottom",
    legend.margin=margin(t = 0, b=0.5, l=1, unit='cm')
  ) +
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              label.hjust = 0.5)
  )

if(nrow(PoachingToday) > 0 ){
  FinalMap <- FinalMap +
    geom_sf(data = PoachingToday, fill = NA, shape = 1, size = 6, stroke = 1.5, colour = "blue")
}
## Add TA logo
TA_logo <- image_read("TAimage.jpg")

FinalMap <- ggdraw() +
  draw_image(TA_logo, x = -0.28, y = 0.36, scale = .2) +
  draw_plot(FinalMap)


##################
## PATROL STATS ##
##################
Orders.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = "event", full.names = TRUE)

PatrolOrders <- read_delim(Orders.file,delim=";")

PatrolOrders <- PatrolOrders %>% 
  drop_na(description) %>% 
  distinct(description, .keep_all = TRUE) %>% 
  rename("Patrol orders" = description, "Patrol type" = `Survey Type`) %>% 
  mutate(
    "Time" = case_when(
      hour(dateOccurred) < 12 ~ "Morning",
      TRUE ~ "Night"
    ),
    Team = paste(`submitted by (first name)`, `submitted by (last name)`),
    Date = as_date(dateOccurred),
    `Patrol orders` = str_wrap(`Patrol orders`, width = 35)
  ) %>% 
  arrange(dateOccurred, Team) %>% 
  filter(`submitted by (first name)` == "Golf") %>% 
  dplyr::select(Date, Team, Time, `Patrol type`, `Patrol orders`)

Order.table <- ggtexttable(PatrolOrders, rows = NULL, 
                           theme = ttheme("blank", base_size = 10)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

Order.grob <- Order.table %>% 
  tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Overview of patrol orders")

if(!exists("Order.grob")) {
  Order.grob <- "No patrol orders entered on c-more" %>% 
    ggtexttable(rows = NULL, 
                theme = ttheme("blank", base_size = 10)) %>%
    tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Overview of patrol orders")
}

##Ensure the right units are used in the summary stats
units(Patrol.sum$Distance) <- with(ud_units, km)
Patrol.sum$Distance <- round(Patrol.sum$Distance, 1)
Patrol.sum$`Time on patrol` <- round(Patrol.sum$`Time on patrol`, 1)

##Create table for the pdf later.
Patrol.grob <- Patrol.sum %>% 
  ggtexttable(rows = NULL, 
              theme = ttheme("blank", base_size = 10)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>% 
  tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Summary of team activity")

##Creating poaching table
Poaching.table <- PoachingToday %>% 
  dplyr::select(dateOccurred, Identifier, Grid, Evidence, `Follow-up`) %>% 
  st_drop_geometry() %>% 
  arrange(dateOccurred) %>% 
  mutate(
    dateOccurred = format(dateOccurred, format = "%Y-%m-%d %H:%M"),
    `Follow-up` = case_when(
      nchar(`Follow-up`) > 3 ~ "Yes",
      (Evidence != "Tracks" & Evidence != "Suspicious person") ~ "NA",
      TRUE ~ "No"
    )
  ) %>%
  rename(c("Reported on" = dateOccurred, "Team" = Identifier, "Observation" = Evidence)) %>% 
  ggtexttable(rows = NULL, 
              theme = ttheme("blank", base_size = 10)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>% 
  tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Reports of illegal activities")

if(!exists("Poaching.table")) {
  Poaching.table <- "No ilegal activities reported today" %>% 
    ggtexttable(rows = NULL, 
                theme = ttheme("blank", base_size = 10)) %>%
    tab_add_title(face = "bold", padding = unit(0.3, "line"), text = "Reports of illegal activities")
}

#####################
## PATROL ACTIVITY ##
#####################
PatrolHour <- Patrol.sf %>%
  mutate(Hour = as.POSIXct(Hour, tz = "Africa/Johannesburg")) %>%
  count(Team, Patrol, Hour) %>%
  group_by(Patrol) %>% 
  mutate(Activity = n / sum(n))

Timeline <- seq(from = as.POSIXct(paste(TodaysDate-1, "17:00:00"), tz = "Africa/Johannesburg"),
                to = as.POSIXct(paste(TodaysDate, "18:00:00"), tz = "Africa/Johannesburg"), by = "hours")

#create a new df with expand.grid to get all combinations of date/id
PatrolExpand <- expand.grid(Team = unique(PatrolHour$Team), Hour = Timeline)

#join original and newdf to have the frequency counts from original df
PatrolJoin <- left_join(PatrolExpand, PatrolHour, by=c("Team","Hour"))   

#replace all NA with 0 for rows which were not in original df
PatrolJoin$Activity[is.na(PatrolJoin$Activity)] <- 0

PatrolActivity <- ggplot(data=PatrolJoin, aes(x=Hour, y=Activity, colour = Team)) + 
  stat_smooth(method = 'loess', span = 0.2, se = FALSE, lwd = 0.9) +
  scale_color_viridis_d() +
  guides(colour = guide_legend(override.aes = list(size=6))) +
  scale_y_continuous("Relative activity", expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) + 
  scale_x_datetime("Time", date_labels = "%d-%b\n%H:%M") +
  ggtitle("Team activity",
          subtitle = 
            if_else(MinPatrolDate != MaxPatrolDate,
                    paste0("Based on data between ", format(MinPatrolDate, "%d% %b"), " - ", format(MaxPatrolDate, "%d% %b '%y")),
                    paste0("Based on data from ", format(MinPatrolDate, "%d% %b '%y"))
            )
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(margin = margin(r = 10, unit = "pt", l = -1)), 
    legend.title=element_blank(),
    legend.spacing.y = unit(1, "mm"), 
    legend.spacing.x = unit(0.5, "mm"),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.margin=margin(t = 0, b=0.5, unit='cm'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  )

####################
## Poaching stats ##
####################
Poaching.sum <- Poaching10Days %>%
  mutate(Date = as.Date(dateOccurred)) %>%
  count(Evidence, Date)

# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

PoachingReports <- ggplot(data = Poaching.sum, aes(x=Date, y=n, fill=Evidence)) + 
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_date(name=NULL, date_labels = "%d\n%b", 
               breaks = seq(from = TodaysDate-9, to = TodaysDate, by = "1 day"), date_breaks = "1 day",
               limits = c(TodaysDate-9.5, TodaysDate+0.49)) +
  scale_y_continuous("N Reports", breaks = integer_breaks(), expand = expansion(mult = c(0, 0.1))) +
  ggtitle("Reports of Poaching Activity",
          subtitle = paste0("In the last 10 days (", format(TodaysDate-9, "%d% %b"), " -", format(TodaysDate, "%d% %b"), ")")) +
  scale_fill_paletteer_d(name = "Observer", "ggthemes::colorblind") +
  theme_bw() +
  theme(
    legend.text = element_text(margin = margin(r = 10, unit = "pt", l = -1)), 
    legend.title=element_blank(),
    legend.spacing.y = unit(1, "mm"), 
    legend.spacing.x = unit(0.5, "mm"),
    panel.border = element_rect(colour = "black", fill=NA),
    legend.background = element_blank(),
    legend.position = "bottom",
    legend.margin=margin(t = 0, b=0.5, unit='cm'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE, override.aes = list(size=6)))


### IDENTIFY NAMES WITHOUT TRACKING DATA TODAY
PhoneOverview <- tibble(
  Device = Phones,
  Status = NA
) %>%
  mutate(
    Status = case_when(
      Device %in% ActiveDevices & is.na(Status) ~ "Tracking working and active",
      Device %in% unique(Patrol.data$Team) & is.na(Status) ~ "Tracking working, but user inactive",
      TRUE ~ "Tracking off / app not working")
  ) %>%
  arrange(Status, Device)

VehicleOverview <- tibble(
  Device = Vehicles,
  Status = NA
) %>%
  mutate(
    Status = case_when(
      Device %in% ActiveDevices & is.na(Status) ~ "Tracking working and active",
      Device %in% unique(Patrol.data$Team) & is.na(Status) ~ "Tracking working, but user inactive",
      TRUE ~ "Device not working: Check batteries")
  ) %>%
  arrange(Status, Device)

DevicesOverview <- rbind(PhoneOverview, VehicleOverview)

Devices.table <- ggtexttable(DevicesOverview, rows = NULL, 
                             theme = ttheme("blank", base_size = 9)) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) 

Devices.grob <- Devices.table %>% 
  tab_add_title(text = "Overview of tracking devices", face = "bold", padding = unit(0.3, "line"))


################
## CREATE PDF ##
################
SecPage <- list(PatrolActivity, PoachingReports)

layout <- "
AABB
DDBB
DDCC
DDCC
"

Cairo::CairoPDF(file=paste0(Data.dir, "/", TodaysDate, "/Grietjie_Daily Report_", TodaysDate, ".pdf"), 
                width=11, height=7)

FinalMap
grid.arrange(
  grobs = SecPage,
  ncol = 2,
  widths = 2:1,
  padding = unit(2, "line"),
  top = textGrob("Activity overview", gp=gpar(cex=1.5))
)
patchwork::wrap_elements(Devices.grob) + 
  patchwork::wrap_elements(Order.grob) +
  patchwork::wrap_elements(Patrol.grob) +
  patchwork::wrap_elements(Poaching.table) +
  patchwork::plot_layout(design = layout)

dev.off()

##############################################################################################################
########################
## MERGE THE TWO PDFS ##
########################
pacman::p_load(pdftools)

PDF.file <- list.files(paste0(Data.dir, "/", TodaysDate), pattern = ".pdf", full.names = TRUE)

pdf_combine(c(PDF.file[2], PDF.file[1]), output = paste0(Data.dir, "/", TodaysDate, "/Daily Report_", TodaysDate, ".pdf"))



