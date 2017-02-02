 
########################################################################################
# Final Script For JBA
# Ben Smith
# 26/01/2017
# Newcastle University
########################################################################################


# ---------------------------------
# From Iteration Preperation
# ---------------------------------

# ----- Load and Fix Dates 

setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Temp_file/")
load("../../Data - JBA_Raw_Data.RData")

library(zoo) # needed for na.approx

# ++++ Temp ++++
# Origional:
# whole_dataset <- setNames(lapply(ls(pattern="tbl"), function(x) get(x)), ls(pattern="tbl"))
# gauge_names   <- sapply(ls(whole_dataset), function(x) x = substr(x,5,(nchar(x)-8)))
# New:
riv_1 = tbl_445210SG_River
riv_2 = tbl_F2290SG_River
riv_3 = tbl_F3004SG_River
rm(list=ls(pattern="tbl_"))

whole_dataset <- setNames(lapply(ls(pattern="riv"), function(x) get(x)), ls(pattern="riv"))
gauge_names   <- ls(whole_dataset)

# ---- **** ----


# (as.POSIXct(x$Date, format="%Y-%m-%d %H:%M:%OS", tz = "UTC"))
# fast_strptime

# ---------------------------------
# Main Script
# ---------------------------------

for (x in 1:length(whole_dataset)){
  
  g_name  <- gauge_names[x]
  g_data  <- whole_dataset[[x]]
  g_data$QualityCode = NULL
  
  # Convert characters dates into POSTIXct:------------------------
  g_data$ObsDate = as.POSIXct(g_data$ObsDate, format="%Y-%m-%d %H:%M:%OS", tz = "UTC")
  
  # Orders data by date as some of the data is JBA-jumbled:
  g_data <- g_data[order(g_data$ObsDate),]
  
  # Round any non-15minute dates:
  wrong.minutes = which(sapply(substr(g_data$ObsDate, 15,19), function(y) {!any(y==c("00:00", "15:00","30:00","45:00"))}))
  if (length(wrong.minutes)>0){ g_data$ObsDate[wrong.minutes] =
    as.POSIXct(round(as.double(g_data$ObsDate[wrong.minutes])/(15*60))*(15*60),
               origin=(as.POSIXct('1970-01-01', tz = "UTC")), tz = "UTC")}
  
  # Construct dataframe of all dates that should exist (used lover down):
  all.dates <- data.frame("ObsDate" = seq(min(g_data$ObsDate), max(g_data$ObsDate), "15 mins"))
  
  # Find any missing date vales, if these can be filled, fill them from all.dates:
  runs = rle(is.na(g_data$ObsDate))
  if (length(runs$values) >1){
    consec_runs = which(runs$values ==T)
    
    # find the index of the end of each set of consequtives:
    consec_ends = cumsum(runs$lengths) 
    
    # list the indexes of run ends:
    ends = consec_ends[consec_runs]
    
    # list the indexes of run starts:
    newindex = ifelse(consec_runs>1, consec_runs-1, 0)
    starts = consec_ends[newindex] + 1
    if (0 %in% newindex) starts = c(1,starts)
    
    # Fill any missing dates that are sandwiched by known dates
    for (i in 1:length(starts)){
      if (g_data$ObsDate[starts[i]-1] == all.dates$ObsDate[starts[i]-1] &
          g_data$ObsDate[ends[i]+1] == all.dates$ObsDate[ends[i]+1]) {
        g_data$ObsDate[starts[i]:ends[i]] = all.dates$ObsDate[starts[i]:ends[i]]
        # = c(all.dates$ObsDate[starts[i]]: all.dates$ObsDate[ends[i]])
      }
    }
  }
  
  # Change any levels with out dates to NA:
  dates_to_remove <-  which(is.na(g_data$ObsDate), arr.ind = TRUE)
  if (length(dates_to_remove)>0){
    g_data$Value[dates_to_remove] = NA}
  
  # Add any missing dates:
  if(length(g_data$ObsDate) < length(all.dates$ObsDate)){
    correlation.data <- merge(x = g_data,
                              y = all.dates,
                              by.x='ObsDate', by.y = "ObsDate",
                              all.x = T, all.y=T)
  }
  
  # Change any negative river levels to NA values:
  missing_data <-  which(g_data$Value <0, arr.ind = TRUE)
  if (length(missing_data)>0){ g_data$Value[missing_data] = NA}
  
  
  # ---------------------------------
  # Save the Dataframe
  # ---------------------------------

  unique_tag <- paste("RL_", g_name, sep="")
  assign(unique_tag, value = g_data)
  
  # ++++ Temp ++++
  # Origional:
  # filepath <- paste("Iteration Preperation - basic data/", unique_tag, '.Rdata', sep="")
  # save(list = unique_tag, file = filepath)
  
  # New:
  # The below works, but I don't think you need to save the data at this pint.
  # filepath <- paste("Temp_file/", unique_tag, '.Rdata', sep="")
  # save(list = unique_tag, file = filepath)
  
  # Note - you need to remove the tbl data now, and leave the RL_data 
  # ---- **** ----

}

# ---------------------------------
# Tidy the Workspace
# ---------------------------------

rm(list=ls(pattern="tbl_"), whole_dataset, g_name, gauge_names, x, g_data,
   runs, dates_to_remove, missing_data, all.dates,wrong.minutes, unique_tag)

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))#
gauge_names    <- ls(whole_dataset)


# ---------------------------------
# Interpolate data 
#
# This records the number of NAs,
# interpolates over any singular NAs
# and overwrites input data. Also
# counts number of interpolations.
# ---------------------------------

# Set up meta data table:
meta_data_patched <- data.frame("Gauge Name"= character(length(whole_dataset)),
                                "Number of NAs"= integer(length(whole_dataset)),
                                "Patched_NAs"= integer(length(whole_dataset)),
                                stringsAsFactors= FALSE)

#---------------------------------------

  # ++++ **** ++++
  # Origional:
  # for (x in 1:length(whole_dataset)){
  # g_data = data.frame("date" = whole_dataset[[x]]$ObsDate, "value" =  whole_dataset[[x]]$Value)
  # New:
whole_dataset <- lapply(whole_dataset, function(x) {
  colnames(x) <- c("date", "value")
  return(x)})

for (x in 1:length(whole_dataset)){
  g_data = whole_dataset[[x]]
  # ---- **** ----
  
  g_name  <- gauge_names[x]
  
  #---------------------------------------
  
  # Determin the number of initial errors in river level:
  origional_NAs = length(which(is.na(g_data$value)))
  
  #---------------------------------------
  
  # Replace single missing values with mean of neighbours:
  g_data$value <- na.approx(g_data$value, maxgap = 1, na.rm = F)
  
  #---------------------------------------
  
  # Determin the final number of errors in river level:
  updated_NAs = length(which(is.na(g_data$value)))
  
  #---------------------------------------
  
  # ++++ **** ++++
  # Origional:
  # Enter metadata:
  # entry    <- data.frame("Gauge Name"= g_name,
  #                        "Number of NAs"= updated_NAs,
  #                        "Patched_NAs"= origional_NAs - updated_NAs,
  #                        stringsAsFactors= FALSE)
  # meta_data_patched  <- rbind(meta_data_patched, entry) # This was origionally empty, not the predefined length above.
  #
  # New:
  meta_data_patched[x,] = c(g_name, updated_NAs, (origional_NAs - updated_NAs)) 
  # ---- **** ----
  
  #---------------------------------------
  
  # runs = rle(is.na(g_data$value))
  # if (length(runs$lengths[runs$values==T & runs$lengths==1]) >0){print("Not all single NAs were removed")
  #   } else {print("worked")}
  
  #---------------------------------------
  
  # Save dataset:
  unique_tag <- g_name
  assign(unique_tag, value = g_data)
  # filepath <- paste(unique_tag, '.Rdata', sep="") <-- Surplus
  # save(list = unique_tag, file = filepath) <-- Surplus (we do not leave workspace)
  
}

#---------------------------------------

# Save meta data:
save(meta_data_patched, file= "meta_data.Rdata")


# ---------------------------------
# ###### Tidy the Workspace #######
# ---------------------------------

rm(g_data, meta_data_patched, gauge_names, whole_dataset, x, unique_tag, g_name, correlation.data, origional_NAs, updated_NAs) # , filepath) # <-- Surplus




########################################################################################
# From Iteration_1
# Aim: calculate base level indexes for each gauge. Display these in a table and have a dataset.
# This simply produces the lowflow objects, it does not process them (I think).
########################################################################################


# ---------------------------------
# Preamble
# ---------------------------------

# install.packages("xts")
# install.packages("lfstat")
# install.packages("foreign")

library(xts)
library(lfstat)
library(foreign)

# setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Interpolated data/") <-- Surplus
# sapply(list.files(pattern="RL_"),load,.GlobalEnv)<-- Surplus
# setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/")<-- Surplus (we have not changed very start)

# Create a driectory for the output data:
if (!dir.exists("Low Flow Objects")){
  dir.create("Low Flow Objects")}


#---------------------------------------
# Calculate Base levels using WMO method
#---------------------------------------

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)

for (x in 1:length(whole_dataset)){
  
  # Create low flow objects
  g_name <- gauge_names[x]
  g_data <- whole_dataset[[x]]
  
  g_data <- xts(g_data$value, g_data$date, tzone = "UTC")
  g_data <- apply.daily(g_data, mean)
  
  g_data <- data.frame("date"  = index(g_data), "value" = coredata(g_data))
  g_data <- data.frame("flow"  = g_data$value,
                       "day"   = as.numeric(substr(g_data$date,9,10)),
                       "month" = as.numeric (substr(g_data$date,6,7)),
                       "year"  = as.numeric(substr(g_data$date,1,4)),
                       stringsAsFactors = F) 
  
  #---------------------------------------
  
  # Patch any 'trivial' missing days:
  
  g_data$flow <- na.approx(g_data$flow, maxgap = 1, na.rm = F)
  
  #---------------------------------------
  
  # Remove any long gaps:
  
  # .....
  
  #---------------------------------------
  
  # Calculate WMO Base Level:
  
  # 1 - create low flow object for analysis
  g_lfo <- suppressWarnings(createlfobj(x = g_data,
                                        baseflow = TRUE,
                                        hyearstart = 1)) # can change start of hydrolocical year (1-12)
  
  date <- paste(g_lfo$year, g_lfo$month, g_lfo$day, sep = "-")
  date <- as.POSIXct(date, format="%Y-%m-%d", tz = "UTC")
  
  # g_lfo <- cbind(date, g_lfo) <-- surplus
  
  temp_data <- data.frame("date"    = date,
                          "value"   = round(g_lfo$flow, digits=3),
                          "bl_wmo"  = round(g_lfo$baseflow, digits = 3))
  
  temp_data = temp_data[c(min(which(!is.na(temp_data$value))):
                            max(which(!is.na(temp_data$value)))),]
  
  temp_data$bl_wmo[is.na(temp_data$value)] = NA
  
  name      <- paste(g_name, "lfo", sep = "_")
  assign(name, value = temp_data)
  
  #---------------------------------------
  
  # Save low flow objects:
  # filepath <- paste("Low Flow Objects/", name,'.Rdata',sep = "") # <-- surplus
  # save(list = name, file = filepath)# <-- surplus
}

#### As far as I can tell from the test data, all dates are still there ###
#### Some Na's are now at the end of the data ###

#---------------------------------------

# Tidy workspace (leaves only lfo's):


# ++++ **** ++++
# Origional:
# rm(g_lfo, list=setdiff(ls(), ls(pattern = "*lfo")))
# whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
# New:
rm(list=setdiff(ls(), ls(pattern = "RL")))
whole_dataset  <- setNames(lapply(ls(pattern="*lfo"), function(x) get(x)), ls(pattern="lfo"))
# ---- **** ----

gauge_names    <- ls(whole_dataset)


#---------------------------------------

# Patch any 'trivial' missing days in base level data:

for (x in 1:length(whole_dataset)){
  
  g_name <- gauge_names[x]
  g_data <- whole_dataset[[x]]
  
  g_data$value <- na.approx(g_data$value, maxgap = 1, na.rm = F)
  g_data$bl_wmo <- na.approx(g_data$bl_wmo, maxgap = 1, na.rm = F)

  # ++++ **** ++++
  # Origional:
  # ...
  # New:
  # # This is to save what is done in the funciton - currently NAs are filled and forgoten.
  assign(g_name, value = g_data)
  # ---- **** ----
}

# Refill 'whole dataset' with patched data:
whole_dataset  <- setNames(lapply(ls(pattern="*lfo"), function(x) get(x)), ls(pattern="lfo"))

#---------------------------------------

# Remove any long gaps:

# ....

#---------------------------------------

# Calculate WMO Base Level indexes for each record:

# ++++ *** ++++
# Origional:
# ptm <- proc.time()
#  wmo_blis <- data.frame("gauge"  = character(),
#                         "bli_wmo" = numeric(),
#                         stringsAsFactors=F)
# 
#  for (x in 1:length(whole_dataset)){
#    g_name <- gauge_names[[x]]
#    g_data <- whole_dataset[[x]]
# 
#    bli_wmo <- sum(g_data$bl_wmo, na.rm = TRUE) / sum(g_data$value, na.rm = TRUE)
#    entry   <- data.frame("gauge" = substr(g_name, 1, (nchar(g_name)-4)),
#                          "bli_wmo" = bli_wmo,
#                          stringsAsFactors=F)
#    wmo_blis <- rbind(wmo_blis, entry)
# }
# proc.time() - ptm
#
# New:
# ptm <- proc.time()
base_level_indexes <- data.frame("gauge"  = character(length(whole_dataset)),
                                 "bli_wmo" = numeric(length(whole_dataset)),
                                 "bli_lh" = numeric(length(whole_dataset)),
                                 stringsAsFactors=F)

for (x in 1:length(whole_dataset)){
  g_name <- gauge_names[[x]]
  g_data <- whole_dataset[[x]]

  missing = sort(unique(c(which(is.na(g_data$value)), which(is.na(g_data$bl_wmo)))))
  bli_wmo <- sum(g_data$bl_wmo[-missing]) / sum(g_data$value[-missing])
  
  base_level_indexes[x,c(1,2)] <- c(substr(g_name, 1, (nchar(g_name)-4)), round(bli_wmo, digits = 3))
}
# proc.time() - ptm
# ---- **** ----
  

#---------------------------------------

# Calculate Lyne and Hollic Base Level and add BLI to BLI table:

source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

for (x in 1:length(whole_dataset)){
  
  g_name  <- gauge_names[x]
  g_data <- whole_dataset[[x]]
  
  lyne_hollick = BFI(Q = g_data$value, alpha=0.925, ReturnQbase=TRUE, passes=3)
  
  g_data$"bl_lh" <- round(lyne_hollick$Qbase, digits = 3)
  assign(g_name, value = g_data) # Changed slightly to remove whole_data[[x]]
  
  # name     <- g_name # <--- surplus
  filepath <- paste("Low Flow Objects/", g_name,'.Rdata',sep = "") # was using 'name' instead of g_name
  save(list = g_name, file = filepath) # was using 'name' instead of g_name

    # ++++ **** ++++
  # Origional:
  
# }
# 
# #---------------------------------------
# 
# whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
# gauge_names    <- ls(whole_dataset)
# 
# 
# lh_blis <- data.frame("gauge"  = character(),
#                       "bli_lh" = numeric(),
#                       stringsAsFactors=F)
# 
# #---------------------------------------
# 
# for (x in 1:length(whole_dataset)){
#   g_name <- gauge_names[[x]]
#   level  <- whole_dataset[[x]]$value
#   
#   bli <- BFI(level, alpha=0.925)
#   entry <- data.frame(gauge = substr(g_name, 1, (nchar(g_name)-4)),
#                       "bli_lh" = bli$BFI,
#                       stringsAsFactors=F)
#   lh_blis <- rbind(lh_blis, entry)
# }
#  
# #---------------------------------------
# 
# # Save BLI table:
# base_level_indexes <- data.frame("gauge"   = wmo_blis$gauge,
#                                  "BLI_wmo" = round(wmo_blis$bli_wmo, digits = 3), # <-- surplus rounding
#                                  "BLI_lh" = round(lh_blis$bli_lh, digits = 3))
#  
  # New:
  base_level_indexes[3,x] = round(lyne_hollick$BFI, digits = 3)
}
#---- **** ----
  
# Save BLI table:
filepath <- paste("Low Flow Objects/", "Base Level Indexes", '.Rdata',sep = "")
save(base_level_indexes, file = filepath)


#---------------------------------------

# Tidy the Workspace
#rm(bli, filepath, x, BFI, bli_wmo, name,r_level, wmo_blis, entry, g_data, lh_blis, g_data, lyne_hollick, g_name, level)
