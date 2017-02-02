 



########################################################################################
# Final Script For JBA
# Ben Smith
# 26/01/2017
# Newcastle University
########################################################################################


# QUESTIONS #
# Would you like to save the processed/interpolated data to disk midway through the script?
# Alter the below variable accordingly... (y/n)
save_data <- "y"  # <--- This should be "y" for save, and "n" for do not save


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
  
  # Save dataset to workspace:
  unique_tag <- g_name
  assign(unique_tag, value = g_data)
}

# Save dataset to disk:
if (save_data = "y"){
  
  whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
  gauge_names    <- ls(whole_dataset)
  
  # Create a file for data:
  if (!dir.exists("Interpolated Data")){dir.create("Interpolated Data")}
  
  # Save the data to the created folder:
  for (x in 1:length(whole_dataset)){
    
    filepath <- paste('Interpolated Data/', gauge_names[x], '.Rdata', sep="")
    save(list = gauge_names[x], file = filepath)
  }
}


#---------------------------------------

# Save meta data:
save(meta_data_patched, file= "meta_data.Rdata")


# ---------------------------------
# ###### Tidy the Workspace #######
# ---------------------------------

rm(g_data, meta_data_patched, gauge_names, whole_dataset, x, unique_tag, g_name, correlation.data, origional_NAs, updated_NAs, filepath, gauge_names)







########################################################################################
# From Iteration_1
#
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
  base_level_indexes[x,3] = round(lyne_hollick$BFI, digits = 3)
}
#---- **** ----
  
# Save BLI table:
filepath <- paste("Low Flow Objects/", "Base Level Indexes", '.Rdata',sep = "")
save(base_level_indexes, file = filepath)


#---------------------------------------

# Tidy the Workspace
rm(missing, filepath, x, BFI, bli_wmo, lyne_hollick, g_name, base_level_indexes)



################################################################################
# Iteration_2
# Ben Smith
# 02/02/2017
# Newcastle University
#
# Aim: Investigate BLI fluctuated if only peak levels are analysed
# This is not included as it is auxilary information
################################################################################



################################################################################
# Iteration_3
# Ben Smith
# 02/02/2017
# Newcastle University
#
# Aim: Identify rapid rates of rise
################################################################################


# ---------------------------------
# Preamble
# ---------------------------------

# Data should still loaded into the workspace, else it can be loaded using the bewlow:
# setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Temp_file/Interpolated data/")
# sapply(list.files(pattern="RL_"),load,.GlobalEnv)
# setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/")

whole_dataset  <- setNames(lapply(setdiff(ls(pattern="RL"), ls(pattern = "*lfo")),
                                  function(x) get(x)),
                           setdiff(ls(pattern="RL"), ls(pattern = "*lfo")))
gauge_names    <- ls(whole_dataset)


# ----------- PART 1 -------------
# --------------------------------
# ### Calc max rates of rise   ###
# ----- Only run this once ----- #
# --------------------------------

for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  
  rise <- numeric(length(g_data$date))
  
  for (j in 4:(length(g_data$date))){
    
    rise[1:3] = NA
    
    r = c(g_data$value[(j-3):j])
    
    d = diff(r)
    
    d[is.na(d)] = 0 
    
    if (all(d <=0 )) { d = 0
    } else if (all(d >=0 )) { d = sum(d)
    } else if (d[1] & d[2] <=0){ d = d[3]
    } else if (d[1] & d[3] <= 0) { d = d[2]
    } else if (d[2] & d[3] <= 0) { d = d[1]
    } else if (d[1] <= 0) { d = sum(d[2:3])
    } else if (d[3] <= 0) { d = sum(d[1:2])
    } else {d = sum(d)}
    
    rise[j] = d
  }
  
  whole_dataset[[i]]$"rise" = rise
  assign(x = g_name, value = whole_dataset[[i]])
  
}

rm(d, g_name, i, j, r, rise)



# ---------- PART 1b -------------
# --------------------------------
# ### plotting rates of rise   ###
# --------------------------------

# Plot events:

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)

for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  y_lim  <- c(0, summary(g_data$value)[6])
  g_data <- g_data[(length(g_data$date) - 50000) : length(g_data$date),]
  
  
  plot(g_data$date, g_data$value, type = "l", ylim = y_lim, main = g_name,
       xlab = "Date", ylab = "Level (m)")
  
  threshold = (quantile(g_data$value, probs = 0.50, na.rm = T) - 
                 quantile(g_data$value, probs = 0.05, na.rm = T))/4
  lines(g_data$date[g_data$rise > threshold], g_data$value[g_data$rise > threshold],
        type = "p", cex = 0.5, pch = 16, col = 4)
  
  threshold = (quantile(g_data$value, probs = 0.62, na.rm = T) - 
                 quantile(g_data$value, probs = 0.05, na.rm = T))/4
  lines(g_data$date[g_data$rise > threshold], g_data$value[g_data$rise > threshold],
        type = "p", cex = 0.5, pch = 16, col = 3)
  
  threshold = (quantile(g_data$value, probs = 0.75, na.rm = T) - 
                 quantile(g_data$value, probs = 0.05, na.rm = T))/4
  lines(g_data$date[g_data$rise > threshold], g_data$value[g_data$rise > threshold],
        type = "p", cex = 0.5, pch = 16, col = 2)
  
}

# Plot occurences:

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)

for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  y_lim  <- c(0, summary(g_data$value)[6])
  
  # -----
  
  threshold = (quantile(g_data$value, probs = 0.50, na.rm = T) - 
                 quantile(g_data$value, probs = 0.05, na.rm = T))/4
  
  rise = numeric(length = length(g_data$rise))
  rise [g_data$rise > threshold] = 1
  
  plot(g_data$date, g_data$value, type = "l",
       main = g_name, xlab = "level", ylab = "date", ylim = c(0,2))
  
  # -----
  
  lines(g_data$date[g_data$rise > threshold],
        rise[g_data$rise > threshold],
        type = "p", main = g_name, cex = 0.5, pch = 16, col = 4)
  
  # -----
  
  threshold = (quantile(g_data$value, probs = 0.62, na.rm = T) - 
                 quantile(g_data$value, probs = 0.05, na.rm = T))/4
  
  rise = numeric(length = length(g_data$rise))
  rise [g_data$rise > threshold] = 1
  
  lines(g_data$date[g_data$rise > threshold],
        rise[g_data$rise > threshold],
        type = "p", main = g_name, cex = 0.5, pch = 16, col = 3)
  
  # -----
  
  threshold = (quantile(g_data$value, probs = 0.75, na.rm = T) - 
                 quantile(g_data$value, probs = 0.05, na.rm = T))/4
  
  rise = numeric(length = length(g_data$rise))
  rise [g_data$rise > threshold] = 1
  
  lines(g_data$date[g_data$rise > threshold],
        rise[g_data$rise > threshold],
        type = "p", main = g_name, cex = 0.5, pch = 16, col = 2)
  
}


# Calculate proportions ---------------------------------------------------

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)


prop_rise = data.frame("gauge" = character(),
                       "Qt.ft" = integer(),
                       "Qt.st"   = integer(),
                       "Qt.svt"   = integer(),
                       stringsAsFactors = FALSE,
                       row.names = integer())

for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  
  # -----
  tot = length(g_data$value) - sum(is.na(g_data$value))
  
  
  Qt.ft <- (quantile(g_data$value, probs = 0.50, na.rm = T) - 
              quantile(g_data$value, probs = 0.05, na.rm = T))/4
  Qt.st <- (quantile(g_data$value, probs = 0.62, na.rm = T) - 
              quantile(g_data$value, probs = 0.05, na.rm = T))/4
  Qt.svt <- (quantile(g_data$value, probs = 0.75, na.rm = T) - 
               quantile(g_data$value, probs = 0.05, na.rm = T))/4
  
  Qt.ft = length(which(g_data$rise > Qt.ft)) * 100/tot
  Qt.st = length(which(g_data$rise > Qt.st)) * 100/tot
  Qt.svt = length(which(g_data$rise > Qt.svt)) * 100/tot
  
  entry = data.frame("gauge" = g_name,
                     "Qt.ft" = round(Qt.ft, digits = 2),
                     "Qt.st" = round(Qt.st, digits = 2),
                     "Qt.svt" = round(Qt.svt, digits = 2),
                     stringsAsFactors = FALSE, row.names = i)
  
  prop_rise = rbind(prop_rise, entry)
}

View(prop_rise)



# ---------- PART 2 -------------
# -------------------------------
# ## Remove BL for River Level ##
# -------------------------------
# Uses function-Normalise River...

# THIS WILL CLEAR YOUR WORKSPACE!
rm(list = ls())

# Reload all data to workspace:
setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Interpolated data/")
sapply(list.files(pattern="RL_"),load,.GlobalEnv)

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)

#-------

setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Low Flow Objects/")
sapply(list.files(pattern="RL_"),load,.GlobalEnv)
setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/")

lf_dataset  <- setNames(lapply(ls(pattern="lfo"), function(x) get(x)), ls(pattern="lfo"))
lf_names    <- ls(lf_dataset)

#---------------------------------------

for (i in 1:length(lf_dataset)){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  
  lf_name <- lf_names[i]
  lf_data <- lf_dataset[[i]]
  
  g_data$day <- substr(g_data$date,1,10)
  
  #---------------------------------------
  
  # Group the 15 minute record into days:
  runs <- rle(g_data$day)
  
  #---------------------------------------
  
  # Note the end point of each day's record:
  consec_runs <- which(runs$lengths >= 1) 
  consec_ends <- cumsum(runs$lengths) 
  ends        <- consec_ends[consec_runs]
  
  #---------------------------------------
  
  # Determine the starting point of each day's record:
  newindex <- ifelse(consec_runs>1, consec_runs-1, 0)
  starts   <- consec_ends[newindex] + 1
  if (0 %in% newindex) starts <- c(1,starts)
  
  #---------------------------------------
  
  # Set up a blank vector to recieve normalised values:
  normalised <- numeric(length(g_data$date))
  
  for (j in 1:length(starts)){
    
    starts_j <- starts[j]
    ends_j   <- ends[j]
    
    # Determine whether the days in the daily record have a calcuated BL:
    z <- which(lf_data$date == as.POSIXct(g_data$day[starts_j], tz = "UTC"))
    if (length(z)>=1){
      
      # Normalise the river level is so:
      RL <- g_data$value[c(starts_j:ends_j)]
      BL <- lf_data$bl_wmo[z]
      normalised[starts_j:ends_j] <- RL-BL
      
      # If there is not a BL value, fill the space with NA's
    } else{
      normalised[starts_j:ends_j] <- NA
    }
  }
  
  # Make any over normalised (negative values) equal to 0:
  normalised[which(normalised<=0)] <- 0
  
  #---------------------------------------
  
  # keep the normalised data:
  whole_dataset[[i]]$"q_level" = normalised
  assign(x = g_name, value = whole_dataset[[i]])
  
  # Save the data:
  name     <- g_name
  filepath <- paste(name,'.Rdata',sep = "")
  save(list = name, file = filepath)
  
}

#---------------------------------------

# Tidy workspace:
rm(BL,consec_ends, consec_runs, i, newindex, RL, runs, z, ends, ends_j, g_name, j, lf_name, normalised, starts, starts_j, lf_names, lf_dataset, g_data, lf_data, filepath, name)

rm(list= ls(pattern = "lfo"))



# ---------- PART 2b-------------
# -------------------------------
# ---- Analyse normalised RL ----
# -------------------------------

# Determine number of events over threshold:

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)


ql_rise = data.frame("gauge" = character(),
                     "Qt.tf" = integer(),
                     "Qt.ft"   = integer(),
                     "Qt.stf"   = integer(),
                     stringsAsFactors = FALSE,
                     row.names = integer())

for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  
  # -----
  tot = length(g_data$value) - sum(is.na(g_data$value))
  
  threshold <- quantile(g_data$value, probs = 0.95, na.rm = T)
  
  Qt.tf  <- length(which(g_data$q_level >= 0.25*threshold)) * 100/tot
  Qt.ft  <- length(which(g_data$q_level >= 0.50*threshold)) * 100/tot
  Qt.stf <- length(which(g_data$q_level >= 0.75*threshold)) * 100/tot
  
  entry = data.frame("gauge" = g_name,
                     "Qt.tf" = round(Qt.tf, digits = 2),
                     "Qt.ft" = round(Qt.ft, digits = 2),
                     "Qt.stf" = round(Qt.stf, digits = 2),
                     stringsAsFactors = FALSE, row.names = i)
  
  ql_rise = rbind(ql_rise, entry)
}

View(ql_rise)

# --------- PART 2b.2 -----------
# -------------------------------
# ---- Analyse normalised RL ----
#  This time only for those >95%


whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)


ql_rise_nf = data.frame("gauge" = character(),
                        "Qt.tf" = integer(),
                        "Qt.ft"   = integer(),
                        "Qt.stf"   = integer(),
                        stringsAsFactors = FALSE,
                        row.names = integer())

for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  
  # -----
  
  threshold <- quantile(g_data$value, probs = 0.95, na.rm = T)
  index = which(g_data$value >= threshold)
  tot = length(index)
  
  Qt.tf  <- length(which(g_data$q_level[index] >= 0.25*threshold)) * 100/tot
  Qt.ft  <- length(which(g_data$q_level[index] >= 0.50*threshold)) * 100/tot
  Qt.stf <- length(which(g_data$q_level[index] >= 0.75*threshold)) * 100/tot
  
  entry = data.frame("gauge" = g_name,
                     "Qt.tf" = round(Qt.tf, digits = 2),
                     "Qt.ft" = round(Qt.ft, digits = 2),
                     "Qt.stf" = round(Qt.stf, digits = 2),
                     stringsAsFactors = FALSE, row.names = i)
  
  ql_rise_nf = rbind(ql_rise_nf, entry)
}

View(ql_rise_nf)


# -------------------------------

# Plot Data:
pdf(file= "/PhD Documents/Task 1/R Scripts/Iterations/Iteration 3_2b.pdf")

for (i in 1:length(lf_dataset)){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  g_data <- g_data[(length(g_data$date)-50000) : length(g_data$date),]
  
  lf_name <- lf_names[i]
  lf_data <- lf_dataset[[i]]
  
  # -------
  
  plot(g_data$date, g_data$value, ylim = c(0, max(g_data$value, na.rm = T)),
       type = "l", xlab = "Date", ylab = "Level (m)", main = g_name)
  
  # -------
  
  lines(g_data$date, g_data$q_level, type = "l", col = 4)
  
  # -------
  
  bl_start = which(lf_data$date == as.POSIXct(substr(g_data$date[1],1,10), tz = "UTC"))
  bl_stop =  which(lf_data$date == as.POSIXct(substr(g_data$date[length(g_data$date)],1,10), tz = "UTC"))
  if (length(bl_start) > 0 & length(bl_stop) > 0 ){
    lines(lf_data$date[bl_start:bl_stop],
          lf_data$bl_wmo[bl_start:bl_stop],
          type = "l", lty = 1, xlab=NA, ylab=NA, col.axis = 0, col = 2)
    lines(lf_data$date[bl_start:bl_stop],
          lf_data$bl_lh[bl_start:bl_stop],
          type = "l", lty = 1, xlab=NA, ylab=NA, col.axis = 0, col = 3)}
  
}

dev.off()
