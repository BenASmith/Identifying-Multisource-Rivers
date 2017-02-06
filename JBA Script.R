# ------------------------------------------------------------------------------
# Final Script For JBA
# Ben Smith
# 26/01/2017
# Newcastle University
# ------------------------------------------------------------------------------


# ---- QUESTIONS ---- #
# Would you like to save the processed/interpolated data to disk midway through the script?
  # Alter the below variable accordingly... (y/n)
  save_data <- "y"


# ---------------------------------
# Preamble - You will need to change this.
# ---------------------------------
  
  # Set up a file ('Temp_file' and put your raw in it)

  # Set the directory to the above file:
  setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Temp_file/")

  # Load the data - you may need to change the file name:
  load("Data - JBA_Raw_Data.RData")
  
  # Load all required packages:
  install.packages("zoo")
  install.packages("xts")
  install.packages("lfstat")
  install.packages("foreign")
  library(zoo)
  library(xts)
  library(lfstat)
  library(foreign)

  whole_dataset <- setNames(lapply(ls(pattern="tbl"), function(x) get(x)), ls(pattern="tbl"))
  gauge_names   <- sapply(ls(whole_dataset), function(x) x = substr(x,5,(nchar(x)-8)))
  
  # For test runs:
    # riv_1 = tbl_445210SG_River
    # riv_2 = tbl_F2290SG_River
    # riv_3 = tbl_F3004SG_River
    # rm(list=ls(pattern="tbl_"))
    # whole_dataset <- setNames(lapply(ls(pattern="riv"), function(x) get(x)), ls(pattern="riv"))
    # gauge_names   <- ls(whole_dataset)

# ---------------------------------
# Section 1 - Prepare the data
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
# This records the number of NAs, interpolates over any singular NAs
# and overwrites input data. Also counts number of interpolations.
# ---------------------------------

# Set up meta data table:
meta_data_patched <- data.frame("Gauge Name"= character(length(whole_dataset)),
                                "Number of NAs"= integer(length(whole_dataset)),
                                "Patched_NAs"= integer(length(whole_dataset)),
                                stringsAsFactors= FALSE)

#---------------------------------------


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

  # Add information to data.frame:
  meta_data_patched[x,] = c(g_name, updated_NAs, (origional_NAs - updated_NAs)) 
  
  #---------------------------------------
  
  # Save dataset to workspace:
  unique_tag <- g_name
  assign(unique_tag, value = g_data)
}

# Save dataset to disk:
if (save_data == "y"){
  
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
# Tidy the Workspace
rm(g_data, meta_data_patched, gauge_names, whole_dataset, x, unique_tag, g_name,
   correlation.data, origional_NAs, updated_NAs, filepath, gauge_names)


# ------------------------------------------------------------------------------
# Section 2
#
# Calculate base level indexes for each gauge. Display these in a table.
# This simply produces the lowflow objects (calculates base level).
# ------------------------------------------------------------------------------


# ---------------------------------
# Preamble
# ---------------------------------

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
  
  # Create low flow object for analysis
  g_lfo <- suppressWarnings(createlfobj(x = g_data,
                                        baseflow = TRUE,
                                        hyearstart = 1)) # can change start of hydrolocical year (1-12)
  
  date <- paste(g_lfo$year, g_lfo$month, g_lfo$day, sep = "-")
  date <- as.POSIXct(date, format="%Y-%m-%d", tz = "UTC")
  
  temp_data <- data.frame("date"    = date,
                          "value"   = round(g_lfo$flow, digits=3),
                          "bl_wmo"  = round(g_lfo$baseflow, digits = 3))
  
  temp_data = temp_data[c(min(which(!is.na(temp_data$value))):
                            max(which(!is.na(temp_data$value)))),]
  
  temp_data$bl_wmo[is.na(temp_data$value)] = NA
  
  name <- paste(g_name, "lfo", sep = "_")
  assign(name, value = temp_data)
  
}

# As far as I can tell from the test data, all dates are still there
# Some Na's are now at the end of the data

#---------------------------------------

# Tidy workspace (leaves only lfo's):
rm(list=setdiff(ls(), ls(pattern = "RL")))

#---------------------------------------

whole_dataset  <- setNames(lapply(ls(pattern="*lfo"), function(x) get(x)), ls(pattern="lfo"))

gauge_names    <- ls(whole_dataset)

# Patch any 'trivial' missing days in base level data:

for (x in 1:length(whole_dataset)){
  
  g_name <- gauge_names[x]
  g_data <- whole_dataset[[x]]
  
  g_data$value <- na.approx(g_data$value, maxgap = 1, na.rm = F)
  g_data$bl_wmo <- na.approx(g_data$bl_wmo, maxgap = 1, na.rm = F)

  assign(g_name, value = g_data)

}

# Refill 'whole dataset' with patched data:
whole_dataset  <- setNames(lapply(ls(pattern="*lfo"), function(x) get(x)), ls(pattern="lfo"))

#---------------------------------------

# Remove any long gaps:

# ....

#---------------------------------------

# Calculate WMO Base Level indexes for each record:

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

#---------------------------------------

# Calculate Lyne and Hollic Base Level and add BLI to BLI table:

source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

for (x in 1:length(whole_dataset)){
  
  g_name  <- gauge_names[x]
  g_data <- whole_dataset[[x]]
  
  lyne_hollick = BFI(Q = g_data$value, alpha=0.925, ReturnQbase=TRUE, passes=3)
  
  g_data$"bl_lh" <- round(lyne_hollick$Qbase, digits = 3)
  assign(g_name, value = g_data)

  filepath <- paste("Low Flow Objects/", g_name,'.Rdata',sep = "") 
  save(list = g_name, file = filepath) 

  base_level_indexes[x,3] = round(lyne_hollick$BFI, digits = 3)
}
  
# Save BLI table:
filepath <- paste("Low Flow Objects/", "Base Level Indexes", '.Rdata',sep = "")
save(base_level_indexes, file = filepath)


#---------------------------------------

# Tidy the Workspace
rm(missing, filepath, x, BFI, bli_wmo, lyne_hollick, g_name, base_level_indexes)


# ------------------------------------------------------------------------------
# Section 2
#
# Aim: Identify rapid rates of rise
# ------------------------------------------------------------------------------

# ---------------------------------
# Preamble
# ---------------------------------

# If not already in workspace, load the data:
  # setwd(dir="Interpolated data/")
  # sapply(list.files(pattern="RL_"),load,.GlobalEnv)
  # setwd(dir="../")

whole_dataset <- setNames(lapply(setdiff(ls(pattern="RL"), ls(pattern = "*lfo")),
                                 function(x) get(x)),
                          setdiff(ls(pattern="RL"), ls(pattern = "*lfo")))

gauge_names   <- ls(whole_dataset)


# --------------------------------
# Section 2 - Part 1
# --------------------------------

# Calculate the greatest river RISE i.e. from lowest point to highest:
  # Note - Make this section an apply function - it is very slow!
for(i in (1:length(whole_dataset))){
  
  g_name <- gauge_names[i]
  g_data <- whole_dataset[[i]]
  
  rise <- numeric(length(g_data$value))
  
  for (j in 4:(length(g_data$value))){
    
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

rm(d, g_name, i, j, r, rise, g_data)


# Calculate percentages of rises above 4/8, 5/8 & 6/8 of river level quantiles:

whole_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
gauge_names    <- ls(whole_dataset)


prop_rise = data.frame("gauge" = character(length(whole_dataset)),
                       "Qt.ft" = integer(length(whole_dataset)),
                       "Qt.st"   = integer(length(whole_dataset)),
                       "Qt.svt"   = integer(length(whole_dataset)),
                       stringsAsFactors = FALSE)

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

  prop_rise[i,] <- c(g_name,
                     round(Qt.ft, digits = 2),
                     round(Qt.st, digits = 2),
                     round(Qt.svt, digits = 2))
}

# Save the Rise Quantile data:
assign("River_Rise_Quantiles", value = prop_rise)
filepath <- paste("River_Rise_Quantiles", '.Rdata', sep="")
save(list = "River_Rise_Quantiles", file = filepath)

# Tidy workspace:
rm(g_data, prop_rise, g_name, gauge_names, i, Qt.ft, Qt.st, Qt.svt, tot,
   filepath, River_Rise_Quantiles, whole_dataset)

# -------------------------------
# Section 1 - PART 2
# -------------------------------
# Remove base level from the total River Level to leave
# behind only surface water inputs (in theory).
# -------------------------------


# Reload all data to workspace:
# setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Temp_file/Interpolated data/")
# sapply(list.files(pattern="RL_"),load,.GlobalEnv)

gauge_dataset  <- setNames(lapply(setdiff(ls(pattern="RL"), ls(pattern = "*lfo")),
                                  function(x) get(x)),
                           setdiff(ls(pattern="RL"), ls(pattern = "*lfo")))

gauge_names    <- ls(gauge_dataset)

# ---------------------------------

setwd(dir="Low Flow Objects/")
sapply(list.files(pattern="RL_"),load,.GlobalEnv)
setwd(dir="../")

lf_dataset  <- setNames(lapply(ls(pattern="lfo"), function(x) get(x)), ls(pattern="lfo"))
lf_names    <- ls(lf_dataset)

#---------------------------------------

for (i in 1:length(lf_dataset)){
  
  g_name <- gauge_names[i]
  g_data <- gauge_dataset[[i]]
  
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
  
  # Set up a blank vector to recieve 'BL normalised' values:
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
  
  # Keep the normalised data:
  gauge_dataset[[i]]$"q_level" <- normalised
  assign(x = g_name, value = gauge_dataset[[i]])
  
  # Save the data:
  name     <- g_name
  filepath <- paste("Interpolated Data/", name,'.Rdata',sep = "")
  save(list = name, file = filepath)
  
}

#---------------------------------------

# Tidy workspace:
rm(BL,consec_ends, consec_runs, i, newindex, RL, runs, z, ends, ends_j, g_name, gauge_names, gauge_dataset, j, lf_name, normalised, starts, starts_j, lf_names, lf_dataset, g_data, lf_data, filepath, name)

rm(list= ls(pattern = "lfo"))



# ------------------------------------------------------------------------------
# Section 2
# Aim: Find multisource floods using peak data, work out ratios for each event 
# and average them for that gauge.
# ------------------------------------------------------------------------------


# ---------------------------------
# Preamble
# ---------------------------------

# setwd("../")
# setwd(dir="H:/PhD Documents/Task 1/R Scripts/Iterations/Temp_file/Interpolated Data/")
# sapply(list.files(pattern="RL_"),load,.GlobalEnv)

ql_dataset  <- setNames(lapply(ls(pattern="RL"), function(x) get(x)), ls(pattern="RL"))
ql_names    <- ls(ql_dataset)

# Set up a function to find peak fiver levels:
find_peaks <- function (x, m = 672){
  
  # A 'peak' is defined as a local maxima with m points either side of it being smaller than it.
  # Hence, the bigger the parameter m, the more stringent is the peak funding procedure.
  # 'm' is the minimum number of time units either side of a peak where smaller river levels are requires.
  # i.e. when m = 672, there are no larger peaks with in (15 mins*672) 7 days either side of that peak.
  # The function can also be used to find local minima of any sequential vector x via find_peaks(-x).
  # [http://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data]
  
  date <- x$date
  val  <- x$value
  lgth <- length(val)
  
  shape <- diff(sign(diff(val, na.pad = FALSE)))
  
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < lgth, w, lgth)
    if(all(val[c(z : i, (i + 2) : w)] <= val[i + 1], na.rm = T)) # I have added the ", na.rm = T"
      return(i + 1) else return(numeric(0)) 
  })
  
  pks <- unlist(pks)
  
  # Only use those peaks that are above threshold:
  pks <- pks[which(val[pks] >= quantile(val, na.rm = T, probs = (0.98)))]
  
  # Above function find peaks. m is equal to 1 week, so any duplicates should be due to equal values.
  # To remove these duplicates:
  pks = pks[-(which(diff(pks) <= m))]
  
  # Now we need to select the top 10 events: 
  df <- data.frame(index = c(1:length(pks)), value = val[pks])
  df <- df[order(-df$value),]
  df <- df[c(1:10),]
  pks <- pks[df$index]
  
  return(pks)
}


# --------------------------------
# PART 1
# --------------------------------

# Calculate length of dataset for dataframe set up and for loop:
dat_lgth = length(ql_dataset)

# Set up dataframe to recieve source clasifications:
source_percentages <- data.frame("gauge" = character(length = dat_lgth),
                                    "high_ql" = rep(NA, dat_lgth),
                                    "high_bl" = rep(NA, dat_lgth),
                                    "high_ms" = rep(NA, dat_lgth),
                                    stringsAsFactors = FALSE)


for(i in 1:dat_lgth){
  
  g_data <- ql_dataset[[i]]
  
  lgth_date <- length(g_data$date)
  
  # Find the top 10 peak events:
  index <- find_peaks(x = g_data)
  
  # Calculate threshold to define starts/close of peak events:
  rl_threshold  <- quantile(g_data$value, na.rm = T, probs = (0.98))
  
  # Set up dataframe to store values ready for averaging:
  ratios = data.frame("ql" = rep(NA, 10),
                      "bl" = rep(NA, 10),
                      "ms" = rep(NA, 10),
                      "rl" = rep(NA, 10))
  
  # Calculate threshold for defining the occurance of a rapid rise:
    # This finds 1/4 of the median (with the very low levels removed)
    # this is relatively arbitary and can be changed.
  ql_threshold <- (quantile(g_data$value, probs = 0.50, na.rm = T) - 
                   quantile(g_data$value, probs = 0.05, na.rm = T))/4
  
  # Cycle through events and calculate source statistics for each:
  for (j in 1:10){
    
    # Recall start of peak event:
    start <- max(which(g_data$value[1:index[j]] <= rl_threshold)) +1
    
    # Recall end of peak event:
    end   <- index[j] + min(which(g_data$value[index[j]:lgth_date] <= rl_threshold),
                            na.rm=TRUE) - 1
    
    # Set up vector of the indexes of a peak event:
    pk <- c(start:end)
    
    # Define high river levels there are:
    high_rl <- length(which(g_data$value[pk] >= rl_threshold))
    
    # Identify rapid rises during top 10 peak events:
    high_ql = which(g_data$rise[pk] >= ql_threshold)
    
    # Identify high base levels during top 10 peak events:
    high_bl = which(((g_data$value[pk] - g_data$q_level[pk])/g_data$value[pk]) >= 0.70)

    # If you would like to plot a peak - you can run this rough code:
     # plot(g_data$date[c((pk[1]-1000):(pk[length(pk)]+1000))],
     #      g_data$value[c((pk[1]-1000):(pk[length(pk)]+1000))],
     #      xlab = paste("Date (", g_data$date[pk[1]], ")", sep =""), ylab = "River Level (m)",
     #      type = "l", ylim = c(0,0.7))
     #  lines(g_data$date[pk],g_data$q_level[pk],
     #        type="h", col = 8)
     #  lines(x = g_data$date, y =(g_data$value-g_data$q_level),
     #        type = "l", col = 4)
     #  lines(g_data$date[pk[high_bl]],(g_data$value[pk[high_bl]]-g_data$q_level[pk[high_bl]]),
     #        type = "p", col = '4', cex = 0.4, pch = 3)
     #  lines(g_data$date[pk[high_ql]],g_data$value[pk[high_ql]],
     #        type ="p",col = 2, cex = 0.4, pch = 18)
    
    ratios[j,] = c(length(high_ql)/high_rl*100,
                   length(high_bl)/high_rl*100,
                   length(intersect(high_ql, high_bl))/high_rl*100,
                   high_rl)
  }
  
  source_percentages[i,] = c(ql_names[i],
                                round(mean(ratios[,1], na.rm = T),digits = 2),
                                round(mean(ratios[,2], na.rm = T),digits = 2),
                                round(mean(ratios[,3], na.rm = T),digits = 3))
  
}

# Save the data:
# assign("source_percentages", value = source_percentages)
filepath <- paste("source_percentages", '.Rdata', sep="")
save(list = "source_percentages", file = filepath)

# Also write data as a CSV:
write.csv(source_percentages, file = "source_percentages.csv", row.names=FALSE)

print("Analysis Complete, have a cup of tea and then view your results in the Temp File/source_percentages")

# Tidy Workspace:
rm(dat_lgth, start, end, filepath, find_peaks, g_data, high_bl, high_ql, high_rl, i, index, j, pk, lgth_date, source_percentages, ql_dataset, ql_names, ql_threshold, ratios, rl_threshold)

proc.time() - ptm

# --------------------------------
# The End
# --------------------------------
