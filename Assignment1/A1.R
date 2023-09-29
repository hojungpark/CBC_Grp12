## Set-up
setwd("~/Desktop/sfu/CMPT-318/Assignment1")

dataset <- read.table("Group_Assignment_1_Dataset.txt", header=TRUE, sep=",")
dataset$Time <- as.POSIXlt(dataset$Time, format="%H:%M:%S")
dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
dataset_week12 <- subset(dataset, dataset$Date >=  as.POSIXlt("19/3/2007", format="%d/%m/%Y") & dataset$Date <= as.POSIXlt("25/3/2007", format="%d/%m/%Y"))

mode <- function(x, na.rm = FALSE) {
  if(na.rm){ 
    x = x[!is.na(x)]
  }
  
  val <- unique(x)
  return(val[which.max(tabulate(match(x, val)))])
}

### 1
  ## Compute the arithmetic and the geometric mean, the median, the mode and the standard deviation for features A, B and C respectively.
  
  arithmetic_mean_A <- mean(dataset_week12$Global_active_power, na.rm = TRUE)
  geometric_mean_A <- exp(mean(log(dataset_week12$Global_active_power), na.rm = TRUE))
  median_A <- mean(dataset_week12$Global_active_power, na.rm = TRUE)
  mode_A <- mode(dataset_week12$Global_active_power, na.rm=TRUE)
  standard_deviation_A <- sd(dataset_week12$Global_active_power, na.rm = TRUE)
  
  arithmetic_mean_B <- mean(dataset_week12$Global_reactive_power, na.rm = TRUE)
  geometric_mean_B <- exp(mean(log(dataset_week12$Global_reactive_power), na.rm = TRUE))
  median_B <- mean(dataset_week12$Global_reactive_power, na.rm = TRUE)
  mode_B <- mode(dataset_week12$Global_reactive_power, na.rm=TRUE)
  standard_deviation_B <- sd(dataset_week12$Global_reactive_power, na.rm = TRUE)
  
  arithmetic_mean_C <- mean(dataset_week12$Voltage, na.rm = TRUE)
  geometric_mean_C <- exp(mean(log(dataset_week12$Voltage), na.rm = TRUE))
  median_C <- mean(dataset_week12$Voltage, na.rm = TRUE)
  mode_C <- mode(dataset_week12$Voltage, na.rm=TRUE)
  standard_deviation_C <- sd(dataset_week12$Voltage, na.rm = TRUE)
  
  cat("Arithmetic mean of global active power", arithmetic_mean_A,
      "\nGeometric mean of global active power", geometric_mean_A,
      "\nMedian of global active power", median_A,
      "\nMode of global active power", mode_A,
      "\nStandard Deviation of global active power", standard_deviation_A, "\n")
  
  cat("Arithmetic mean of global reactive power", arithmetic_mean_B,
            "\nGeometric mean of global reactive power", geometric_mean_B,
            "\nMedian of global reactive power", median_B,
            "\nMode of global reactive power", mode_B,
            "\nStandard Deviation of global reactive power", standard_deviation_B, "\n")
  
  cat("Arithmetic mean of voltage", arithmetic_mean_C,
            "\nGeometric mean of voltage", geometric_mean_C,
            "\nMedian of voltage", median_C,
            "\nMode of voltage", mode_C,
            "\nStandard Deviation of voltage", standard_deviation_C, "\n")
  
  ## For features A and B compute the min and max values on weekdays and weekend days during day hours and night hours respectively
  days_of_the_week <- as.POSIXlt(dataset_week12$Date, format = "%d/%m/%Y")$wday #wday is the numeric weekday (0-6 starting from Sunday)
  start_of_daytime <- as.POSIXlt("06:00:00", format = "%H:%M:%S")
  end_of_daytime <- as.POSIXlt("18:00:00", format = "%H:%M:%S")
  
  weekdays_day <- subset(dataset_week12, days_of_the_week != 0 & days_of_the_week != 6 & dataset_week12$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & dataset_week12$Time <= as.POSIXlt("18:00:00", format = "%H:%M:%S"))
  weekdays_night <- subset(dataset_week12, days_of_the_week != 0 & days_of_the_week != 6 & (dataset_week12$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S") | dataset_week12$Time > as.POSIXlt("18:00:00", format = "%H:%M:%S")))
  weekends_day <- subset(dataset_week12, (days_of_the_week == 0 | days_of_the_week == 6) & dataset_week12$Time >= as.POSIXlt("06:00:00", format = "%H:%M:%S") & dataset_week12$Time <= as.POSIXlt("18:00:00", format = "%H:%M:%S"))
  weekends_night <- subset(dataset_week12, (days_of_the_week == 0 | days_of_the_week == 6) & (dataset_week12$Time < as.POSIXlt("06:00:00", format = "%H:%M:%S")| dataset_week12$Time > as.POSIXlt("18:00:00", format = "%H:%M:%S")))
  
  # min & max for weekdays day & night (Global active power)
  weekdays_day_min_A <- min(weekdays_day$Global_active_power, na.rm =TRUE)
  weekdays_day_max_A <- max(weekdays_day$Global_active_power, na.rm =TRUE)
  weekdays_night_min_A <- min(weekdays_night$Global_active_power, na.rm =TRUE)
  weekdays_night_max_A <- max(weekdays_night$Global_active_power, na.rm =TRUE)
  
  # min & max for weekends day & night (Global active power)
  weekends_day_min_A <- min(weekends_day$Global_active_power, na.rm = TRUE)
  weekends_day_max_A <- max(weekends_day$Global_active_power, na.rm = TRUE)
  weekends_night_min_A <- min(weekends_night$Global_active_power, na.rm = TRUE)
  weekends_night_max_A <- max(weekends_night$Global_active_power, na.rm = TRUE)

    # min & max for weekdays day & night (Global reactive power)
  weekdays_day_min_B <- min(weekdays_day$Global_reactive_power, na.rm =TRUE)
  weekdays_day_max_B <- max(weekdays_day$Global_reactive_power, na.rm =TRUE)
  weekdays_night_min_B <- min(weekdays_night$Global_reactive_power, na.rm =TRUE)
  weekdays_night_max_B <- max(weekdays_night$Global_reactive_power, na.rm =TRUE)
  
  # min & max for weekends day & night (Global reactive power)
  weekends_day_min_B <- min(weekends_day$Global_reactive_power, na.rm = TRUE)
  weekends_day_max_B <- max(weekends_day$Global_reactive_power, na.rm = TRUE)
  weekends_night_min_B <- min(weekends_night$Global_reactive_power, na.rm = TRUE)
  weekends_night_max_B <- max(weekends_night$Global_reactive_power, na.rm = TRUE)

  cat("Minimum global active power on day time of weekdays", weekdays_day_min_A,
      "\nMaximum global active power on day time of weekdays", weekdays_day_max_A,
      "\nMinimum global active power on night time of weekdays", weekdays_night_max_A,
      "\nMaximum global active power on night time of weekdays", weekdays_night_max_A,
      
      "\nMinimum global active power on day time of weekends", weekends_day_min_A,
      "\nMaximum global active power on day time of weekends", weekends_day_max_A,
      "\nMinimum global active power on night time of weekends", weekends_night_max_A,
      "\nMaximum global active power on night time of weekends", weekends_night_max_A,
      
      "\nMinimum global reactive power on day time of weekdays", weekdays_day_min_B,
      "\nMaximum global reactive power on day time of weekdays", weekdays_day_max_B,
      "\nMinimum global reactive power on night time of weekdays", weekdays_night_max_B,
      "\nMaximum global reactive power on night time of weekdays", weekdays_night_max_B,
      
      "\nMinimum global reactive power on day time of weekends", weekends_day_min_B,
      "\nMaximum global reactive power on day time of weekends", weekends_day_max_B,
      "\nMinimum global reactive power on night time of weekends", weekends_night_max_B,
      "\nMaximum global reactive power on night time of weekends", weekends_night_max_B)

### 2
  # Compute the correlation for each disjoint pair of the responses, A, B, C, D, E, F and G, using Pearson’s sample correlation coefficient
  dataset <- read.table("Group_Assignment_1_Dataset.txt", header = TRUE, sep = ",")
  dataset$Date <- as.POSIXlt(dataset$Date, format = "%d/%m/%Y")
  dataset$Time <- as.POSIXlt(dataset$Time, format = "%H:%M:%S")
  dataset_week12 <- subset(dataset, dataset$Date >= as.POSIXlt("29/1/2007", format = "%d/%m/%Y") & dataset$Date <= as.POSIXlt("4/2/2007", format = "%d/%m/%Y"))##week 5 data (starts on Jan 29, 2007 - Feb 4 2007 inclusive)
  
  AB <- cor(dataset_week12$Global_active_power, dataset_week12$Global_reactive_power, method = "pearson")
  AC <- cor(dataset_week12$Global_active_power, dataset_week12$Voltage, method = "pearson")
  AD <- cor(dataset_week12$Global_active_power, dataset_week12$Global_intensity, method = "pearson")
  AE <- cor(dataset_week12$Global_active_power, dataset_week12$Sub_metering_1, method = "pearson")
  AF <- cor(dataset_week12$Global_active_power, dataset_week12$Sub_metering_2, method = "pearson")
  AG <- cor(dataset_week12$Global_active_power, dataset_week12$Sub_metering_3, method = "pearson")

  cat("Pearson correlation between:",
      "\nGlobal active power & Global reactive power", AB,
      "\nGlobal active power & Voltage", AC,
      "\nGlobal active power & Global intensity", AD,
      "\nGlobal active power & Submetering 1", AE,
      "\nGlobal active power & Submetering 2", AF,
      "\nGlobal active power & Submetering 3", AG)