plot2 <- function(filePath) {
      ##read data from file
      dt <- read.csv(filePath, sep=";", header=TRUE, stringsAsFactors=FALSE);
      
      ##format Date column
      dt$Date = as.Date(dt$Date, format="%d/%m/%Y");
      
      ##extract relevant data
      data <- subset(dt, Date >= "2007-02-01" & Date <= "2007-02-02");
      
      ##garbage collection
      rm(dt);

      ##formatting data columns
      data$Global_active_power = as.numeric(data$Global_active_power);
      
      ##creating datetime column
      datetime <- paste(data$Date, data$Time);
      data$DateTime <- as.POSIXct(datetime);
      
      ##plotting charts
      windows();
      png("plot2.png", width=480, height=480);
      
      plot(data$Global_active_power ~ data$DateTime, type="l", ylab="Global Active Power (kilowatt)", xlab="");
      dev.off();
      
      ##garbage collection
      rm(data);
}