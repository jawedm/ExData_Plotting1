plot1 <- function(filePath) {
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
      
      ##plotting charts
      windows();
      png("plot1.png", width=480, height=480);
      
      hist(data$Global_active_power, col="red", main="Global Active Power", xlab = "Global Active Power (kilowatts)");
      dev.off();
      
      ##garbage collection
      rm(data);
}