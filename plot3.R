plot3 <- function(filePath) {
      ##read data from file
      dt <- read.csv(filePath, sep=";", header=TRUE, stringsAsFactors=FALSE);
      
      ##format Date column
      dt$Date = as.Date(dt$Date, format="%d/%m/%Y");
      
      ##extract relevant data
      data <- subset(dt, Date >= "2007-02-01" & Date <= "2007-02-02");
      
      ##garbage collection
      rm(dt);
      
      ##formatting data columns
      data$Sub_metering_1 <- as.numeric(data$Sub_metering_1);
      data$Sub_metering_2 <- as.numeric(data$Sub_metering_2);
      data$Sub_metering_3 <- as.numeric(data$Sub_metering_3);
      
      ##creating datetime column
      datetime <- paste(data$Date, data$Time);
      data$DateTime <- as.POSIXct(datetime);
      
      ##plotting charts
      windows();
      png("plot3.png", width=480, height=480);
      
      with(data, {
            plot(Sub_metering_1 ~ DateTime, type="l", ylab="Energy sub metering", xlab="");
            lines(Sub_metering_2 ~ DateTime, col="Red");
            lines(Sub_metering_3 ~ DateTime, col="Blue");
            legend("topright", col=c("black","red", "blue"), lty=1, lwd=2, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"));
      });
      
      dev.off();
      
      ##garbage collection
      rm(data);
}