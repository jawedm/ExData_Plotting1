plot4 <- function(filePath) {
      ##read data from file
      dt <- read.csv(filePath, sep=";", header=TRUE, stringsAsFactors=FALSE);
      
      ##format Date column
      dt$Date = as.Date(dt$Date, format="%d/%m/%Y");
      
      ##extract relevant data
      data <- subset(dt, Date >= "2007-02-01" & Date <= "2007-02-02");

      ##garbage collection
      rm(dt);
      
      ##formatting data columns
      data$Global_active_power   <- as.numeric(data$Global_active_power);
      data$Global_reactive_power <- as.numeric(data$Global_reactive_power);
      data$Voltage               <- as.numeric(data$Voltage);
      data$Sub_metering_1        <- as.numeric(data$Sub_metering_1);
      data$Sub_metering_2        <- as.numeric(data$Sub_metering_2);
      data$Sub_metering_3        <- as.numeric(data$Sub_metering_3);
      
      ##creating datetime column
      datetime <- paste(data$Date, data$Time);
      data$DateTime <- as.POSIXct(datetime);
      
      ##plotting charts
      windows();
      png("plot4.png", width=480, height=480);
      
      par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0));
      
      with(data, {
            plot(Global_active_power ~ DateTime, type="l", ylab="Global Active Power", xlab="");
      
            plot(Voltage ~ DateTime, type="l", ylab="Voltage", xlab="datetime");
                  
            plot(Sub_metering_1 ~ DateTime, type="l", ylab="Energy sub metering", xlab="");
            lines(Sub_metering_2 ~ DateTime, col="Red");
            lines(Sub_metering_3 ~ DateTime, col="Blue");
            legend("topright", col=c("black","red", "blue"), lty=1, lwd=2, legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"));
            
            plot(Global_reactive_power ~ DateTime, type="l", ylab="global_reactive_power", xlab="datetime");
      });
      
      dev.off();
      
      ##garbage collection
      rm(data);
}