library(dplyr)
HPC = read.table('household_power_consumption.txt', sep = ";", header = T, stringsAsFactors = FALSE, col.names = c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3"))
HPC$Date = as.Date(HPC$Date, "%d/%m/%Y")
HPC_filtered = filter(HPC, Date > "2007-01-31" & Date < "2007-02-03")
HPC_filtered$Global_active_power = as.numeric(HPC_filtered$Global_active_power)
datetime = paste(HPC_filtered$Date, HPC_filtered$Time)
datetime = strptime(datetime, "%Y-%m-%d %H:%M:%S")
HPC_filtered_datetime <- cbind(datetime, HPC_filtered[,c(-1,-2)])

png("plot4.png", width=480, height=480)
par(mfrow=c(2,2))

#Figure [1,1]
plot(HPC_filtered_datetime$Global_active_power~HPC_filtered_datetime$datetime,type="l",ylab="Global Active Power",xlab ="")

#Figure [2,1]
plot(HPC_filtered_datetime$Voltage~HPC_filtered_datetime$datetime,type="l",ylab="Voltage",xlab ="")

#Figure [1,2]
with(HPC_filtered_datetime,plot(Sub_metering_1~datetime,type="l",xlab="",ylab="Energy sub metering"),legend=F)
with(HPC_filtered_datetime,points(Sub_metering_2~datetime,type="l",xlab="",ylab="", col="red"))
with(HPC_filtered_datetime,points(Sub_metering_3~datetime,type="l",xlab="",ylab="", col="blue"))
legend("topright",col = c("black", "red", "blue"),lty=1,legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), bty = "n")

##Figure [2,2]
plot(HPC_filtered_datetime$Global_reactive_power~HPC_filtered_datetime$datetime,type="l",ylab="Global_reactive_power",xlab ="")
dev.off()