plot3 <- function(){
    
    ##Fetch dataset, if not present, and unzip in the user's working directory
    
    myzip = "exdata_data_household_power_consumption.zip"
    #if data file has not yet been downloaded, fetch it
    if (!file.exists(myzip)) {
        print("Household Power Consumption Data Set does not exist in User's working directory.")
        print("Downloading Household Power Consumption Data Set")
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                      destfile=myzip,method="curl")
        unzip(myzip)
        
    }
    
    ## Read in data set
    print("Reading data set")
    householdPowerConsumption <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", stringsAsFactors = FALSE)
    
    ## Take only the subset of Dates that correspond to "1/2/2007"  or "2/2/2007"
    print("Taking the subset of 1/2/2007 and 2/2/2007 dates; day/month/year")
    householdPowerConsumption <- subset(householdPowerConsumption, Date == "1/2/2007" | Date == "2/2/2007")
    
    ## Merge Date and Time and store in the Date column, converting to POSIXct
    print("Merging Date and Time and storing the result in the Date column, converting to POSIXct")
    householdPowerConsumption$Date <- as.POSIXct(paste(householdPowerConsumption$Date, householdPowerConsumption$Time), format="%d/%m/%Y %H:%M:%S")
    
    ## Convert other columns, aside from Date and Time, into numeric type; suppress na warnings
    print("Converting other columns into numeric type; suppress na warnings.")
    
    for(i in 3:9){householdPowerConsumption[,i] <- suppressWarnings(as.numeric(householdPowerConsumption[,i])) }
    
    ##Create "plot3.png" in the user's working directory
    print("Creating plot3.png in User's working directory.")
    
    png("plot3.png", width = 480, height = 480)
    plot( householdPowerConsumption$Sub_metering_1 ~ householdPowerConsumption$Date,type = "l", col = "black",							 ylab = "Energy sub metering", xlab = "")
    lines(householdPowerConsumption$Sub_metering_2 ~ householdPowerConsumption$Date, type = "l", col = "red")
    lines(householdPowerConsumption$Sub_metering_3 ~ householdPowerConsumption$Date, type = "l", col = "blue")
    legend("topright", legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"),				 lty = c(1,1,1))
    dev.off()
    
    print("Done.")
}
 