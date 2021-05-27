setwd("C:/Users/TOsosanya/Desktop/Electricity/Danish DNOs/Evonet/Model")
files <- list.files(paste0(getwd(), "//Exports"))

# a <- fread(paste0(getwd(), "//Exports//", files[1]))
# b <- fread(paste0(getwd(), "//Exports//", files[2]))
# c <- fread(paste0(getwd(), "//Exports//", files[3]))
# d <- fread(paste0(getwd(), "//Exports//", files[4]))

req.names <- c("Result", "Health_Index_Asset_Category", "Asset_Number", "Timestep", "Quantity Resource", "LevelCode",
               "Intervention Costs", "Future PoF", "Financial Consequence Cost","Safety Consequence Cost", 
               "Environmental Consequence Cost", "Network Consequence Cost","Monetised Risk Cost", 
               "Probability of Failure Mode One", "Probability of Failure Mode Two","Probability of Failure Mode Three",
               "Number of Customer Interruptions", "CML","Health Index", "Criticality Index", "Stepwise NPV")

for(i in 1:length(files)){
   dt <- fread(paste0(getwd(), "//Exports//", files[i]))
   
   if(is.null(dt$LevelCode)){
     dt$LevelCode <- dt$`Level Code`
   }  
   
   req <- names(dt) %in% req.names
   dt <- dt[, req, with = F]
   
   if(is.null(dt$Result)){
     dt$Result <- files[i]
   }  
   
      if(i == 1){
     output <- dt
   } else {
     output <- rbind(output, dt)
   }
}

unique(output$Result)

output$Result <- ifelse(grepl("Maintain", output$Result), "Maintain", output$Result)
output$Result <- ifelse(grepl("No Investment", output$Result), "No Investment", output$Result)
output$Result <- ifelse(grepl("Increase", output$Result), "Increase", output$Result)
output$Result <- ifelse(grepl("Decrease", output$Result), "Decrease", output$Result)

#output[, .(sum(`Monetised Risk Cost`)), by = list(Health_Index_Asset_Category, Result, Timestep)]

fwrite(output, "output.csv")
