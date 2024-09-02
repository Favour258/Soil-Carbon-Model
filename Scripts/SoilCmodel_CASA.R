#load required packages:--------
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)

#read in the input data & clean it:-----
setwd("C:/Users/etink/Documents/George August University, Gottingen/Semester Courses/Ecosystem-Atmosphere Processes/Terrestrial Ecosystem Modelling/canopy-model-2023-24/measurements")

#litter input data:
input_fruit <- read_excel("Measuremets_CarbonComponents_Hainich.xlsx",sheet = "NPP - Fruit")

input_leaves <- read_excel("Measuremets_CarbonComponents_Hainich.xlsx",sheet = "NPP - Wood&Leaf")

#clean the dataset Fruit:
summary(input_fruit)

#Only keep relevant columns
input_fruit <- input_fruit[10:31,1:2]

#change column description
colnames(input_fruit)[colnames(input_fruit) == "Annual Fruit Production"] <- "Year"
colnames(input_fruit)[colnames(input_fruit) == "...2"] <- "NPP_F"

# Replace "nd" with NA
input_fruit$NPP_F[input_fruit$NPP_F == "nd"] <- NA

#Now check the dataset:
summary(input_fruit)

#Change to the right mode:
input_fruit$NPP_F <- as.numeric(x = input_fruit$NPP_F)
input_fruit$Year <- as.factor(input_fruit$Year) # I defined the year as factor, since otherwise it would always add a month and day..

# Clean the dataset Leaves:
summary(input_leaves)

#Only keep relevant columns
input_leaves <- input_leaves[-1,1:5]

#change column description
colnames(input_leaves)[colnames(input_leaves) == "Total NPP"] <- "NPP"
colnames(input_leaves)[colnames(input_leaves) == "Total wood-NPP"] <- "wood_NPP"
colnames(input_leaves)[colnames(input_leaves) == "Fagus wood-NPP"] <- "fagus_wood_NPP"
colnames(input_leaves)[colnames(input_leaves) == "Leaf-NPP"] <- "leaf_NPP"

#Now check the dataset:
summary(input_leaves)

#Change to the right mode:
input_leaves$NPP <- as.numeric(x = input_leaves$NPP)
input_leaves$wood_NPP <- as.numeric(x = input_leaves$wood_NPP)
input_leaves$fagus_wood_NPP <- as.numeric(x = input_leaves$fagus_wood_NPP)
input_leaves$leaf_NPP <- as.numeric(x = input_leaves$leaf_NPP)
input_leaves$Year <- as.factor(input_leaves$Year) # I defined the year as factor, since otherwise it would always add a month and day..


#meteorological data:
data_met <-read.csv("Measurements_meteo_hourly_201601_201712_gapfilled.csv")
data_soil <- read.csv("Measurements_soil_hourly_201601_201712_gapfilled.csv")
data_resp <- read.csv("Measurements_soilresp_hourly_201601_201712.csv")
data_fluxes <- read.csv("Measurements_fluxes_hourly_201601_201712_gapfilled.csv")

#Set the initial parameter and create output df:-------
par <- read_csv("parameters.csv")

#take the k-values from the parameter dataset (c.f. Bonan p. 303)
k1 <- par$value[par$variable=="k1"] 
k2 <- par$value[par$variable=="k2"]
k3 <- par$value[par$variable=="k3"]
k4 <- par$value[par$variable=="k4"]
k5 <- par$value[par$variable=="k5"]
k6 <- par$value[par$variable=="k6"]
k7 <- par$value[par$variable=="k7"]
k8 <- par$value[par$variable=="k8"]
k9 <- par$value[par$variable=="k9"]

#for naming the a-values I used the same description as CASA-CNP model
a51 <- par$value[par$variable=="a51"]
a41 <- par$value[par$variable=="a41"]
a41 <- par$value[par$variable=="a41"]
a42 <- par$value[par$variable=="a42"]
a52 <- par$value[par$variable=="a52"]
a63 <- par$value[par$variable=="a63"]
a85 <- par$value[par$variable=="a85"]
a74 <- par$value[par$variable=="a74"] 
a76 <- par$value[par$variable=="a76"]
a75 <- par$value[par$variable=="a75"] 
a86 <- par$value[par$variable=="a86"] 
a87 <- par$value[par$variable=="a87"] 
a97 <- par$value[par$variable=="a97"] 
a98 <- par$value[par$variable=="a98"] 


#read out the values for the scaling factors: 
S1 <- par$value[par$variable=="S1"]
S2 <- par$value[par$variable=="S2"]
S3 <- par$value[par$variable=="S3"]
S4 <- par$value[par$variable=="S4"] 
S5 <- par$value[par$variable=="S5"] 
S6 <- par$value[par$variable=="S6"] 
S7 <- par$value[par$variable=="S7"] 
S8 <- par$value[par$variable=="S8"] 
S9 <- par$value[par$variable=="S9"] 

#Here is space for the temperature and moisture function:-----


###calculate weighted average moisture and temperature for the profile (30cm)
soil_filtered <- data_soil %>%
    #create new columns based on row-wise means of multiple columns
    mutate(
        TS_profile = (TS_2cm_degC*2/30 + TS_5cm_degC*3/30 + TS_15cm_degC*10/30 + TS_30cm_degC*15/30),
        SWC_profile = (2*SWC_8cm_.*0.08 + (SWC_8cm_. + SWC_16cm_.) * 0.16 + (SWC_16cm_. + SWC_32cm_.) * 0.3) / (2 * (0.08 + 0.16 + 0.3))
    ) %>%
    #select only these columns for new dataframe
    select(TIMESTAMP_END, TS_profile, SWC_profile)


#check the df:
summary(soil_filtered)

#assign time-format to first column:
soil_filtered$TIMESTAMP_END <- as.POSIXct(soil_filtered$TIMESTAMP_END, format = "%Y-%m-%d %H:%M:%S")

# SE function (=pore space filled with water)

# equation based on equation 18.13 in Bonan (chapter 18)
# SE --> SWC: volumetric WC, soil porosity 
#-->  fraction of the volume of voids over the total volume --> Porosity = (1 - (Bulk Density ÷ Particle Density)) x 100
# function according to original source Kelly et al. 2000
SE_function <- function(soil_filtered){
    soil_filtered$SE <- numeric(nrow(soil_filtered))
    for (i in 1:nrow(soil_filtered)){
        soil_filtered$SE[i] <- soil_filtered$SWC_profile[i] / ((1 - (0.89 / 2.65)) * 100)
    }
    return(soil_filtered)
}
# 15: thickness soil layer; 0.89: mean bulk density of 10cm soil layer (Knautschtal et al, 2010); 
#2.65: particle density --> constant

# a, b,c  and d depend on soil texture --> here use of values for fine soil texture as soil in Hainich 
#is described as silt-clayey
a <- 0.6
b <- 1.27
c <- 0.0012
d <- 2.84
e <- d * (b - a) / (a - c)

SWC_function <- function(soil_filtered, a, b, c, d, e){
    soil_filtered$SWC_factor <- numeric(nrow(soil_filtered))
    for (i in 1:nrow(soil_filtered)) {
        soil_filtered$SWC_factor[i] <- ((soil_filtered$SE[i] - b) / (a - b)) ^ e * ((soil_filtered$SE[i] - c) / (a - c)) ^ d
    }
    return(soil_filtered)
}

soil_filtered <- SE_function(soil_filtered) #(=pore space filled with water)
soil_filtered <- SWC_function(soil_filtered, a=a, b=b,c=c,d=d,e=e)


###Now do temperature and add it to the moisture DF:

#define input:
celsius <- soil_filtered$TS_profile

##For both model types use temperature in Kelvin!! Transfer °C to K:
kelvin <- celsius + 273.15

temperature_function <- function(Temp, model){
    if (model == "CASA"){
        temperature <- exp(308.56*((1/56.02)-(1/(Temp-227.13))))
    } 
    else if (model == "CENTURY"){ # atan(x) is the command for arctangant 
        temperature <- 0.56 + (1.46/pi)*atan(0.031*pi*(Temp-288.85))
    } else { #this is for weirdos that try even other models
        print("Temperature function not applicable")
    }
    return(temperature=temperature)
}

#now run the function and add the results to the data frame "soil_filtered":

temp_factor_casa <- temperature_function(Temp=kelvin, model="CASA")
temp_factor_century <-temperature_function(Temp=kelvin, model="CENTURY") 

#add the results to the table:
soil_filtered <- cbind(soil_filtered, temp_factor_casa, temp_factor_century)
##soil_filtered is the finished input vector for our SoilCmodel function

#creating new columns to store the results 

soil_filtered$meta <- 0
soil_filtered$struc <- 0 
soil_filtered$CWD <- 0 
soil_filtered$fastSOM <- 0 
soil_filtered$slowSOM <- 0
soil_filtered$RESP <- 0


# setting initial values for carbon pools
soil_filtered[1, "meta"] <- 1200 # value from metabolic litter
soil_filtered[1, "struc"] <- 400 # value from structural litter
soil_filtered[1, "fastSOM"] <- 800 #value from fast SOM
soil_filtered[1, "slowSOM"] <- 6000 #value from slow SOM
soil_filtered[1, "CWD"] <- 2400 #value from CWD


#keep in mind that this is only for the fruit litter pool!!

# litter input leaves and fruit
input_fruit_total_2016 <- 202.8 # NPP fruits (gC/m2)
input_fruit_total_2017 <- 11.0

input_leaves_total_2016 <- 157 # Leaf-NPP g C  m-2 yr-1
input_leaves_total_2017 <- mean(input_leaves$leaf_NPP) 

# distributing litter input leaves +fruits over 20 days in October
for(j in 1:nrow(soil_filtered)){
    soil_filtered$input_leaves_hourly <- ifelse(
        soil_filtered$TIMESTAMP_END >= as.POSIXct("2016-10-07 00:00:00") & soil_filtered$TIMESTAMP_END <= as.POSIXct("2016-10-27 23:00:00"),
        (input_leaves_total_2016 / 480), #yearly input divided by 480 --> 20 days * 24 hours = 480
        ifelse(
            soil_filtered$TIMESTAMP_END >= as.POSIXct("2017-10-07 00:00:00") & soil_filtered$TIMESTAMP_END <= as.POSIXct("2017-10-27 23:00:00"),
            (input_leaves_total_2017 / 480),
            0
        )
    )
    soil_filtered$input_fruits_hourly <- ifelse(
        soil_filtered$TIMESTAMP_END >= as.POSIXct("2016-10-07 00:00:00") & soil_filtered$TIMESTAMP_END <= as.POSIXct("2016-10-27 23:00:00"),
        (input_fruit_total_2016 / 480),
        ifelse(
            soil_filtered$TIMESTAMP_END >= as.POSIXct("2017-10-07 00:00:00") & soil_filtered$TIMESTAMP_END <= as.POSIXct("2017-10-27 23:00:00"),
            (input_fruit_total_2017 / 480),
            0
        )
    )
}



###There is 2 NA values in the timestamp vector, which leads to NA values in the input values. Therefore replace the NA values with 0 in order to keep the loop stable:
soil_filtered$input_leaves_hourly[is.na(soil_filtered$input_leaves_hourly)] <- 0
soil_filtered$input_fruits_hourly[is.na(soil_filtered$input_fruits_hourly)] <- 0

#function for CASA
SoilCfunction <- function(time){
    
    #defining the first input step (not sure if this is neccessary after step above)
    soil_filtered$leaf[1] <- soil_filtered$input_leaves_hourly[1]
    soil_filtered$fruits[1] <- soil_filtered$input_fruits_hourly[1]
    
    for (i in 1:(time)) { #starting from the second timestep due to [i-1]
    
        soil_filtered$leaf[i+1] <- soil_filtered$leaf[i] + soil_filtered$input_leaves_hourly[i+1] - k1*S1*soil_filtered$leaf[i]
        
        soil_filtered$fruits[i+1] <- soil_filtered$fruits[i] + soil_filtered$input_fruits_hourly[i+1] - k3*S3*soil_filtered$fruits[i]
        
        soil_filtered$meta[i+1] <- soil_filtered$meta[i] + a41*k1*S1*soil_filtered$leaf[i] - k4*S4*soil_filtered$meta[i]
        
        soil_filtered$CWD[i+1] <- soil_filtered$CWD[i] + a63*k3*S3*soil_filtered$fruits[i] - k6*S6*soil_filtered$CWD[i]
        
        soil_filtered$struc[i+1] <- soil_filtered$struc[i] + a51*k1*S1*soil_filtered$leaf[i] - k5*S5*soil_filtered$struc[i]
        
        soil_filtered$fastSOM[i+1] <- soil_filtered$fastSOM[i] + k4*S4*a74*soil_filtered$meta[i] + k5*S5*a75*soil_filtered$struc[i] + a76*k6*S6*soil_filtered$CWD[i] - k7*S7*soil_filtered$fastSOM[i]
        
        soil_filtered$slowSOM[i+1] <- soil_filtered$slowSOM[i] + k7*S7*a87*soil_filtered$fastSOM[i] + k5*S5*a85*soil_filtered$struc[i] + a86*k6*S6*soil_filtered$CWD[i]- k8*S8*soil_filtered$slowSOM[i]
        
        soil_filtered$RESP[i+1] <- sum(soil_filtered$meta[i]*k4*S4*(1-a74) + soil_filtered$struc[i]*k5*S5*(1-a85-a75) + soil_filtered$CWD[i]*k6*S6*(1-a76-a86) + soil_filtered$fastSOM[i]*k7*S7*(1-a87-a97) + soil_filtered$slowSOM[i]*k8*S8*(1-a98))
    
    }
    return(soil_filtered)
}

##now make a testrun!!
testrun <- SoilCfunction(time = (length(soil_filtered$TIMESTAMP_END)-1))

#converting Respiration units to µmolC / m2 / s
testrun$RESP_mol <- testrun$RESP * 23.14814815

plot((testrun$input_fruits_hourly)~testrun$TIMESTAMP_END, type="l", main=" Pool of fruit litter" )
plot((testrun$input_leaves_hourly)~testrun$TIMESTAMP_END, type="l", main=" Pool of leaf litter" )

plot(testrun$meta~testrun$TIMESTAMP_END,col="red", type="l", ylim=c(1,2500), main= "Developement of different soil C pools")
lines(testrun$struc~testrun$TIMESTAMP_END,col="blue" )
lines(testrun$CWD~testrun$TIMESTAMP_END,col="black")
lines(testrun$fastSOM~testrun$TIMESTAMP_END,col="green" )
lines(testrun$slowSOM~testrun$TIMESTAMP_END,col="yellow" )
#lines(testrun$RESP~testrun$TIMESTAMP_END,col="purple" )

legend("topleft",col=c( "red", "blue", "black", "green", "yellow"), legend=c( "meta", "struc", "CWD", "fastSOM", "slowSOM"), lty=1,  cex = 0.8, text.width = 0.3, bty="n" )


plot(testrun$RESP_mol~testrun$TIMESTAMP_END,col="purple", type="l", main= "Respiration")


