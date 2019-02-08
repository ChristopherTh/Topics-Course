library(dplyr)
#load data
xbox <- read.csv(file="~/Dokumente/Topics/Daten/7_day.csv")
palm <- read.csv(file="~/Dokumente/Topics/Daten/palm.csv")

#concanenate both data frames
data_raw <- rbind(palm,xbox)

data <- rbind(palm,xbox)

#group auctions by there id
data_raw$group_id <- data_raw %>% group_indices(auctionid) 
data_raw <- data_raw %>% select(-auctionid)

data$group_id <- data %>% group_indices(auctionid) 
data <- data %>% select(-auctionid)
#group xbox auction by id
xbox$group_id <- xbox %>% group_indices(auctionid) 
xbox <- xbox %>% select(-auctionid)
#get information of auction i with command data[data$group_id==i,]


plot(xbox[xbox$group_id==28,][,2],xbox[xbox$group_id==28,][,1])




for (j in 1:max(data$group_id)) {
  bids <- data_raw[data_raw$group_id == j,][,1]

  aux <- integer(length(bids))
  for (i in 1:length(bids)){
    
    if (i == 1) {
      #aux[i] <- bids[i]
      bids[i] <- data_raw[data_raw$group_id == j,][1,5]
    }
    
    
      
        if (bids[i] > max(bids[1:(i-1)])) {
          
          
          
          if ( 0.01 <= bids[i] | bids[i] < 0.99) {
            aux[i] <- bids[i-1] + 0.05
          } else if ( 1 < bids[i] | bids[i] < 4.99) {
            aux[i] <- bids[i-1] + 0.25
          } else if ( 5 < bids[i] | bids[i] < 24.99) {
            aux[i] <- bids[i-1] + 0.50
          } else if ( 25 < bids[i] | bids[i] < 99.99) {
            aux[i] <- bids[i-1] + 1
          }else if ( 100 < bids[i] | bids[i] < 249.99) {
            aux[i] <- bids[i-1] + 2.5
          }else if ( 250 < bids[i] | bids[i] < 499.99) {
            aux[i] <- bids[i-1] + 5
          }else if ( 500 < bids[i] | bids[i] < 999.99) {
            aux[i] <- bids[i-1] + 10
          }else if ( 1000 < bids[i] | bids[i] < 2499.99) {
            aux[i] <- bids[i-1] + 25
          }else if ( 2500 < bids[i] | bids[i] < 4999.99) {
            aux[i] <- bids[i-1] + 50
          }
          else {
            aux[i] <- bids[i-1] + 100
          }
          
        } else {
          
          
          
          if ( 0.01 <= bids[i] | bids[i] < 0.99) {
            aux[i] <- bids[i] + 0.05
          } else if ( 1 < bids[i] | bids[i] < 4.99) {
            aux[i] <- bids[i] + 0.25
          } else if ( 5 < bids[i] | bids[i] < 24.99) {
            aux[i] <- bids[i] + 0.50
          } else if ( 25 < bids[i] | bids[i] < 99.99) {
            aux[i] <- bids[i] + 1
          }else if ( 100 < bids[i] | bids[i] < 249.99) {
            aux[i] <- bids[i] + 2.5
          }else if ( 250 < bids[i] | bids[i] < 499.99) {
            aux[i] <- bids[i] + 5
          }else if ( 500 < bids[i] | bids[i] < 999.99) {
            aux[i] <- bids[i] + 10
          }else if ( 1000 < bids[i] | bids[i] < 2499.99) {
            aux[i] <- bids[i] + 25
          }else if ( 2500 < bids[i] | bids[i] < 4999.99) {
            aux[i] <- bids[i] + 50
          }
          else {
            aux[i] <- bids[i] + 100
          }
        }
    if (i == 1) {
      #aux[i] <- bids[i]
      bids[i] <- data_raw[data_raw$group_id == j,][,1][1]
    }
        
        
    
    
  }
  data[data_raw$group_id == j,][,1] <- aux
}  

time1 <- data[data$group_id==269,][,2]
bids1 <- data[data$group_id==269,][,1]

time2 <- data_raw[data_raw$group_id==269,][,2]
bids2 <- data_raw[data_raw$group_id==269,][,1]

plot(time1,bids1)
plot(time2,bids2)
percentChange(data[data$group_id == 1,][,1])
################## Adding Number of bids Variable #######################
data["number_of_bids"] <- 0

for (i in 1:max(data$group_id)) {
  data[data$group_id == i,8] <- 1:length(data_raw[data_raw$group_id == i,][,1])
}

################# Adding dummy variable 'early bidding'   ####################
data["early_bidding"] <- 0

for (i in 1:max(data$group_id)) {
  if (sum(data[data$group_id == i,2] <= 1.5)> 0.5) {
    data[data$group_id == i,9] <- rep(1,length(data_raw[data_raw$group_id == i,][,1]))
  } else {
    data[data$group_id == i,9] <- rep(0,length(data_raw[data_raw$group_id == i,][,1]))
    
  }
}

################ Addin jump bidding Dummy ########################
data["jump_bidding"] <- NA

for (i in 1:max(data$group_id)) {
  
  if (             sum((data[data$group_id == i,][,1]/(lag(data[data$group_id == i,][,1],1)) -1    )        > 0.3,na.rm = TRUE) > 0.5) {
    data[data$group_id == i,10] <- rep(1,length(data_raw[data_raw$group_id == i,][,1]))
  } else {
    data[data$group_id == i,10] <- rep(0,length(data_raw[data_raw$group_id == i,][,1]))
    
  }
}

