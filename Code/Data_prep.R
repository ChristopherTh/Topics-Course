library(dplyr)
library(fda)
library(cplm)
#load data
xbox <- read.csv(file="~/Dokumente/Topics/Daten/7_day.csv")
palm <- read.csv(file="~/Dokumente/Topics/Daten/palm.csv")

#concanenate both data frames
data_raw <- rbind(palm,xbox)

data <- rbind(palm,xbox)
data <- na.omit(data)
data_raw <- na.omit(data_raw)
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
########### adding curent average bidder rating   #########################
data["avg. bidder rating"] <- 1
for (i in 1:max(data$group_id)) {
 
  for (j in 1:length(data[data$group_id == i,][,1])) {
    
    data[data$group_id == i,11][j] <- sum(data[data$group_id == i,4][1:j])/j
    
  }
   
}



############# log the bids ################
data_log <- data
data_log[,1] <- log(data[,1])

############ linear interpolate and sampole from grid ######################
grid1 <- c(0, 1, 2, 3, 4, 5, 6, 6.25, 6.5, 6.75, 6.8125, 6.875, 6.9375, 7)
grid <- seq(0, 7, len = 50)
auctions <- matrix(1,length(grid),max(data$group_id))
for (i in 1:max(data$group_id)) {
  
  if (length(data_log[data_log$group_id == i,][,1]) == 1) {
    
    auctions[,i] <- approx(data_log[data_log$group_id == i,][,2] , y = data_log[data_log$group_id == i,][,1] , grid , method="constant" , rule = 0)$y
  } else {
    auctions[,i] <- approx(data_log[data_log$group_id == i,][,2] , y = data_log[data_log$group_id == i,][,1] , grid , method="linear" , rule = 0)$y
  }
}

# number of bids
num_of_bids <- matrix(1,length(grid),max(data$group_id))

for (i in 1:max(data$group_id)) {
  
  if (length(data_log[data_log$group_id == i,][,1]) == 1) {
    
    num_of_bids[,i] <- approx(data_log[data_log$group_id == i,][,2] , y = data_log[data_log$group_id == i,][,'number_of_bids'] , grid , method="constant" , rule = 0)$y
  } else {
    num_of_bids[,i] <- approx(data_log[data_log$group_id == i,][,2] , y = data_log[data_log$group_id == i,][,8] , grid , method="constant" , rule = 0)$y
  }
}

# current avg bidder rating
avg_bidder_rating <- matrix(1,length(grid),max(data$group_id))

for (i in 1:max(data$group_id)) {
  
  if (length(data[data$group_id == i,][,1]) == 1) {
    
    avg_bidder_rating[,i] <- approx(data[data$group_id == i,][,2] , y = data[data$group_id == i,]['avg. bidder rating'] , grid , method="constant" , rule = 0)$y
  } else {
    avg_bidder_rating[,i] <- approx(data[data$group_id == i,][,2] , y = data[data$group_id == i,][,'avg. bidder rating'] , grid , method="linear" , rule = 0)$y
  }
}

########## Splines #############

test   <- smooth.spline(grid, auctions[,204] ,df = 4)
plot(grid ,auctions[,204])
lines(predict(test, grid))
#Plot first deriv
plot(grid,predict(test, grid, deriv = 1)$y)
lines(predict(test, grid, deriv = 1))
# plot second deriv
plot(grid,predict(test, grid, deriv = 2)$y)
lines(predict(test, grid, deriv = 2))
# plot third deriv
plot(grid,predict(test, grid, deriv = 3)$y)
lines(predict(test, grid, deriv = 3))

#auxauxaux <- tp(auctions[,269], degree=3, k=53, by=NULL, allPen=FALSE, varying=NULL, diag=FALSE,
#   knots=c(0, 1, 2, 3, 4, 5, 6, 6.25, 6.5, 6.75, 6.8125, 6.875, 6.9375, 7), 
#   centerscale=NULL, scaledknots=FALSE)
#
#plot(grid,auxauxaux$X)
#plot(grid,auctions[,269])
#lines(grid,auxauxaux$X)

###########constructing design matrix
my.array <- array(NA,dim=c(max(data$group_id),7,length(grid1)))

for (i in 1:max(data$group_id)) {
  
  
  # bids
  aux <- smooth.spline(grid, auctions[,i] ,df = 4)
  my.array[i,1,] <- predict(aux,grid1)$y
  
  # opening bid
  my.array[i,2,] <- data_log[data_log$group_id == i,][1,5]
  
  # final price
  my.array[i,3,] <- data_log[data_log$group_id == i,][1,6]
  
  # early bidding
  my.array[i,4,] <- data_log[data_log$group_id == i,][1,9]
  
  ## jump bidding 
  my.array[i,5,] <- data_log[data_log$group_id == i,][1,10]
  
  ## number of bids
  aux1 <- smooth.spline(grid, num_of_bids[,i] ,df = 4)
  my.array[i,6,] <- predict(aux1,grid1)$y
  # average bidder rating
  aux2 <- smooth.spline(grid, avg_bidder_rating[,i] ,df = 4)
  my.array[i,7,] <- predict(aux2,grid1)$y
}

########## FDA ################
plot(my.array[,1,1], my.array[,7,1])

linearMod <- lm(my.array[,1,9] ~ my.array[,2:7,9], data=as.data.frame(my.array))$coefficients
summary(linearMod)

container <- array(NA,dim=c(14,14))

for (i in 1:14) {
  
  container[,i] <- lm(my.array[,1,i] ~ my.array[,2:7,i], data=as.data.frame(my.array))$coefficients
  
}

plot(grid1,container[2,])
lines(grid1,container[2,])

