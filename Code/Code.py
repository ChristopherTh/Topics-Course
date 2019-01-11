import pandas as pd
import matplotlib.pyplot as plt
from scipy.interpolate import UnivariateSpline
import numpy as np

Path = "D:/Topics/Daten/7_day.csv"



auction_data = pd.read_csv(Path)



#print(auction_data.describe())

#print(auction_data['auctionid'].nunique())



#print(auction_data.head(30))





#a = auction_data['bid'].iloc[0:12].interpolate()



a = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[75]['bid']).interpolate()
bbbbbb = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[9]['bidtime'])

#y = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[75]['bid'])
#x = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[75]['bidtime'])

#s = UnivariateSpline(x, y,k=1)

#xs = np.linspace(1, 7, 100)
#ys = s(xs)
#plt.plot(x, y, '.-')
#plt.plot(xs, ys)
    

#print(s(np.array([1,2,3,4,5,6,7])))

#pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[0]['bid']).interpolate() #Nummer in iloc = auktion 1

plt.xlabel('Day of Auction')
plt.ylabel('Bid Amount')

auction_one_bid = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[30]['bid'])
auction_one_bidtime = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[30]['bidtime'])

#original
plt.scatter(auction_one_bidtime,auction_one_bid)
plt.plot(auction_one_bidtime,auction_one_bid)

#cleaned
plt.scatter(auction_one_bidtime.drop(11).drop(18),auction_one_bid.drop(11).drop(18))
plt.plot(auction_one_bidtime.drop(11).drop(18),auction_one_bid.drop(11).drop(18))

#log
plt.scatter(auction_one_bidtime.drop(11).drop(18),np.log(auction_one_bid.drop(11).drop(18)))
plt.plot(auction_one_bidtime.drop(11).drop(18),np.log(auction_one_bid.drop(11).drop(18)))




auction_two_bid = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[9]['bid'])
auction_two_bidtime = a = pd.DataFrame(auction_data.groupby('auctionid').agg(lambda x: x.tolist()).iloc[9]['bidtime']) #74


#Original
plt.scatter(auction_two_bidtime,auction_two_bid)
plt.plot(auction_two_bidtime,auction_two_bid)


#cleaned
plt.scatter(auction_two_bidtime.drop(1).drop(15),auction_two_bid.drop(1).drop(15))
plt.plot(auction_two_bidtime.drop(1).drop(15),auction_two_bid.drop(1).drop(15))

#log
plt.scatter(auction_two_bidtime.drop(1).drop(15),np.log(auction_two_bid.drop(1).drop(15)))
plt.plot(auction_two_bidtime.drop(1).drop(15),np.log(auction_two_bid.drop(1).drop(15)))

#Spline
zz = UnivariateSpline(auction_two_bidtime.drop(1).drop(15), np.log(auction_two_bid.drop(1).drop(15)),k=1)
xs = auction_two_bidtime.drop(1).drop(15)#np.linspace(1, 7, 100000)
ys = zz(xs)
plt.scatter(xs,ys)

zz = UnivariateSpline(auction_two_bidtime.drop(1).drop(15), np.log(auction_two_bid.drop(1).drop(15)),k=5,s=3000)
xs = auction_two_bidtime#
ys = zz(xs)
plt.plot(xs,ys)
plt.scatter(auction_two_bidtime.drop(1).drop(15),np.log(auction_two_bid.drop(1).drop(15)))



#test1 = np.log(test1)
#test2 = test2
test3 = np.log(test3)
test4 = test4

zz = UnivariateSpline(test2, test1,k=1)
xs = np.linspace(1, 7, 100)
ys = zz(xs)
#plt.plot(test2, test1, '.-')
#plt.plot(xs, ys)
#auction_data[['bid','bidtime']].iloc[0:12].plot.scatter('bidtime','bid')

#test11234 = plt.figure()

#plt.xlabel('Day of Auction')
#plt.ylabel('Bid Amount')
#plt.scatter(test4,test3)
#plt.plot(test4,test3)

#.savefig("test123.pdf",bbox_inches='tight')
#plt.scatter(test4,test3)

#plt.show()

#plt.savefig('Auction2.pdf')
