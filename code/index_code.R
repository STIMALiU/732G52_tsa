

#-------------------------------------------------------------------------------
# Kompendium om index
#-------------------------------------------------------------------------------
# se: https://liuonline.sharepoint.com/:b:/r/sites/Lisam_732G52_2024HT_GU/CourseDocuments/N%C3%A5got%20om%20Index.pdf?csf=1&web=1&e=VgTXES

# nedan visas lösningar till uppgifterna 1, 3 och 4 i R-kod.
# testa att lösa dem själv med kod innan ni tittar nedan.








#-------------------------------------------------------------------------------
# Uppgift 1
#-------------------------------------------------------------------------------


year<-1972:1976
price<-c(225.5,227,230,240,242)
index<-price/price[1]*100

plot(year,index,t="l")

index<-price/price[5]*100
plot(year,index,t="l")


#-------------------------------------------------------------------------------
# Uppgift 3
#-------------------------------------------------------------------------------

year<-1988:1992

sales<-c(12505,12510,13750,14500,15080)
index<-c(100,105,107,110,113)

# vad ska vi göra?

step1<-sales/index
step2<-step1*107
step2


#-------------------------------------------------------------------------------
# Uppgift 4
#-------------------------------------------------------------------------------

year<-1990:1993
# vitvaror
sales1<-c(3.7,3.9,3.8,4.8)
# radio/TV
sales2<-c(4.9,5.2,6.4,5.2)
cbind(year,sales1,sales2)

# antal
belinda_count<-c(155,149,132,160)
fluxor_count<-c(273,281,288,230)

# årsmedelpris:
belinda_price<-c(6500,6700,6800,7000)
fluxor_price<-c(5200,5500,5700,5800)
cbind(year,belinda_count,belinda_price,fluxor_count,fluxor_price)


#-------------------------------------------------------------------------------
# a)
# beräkna sammansatt kedjeindex med årslänkar av Laspeyre-typ
# basår 1990


# vikter
# baseras på total försäljning

sales1[1]/(sales1[1]+sales2[1])
sales2[1]/(sales1[1]+sales2[1])

w1<-sales1[1:3]/(sales1[1:3]+sales2[1:3])
w2<-sales2[1:3]/(sales1[1:3]+sales2[1:3])
cbind(year,c(NA,w1),c(NA,w2))

weight1<-round(sales1[-4]/(sales1[-4]+sales2[-4]),3)

weight2<-round(sales2[-4]/(sales1[-4]+sales2[-4]),3)

belinda_price[2]/belinda_price[1]*weight1[1]*100+fluxor_price[2]/fluxor_price[1]*weight2[1]*100

# Laspeyre-länkar
links<-round((belinda_price[2:4]/belinda_price[1:3]*weight1 + fluxor_price[2:4]/fluxor_price[1:3]*weight2)*100,2)

# beräkna index???
index<-round(c(100,cumprod(links/100)*100),2)
#cumsum(1:5)
#cumprod(1:5)
#links[1]/100*links[2]
cbind(year,index)

#-------------------------------------------------------------------------------
# b)
sales_tot<-sales1+sales2
cbind(year,sales_tot)
index<-c(100,104.6,107.5,109.8)

round(sales_tot/index*index[2],2)



