install.packages("ggmap")
install.packages("ggedit")
install.packages("ggplot2")
devtools::install_github('hadley/ggplot2')
remove.packages("ggplot2")
library(plotly)
library(ggedit)
library(ggmap)
library(maps)
library(ggplot2)
library(sp)
library(ggplot2)
hkmap = readRDS("HKG_adm1.rds")
thismap<-hkmap
ListName = data.frame(Id=thismap$ID_1, Code=thismap$HASC_1, Name=thismap$NAME_1)
ListName$Code = gsub('HK.', '', as.character(ListName$Code))
RegionHK = c("CW", "EA", "SO", "WC")
RegionKLW = c("SS", "KC",  "YT")
RegionKLE<-c("WT","KU")
RegionNTE<-c("NO","TP","SK","ST")
RegionNTW<-c("TW","TM","YL","KI","IS")

ListName$Region[ListName$Code %in% RegionHK] = 'Hong Kong'
ListName$Region[ListName$Code %in% RegionKLW] = 'Kowloon West'
ListName$Region[ListName$Code %in% RegionKLE] = 'Kowloon East'
ListName$Region[ListName$Code %in% RegionNTE] = 'New Territories East'
ListName$Region[ListName$Code %in% RegionNTW] = 'New Territories West'
ListName<-na.omit(ListName)
ListName = ListName[order(ListName$Region),]
ListName$NewID = seq(1,dim(ListName)[1])
ListName[,c("NewID", "Region", "Code", "Name")]
hkmapdf <- fortify(hkmap)
hkmapdf = merge(hkmapdf, ListName, by.x="id", by.y="Id")
head(hkmapdf)

hkstations<-read.csv('/Users/lituntun/Desktop/MJ/social media analysis/HK.csv',header=FALSE,fill=TRUE)
names(hkstations)<-c("station_number","station_name","1","2","3","4","totol","address_list","district")
for(i in 1:101){
  hkstations$max[i]<-max(hkstations$`1`[i],hkstations$`2`[i],hkstations$`3`[i],hkstations$`4`[i])}
hkstations$name[hkstations$max==hkstations$`1`]<-"AU NOK HIN"
hkstations$party[hkstations$max==hkstations$`1`]<-"pan- democrat"
hkstations$name[hkstations$max==hkstations$`2`]<-"NG DICK HAY"
hkstations$party[hkstations$max==hkstations$`2`]<-"pro-establishment"
hkstations$name[hkstations$max==hkstations$`3`]<-"YUM EDWARD LIANG HSIEN"
hkstations$party[hkstations$max==hkstations$`3`]<-"pan- democrat"
hkstations$name[hkstations$max==hkstations$`4`]<-"CHAN JUDY KAPUI"
hkstations$party[hkstations$max==hkstations$`4`]<-"pro-establishment"
whole_position<-data.frame()
for(address in hkstations$address_list){
  position<-geocode(address)
  whole_position<-rbind(whole_position,position)
}
hkstations$lon<-whole_position$lon
hkstations$lat<-whole_position$lat
hkstations<-na.omit(hkstations)

kowloon_west<-read.csv('/Users/lituntun/Desktop/MJ/social media analysis/kowloon.csv',header=FALSE,fill=TRUE)
kowloon_west<-kowloon_west[1:72,1:8]
names(kowloon_west)<-c("station_number","station_name","1","2","3","totol","address_list","district")
for(i in 1:72){
  kowloon_west$max[i]<-max(kowloon_west$`1`[i],kowloon_west$`2`[i],kowloon_west$`3`[i])}
kowloon_west$name[kowloon_west$max==kowloon_west$`1`]<-"YIU CHUNG YIM"
kowloon_west$party[kowloon_west$max==kowloon_west$`1`]<-"pan- democrat"
kowloon_west$name[kowloon_west$max==kowloon_west$`2`]<-"CHENG WING SHUN VINCENT"
kowloon_west$party[kowloon_west$max==kowloon_west$`2`]<-"pro-establishment"
kowloon_west$name[kowloon_west$max==kowloon_west$`3`]<-"TSOI TUNG CHAU"
kowloon_west$party[kowloon_west$max==kowloon_west$`3`]<-"pan- democrat"

NT_east<-read.csv('/Users/lituntun/Desktop/MJ/social media analysis/NT.csv',header=FALSE,fill=TRUE)
names(NT_east)<-c("station_number","station_name","1","2","3","4","5","6","totol","address_list","district")
for(i in 1:143){
  NT_east$max[i]<-max(NT_east$`1`[i],NT_east$`2`[i],NT_east$`3`[i],NT_east$`4`[i],NT_east$`5`[i],NT_east$`6`[i])
}
NT_east$name[NT_east$max==NT_east$`1`]<-"WONG SING CHI"
NT_east$party[NT_east$max==NT_east$`1`]<-"pan- democrat"
NT_east$name[NT_east$max==NT_east$`2`]<-"FONG KWOK SHAN CHRISTINE"
NT_east$party[NT_east$max==NT_east$`2`]<-"neither"
NT_east$name[NT_east$max==NT_east$`3`]<-"CHAN YUK NGOR ESTELLA"
NT_east$party[NT_east$max==NT_east$`3`]<-"pan- democrat"
NT_east$name[NT_east$max==NT_east$`4`]<-"TANG KA PIU"
NT_east$party[NT_east$max==NT_east$`4`]<-"pro-establishment"
NT_east$name[NT_east$max==NT_east$`5`]<-"CHIU PUI YUK JOYCE"
NT_east$party[NT_east$max==NT_east$`5`]<-"neither"
NT_east$name[NT_east$max==NT_east$`6`]<-"FAN GARY KWOK WAI"
NT_east$party[NT_east$max==NT_east$`6`]<-"pan- democrat"

whole_position_1<-data.frame()
for(address in kowloon_west$address_list){
  position<-geocode(address)
  whole_position_1<-rbind(whole_position_1,position)
}
kowloon_west$lon<-whole_position_1$lon
kowloon_west$lat<-whole_position_1$lat
kowloon_west<-na.omit(kowloon_west)
whole_position_2<-data.frame()
for(address in NT_east$address_list){
  position<-geocode(address)
  whole_position_2<-rbind(whole_position_2,position)
}
NT_east$lon<-whole_position_2$lon
NT_east$lat<-whole_position_2$lat
NT_east<-na.omit(NT_east)

hkstations$Region<-rep(NA,60)
hkstations$group<-rep('1.1',60)
kowloon_west$Region<-rep('HK',43)
kowloon_west$group<-rep('1.1',43)
NT_east$Region<-rep('HK',101)
NT_east$group<-rep('1.1',101)

p<-ggplot(data=hkmapdf,aes(long, lat,group=group, fill=Region)) +
  geom_polygon() + 
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Hong Kong 2018 Legislative Council Election Result")+
  geom_point(data=hkstations,aes(lon,lat,size=max,color=name,shape=party))+scale_size_continuous(range = c(0, 2.8))+scale_color_brewer(palette = 'Set1')+
  geom_point(data=kowloon_west,aes(lon,lat,size=max,color=name,shape=party))+
  geom_point(data=NT_east,aes(lon,lat,size=max,color=name,shape=party))
p

hkstations<-hkstations[,-c(3,4,5,6,7)]
kowloon_west<-kowloon_west[,-c(3,4,5,6)]
NT_east<-NT_east[,-c(3,4,5,6,7,8)]
NT_east<-NT_east[,-3]
stations<-rbind(hkstations,kowloon_west,NT_east)  
stations$Region<-NA
stations$group<-NA


p1<-data.frame(table(stations$party)) 
p2<-data.frame(table(kowloon_west$party)) 
p3<-data.frame(table(NT_east$party)) 
p1
pie1<-plot_ly(p1,labels=~Var1,values=~Freq,type="pie",
             textposition="inside",textinfo="label+percent",insidetextfont=list(color="#000000"))%>%
  layout(title="Election results by party",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie1 

pie2<-plot_ly(p2,labels=~Var1,values=~Freq,type="pie",
             textposition="inside",textinfo="label+percent",insidetextfont=list(color="#000000"))%>%
  layout(title="Election results by party in Kowloon West",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie2

pie3<-plot_ly(p3,labels=~Var1,values=~Freq,type="pie",
              textposition="inside",textinfo="label+percent",insidetextfont=list(color="#000000"))%>%
  layout(title="Election results by party in NT East",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
pie3

pie(p1,main="hha",radius=1)
par(mfrow=c(1,3)) 
subplot(pie1,pie2,pie3)


Sys.setenv("plotly_username"="LiTongtong")
Sys.setenv("plotly_api_key"="pj2y1IXwrgVRPwDGtTVJ")

chart_link = api_create(p, filename="Hong Kong 2018 Legislative Council Election Result")  
chart_link

legend()
ggplotly(p)
?legend
?map_data




mytext=paste("votes:", stations$max, "\n" , "Elected Candidate: ", stations$name, "\n", "party:",stations$party,  sep="")   
pp<-plotly_build(p)
style(pp,text=mytext,hoverinfo="text")
ggplotly(p)


  