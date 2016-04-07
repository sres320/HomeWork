
##---1
#讀取柯P粉絲團資料

install.packages("Rfacebook")
library(Rfacebook)
token = "CAACEdEose0cBABB0wxI4qHhqJgq0vIbNHZBfR1lT0QjEAOEEiA66MW15bO2BmZCOEQtWeZAq8Dg4YcdSkwD0DseEGbq0DPNA12XxgVjs4wnsFNV893XvasENPa02CyFGdpHm4zekpiM4s3ZAOaghNt3vbdcIhWgbDXa6OnLYQ31fN0oG7ZA3JiIB1itb6krRm66FgVZAsm5mjSN1mtrWpj"
FBData = GET (paste0 ("https://graph.facebook.com/v2.5/DoctorKoWJ?fields=posts%7Blikes%2Ccomments%2Cshares%7D&access_token=",token))
names(FBData)

#截至2016/01/01至2016/04/06柯P粉絲團一共有56篇文章*^%$####$%^^&^$##%&&%$
totalPage = NULL
lastDate = Sys.Date()
DateVectorStr = as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage <- getPage("DoctorKoWJ", token, since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage <- rbind(totalPage,tempPage)
}
nrow(totalPage)


##---2
##每日發文數分析
#因為Facebook記錄時間的方式較特殊,例如柯文哲今年的第一天文章,發表時間(created_time)為"2016-01-01T00:48:51+0000"，為了方便資料的建構，另存記錄時間(datetime)、記錄日期(dateday)及記錄星期(weekdays)，分別省略成"2016-01-01 00:48:51"、"2016-01-01 00:48:51"、"2016-01-01"及"星期五"。
totalPage$datetime = as.POSIXct(totalPage$created_time,format = "%Y-%m-%dT%H:%M:%S+0000",tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$PostDate = format(totalPage$datetime, "%Y-%m-%d",tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays = weekdays(as.Date(totalPage$dateTPE))
Posts = totalPage$id
PostCount = aggregate(Posts~PostDate,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$Posts,decreasing = T),]))



##---3
#每日likes數分析

likes = aggregate(likes_count~PostDate,totalPage,mean)
totalPagelikes = merge(totalPage,likes)
Like_Analysis = totalPagelikes[order(totalPagelikes$likes_count,decreasing=T),c("PostDate","message","likes_count")]


##---4
#每日comments數分析

comments = aggregate(comments_count~PostDate,totalPage,mean)
totalPagelikes = merge(totalPage,comments)
Comments_Analysis = totalPagelikes[order(totalPagelikes$comments_count,decreasing=T),c("PostDate","message","comments_count")]


##---5
#每日shares數分析
shares <- aggregate(shares_count~PostDate,totalPage,mean)
totalPagelikes = merge(totalPage,shares)
Shares_Analysis = totalPagelikes[order(totalPagelikes$shares_count,decreasing=T),c("PostDate","message","shares_count")]






















