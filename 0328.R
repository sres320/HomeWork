
##---1
#讀取柯P粉絲團資料

install.packages("Rfacebook")
library(Rfacebook)
token = "CAACEdEose0cBAJNmw7cEpCtNs3HZCQovhQmkX0WrA3sKKYRdFieS6epMa2sAwCSZBp2FAoFuAXijyaTwfx15UPYSg9M0sfMMHcfDaLx75kr7EDct1PyM9AMqVZCQZAo6fFiN0mpinnYPNMFYZCiULITjV3x4ggjr1eJCfQsdwDoujWHbq6rKPEPPus8PRQcWZBZCXvetn7UUgUiHaBwAoaO"
FBData = GET (paste0 ("https://graph.facebook.com/v2.5/DoctorKoWJ?fields=posts%7Blikes%2Ccomments%2Cshares%7D&access_token=",token))
names(FBData)

#截至2016/01/01至2016/04/10柯P粉絲團一共有57篇文章
totalPage = NULL
lastDate = Sys.Date()
DateVectorStr = as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
  tempPage = getPage("DoctorKoWJ",token,since = DateVectorStr[i],until = DateVectorStr[i+1])
  totalPage = rbind(totalPage,tempPage)
}
nrow(totalPage)


##---2
##每日發文數分析
#因為Facebook記錄時間的方式較特殊,例如柯文哲今年的第一天文章,發表時間(created_time)為"2016-01-01T00:48:51+0000"，為了方便資料的建構，另存記錄時間(datetime)、記錄日期(dateday)及記錄星期(weekdays)，分別省略成"2016-01-01 00:48:51"、"2016-01-01 00:48:51"、"2016-01-01"及"星期五"。

totalPage$datetime = as.POSIXct(totalPage$created_time,format = "%Y-%m-%dT%H:%M:%S+0000",tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$PostDateTPE = format(totalPage$datetime, "%Y-%m-%d",tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays = weekdays(as.Date(totalPage$PostDateTPE))
Posts = totalPage$id
PostCount = aggregate(Posts~PostDateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$Posts,decreasing = T),],10))


##---3
#每日likes數分析

likes = aggregate(likes_count~PostDateTPE,totalPage,mean)
for(i in 1:nrow(PostCount)){
  for(j in PostCount$Posts){
    totalPageLikes = merge(totalPage,likes,by = "PostDateTPE")
  }
}
Likes_Analysis = totalPageLikes[order(totalPageLikes$likes_count.y, decreasing = T), c("PostDateTPE","message","likes_count.y","likes_count.x")]
kable(head(Likes_Analysis[order(Likes_Analysis$likes_count.y, decreasing = T), c("likes_count.y","PostDateTPE")],10))




##---4
#每日comments數分析

comments = aggregate(comments_count~PostDateTPE,totalPage,mean)
for(i in 1:nrow(PostCount)){
  for(j in PostCount$Posts){
    totalPagecomments = merge(totalPage, comments, by = "PostDateTPE")
  }
}
Comments_Analysis = totalPagecomments[order(totalPagecomments$comments_count.y, decreasing = T),c("PostDateTPE","message","comments_count.y","comments_count.x")]
kable(head(Comments_Analysis[order(Comments_Analysis$comments_count.y, decreasing = T),c("comments_count.y","PostDateTPE")],20))




##---5
#每日shares數分析

shares = aggregate(shares_count~PostDateTPE,totalPage,mean)
for(i in 1:nrow(PostCount)){
  for(j in PostCount$Posts){
    totalPageshares = merge(totalPage,shares,by = "PostDateTPE")
  }
}
Shares_Analysis = totalPageshares[order(totalPageshares$shares_count.y, decreasing = T),c("PostDateTPE","message","shares_count.y","shares_count.x")]
kable(head(Shares_Analysis[order(Shares_Analysis$shares_count.y,decreasing = T),c("shares_count.y","PostDateTPE")],20))




















