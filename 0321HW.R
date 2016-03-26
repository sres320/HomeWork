#Title: NBA 2014-2015球季各隊分析

install.packages("SportsAnalytics")
library(SportsAnalytics)
NBA1415=fetch_NBAPlayerStatistics("14???15")


##次標題1:各隊最辛苦的球員(標題，說明，程式碼 和結果) 
         #結果要包括隊名.球員姓名.總出場分鐘數.按照總出場分鐘數排序(大到小)

Maxtired=aggregate(TotalMinutesPlayed~Team,NBA1415,max)
NBA1415Maxtired=merge(NBA1415,Maxtired)
print=NBA1415Maxtired(order(NBA1415Maxtired$TotalMinutesPlayed,decreasing = T),c("Team","Name","TotalMinutesPlayed"))
print


##次標題2:各隊得分王(程式碼和結果)
         #輸出要包括隊名與球員姓名與總分.按照總分排序 

MaxPoint=aggregate(TotalPoints~Team,NBA1415,max)
NBA1415MaxPoint=merge(NBA1415,MaxPoint)
print=NBA1415MaxPoint[order(NBA1415MaxPoint$TotalPoints,decreasing = T),c("Team","Name","TotalPoints")]
print


##次標題3:各隊最有效率的球員.程式碼和結果
         #輸出要包括隊名與球員姓名與效率計算結果.按照效率計算結果排序 

NBA1415$best=NBA1415$TotalPoints/NBA1415$TotalMinutesPlayed
Maxbest=aggregate(best~Team,NBA1415,max)
NBA1415Maxbest=merge(NBA1415,Maxbest)
print=NBA1415Maxbest[order(NBA1415Maxbest$best,decreasing = T),c("Team","Name","best")]
print


##次標題4:各隊三分球出手最準的球員.程式碼和結果
         #輸出要包括隊名與球員姓名與三分球命中率.按照三分球命中率排序

NBA1415$good3=NBA1415$ThreesMade/NBA1415$ThreesAttempted
Maxgood3=aggregate(good3~Team,NBA1415,max)
NBA1415Maxgood3=merge(NBA1415,Maxgood3)
print=NBA1415Maxgood3[order(NBA1415Maxgood3$good3,decreasing = T),c("Team","Name","good3")]
print



