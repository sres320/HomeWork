
# 請用圖表呈現1928-1969間，小兒麻痺在美國各州的發生率變化
# 請務必用`ggplot2` package做圖，呈現各州，各年，發生率的變化
# 注意:1955年開始有小兒麻痺疫苗

# - Title：1928-1969間，小兒麻痺在美國各州的發生率變化 (`5 pt`) 

# - 次標題1：資料前處理 (`20 pt`) 
#     - 把資料讀進來 (`5 pt`) 
polio = read.csv("POLIO_Incidence.csv",stringsAsFactors=F)
head(polio)

#     - 將寬表格轉為長表格 (`5 pt`) 
install.packages("reshape")
library(reshape)
polio.m = melt(polio,id.vars=c('YEAR','WEEK'),stringsAsFactors=F)
head(polio.m)

#     - 處理缺值 (`5 pt`) 
polio.m[polio.m$value == "-",]$value = NA                     #處理缺值,將"-"轉為NA
polio.m$value = as.numeric(as.character(polio.m$value))       ###將value欄位轉為數字

#     - 計算年度發生率 (`5 pt`) 
polio.sumYear = aggregate(value~YEAR+variable,data=polio.m,FUN=sum,na.rm=F)#各州各年度加總,計算該年度的總發生率
head(polio.sumYear)


# - 次標題2：視覺畫呈現 (`80 pt`) 
install.packages("ggplot2")
library(ggplot2)
ggplot(polio.sumYear)+geom_line(aes(x=YEAR,y=value,color=variable))+geom_vline(xintercept=1955,colour="black",linetype="longdash")
#資料為polio.sumYear
#geom_line:畫折線圖
#疫苗

#     - 解釋如何選擇圖形種類 (`10 pt`) 
#     - 程式碼 (`20 pt`) 
#     - 圖形呈現 (`40 pt`)，按照是否可以輕易看懂圖形給分 
#     - 解釋圖形 (`10 pt`)
p=ggplot(polio.sumYear, aes(YEAR, variable)) + geom_tile(aes(fill = polio.sumYear$value),colour = "white") + scale_fill_gradient(name="發生率", low = "white",high = "Navy")+ geom_vline(xintercept = 1955,colour= "black",linetype = "longdash" )
p




