
# �ХιϪ�e�{1928-1969���A�p��·��b����U�{���o�Ͳv�ܤ�
# �аȥ���`ggplot2` package���ϡA�e�{�U�{�A�U�~�A�o�Ͳv���ܤ�
# �`�N:1955�~�}�l���p��·��̭]

# - Title�G1928-1969���A�p��·��b����U�{���o�Ͳv�ܤ� (`5 pt`) 

# - �����D1�G��ƫe�B�z (`20 pt`) 
#     - ����Ū�i�� (`5 pt`) 
polio = read.csv("POLIO_Incidence.csv",stringsAsFactors=F)
head(polio)

#     - �N�e����ର����� (`5 pt`) 
install.packages("reshape")
library(reshape)
polio.m = melt(polio,id.vars=c('YEAR','WEEK'),stringsAsFactors=F)
head(polio.m)

#     - �B�z�ʭ� (`5 pt`) 
polio.m[polio.m$value == "-",]$value = NA                     #�B�z�ʭ�,�N"-"�ରNA
polio.m$value = as.numeric(as.character(polio.m$value))       ###�Nvalue����ର�Ʀr

#     - �p��~�׵o�Ͳv (`5 pt`) 
polio.sumYear = aggregate(value~YEAR+variable,data=polio.m,FUN=sum,na.rm=F)#�U�{�U�~�ץ[�`,�p��Ӧ~�ת��`�o�Ͳv
head(polio.sumYear)


# - �����D2�G��ı�e�e�{ (`80 pt`) 
install.packages("ggplot2")
library(ggplot2)
ggplot(polio.sumYear)+geom_line(aes(x=YEAR,y=value,color=variable))+geom_vline(xintercept=1955,colour="black",linetype="longdash")
#��Ƭ�polio.sumYear
#geom_line:�e��u��
#�̭]

#     - �����p���ܹϧκ��� (`10 pt`) 
#     - �{���X (`20 pt`) 
#     - �ϧΧe�{ (`40 pt`)�A���ӬO�_�i�H���������ϧε��� 
#     - �����ϧ� (`10 pt`)
p=ggplot(polio.sumYear, aes(YEAR, variable)) + geom_tile(aes(fill = polio.sumYear$value),colour = "white") + scale_fill_gradient(name="�o�Ͳv", low = "white",high = "Navy")+ geom_vline(xintercept = 1955,colour= "black",linetype = "longdash" )
p




