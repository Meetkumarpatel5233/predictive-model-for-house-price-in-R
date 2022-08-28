train=read.csv("D:\\Trim 3\\R\\Assignment\\housing_train.csv",sep=",")
test=read.csv("D:\\Trim 3\\R\\Assignment\\housing_test.csv",sep=",")
View(train)
View(test)
test$Price=NA
train$data="Train"
test$data="test"
Final=rbind(train,test)
View(Final)

glimpse(Final)
str(Final)
summary(Final)


library(dplyr)
library(psych)
library(ggplot2)
library(skimr)



# For Null


apply(Final,2,function(x)sum(is.na(x)))
Final=Final[-c(2,8,14)]
Final


Final$Bedroom2[is.na(Final$Bedroom2)]=round(mean(Final$Bedroom2,na.rm=T),0)
Final$Bathroom[is.na(Final$Bathroom)]=round(mean(Final$Bathroom,na.rm=T),0)
Final$Car[is.na(Final$Car)]=round(mean(Final$Car,na.rm=T),0)
Final$Landsize[is.na(Final$Landsize)]=round(mean(Final$Landsize,na.rm=T),0)
Final$Bedroom2[is.na(Final$Bedroom2)]=round(mean(Final$Bedroom2,na.rm=T),0)
Final$YearBuilt[is.na(Final$YearBuilt)]=round(mean(Final$YearBuilt,na.rm=T),0)

table(Final$Type)


Final=Final %>%
  mutate(Type_t=as.numeric(Type=="t"),
         Type_u=as.numeric(Type=="u"))
Final=Final %>%
  select(-Type)
glimpse(Final)


table(Final$Method)
Final=Final%>%
  mutate(PI=as.numeric(Method=="PI"),
         SA=as.numeric(Method=="SA"),
         SP=as.numeric(Method=="SP"),
         VB=as.numeric(Method=="VB")) %>%
  select(-Method)
glimpse(Final)


table(Final$CouncilArea)

Final=Final%>%
  mutate(Banyule=as.numeric(CouncilArea=="Banyule"),
         Bayside=as.numeric(CouncilArea=="Bayside"),
         Boroondara=as.numeric(CouncilArea=="Boroondara"),
         Brimbank=as.numeric(CouncilArea=="Brimbank"),
         Darebin=as.numeric(CouncilArea=="Darebin"),
         Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         Monash=as.numeric(CouncilArea=="Monash"),
         Melbourne=as.numeric(CouncilArea=="Melbourne"),
         Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         Manningham=as.numeric(CouncilArea=="Manningham"),
         Kingston=as.numeric(CouncilArea=="Kingston"),
         Hume=as.numeric(CouncilArea=="Hume"),
         HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         Moreland=as.numeric(CouncilArea=="Moreland"),
         PortP=as.numeric(CouncilArea=="Port Phillip"),
         Stonnington=as.numeric(CouncilArea=="Stonnington"),
         Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         Yarra=as.numeric(CouncilArea=="Yarra")) %>%
  select(-CouncilArea)


table(Final$SellerG)
Final=Final %>%
  mutate(nelson=as.numeric(SellerG=="Nelson"),
         Jellis=as.numeric(SellerG=="Jellis"),
         hstuart=as.numeric(SellerG=="hockingstuart"),
         barry=as.numeric(SellerG=="Barry"),
         Marshall=as.numeric(SellerG=="Marshall"),
         Woodards=as.numeric(SellerG=="Woodards"),
         Brad=as.numeric(SellerG=="Brad"),
         Biggin=as.numeric(SellerG=="Biggin"),
         Ray=as.numeric(SellerG=="Ray"),
         Fletchers=as.numeric(SellerG=="Fletchers"),
         RT=as.numeric(SellerG=="RT"),
         Sweeney=as.numeric(SellerG=="Sweeney"),
         Greg=as.numeric(SellerG=="Greg"),
         Noel=as.numeric(SellerG=="Noel"),
         Gary=as.numeric(SellerG=="Gary"),
         Jas=as.numeric(SellerG=="Jas"),
         Miles=as.numeric(SellerG=="Miles"),
         McGrath=as.numeric(SellerG=="McGrath"),
         Hodges=as.numeric(SellerG=="Hodges"),
         Kay=as.numeric(SellerG=="Kay"),
         Stockdale=as.numeric(SellerG=="Stockdale"),
         Love=as.numeric(SellerG=="Love"),
         Douglas=as.numeric(SellerG=="Douglas"),
         Williams=as.numeric(SellerG=="Williams"),
         Village=as.numeric(SellerG=="Village"),
         Raine=as.numeric(SellerG=="Raine"),
         Rendina=as.numeric(SellerG=="Rendina"),
         Chisholm=as.numeric(SellerG=="Chisholm"),
         Collins=as.numeric(SellerG=="Collins"),
         LITTLE=as.numeric(SellerG=="LITTLE"),
         Nick=as.numeric(SellerG=="Nick"),
         Harcourts=as.numeric(SellerG=="Harcourts"),
         Cayzer=as.numeric(SellerG=="Cayzer"),
         Moonee=as.numeric(SellerG=="Moonee"),
         YPA=as.numeric(SellerG=="YPA")) %>%
  select(-SellerG)
glimpse(Final)

t1=round(tapply(Final$Price,Final$Suburb,mean,na.rm=T),0)
View(t1)
sort(t1)

Final=Final %>%
  mutate(
    A1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    A2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    A3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    A4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    A5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    A6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    A7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    A8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    A9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    A10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    A11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    A12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    A13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    A14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    A15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    A16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))) %>%
  select(-Suburb)

glimpse(Final)

test=Final %>%
  filter(data=='test') %>%
  select(-data)
View(test)
Train=Final%>%
  filter(data=='Train') %>%
  select(-data)
View(Train)


set.seed(123)
s=sample(1:nrow(train),0.75*nrow(train))
train75=Train[s,]
train25=Train[-s,]
train25


LRf=lm(Price ~. ,data=train75)
summary(LRf)

LRf=lm(formula = Price ~ Rooms + Bedroom2 + Bathroom + 
         Car + Landsize + YearBuilt + Type_t + Type_u + PI + 
         SP + VB + Banyule +Boroondara + Glen_Eira + 
         Monash + Melbourne +Maribyrnong + Moreland + 
         PortP + Stonnington + Whitehorse + nelson + Jellis + 
         hstuart + Marshall + RT + Sweeney + Miles + McGrath + 
         Kay + Stockdale + Douglas + Raine + Chisholm + Collins + 
         A1 + A2 + A5 + A6 + A7 + A8 + A9 + A10 + 
         A11 + A12 + A13 + A14 + A15 + A16, data = train75)

pp_test=predict(LRf,newdata =train25)
PP_test=round(pp_test,1)
class(PP_test)
View(train25$Price)


res=train25$Price-PP_test
RMSE_train25=sqrt(mean(res^2))
RMSE_train25
212467/RMSE_train25


PP_test_final=predict(LRf,newdata =test)
PP_test_final=round(PP_test_final,1)
class(PP_test_final)

