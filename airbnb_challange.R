###Libraries
set.seed(123)
setwd("c:/Users/szekely.balazs/Desktop/Challange2")
library(readr)
library(nortest)
library(car)
library(ggmap)
library(ggplot2)
library(fmsb)
adat<-read_csv("listings.csv")

###Formatting variables
adat$neighbourhood_cleansed<-as.factor(adat$neighbourhood_cleansed)
adat$market<-as.factor(adat$market)
adat$property_type<-as.factor(adat$property_type)
adat$room_type<-as.factor(adat$room_type)
adat$bed_type<-as.factor(adat$bed_type)
adat$price<-as.numeric(gsub("\\$","",adat$price))
adat$security_deposit<-as.numeric(gsub("\\$","",adat$security_deposit))
adat$cleaning_fee<-as.numeric(gsub("\\$","",adat$cleaning_fee))

### Excluding cases
adat<-adat[c(1,20,40,45,49,50,52,53,54,55,56,57,58,61,64,65,68,69,75,77,80,81,82,83,84,85,86,94,95)]
length(which(adat$market!="Paris"))
length(adat$price)
adat<-adat[which(adat$market=="Paris"),]
length(which(adat$property_type=="Apartment"))
length(adat$price)
adat<-adat[which(adat$property_type=="Apartment"),]
length(which(!is.na(adat$price)))
length(adat$price)
adat<-adat[which(!is.na(adat$price)),]
length(which(adat$price<=200 & adat$price>=10))
length(adat$price)
adat<-adat[which(adat$price<=200 & adat$price>=10),]

###Descriptives
scale<-list(adat$latitude,adat$longitude,adat$accommodates,adat$bathrooms,
            adat$bedrooms,adat$beds,adat$price,adat$security_deposit,adat$cleaning_fee,
            adat$minimum_nights,adat$maximum_nights,adat$availability_365,adat$number_of_reviews,
            adat$review_scores_rating,adat$review_scores_accuracy,adat$review_scores_cleanliness,
            adat$review_scores_checkin,adat$review_scores_communication,adat$review_scores_location,
            adat$review_scores_value,adat$calculated_host_listings_count,adat$reviews_per_month)

name_sc<-c("Latitude","Longitude","Accommodates","Bathrooms","Bedrooms","Beds","Price",
           "Deposit","CleaningFee","Min_Nights","Max_Nights","Availability","Review_Nr","Rating",
           "Accuracy","Cleanliness","Checkin","Communication","Location","Value","Host_List_Count","Review_monthly")

cat<-list(adat$neighbourhood_cleansed,adat$market,adat$property_type,
          adat$room_type,adat$bed_type)

name_cat<-c("Neighbourhood","Market","Property_type","Room_type","Bed_type")

names(scale)<-name_sc
names(cat)<-name_cat

scale_sum<-lapply(scale,summary)
cat_sum<-lapply(cat,function(x){cbind(summary(x))})

###Price categories (subjective)
adat$pricecat<-cut(adat$price,c(0,31,51,71,101,151,Inf),right=FALSE,
                   labels=c("0-30$/night","31-50$/night","51-70$/night","71-100$/night","101-150$/night",
                   "More than 150$/night"))

###Summary
nei<-summary(adat$neighbourhood_cleansed)
neighb<-as.data.frame(cbind(nei,round(((nei/sum(nei))*100),1),
                            round(tapply(adat$price,adat$neighbourhood_cleansed,mean),1),
                            round(tapply(adat$price,adat$neighbourhood_cleansed,median),0),
                            round(tapply(adat$price,adat$neighbourhood_cleansed,sd),1)))
names(neighb)<-c("Count","Percentage","Mean price","Median price","Std. Dev. price")
new_row<-c(sum(nei),round(((sum(nei)/sum(nei))*100),1),round(mean(adat$price),1),
           round(median(adat$price),1),round(sd(adat$price),1))
neighb<-rbind(neighb,new_row)

chal_func<-function(x,conf.lev){
  z<-qnorm((1-conf.lev)/2,lower.tail=FALSE)
  mean<-mean(x)
  sd<-sd(x)
  conf.low<-mean-z*(sd/sqrt(length(x)))
  conf.up<-mean+z*(sd/sqrt(length(x)))
  res<-c(mean,sd,conf.low,conf.up)
  name<-c("Mean","Std. Deviation",paste(as.character((conf.lev*100)),"% conf. int. lower limit"),
  paste(as.character((conf.lev*100)),"% conf. int. upper limit"))
  names(res)<-name
  return(res)
}

###Percent
ggplot(adat,aes(adat$neighbourhood_cleansed))+geom_bar(aes(
  y=((..count..)/sum(..count..))*100))+coord_flip()+scale_y_continuous(breaks=seq(0,12,1))+scale_x_discrete(
  limits=rev(levels(adat$neighbourhood_cleansed)))+theme(axis.text.y=element_text(size=17))+theme(
  axis.text.x=element_text(size=15))+ggtitle("Percent of apartments in districts")+
  labs(x="",y="")+theme(plot.title=element_text(face="bold",size=30,hjust=0.5))

###Average price per night
ggplot(adat,aes(x=adat$neighbourhood_cleansed,
  y=adat$price,))+stat_summary(fun.y="mean",geom="bar",show.legend=TRUE)+ggtitle("Average price per night (in $)")+
  labs(x="",y="")+theme(plot.title=element_text(face="bold",size=30,hjust=0.5))+scale_y_continuous(breaks=
  seq(0,110,10))+scale_x_discrete(limits=rev(levels(
  adat$neighbourhood_cleansed)))+theme(axis.text.y=element_text(size=17))+theme(
  axis.text.x=element_text(size=15))+coord_flip()

##Maps
map<-get_map(location=c(mean(c(2.3,2.38)),mean(c(48.85,48.87))),maptype="toner",zoom=12)
map2<-get_map(location=c(mean(c(2.3,2.38)),mean(c(48.85,48.87))),zoom=12)


###Density
ggmap(map3)+geom_density2d(data=adat,aes(x=adat$longitude,
  y=adat$latitude), size=0.3)+stat_density2d(data=adat,
  aes(x=adat$longitude,y=adat$latitude,fill=..level..,
  alpha=..level..),size=0.01,bins=16,geom="polygon")+
  scale_fill_gradient(low="green",
  high="red")+scale_alpha(range=c(0.1,0.5),guide=FALSE)+theme(
  axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),
  axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  ggtitle("Density of available apartments in Paris")+theme(legend.position="none")+theme(
  plot.title=element_text(face="bold",size=30,hjust=0.5))

#####Cluster
cl<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:15) cl[i]<-sum(kmeans(adat$price,i)$withinss)
plot(1:15,cl,type="b",xlab="Number of clusters",
     ylab="Within groups sum of squares",pch=20,cex=2)

set.seed(123)
clust<-kmeans(adat$price,6)
adat$price_cl<-clust$cluster
adat$price_cl<-as.factor(adat$price_cl)
clust_lab<-as.character(paste("$",round(c(clust$centers),1),sep=""))
adat$price_cl<-ordered(adat$price_cl,levels=c(1,2,3,4,5,6),labels=clust_lab)

chisq.test(table(adat$neighbourhood_cleansed,adat$price_cl))

ggplot(adat,aes(x=adat$price_cl,
  y=adat$price))+geom_boxplot()+theme(axis.text.x=
  element_text(size=18,face="bold",hjust=1))+scale_x_discrete(labels=
  clust_lab,name="Cluster centers")+scale_y_continuous(breaks=
  seq(0,200,20),name="Price per night (in $)")+theme(axis.title=
  element_text(face="bold",size=18))+ggtitle("Clusters by price (in $)")+theme(plot.title=element_text(face=
  "bold",size=30,hjust=0.5))+theme(axis.text.y=element_text(size=15,face="bold"))


#####ANOVA
nei_lab<-c("Batignolles-\nMonceau", "Bourse", "Buttes-\nChaumont", 
           "Buttes-\nMontmartre", "Élysée", "Entrepôt", "Gobelins", 
           "Hôtel-\nde-Ville", "Louvre", "Luxembourg", "Ménilmontant", 
           "Observatoire", "Opéra", "Palais-\nBourbon", "Panthéon", 
           "Passy", "Popincourt", "Reuilly", "Temple", "Vaugirard")

###Boxplot
ggplot(adat,aes(x=adat$neighbourhood_cleansed,
  y=adat$price))+geom_boxplot()+theme(axis.text.x=
  element_text(size=11,angle=45,hjust=1))+scale_x_discrete(labels=
  nei_lab,name="Districts")+scale_y_continuous(breaks=
  seq(0,200,20),name="Price in $")+theme(axis.title=
  element_text(face="bold",size=12))+ggtitle("Prices in districts (in $)")+theme(
  plot.title=element_text(face="bold",size=30,hjust=0.5))+theme(
  axis.title=element_text(face="bold",size=18))

###Test
sapply(scale,lillie.test)
anova<-aov(adat$price~adat$neighbourhood_cleansed)
summary(anova)
TukeyHSD(anova)

###Map
adat1<-adat[which(adat$neighbourhood_cleansed=="Élysée" | adat$neighbourhood_cleansed=="Hôtel-de-Ville" | adat$neighbourhood_cleansed=="Louvre" | adat$neighbourhood_cleansed=="Luxembourg"),]
adat2<-adat[which(adat$neighbourhood_cleansed=="Reuilly" | adat$neighbourhood_cleansed=="Popincourt" | adat$neighbourhood_cleansed=="Entrepôt" | adat$neighbourhood_cleansed=="Observatoire"),]
adat3<-adat[which(adat$neighbourhood_cleansed=="Bourse" | adat$neighbourhood_cleansed=="Panthéon" | adat$neighbourhood_cleansed=="Passy"),]
adat4<-adat[which(adat$neighbourhood_cleansed=="Buttes-Chaumont" | adat$neighbourhood_cleansed=="Ménilmontant"),]

ggmap(map)+theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),
  axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank())+
  geom_point(data=adat1,aes(x=adat1$longitude,y=adat1$latitude),color="yellow",size=0.6,alpha=1)+
  geom_point(data=adat2,aes(x=adat2$longitude,y=adat2$latitude),color="green",size=0.6,alpha=1)+
  geom_point(data=adat3,aes(x=adat3$longitude,y=adat3$latitude),color="blue",size=0.6,alpha=1)+
  geom_point(data=adat4,aes(x=adat4$longitude,y=adat4$latitude),color="red",size=0.6,alpha=1)+
  ggtitle("Significantly not different districts")+theme(
  plot.title=element_text(face="bold",size=30,hjust=0.5))


#####Regression
adat$mean_pr<-as.factor(ifelse(adat$price>mean(adat$price),1,0))

mod_glm<-glm(adat$mean_pr~adat$neighbourhood_cleansed+adat$room_type+
             adat$bathrooms+adat$bedrooms+adat$beds+adat$bed_type+adat$minimum_nights+adat$maximum_nights+
             adat$number_of_reviews+adat$review_scores_rating+adat$security_deposit+adat$cleaning_fee+
             adat$availability_365,family=binomial(link="logit"))

summary(mod_glm)
NagelkerkeR2(mod_glm)

mod_glm2<-glm(adat$mean_pr~adat$neighbourhood_cleansed+adat$room_type+
              adat$bathrooms+adat$bedrooms+adat$beds+adat$bed_type+adat$minimum_nights+adat$maximum_nights+
              adat$number_of_reviews+adat$review_scores_rating+adat$security_deposit+adat$cleaning_fee+
              adat$availability_365,family=binomial(link="logit"))

summary(mod_glm2)
NagelkerkeR2(mod_glm2)

mod_glm3<-glm(adat$mean_pr~adat$neighbourhood_cleansed+adat$room_type+
              adat$bathrooms+adat$bedrooms+adat$beds+adat$bed_type+adat$number_of_reviews+
              adat$review_scores_rating+adat$security_deposit+adat$cleaning_fee+adat$availability_365,family=
              binomial(link="logit"))

summary(mod_glm3)
NagelkerkeR2(mod_glm3)