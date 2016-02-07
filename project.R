library(stringr)
library(downloader)
library(openxlsx)
library(lubridate)
library(mongolite)


url1<-"http://www.usaid.gov/opengov/developer/datasets/SCMS_Delivery_History_Dataset_20150929.csv"
supplyD<-read.csv(url1)
summary(supplyD)


#Function toconvert dates into date class and w ger deliver time and delay time by converting dates in to nmeric
deriveDates<-function(){
  dateSent<- mdy(supplyD$PO.Sent.to.Vendor.Date)# converting all the element sin the flihtdate column to date class
  dateScheduled <- dmy(supplyD$Scheduled.Delivery.Date)
  dateDelivered<- dmy(supplyD$Delivered.to.Client.Date)
  
  dateSentScore<-as.numeric(dateSent)/86400
  dateScheduledScore<-as.numeric(dateScheduled)/86400
  dateDeliveredScore<-as.numeric(dateDelivered)/86400
  dateScores<-cbind(dateSentScore,dateScheduledScore,dateDeliveredScore)
  dateScores<-as.data.frame(dateScores)
  colnames(dateScores)<-c("sent","scheduled","delivered")
  delivTime<-c()
  delayTime<-c()
  for(i in 1:length(dateScores$sent)){
    dT<-dateScores$delivered[i]-dateScores$sent[i]
    deT<-dateScores$delivered[i]-dateScores$scheduled[i]
    delivTime<-c(delivTime,dT)
    delayTime<-c(delayTime,deT)
  }
  
  supplyData$PO_Sent_to_Vendor_Date<-dateSent
  supplyData$Scheduled_Delivery_Date<-dateScheduled
  supplyData$Delivered_t_Client_Date<-dateDelivered
  supplyData$Year<-year(dateDelivered)
  supplyData$Delivery_Time<-delivTime
  supplyData$Delay_Time<-delayTime
  print(supplyData)
  
}

#function to specify product name, we choose the name from molecule list if brand name is generic
dataProduct<-function(){
  productData<-c()
  for(i in 1:length(supplyD$Brand)){
    if(supplyD$Brand[i]=="Generic"){
      prD<-as.character(supplyD$Molecule.Test.Type[i])
      
    }
    else{
      prD<-as.character(supplyD$Brand[i])
    }
    productData<-c(productData,prD)
  }
  supplyData$Product_Name<-productData
  print (supplyData)
}

#the following function splits the elements into 3 parts depending on the sym bol / and assigns the column values depending on user input
#x=column name,y=data frame the new data to be written to, b= 2nd and 3rd column NA values(giveNA),c= 1st column empt values generally 'test' for molecule name
# NA for dosage
splitElements<-function(x,y,b,c,d,e,f){
  x<-as.character(x)
  mol1<-c()
  mol2<-c()
  mol3<-c()
  for(i in 1:length(x)){
    if (length(grep("HIV",x[i],perl=T,value=F))>0){
      v1<-c
      v2<-b
      v3<-b
    }
    else {
      v1<-strsplit(x[i],"[/]")[[1]][1]
      v2<-strsplit(x[i],"[/]")[[1]][2]
      v3<-strsplit(x[i],"[/]")[[1]][3]
    }
    mol1<-c(mol1,v1)
    mol2<-c(mol2,v2)
    mol3<-c(mol3,v3)
  }
  
  y$d<-mol1
  y$e<-mol2
  y$f<-mol3
  names(y)[names(y) == 'd'] <- d
  names(y)[names(y) == 'e'] <- e
  names(y)[names(y) == 'f'] <- f
  
  
  y
}

#divide the dosage into two columns. one column with mg/ml and one with mg
designateDosageUnit<-function(){
  mlData<-c()
  mgData<-c()
  mg1Data<-c()
  for(i in 1:length(supplyD$Dosage)){
    if (length(grep("mg/ml",supplyD$Dosage[i],perl=T,value=F))>0){
      dVal<-strsplit(as.character(supplyD$Dosage[i]),"mg/ml")[[1]]
      
    }
    else{
      dVal<-NA
    }
    mlData<-c(mlData,dVal)
  }
  for(i in 1:length(supplyD$Dosage)){
    if (length(grep("mg/ml",supplyD$Dosage[i],perl=T,value=F))>0){
      dVal1<-NA
      
    }
    else if (length(grep("mg",supplyD$Dosage[i],perl=T,value=F))>0){
      dVal1<-strsplit(as.character(supplyD$Dosage[i]),"mg")[[1]][1]

    }
    else if (length(grep("g",supplyD$Dosage[i],perl=T,value=F))>0){
      dVal1<-strsplit(as.character(supplyD$Dosage[i]),"g")[[1]]
      dVal1<-as.character(as.numeric(dVal1)*1000)

    }
    else{
      dVal1<-NA
      
    }
    mgData<-c(mgData,dVal1)
  
  }
  for(i in 1:length(supplyD$Dosage)){

    if (length(grep("mg",supplyD$Dosage[i],perl=T,value=F))>0){
      dVal1<-strsplit(as.character(supplyD$Dosage[i]),"mg")[[1]][1]
      
    }
    else if (length(grep("g",supplyD$Dosage[i],perl=T,value=F))>0){
      dVal1<-strsplit(as.character(supplyD$Dosage[i]),"g")[[1]]
      dVal1<-as.character(as.numeric(dVal1)*1000)
      
    }
    else{
      dVal1<-NA
      
    }
    mg1Data<-c(mg1Data,dVal1)
    
  }
  supplyData$Dosage_ml_mg<-mlData
  supplyData$Dosage_mg<-mgData
  supplyData$Dosage_Unit_mg<-mg1Data
  
  supplyData

}

#designate donor=T if pack values is less than 2.5 dollars
designateDonor<-function(){
  desData<-c()
  for(i in 1:length(supplyD$Pack.Price)){
    if(supplyD$Pack.Price[i]<=2.5){
      des<-"Y"
    }
    else{
      des<-"N"
    }
    desData<-c(desData,des)
  }
  supplyData$Donation_Designation<-desData
  print(supplyData)
}

#popullating the supply data dataframe from the initial dataframe

populateData<-function(){
  supplyData<-data.frame("ID"=supplyD$ID)
  supplyData$Project_Code<-as.character(supplyD$Project.Code)
  supplyData$PO_SO<-as.character(supplyD$PO...SO..)
  supplyData$ASN_DN<-as.character(supplyD$ASN.DN..)
  supplyData$Country<-as.character(supplyD$Country)
  supplyData$Managed_By<-as.character(supplyD$Managed.By)
  supplyData$Fulfill_Via<-as.character(supplyD$Fulfill.Via)
  supplyData$Shipment_Mode<-as.character(supplyD$Shipment.Mode)
  supplyData<-deriveDates()
  supplyData$Product_Group<-supplyD$Product.Group
  supplyData$Vendor<-supplyD$Vendor
  supplyData$Molecule_Test_Type<-supplyD$Molecule.Test.Type
  supplyData<-dataProduct()
  supplyData<-splitElements(supplyD$Molecule.Test.Type,supplyData,NA,"test","Molecule_First_Test","Molecule_Second","Molecule_Third")
  supplyData<-designateDosageUnit()
  supplyData<-splitElements(supplyData$Dosage_Unit_mg,supplyData,"0","0","Dosage_First","Dosage_Second","Dosage_Third")
  supplyData$Dosage_Form<-as.character(supplyD$Dosage.Form)
  supplyData$Unit_of_Measure_Per_Pack<-as.numeric(supplyD$Unit.of.Measure..Per.Pack.)
  supplyData$Line_Item_Quantity<-as.numeric(supplyD$Line.Item.Quantity)
  supplyData$Pack_Price<-supplyD$Pack.Price
  supplyData<-designateDonor()
  supplyData$First_Line_Designation<-as.character(supplyD$First.Line.Designation)
  supplyData$Weight_Kilograms<-as.numeric(as.character(supplyD$Weight..Kilograms.))
  supplyData$Line_Item_Insurance<-as.numeric(supplyD$Line.Item.Insurance..USD.)
  
  
  #converting the NA values in the nmeric columns to 0
  supplyData$Delivery_Time[is.na(supplyData$Delivery_Time)]<-0
  supplyData$Delay_Time[is.na(supplyData$Delay_Time)]<-0
  supplyData$Dosage_First[is.na(supplyData$Dosage_First)]<-0
  supplyData$Dosage_Second[is.na(supplyData$Dosage_Second)]<-0
  supplyData$Dosage_Third[is.na(supplyData$Dosage_Third)]<-0
  supplyData$Unit_of_Measure_Per_Pack[is.na(supplyData$Unit_of_Measure_Per_Pack)]<-0
  supplyData$Line_Item_Quantity[is.na(supplyData$Line_Item_Quantity)]<-0
  supplyData$Pack_Price[is.na(supplyData$Pack_Price)]<-0
  supplyData$Weight_Kilograms[is.na(supplyData$Weight_Kilograms)]<-0
  supplyData$Line_Item_Insurance[is.na(supplyData$Line_Item_Insurance)]<-0
  
}



url2<-"http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/MDG_0000000029&profile=crosstable&filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY&x-topaxis=GHO;YEAR"
url3<-"http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/HIV_0000000001&profile=crosstable&filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY&x-topaxis=GHO;YEAR"
url4<-"http://apps.who.int/gho/athena/data/xmart.csv?target=GHO/HIV_0000000006&profile=crosstable&filter=COUNTRY:*;REGION:*&x-sideaxis=COUNTRY&x-topaxis=GHO;YEAR"

download.file(url4,"hiv prevalence.csv")
download.file(url5,"people with hiv.csv")
download.file(url6,"hiv deaths.csv")

hivP<-read.csv("hiv prevalence.csv",skip=1)
hivD<-read.csv("hiv deaths.csv",skip=1)
hivN<-read.csv("people with hiv.csv",skip=1)

#splitting the data into value and confident interval CI
numSplit<-function(x){
  a<-strsplit(x,"[[]")
  aa<-c()
  bb<-c()
  for (i in 1:length(a)){
    aa<-c(aa,a[[i]][1])
    ab<-a[[i]][2]
    ab<-strsplit(ab,"[]]")
    ab<-ab[[1]]
    bb<-c(bb,ab)
    
  }
  d<-cbind(aa,bb)
  print(d)
}


#clean the complete dataframe from the &lt elements and creating different columns for values and confidence intervals
dataClean<-function(x){
  country<-as.character(x[,1])
  d2013<-as.character(gsub("&lt;","",x[,2]))
  d2009<-as.character(gsub("&lt;","",x[,3]))
  d2005<-as.character(gsub("&lt;","",x[,4]))
  d2001<-as.character(gsub("&lt;","",x[,5]))
  
  d2013<-as.character(gsub(" ","",d2013))
  d2009<-as.character(gsub(" ","",d2009))
  d2005<-as.character(gsub(" ","",d2005))
  d2001<-as.character(gsub(" ","",d2001))
  
  d2013<-numSplit(d2013)
  d2009<-numSplit(d2009)
  d2005<-numSplit(d2005)
  d2001<-numSplit(d2001)
  
  dataD<-cbind(country,d2013,d2009,d2005,d2001)
  dataD<-as.data.frame(dataD)
  colnames(dataD)<-c("Country","2013","CI_2013","2009","CI_2009","2005","CI_2005","2001","CI_2001")
  print(dataD)
  
}
#renaming the individual dataframes with respect to their types.x= dataframe, y= name to be concatanated to the column name
rename<-function(x,y){
  names<-colnames(x)
  df<-as.data.frame(x)
  names<-as.character(names)
  nameList<-c()
  for (i in 1:length(names)){
    name<-paste(y,names[i],sep="_")
    nameList<-c(nameList,name)
    
  }
  colnames(df)<-nameList
  print(df)
}


#cleaning the data
dataP<-dataClean(hivP)
dataD<-dataClean(hivD)
dataN<-dataClean(hivN)

#renaming the data
dataP<-rename(dataP,"prevalence")
dataD<-rename(dataD,"Deaths")
dataN<-rename(dataN,"numberInf")

#merging the data
merged.data<-merge(dataP,dataD,by.x="prevalence_Country",by.y="Deaths_Country")
merged.data1<-merge(merged.data,dataN,,by.x="prevalence_Country",by.y="numberInf_Country")
names<-colnames(merged.data1)
names[1]<-"Country"
colnames(merged.data1)<-names

finalData<-merged.data1


#creating a database

mongoData<-mongo("SCMS")
mongoData$insert(supplyData)
mongoData1<-mongo("HIVdata")
mongoData1$insert(finalData)

mongoData$count('{"Country":"Vietnam"}')

yeard<-mongoData$find('{"Year":2008}')
head(yeard)
dat <- mongoData$find('{"Product_Name":"Nevirapine"}', fields = '{"_id":0,"Pack_Price":1, "Country":1,"Year":1}')
head(dat)
countryd<-mongoData1$find('{"Country":"Afghanistan"}')
head(countryd)
mxdel<-mongoData$aggregate('[{"$group":{"_id":"$Country", "count": {"$sum":1},"max":{"$max":"$Delivery_Time"}}}]')
head(mxdel)

