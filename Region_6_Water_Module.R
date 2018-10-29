

#       _______.___________.    ___      .______     .___________.
#      /       |           |   /   \     |   _  \    |           |
#     |   (----`---|  |----`  /  ^  \    |  |_)  |   `---|  |----`
#      \   \       |  |      /  /_\  \   |      /        |  |     
#  .----)   |      |  |     /  _____  \  |  |\  \----.   |  |     
#  |_______/       |__|    /__/     \__\ | _| `._____|   |__|     
  
####################--------Instructions Below------------------######################################

#Before you run the program load the data set to be assessed into the folder for the region the data set 
#applies toand rename the data set something unique.
#Once R is open you will need to install the pacakges plyr, dplyr, and lubridate
#Then change the working directory file path to match the location of ReLEP on your computer
#make sure to use double forward slashes in the file path name
#change the name of the file in the "Load Data Set" command to match the file you want assessed
#Change the working directory file pat command at the bottom to match the location of the output 
#folder on your computer once the program has been run check the output folder for the LOEs and 
#the missing rows files the missing rows contain the data that could not run through the program because
#of mismatched names (station, analyte, waterbody, etc.)
#check those files to make sure that all data that should have been assessed WAS assessed

#######################-----------------------------------------#######################################

#This section loads nexeccary pagages for assessment
#try not to add unecessary pacages here because
#sometimes packages conflict with eachother
#or they will mask the ommands and cause unpredicable behavior in the code

library(readxl)
library(dplyr,warn.conflicts=TRUE)
library(tidyr)
library(data.table, warn.conflicts=TRUE)
library(lubridate,warn.conflicts=TRUE)
library(rio)


#Sets path to use Rtools to zip up xls/xlsx file with rio
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe")

#Sets path to use rJava for package (unsure if explicitly needed)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')

#Removes all defined variables this is only imporant when you run the
#code multiple times in one session.  it removes previously defined variables
rm(list=ls(all=TRUE))

#saves system time to display how long the code takes to run at the end
Start_Time<- Sys.time()

########################################################################################################
########################################################################################################
####################_______________  Begin Water Module _____________________________###################
########################################################################################################
########################################################################################################

########################################################################################################
###################___ Enter the working location of the ReLEP folder Here _________####################
########################################################################################################
################                                                                        ################
#############                                                                              #############
###########                                                                                  ###########
#########                                                                                      #########
#######                                                                                          #######
#####                                                                                              #####
####                                                                                                #### 
###                                                                                                  ###
##                                                                                                    ##
#                                                                                                      #


#>>>>>     Change the values input in Region and Author to get the program to run        <<<<<<<<<<<<<#
#>>>>>     The region should be the region number you are assessing and the author       <<<<<<<<<<<<<#
#>>>>>     should be the username login you use to log into your computer                <<<<<<<<<<<<<#

Region<-"6"                                                                    #<<<<<<<<<<<-<<<<<<<<<<#

Author<-"Jkaplan"                                                              #<<<<<<<<<<<-<<<<<<<<<<#

ReLEP_Version<-"Version 1.0"								       #<<<<<<<<<<<-<<<<<<<<<<#

######You will want to change the name of the below file name to match the name of the data file you
#####are trying to assess.  The file name is currently "Region 2 export.txt" but you could change
####it to any text file.  Just make sure the field names match.  Do the same thing with each of the
###tables below.  Each table is used by the program but is specific to each Region. Make sure the file
##ends in the .txt extension, and put the name in quotations like the examples below.Put the name to the right
#of the "gets" symbol <-


FileNameInQuotations<-"Region_6_STORET_2010-2017_Data.txt"



WorkingDirectory<-paste0("C:\\Users\\",Author,"\\Desktop\\Final_ReLEP\\Region ",Region,"\\Water_Module")

#Set the directory so that relative paths work for loading the files
setwd(WorkingDirectory)

SitesTable<-paste0("R",Region,"_Sites_FINAL.txt")

BeneficialUseTable<-paste0("R",Region,"_Beneficial_Use_FINAL.txt")

ObjectivesTable<-paste0("R",Region,"_Analytes_Water_FINAL.txt")

SummingPollutantsTable<-"SummingPollutants.txt"

PAHTable<-"PAH_TEFs.txt"

DataReferencesTable<-paste0("R",Region,"_Data_Used_DRAFT.txt")

SSOTable<-paste0("R",Region,"_Site_Specifics_FINAL.txt")

SampleTypeNameTable<-paste0("R",Region,"_Acceptable_SampleTypeNames.txt")



#                                                                                                      #
##                                                                                                    ##
#####                                                                                               ####
#######                                                                                            #####
#########                                                                                         ######
###########                                                                                      #######
#############                                                                                   ########
###############                                                                               ##########
#################                                                                           ############
###################                                                                       ##############
#####################                                                                   ################
########################################################################################################


# Load Region 2 Data file, remove unecessary fields, remove J Flagged samples, and convert to tbl_df
data<-tbl_df(read.delim(FileNameInQuotations,header=TRUE,stringsAsFactors=FALSE))
data$ProjectName<-data$ParentProjectName
#Change negative results with ResQualCodes of ND or DNQ to be equal to zero
#the negative results are neative MDLs which means they will eather be clean samples
#or quantitation discards depending on the objective used so zero is appropriate
data$Result[which(data$Result<0 & (data$ResQualCode=="ND"|data$ResQualCode=="DNQ"))]<-0 #perfect scenario
ExportedData<-data
data<-data[which(data$MatrixName=="samplewater"|data$MatrixName=="Surface Water"),]
ExportedData$SampleDate<-mdy(ExportedData$SampleDate)
ExportedData<-ExportedData[which(ExportedData$MatrixName!="air"|ExportedData$MatrixName!="sediment"|ExportedData$MatrixName!="sediment, <63um"),]
data<-subset(data,select=c("ProjectName","StationCode","SampleDate","MatrixName","AnalyteName","FractionName"
,"UnitName","Result","MDL","RL","ResQualCode","QACode","SampleTypeName","TargetLatitude","TargetLongitude","DataQuality","DataQualityIndicator"))
data$SampleDate<-mdy(data$SampleDate)
data$FractionName[which(data$FractionName=="Recoverable"|data$FractionName=="Total Recoverable")]<-"Total"

test<-data
BadMatrixName<-subset(ExportedData,!(MatrixName=="samplewater"|MatrixName=="Surface Water"))
BadMatrixName$Issue<-"Check MatrixName"
AllExportedData<-tbl_df(BadMatrixName)

#Load the acceptable SampleTypeName List and filter for "acceptable" sample types
AcceptableSampleTypes<-tbl_df(read.delim(SampleTypeNameTable,header=TRUE,stringsAsFactors=FALSE))
AcceptableSampleTypes<-AcceptableSampleTypes[AcceptableSampleTypes$Acceptable=="Yes",]
AcceptableSampleTypes$Acceptable<-NULL


#Add data with results < 0 and ResQualCode of "=" to tossed data
LessThanZero<-ExportedData[which(ExportedData$ResQualCode=="="&ExportedData$Result<0),]
LessThanZero$Issue<-"Result less than Zero AND ResQualCode of '=' Check data for errors"
AllExportedData<-rbind.data.frame(AllExportedData,LessThanZero)

#Remove data with results < 0 and ResQualCode of "=" to tossed data
data<-data[which(!(data$ResQualCode=="="&data$Result<0&!is.na(data$Result))),]


#Create table of data with bad QA Codes to be exported and reviewed
BadQACodes<-filter(ExportedData,DataQuality!="Passed")
BadQACodes$Issue<-"Check DataQuality Field"
AllExportedData<-tbl_df(rbind.data.frame(AllExportedData,BadQACodes))


#Now do the opposite command on the data to get the remaining rows of data with acceptable QACodes
data<-filter(data,DataQuality=="Passed")
data$DataQuality<-NULL
data$DataQualityIndicator<-NULL


#Change negative results with ResQualCodes of ND or DNQ to be equal to zero
#the negative results are neative MDLs which means they will eather be clean samples
#or quantitation discards depending on the objective used so zero is appropriate
data$Result[which(data$Result<0 & (data$ResQualCode=="ND"|data$ResQualCode=="DNQ"))]<-0 #perfect scenario



#tbl_df is a great command for coding
#it allows you to see only the top ten rows of data, but it shows you 
#a count of the number of rows, and the names and data types for all
#of the columns i use it all the time in the code below

data<-tbl_df(data)

#only water data moves on beyond this point

data<-tbl_df(merge(data,AcceptableSampleTypes))
data$SampleTypeName<-NULL

BadSampleType<-anti_join(ExportedData,AcceptableSampleTypes)
BadSampleType$Issue<-"Check Sample type"
AllExportedData<-rbind.data.frame(AllExportedData,BadSampleType)

#Create a table of bad ResQualCodes
MissingResQualCode<-filter(AllExportedData, ((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))
MissingResQualCode$Issue<-"Missing ResQualCode"
AllExportedData<-rbind.data.frame(AllExportedData,MissingResQualCode)

#Remove data with bad res qual codes from the main data table
data<-filter(data, !((ResQualCode=="NA"|ResQualCode==" "|ResQualCode==""|is.na(ResQualCode))&(RL=="NA"|RL==" "|RL==""|is.na(RL))&(MDL=="NA"|MDL==" "|MDL==""|is.na(MDL))))

#Add ND and DNQ data that does not have MDL and RL information to exported data table
MissingLimits<-subset(ExportedData,((ResQualCode=="ND"|ResQualCode=="DNQ")&(RL=="NA"|RL==" "|RL==""|is.na(RL))))
MissingLimits$Issue<-"ND or DNQ Sample Missing RL"
AllExportedData<-rbind.data.frame(AllExportedData,MissingLimits)

#Remove ND and DNQ data that does not have MDL and RL information from main data table
data<-subset(data,!((ResQualCode=="ND"|ResQualCode=="DNQ")&(RL=="NA"|RL==" "|RL==""|is.na(RL))))


#Multiply MDL by 3.18 to get RL if RL is missing as per quantitaiton guidance
data$RL[which((is.na(data$RL)|data$RL==""|data$RL==" ")&(data$MDL!="NA"|data$MDL!=""|data$MDL!=
" "))]<-data$MDL[which((is.na(data$RL)|data$RL==""|data$RL==" ")&(data$MDL!="NA"|data$MDL!=""|data$MDL!=" "))]*3.18

####################################################################################################################
#########################OTHER TABLE LOADING SECTION###############################################################


# Load Region sites table and convert to tbl_df
Sites<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Sites<-Sites[Sites$STATUS=="Completed",]
Sites<-subset(Sites,select=c(Waterbody,StationCode,WBID))
names(Sites)<-c("Waterbody","StationCode","WBID")
Sites<-tbl_df(Sites)

#Create tables of fresh and saltwater stations
Ocean_Marine<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Ocean_Marine<-subset(Ocean_Marine,select=c(StationCode,FreshMarine))
Ocean_Marine<-Ocean_Marine[which(Ocean_Marine$FreshMarine=="O"|Ocean_Marine$FreshMarine=="M"),]

Fresh_Fresh<-tbl_df(read.delim(SitesTable,sep="\t",header=TRUE,stringsAsFactors=FALSE))
Fresh_Fresh<-subset(Fresh_Fresh,select=c(StationCode,FreshMarine))
Fresh_Fresh<-Fresh_Fresh[Fresh_Fresh$FreshMarine=="F",]


# Load Beneficial Uses table and convert to tbl_df
Beneficial_Uses<-read.delim(BeneficialUseTable,sep="\t",header=TRUE,stringsAsFactors=FALSE)
Beneficial_Uses<-na.omit(Beneficial_Uses)
Beneficial_Uses<-tbl_df(Beneficial_Uses)
Beneficial_Uses<-subset(Beneficial_Uses,select=c(Waterbody,WBID,Wbtype,BeneficialUse))

# Load Region 2 objecives table and convert to tbl_df
Analytes<-read.delim(ObjectivesTable,header=TRUE,stringsAsFactors=FALSE)
#The NA omit command that follows may be a problem because the code for one of the 
#beneficial uses is NA and so I think the code will remove it.  There should be some 
#workaround for this.
#Analytes<-na.omit(Analytes)
Analytes$Alias<-NULL
Analytes$CAS_Number<-NULL
Analytes<-tbl_df(Analytes)

#Each AnalyteName may apprear multiple times, but each row should be unique
SummingPollutants<-read.delim(SummingPollutantsTable,header=TRUE,stringsAsFactors=FALSE)
SummingPollutants<-tbl_df(SummingPollutants)

PAH_TEFs<-read.delim(PAHTable,header=TRUE,stringsAsFactors=FALSE)
PAH_TEFs<-tbl_df(PAH_TEFs)

#Contains CalWQA ref numbers for the data set and QAPPs
DataReferences<-read.delim(DataReferencesTable,header=TRUE,stringsAsFactors=FALSE)
DataReferences<-tbl_df(DataReferences)

#Contains site specific objectives information
SSOs<-tbl_df(read.delim(SSOTable,header=TRUE,stringsAsFactors=FALSE))
SSOs$Waterbody<-NULL

#Contains conversion of ReLEP AnalyteName to CalWQA names
ReLEP_to_CalWQA_Lookup<-tbl_df(read.delim("ReLEP_to_CalWQA_Lookup.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE))
names(ReLEP_to_CalWQA_Lookup)[names(ReLEP_to_CalWQA_Lookup)=="ReLEP_AnalyteName"]<-"POLLUTANT"


#############_______________End Data Loading Section___________###############################
##############################################################################################
##############################################################################################
##############################################################################################

##############################################################################################
##############################################################################################
##############################################################################################
######################Begin table joining and data splitting section##########################

# Add the beneficial uses to the waterbodies table
Waterbodies<-merge(Sites,Beneficial_Uses,by=c("Waterbody","WBID"))
Waterbodies<-tbl_df(Waterbodies)


# Add waterbody names and beneficial uses to the data
W_Waterbodies<-merge(data,Waterbodies,by="StationCode")
W_Waterbodies<-tbl_df(W_Waterbodies)

#Create a table of data from stations that do not have waterbody 
#information assigned in the sties tables
Missing_Stations<-anti_join(ExportedData,Waterbodies,by="StationCode")
Missing_Stations$Issue<-"Missing waterbody information"
AllExportedData<-rbind.data.frame(AllExportedData,Missing_Stations)


#Split out pH and Temperatue for ammonia calculations
temp<-W_Waterbodies[W_Waterbodies$AnalyteName=="Temperature",]
names(temp)[names(temp)=="Result"]<-"TempResult"
temp$Fraction<-NULL
temp$AnalyteName<-NULL
temp$MDL<-NULL
temp$RL<-NULL
temp$UnitName<-NULL
temp$ResQualCode<-NULL
temp$QACode<-NULL
temp$FractionName<-NULL

#Convert temp in celsius to Kelvin
KelvinTemp<-temp
KelvinTemp<-mutate(KelvinTemp,Kelvin=TempResult+273.15)
KelvinTemp$TempResult<-NULL


#Split out pH for pentachlorophenol identify result as being pH result, remove all fields
#unecessary for unique join
PentapH<-W_Waterbodies[W_Waterbodies$AnalyteName=="pH",]
names(PentapH)[names(PentapH)=='Result']<-"pHResult"
PentapH$Fraction<-NULL
PentapH$AnalyteName<-NULL
PentapH$MDL<-NULL
PentapH$RL<-NULL
PentapH$UnitName<-NULL
PentapH$ResQualCode<-NULL
PentapH$QACode<-NULL
PentapH$FractionName<-NULL

Salinity<-tbl_df(W_Waterbodies[W_Waterbodies$AnalyteName=="Salinity",])
names(Salinity)[names(Salinity)=='Result']<-"SalinityResult"
Salinity$Fraction<-NULL
Salinity$AnalyteName<-NULL
Salinity$MDL<-NULL
Salinity$RL<-NULL
Salinity$UnitName<-NULL
Salinity$ResQualCode<-NULL
Salinity$QACode<-NULL
Salinity$FractionName<-NULL


#Split out organic carbon
DOC<-W_Waterbodies[W_Waterbodies$AnalyteName=="Dissolved Organic Carbon",]

W_SSOs<-tbl_df(merge(ExportedData,Waterbodies))
W_SSOs<-tbl_df(merge(W_SSOs,SSOs))
W_SSOs_for_Export<-W_SSOs
W_SSOs_for_Export$BeneficialUse<-NULL
W_SSOs_for_Export<-unique(W_SSOs_for_Export)

W_SSOs<-subset(W_SSOs,select=c(WQID,ProgramCode,ProgramName,ParentProjectCode,ParentProjectName,ProjectCode,ProjectName,ProjectDescr
,QAPPCode,QAPPName,PublicRelease,SampleDate,StationCode,StationName,StationDescr,TargetLatitude,TargetLongitude,Datum,RegionalBoardID
,WaterBodyType,SampleAgency,SampleAgencyCode,SampleComments,CollectionTime,PositionWaterColumn,LabCollectionComments,CollectionMethodName
,SampleTypeCode,SampleTypeName,Replicate,CollectionDeviceCode,CollectionDeviceName,CollectionDepth,UnitCollectionDepth,LabAgencyCode,LabAgencyName
,SubmittingAgency,LabSubmissionCode,LabBatchComments,MatrixCode,MatrixName,MethodCode,MethodName,AnalyteCode,AnalyteName,FractionCode,FractionName
,UnitCode,UnitName,MDL,RL,QACode,LabBatch,AnalysisDate,LabReplicate,Result,ResQualCode,LabResultComments,SampleLatitiude,SampleLongitude,SampleDatum
,SampleElevation,SampleUnitElevation,DataQuality,DataQualityIndicator,FieldWater))
W_SSOs$Issue<-"Data row has SSO for one or more beneficial uses that apply to it"
W_SSOs<-unique(W_SSOs)
AllExportedData<-tbl_df(rbind.data.frame(AllExportedData,W_SSOs))


SSOs$BeneficialUse<-NULL
SSOs<-filter(SSOs,AnalyteName!="Sodium")
#Remove data for watebody/pollutant/BU combinations that have SSOs
W_Waterbodies<-anti_join(W_Waterbodies,SSOs)

#Add data from projects that are missing QA information to the exproted data table
No_Ref_Codes<-anti_join(ExportedData,DataReferences)
No_Ref_Codes$Issue<-"Parent Project Not in Data Used Table.  Missing QAPP?"
AllExportedData<-tbl_df(rbind.data.frame(AllExportedData,No_Ref_Codes))


##########  ____________Begin Freshwater Total Ammonia to unionized ammonia conversion________________######
##########   Currently this section throws out all data that does not have coresponding pH and temp data   # 
##########   I am not sure if this is the correct course of action or not                             ######
##########   The other option would be to join the data with pH and temp data and then fill in the blanks ##
##########   (rows without coresponding pH and temp data)  								######
##########   with default values described in the EPA criteria document.  I am not sure if this          ###
##########   is appropriate though. If no default numbers are used, do we want to track the            ##### 
##########   rows that are tossed?                                                                   #######

#Split out Total Ammonia for sample days that did not have un-ionized ammonia also reported
UnIonized<-W_Waterbodies[which(W_Waterbodies$AnalyteName=="Ammonia as N, Unionized"|W_Waterbodies$AnalyteName=="Ammonia as NH3, Unionized"|W_Waterbodies$AnalyteName=="Ammonia as NH3"),]
UnIonized<-distinct(select(UnIonized,StationCode,SampleDate))

#Do antijoin with the main data frame to get list of staion days that only reported Total Ammonia
TotalAmmoniaOnly<-anti_join(W_Waterbodies,UnIonized,by=c("StationCode","SampleDate"))
TotalAmmoniaOnly<-TotalAmmoniaOnly[TotalAmmoniaOnly$AnalyteName=="Ammonia as N",]

#Filter out Total Ammonia data that is from saline water
#It needs to be assessed using a different formula.
FreshTotalAmmoniaOnly<-anti_join(TotalAmmoniaOnly,Ocean_Marine)
TotalAmmoniaOnly<-FreshTotalAmmoniaOnly

#Merge Ammonia Data with Temp and pH data.  Any row that does not have a coresponding pH value will be thrown out.
TotalAmmoniaOnly_W_pH<-tbl_df(merge(TotalAmmoniaOnly,PentapH,by=c("StationCode","ProjectName","SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaOnlyJoin<-subset(TotalAmmoniaOnly,select=c(StationCode,ProjectName,SampleDate,MatrixName,AnalyteName,FractionName,Result,UnitName,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude))
AmmoniaNopH<-tbl_df(merge(ExportedData,TotalAmmoniaOnlyJoin))
AmmoniaNopH<-anti_join(AmmoniaNopH,PentapH)
AmmoniaNopH<-unique(AmmoniaNopH)
AmmoniaNopH$Issue<-"Ammonia missing pH data"
AllExportedData<-rbind.data.frame(AllExportedData,AmmoniaNopH)


#Merge Ammonia data with temp data.  Any row that does not have a coresponding pH value will be thrown out.
TotalAmmonia_For_Conversion<-tbl_df(merge(TotalAmmoniaOnly_W_pH,temp,by=c("StationCode","ProjectName","SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaOnlyJoin<-subset(TotalAmmoniaOnly,select=c(StationCode,ProjectName,SampleDate,MatrixName,AnalyteName,FractionName,Result,UnitName,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude))
TotalAmmoniaOnlyJoin<-unique(TotalAmmoniaOnlyJoin)
AmmoniaNoTemp<-tbl_df(merge(ExportedData,TotalAmmoniaOnlyJoin))
AmmoniaNoTemp<-anti_join(AmmoniaNoTemp,temp)
AmmoniaNoTemp<-unique(AmmoniaNoTemp)
AmmoniaNoTemp$Issue<-"Ammonia as N data missing temp data"
AllExportedData<-rbind.data.frame(AllExportedData,AmmoniaNoTemp)


#Convert Total Ammonia to unionized ammonia using formula and then remove unecessary fields
Converted_Ammonia<-mutate(TotalAmmonia_For_Conversion,pKA=.09018+(2729.92/(273.2+TempResult)))
Converted_Ammonia<-mutate(Converted_Ammonia,UnionizedAmmoniaResult=(14/17)*Result*(1/(1+10^((.09018+(2729.92/(273.2+TempResult)))-pHResult))))
Converted_Ammonia$AnalyteName<-"Ammonia as N, Unionized"
Converted_Ammonia$Result<-NULL
Converted_Ammonia$pKA<-NULL
Converted_Ammonia$pHResult<-NULL
Converted_Ammonia$TempResult<-NULL
names(Converted_Ammonia)[names(Converted_Ammonia)=="UnionizedAmmoniaResult"]<-"Result"

###_____Converted ammonia added back to W_Waterbodies between hardness conversion
###_____and the unit conversion sections                                         

###############__________________________End Conversion______________________##############
###########################################################################################


#####################____Begin Saltwater Ammonia calculation____###########################

#Create Table of Salty Total Ammonia
SaltyAmmoniaOnly<-W_Waterbodies[W_Waterbodies$AnalyteName=="Ammonia as N",]
SaltyAmmoniaOnly<-anti_join(SaltyAmmoniaOnly,Fresh_Fresh)

#Toss data that is missing coresponding temperature data
SaltyAmmoniaJoin<-unique(subset(SaltyAmmoniaOnly,select=c(StationCode,ProjectName,SampleDate,MatrixName,AnalyteName,FractionName,Result,UnitName,MDL,RL,ResQualCode,QACode,TargetLatitude,TargetLongitude)))
SaltyAmmoniaMissingTemp<-tbl_df(merge(ExportedData,SaltyAmmoniaJoin))
SaltyAmmoniaMissingTemp<-anti_join(SaltyAmmoniaMissingTemp,KelvinTemp)
SaltyAmmoniaMissingTemp$Issue<-"Ammonia data missing coresponding temp"
AllExportedData<-rbind.data.frame(AllExportedData,SaltyAmmoniaMissingTemp)

#Toss data that is missing corresponding salinity data
SaltyAmmoniaMissingSalt<-tbl_df(merge(ExportedData,SaltyAmmoniaJoin))
SaltyAmmoniaMissingSalt<-anti_join(SaltyAmmoniaMissingSalt,Salinity)
SaltyAmmoniaMissingSalt$Issue<-"Ammonia data missing coresponding salinity"
AllExportedData<-rbind.data.frame(AllExportedData,SaltyAmmoniaMissingSalt)

#Removereplicates from alinity and kelvin temp
Salinity<-tbl_df(as.data.table(Salinity)[,mean(SalinityResult),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype)])
	names(Salinity)[names(Salinity)=="V1"]<-"SalinityResult"
KelvinTemp<-tbl_df(as.data.table(KelvinTemp)[,mean(Kelvin),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype)])
	names(KelvinTemp)[names(KelvinTemp)=="V1"]<-"Kelvin"
pH<-tbl_df(as.data.table(PentapH)[,mean(pHResult),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,ProjectName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype)])
	names(pH)[names(pH)=="V1"]<-"pH"




#Calculate pKa for objective formula
pKaSalinity<-tbl_df(merge(Salinity,KelvinTemp))
pKaSalinity<-tbl_df(merge(pKaSalinity,pH))
pKaSalinity<-mutate(pKaSalinity,I=((19.9273*SalinityResult)/(1000-(1.005109*SalinityResult))))
pKaSalinity<-mutate(pKaSalinity,pKa=(9.245+.116*I))
pKaSalinity<-mutate(pKaSalinity,fNH3=(1/(1+(10^(pKa+(.0324*(298-Kelvin))+(.0415/Kelvin)-pH)))))
pKaSalinity<-mutate(pKaSalinity,Objective=(.035/fNH3))

#Create Objective table
SalinityObjectives<-tbl_df(pKaSalinity)
SalinityObjectives$SalinityResult<-NULL
SalinityObjectives$Kelvin<-NULL
SalinityObjectives$I<-NULL
SalinityObjectives$pH<-NULL
SalinityObjectives$pKa<-NULL
SalinityObjectives$fNH3<-NULL

#Add objective language, and reference codes
SalinityObjectives$Evaluation_Guideline<-"NA"
SalinityObjectives$Eval_Ref_Number<-"NA"
SalinityObjectives$Objective_Language<-paste0("The Total Ammonia criterion continuous concentration (expressed as a 4-day average) to protect aquatic life in saltwater is temperature, pH and salinity dependent; and was calculated according to the formula listed in the Ambient Water QualityCriteria for Ammonia (Saltwarer) - 1989 document.")
SalinityObjectives$Objective_Ref_Number<-"Need"
SalinityObjectives$AveragingPeroid<-4
SalinityObjectives<-tbl_df(SalinityObjectives)


SalinityObjectives$Region<-substr(SalinityObjectives$WBID,4,4)
SalinityObjectives<-filter(SalinityObjectives,!(SalinityObjectives$Region=="5"|SalinityObjectives$Region=="6"|SalinityObjectives$Region=="7"))
SalinityObjectives$Region<-NULL
SalinityAmmoniaObjectives<-SalinityObjectives[which(SalinityObjectives$BeneficialUse=="ES"|SalinityObjectives$BeneficialUse=="MA"),]
SalinityAmmoniaObjectives$AnalyteName<-"Ammonia as N"
SalinityAmmoniaObjectives<-anti_join(SalinityAmmoniaObjectives,Fresh_Fresh)



#############_________________End Saltwater_Conversion______________#######################
###########################################################################################

###########################################################################################
###################_____________________Begin_Hardness______________#######################


#Metals Assessments
HardnessSubset<-subset(W_Waterbodies,select=c("ProjectName","StationCode","SampleDate"
	,"MatrixName","AnalyteName","UnitName","Result", "BeneficialUse")) 
Hardness<-tbl_df(HardnessSubset)
Hardness<-Hardness[Hardness$AnalyteName=="Hardness as CaCO3",]
Hardness<-Hardness[which(Hardness$BeneficialUse=="CO"
	|Hardness$BeneficialUse=="WA"|Hardness$BeneficialUse=="ES"|Hardness$BeneficialUse=="MA"),]

###Create a table of rows of hardness that have units other than mg/L 
##because they will need to be converted. This is a very unlikely 
###thing to happen so it does not make sense to program the conversion in here
###But, in the unlikely event it does happen. This table will flag 
###the issue and show you the rows that need to be changed
HardnessUnitIssues<-ExportedData[ExportedData$UnitName!="mg/L",]
HardnessUnitIssues<-subset(HardnessUnitIssues,(AnalyteName=="Hardness as CaCO3"
	|AnalyteName=="Alkalinity as CaCO3"))
HardnessUnitIssues$Issue<-"Hardness data with weird units"
AllExportedData<-rbind.data.frame(AllExportedData,HardnessUnitIssues)


#Convert hardness value to number (if it isnt already) then assign values 
###of 400 if hardness is greater than 400 and hardness value of 100 
###if hardness if the result field is empty
Hardness$Result<-as.numeric(as.character(Hardness$Result))
Hardness$Result[Hardness$Result>400]<-400
Hardness$Result[Hardness$Result=="NA"]<-100


#Remove replicates (samples collected on the same day) of the hardness data
Hardness<-tbl_df(as.data.table(Hardness)[,mean(Result),list(AnalyteName,MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,UnitName)])
	names(Hardness)[names(Hardness)=="V1"]<-"Result"


#Create tables of objectives by date and station separated by pollutant name
Cadmium<-mutate(Hardness, Objective=(1.101672-(log(Result)*.041838))*exp(.7825*log(Result)-2.715))
Cadmium$AnalyteName<-"Cadmium"
Copper<-mutate(Hardness,Objective=.960*exp(.8545*log(Result)-1.702))
Copper$AnalyteName<-"Copper"
Chromium<-mutate(Hardness,Objective=.860*exp(.8190*log(Result)+1.561))
Chromium$AnalyteName<-"Chromium"
ChromiumIII<-mutate(Hardness,Objective=.860*exp(.8190*log(Result)+1.561))
ChromiumIII$AnalyteName<-"Chromium III"
Lead<-mutate(Hardness,Objective=(1.46203-(log(Result))*.145712)*exp(1.273*log(Result)-4.705))
Lead$AnalyteName<-"Lead"
Nickel<-mutate(Hardness,Objective=.997*exp(.8460*log(Result)+.0584))
Nickel$AnalyteName<-"Nickel"
Silver<-mutate(Hardness,Objective=.5*.85*exp(1.72*log(Result)-6.52))
Zinc=mutate(Hardness,Objective=.986*exp(.8473*log(Result)+.884))
Zinc$AnalyteName<-"Zinc"



#Createplaceholder information in case there is no hardness data so that
#we can still create the HardnessObjectives table for joining and assessment
colnames<-c("AnalyteName","MatrixName","SampleDate","BeneficialUse","StationCode","ProjectName","UnitName","Result","Objective","Type")
fillerinfo<-c("Placeholder","Placeholder","1850-01-01","Fake","Placeholder","Placeholder","Placeholder",0,0,"Brack")
HardnessObjectives<-as.data.frame(rbind(colnames,fillerinfo))
colnames(HardnessObjectives)<-colnames
row.names(HardnessObjectives)<-NULL
HardnessObjectives<-HardnessObjectives[-1,]
HardnessObjectives<-tbl_df(HardnessObjectives)
HardnessObjectives$SampleDate<-Sys.Date()
HardnessObjectives$Objective<-0
HardnessObjectives$Result<-0




# Combine all of these tables into one large table
HardnessObjectives2<-tbl_df(rbind(Cadmium, Copper, Chromium, ChromiumIII, Lead, Nickel, Silver, Zinc))
HardnessObjectives2$Type<-"Fresh"
HardnessObjectives<-rbind.data.frame(HardnessObjectives,HardnessObjectives2)





#Create tables of objectives by date and station separated by pollutant name
SaltCadmium<-mutate(Hardness, Objective=9.3)
SaltCadmium$AnalyteName<-"Cadmium"
SaltCopper<-mutate(Hardness,Objective=3.1)
SaltCopper$AnalyteName<-"Copper"
SaltLead<-mutate(Hardness,Objective=8.1)
SaltLead$AnalyteName<-"Lead"
SaltNickel<-mutate(Hardness,Objective=8.2)
SaltNickel$AnalyteName<-"Nickel"
SaltSilver<-mutate(Hardness,Objective=.95)
SaltZinc=mutate(Hardness,Objective=81)
SaltZinc$AnalyteName<-"Zinc"

SaltHardnessObjectives<-tbl_df(rbind(SaltCadmium, SaltCopper, SaltLead, SaltNickel, SaltSilver, SaltZinc))
SaltHardnessObjectives$Type<-"Salt"


#Combine the salt and freshwater criterion together
HardnessObjectives<-tbl_df(rbind.data.frame(HardnessObjectives,SaltHardnessObjectives))

#Delete unneeded columns
HardnessObjectives$Result<-NULL
HardnessObjectives$UnitName<-NULL

###############################################################################################
#Convert Total fraction results field to dissolved values using conversion factor then 
#change name of fraction to dissolved. Only do this to sample days that ONLY reported total 
#fraction of that metal on that day
FirstHardnessDependentAnalytes<-tbl_df(filter(W_Waterbodies,((AnalyteName=="Cadmium"|AnalyteName=="Copper"
|AnalyteName=="Chromium"|AnalyteName=="Chromium VI"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") &(BeneficialUse=="CO"|BeneficialUse=="WA"
|BeneficialUse=="ES"|BeneficialUse=="MA"))))

# Convert Results, MDL, and RL that are in mg/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]*1000
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]*1000
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="mg/L")]*1000
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="mg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in pg/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]*.000001
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]*.000001
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="pg/L")]*.000001
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="pg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in ng/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]*.001
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]*.001
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="ng/L")]*.001
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="ng/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in g/L to ug/L datasetwide and change UnitName to ug/L
FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]<-FirstHardnessDependentAnalytes$Result[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]*1000000
FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]<-FirstHardnessDependentAnalytes$MDL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]*1000000
FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]<-FirstHardnessDependentAnalytes$RL[which(FirstHardnessDependentAnalytes$UnitName=="g/L")]*1000000
FirstHardnessDependentAnalytes$UnitName[FirstHardnessDependentAnalytes$UnitName=="g/L"]<-"ug/L"



HardnessDependentAnalytes<-filter(FirstHardnessDependentAnalytes, (AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") & (BeneficialUse=="CO"|BeneficialUse=="WA"))
	
HardnessDependentAnalytes$Type<-"Fresh"


SaltHardnessDependentAnalytes<-filter(FirstHardnessDependentAnalytes, (AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium VI"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") & (BeneficialUse=="ES"|BeneficialUse=="MA"))
	
SaltHardnessDependentAnalytes$Type<-"Salt"

#Combine fresh and salt hardness analytes together
HardnessDependentAnalytes<-tbl_df(rbind.data.frame(HardnessDependentAnalytes,SaltHardnessDependentAnalytes))

W_Waterbodies_hardness<-tbl_df(HardnessDependentAnalytes)



#Create List of stations that have data for the dissolved fraction of metals
DissolvedOnly<-distinct(select(W_Waterbodies_hardness,AnalyteName,StationCode,FractionName,SampleDate))
DissolvedOnly<-DissolvedOnly[DissolvedOnly$FractionName=="Dissolved",]
#DissolvedOnly$FractionName<-NULL

#Create data frame of data that needs to be converted from total to dissolved fraction
NeedsConverting<-anti_join(W_Waterbodies_hardness,DissolvedOnly,by=c("AnalyteName","StationCode","SampleDate"))

#Create table of this data for the station information calculation
#That happens later, just before the main quantitation check
NeedsConvertingForStations<-NeedsConverting
NeedsConvertingForStations$Type<-NULL
NeedsConvertingForStations<-unique(NeedsConvertingForStations)
NeedsConvertingForStations$FractionName<-"Dissolved"



#Create a data frame of data that does not need converting 
#(has dissolved data only, or both dissolved and total for each day)
W_Waterbodies_hardness_no_conversion<-W_Waterbodies_hardness[W_Waterbodies_hardness$FractionName=="Dissolved",]


#Calculate the converstion factors for the rows of data where only total fraction was reported and
#we need to convert total fraction into dissolved Before we can compare to the objectives

CadmiumCF<-mutate(Hardness, CF=1.101672-(log(Result)*.041838))
CadmiumCF$AnalyteName<-"Cadmium"
LeadCF<-mutate(Hardness,CF=1.46203-(log(Result)*.145712))
LeadCF$AnalyteName<-"Lead"
CopperCF<-mutate(Hardness,CF=.960)
CopperCF$AnalyteName<-"Copper"
ChromiumVICF<-mutate(Hardness,CF=.962)
ChromiumVICF$AnalyteName<-"Chromium VI"
ChromiumIIICF<-mutate(Hardness,CF=.860)
ChromiumIIICF$AnalyteName<-"Chromium III"
NickelCF<-mutate(Hardness,CF=.997)
NickelCF$AnalyteName<-"Nickel"
ZincCF<-mutate(Hardness,CF=.986)
ZincCF$AnalyteName<-"Zinc"
ConversionFactors<-rbind.data.frame(CadmiumCF, CopperCF, ChromiumVICF,ChromiumIIICF, LeadCF, NickelCF, ZincCF)
ConversionFactors$Result<-NULL
ConversionFactors$UnitName<-NULL
ConversionFactors$MatrixName<-NULL
ConversionFactors$Type<-"Fresh"


SaltCadmiumCF<-mutate(Hardness, CF=.994)
SaltCadmiumCF$AnalyteName<-"Cadmium"
SaltLeadCF<-mutate(Hardness,CF=.951)
SaltLeadCF$AnalyteName<-"Lead"
SaltCopperCF<-mutate(Hardness,CF=.83)
SaltCopperCF$AnalyteName<-"Copper"
SaltChromiumVICF<-mutate(Hardness,CF=.993)
SaltChromiumVICF$AnalyteName<-"Chromium VI"
SaltNickelCF<-mutate(Hardness,CF=.990)
SaltNickelCF$AnalyteName<-"Nickel"
SaltZincCF<-mutate(Hardness,CF=.946)
SaltZincCF$AnalyteName<-"Zinc"
SaltConversionFactors<-rbind.data.frame(SaltCadmiumCF, SaltCopperCF, SaltChromiumVICF, SaltLeadCF, SaltNickelCF, SaltZincCF)
SaltConversionFactors$Result<-NULL
SaltConversionFactors$UnitName<-NULL
SaltConversionFactors$MatrixName<-NULL
SaltConversionFactors$Type<-"Salt"


#Createplaceholder information in case there is no hardness data so that
#we can still create the ConversionFactors table for joining and assessment
Tempinfo1<-c("AnalyteName","SampleDate","BeneficialUse","StationCode","ProjectName","CF","Type")
fillerinfo<-c("Placeholder","1850-01-01","Fake","Placeholder","Placeholder",0,"Brack")
FakeCF<-as.data.frame(rbind(Tempinfo1,fillerinfo),stringsAsFactors=FALSE)
colnames(FakeCF)<-Tempinfo1
row.names(FakeCF)<-NULL
FakeCF<-FakeCF[-1,]
FakeCF<-tbl_df(FakeCF)
FakeCF$SampleDate<-Sys.Date()
FakeCF$CF<-0
ConversionFactors1<-FakeCF



#Stack the conversion factors and the fake conversionfactors to create a full size conversion factor table
ConversionFactors1<-tbl_df(rbind.data.frame(ConversionFactors1,ConversionFactors))
ConversionFactors<-ConversionFactors1
ConversionFactors<-tbl_df(rbind.data.frame(ConversionFactors,SaltConversionFactors))



#Add the conversion factors to the data that needs converting by pollutant, matrix, BeneficalUse, SampleDate, ProjectDate
NeedsConverting<-tbl_df(merge(NeedsConverting,ConversionFactors,all.x=TRUE))


#Add a converstion factor to the rows that did not have cresponding hardness data collected
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Cadmium"& is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.909001
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Copper" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.960
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Chromium VI" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.962
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Chromium III" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.860
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Lead" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.791001
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Nickel" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.997
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Silver"&is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.85
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Zinc" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Fresh")]<-.986

NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Cadmium"& is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.994
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Copper" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.83
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Chromium VI" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.993
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Lead" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.951
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Nickel" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.990
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Silver"&is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.85
NeedsConverting$CF[which(NeedsConverting$AnalyteName=="Zinc" & is.na(NeedsConverting$CF)&NeedsConverting$Type=="Salt")]<-.946


#Convert the results field for all rows in the needs converting table based on their converstion factos
#Then delete the CF field and results field, then change the name of NewResults to Result
NeedsConverting<-mutate(NeedsConverting, NewResults=Result*CF)
NeedsConverting$CF<-NULL
NeedsConverting$Result<-NULL
names(NeedsConverting)[names(NeedsConverting)=="NewResults"]<-"Result"

#Change fraction from total to dissolved, because that is what we did with this calculation
NeedsConverting$FractionName[NeedsConverting$FractionName=="Total"]<-"Dissolved"

#Combine the results that were converted to the results that did not need converting
W_Waterbodies_hardness<-rbind.data.frame(W_Waterbodies_hardness_no_conversion, NeedsConverting)


#Join Hardness Objectives to W_Waterbodies_hardness (analytes that require hardness objective)
Hardness_W_Objectives<-tbl_df(merge(W_Waterbodies_hardness,HardnessObjectives,by=c("ProjectName"
	,"StationCode","SampleDate","MatrixName","AnalyteName","BeneficialUse","Type"),all.x=TRUE))

#Will introduce NA for any analyte that does not have hardness data collected with it

#Replace NAs created in step above for values from CTR (CCC) when hardness is taken to be 100
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Cadmium"&Hardness_W_Objectives$Type=="Fresh"]<-2.2
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Copper"&Hardness_W_Objectives$Type=="Fresh"]<-9
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium VI"&Hardness_W_Objectives$Type=="Fresh"]<-11
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium"&Hardness_W_Objectives$Type=="Fresh"]<-180
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium III"&Hardness_W_Objectives$Type=="Fresh"]<-180
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Lead"&Hardness_W_Objectives$Type=="Fresh"]<-2.5
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Nickel"&Hardness_W_Objectives$Type=="Fresh"]<-52
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Fresh"]<-1.7
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Zinc"&Hardness_W_Objectives$Type=="Fresh"]<-120
Hardness_W_Objectives<-tbl_df(Hardness_W_Objectives)


#Replace NAs created in step above for values from CTR (CCC) when hardness is taken to be 100
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Cadmium"&Hardness_W_Objectives$Type=="Salt"]<-9.3
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Copper"&Hardness_W_Objectives$Type=="Salt"]<-3.1
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Chromium VI"&Hardness_W_Objectives$Type=="Salt"]<-50
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Lead"&Hardness_W_Objectives$Type=="Salt"]<-8.1
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Nickel"&Hardness_W_Objectives$Type=="Salt"]<-8.2
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Salt"]<-.95
Hardness_W_Objectives$Objective[is.na(Hardness_W_Objectives$Objective) & Hardness_W_Objectives$AnalyteName=="Zinc"&Hardness_W_Objectives$Type=="Salt"]<-81
Hardness_W_Objectives<-tbl_df(Hardness_W_Objectives)

#Tell R that Hardness_W_Objectives column "Objective" is a numeric value just in case it wants to believe otherwise
Hardness_W_Objectives$Objective<-as.numeric(Hardness_W_Objectives$Objective)

#Add objective and Eval Guideline fields so that we can join this back in to main table later in the process
Hardness_W_Objectives$Evaluation_Guideline<-"NA"
Hardness_W_Objectives$Eval_Ref_Number<-"NA"
Hardness_W_Objectives$Objective_Language<-"NA"
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$Type=="Fresh")]<-paste0("California Toxics Rule (CTR) lists criterion continuous concentrations to protect aquatic life in freshwater. The criterion in freshwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")
Hardness_W_Objectives$Objective_Ref_Number<-"476"

#Add language for salt water.  keep the below command on a single line or it will cause issues with the LOE export
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$Type=="Salt")]<-paste0("California Toxics Rule (CTR) lists criterion continuous concentrations to protect aquatic life in saltwater. The criterion in saltwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")

#Add language to silver because silver is CMC value not CCC value
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Salt")]<-paste0("California Toxics Rule (CTR) lists criterion maximum concentrations to protect aquatic life in saltwater. The criterion in saltwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")
Hardness_W_Objectives$Objective_Language[which(Hardness_W_Objectives$AnalyteName=="Silver"&Hardness_W_Objectives$Type=="Fresh")]<-paste0("California Toxics Rule (CTR) lists criterion maximum concentrations to protect aquatic life in freshwater. The criterion in freshwater is hardness dependent for each sample and varies based on the ambient hardness during sampling. Section (b)(1) in CTR contains the hardness dependent formula for the metals criterion.")


#Richard's code to correct results based on quantitation limtis
Hardness_W_Objectives$Result[which(Hardness_W_Objectives$RL<Hardness_W_Objectives$Objective & (Hardness_W_Objectives$ResQualCode=="ND"
	|Hardness_W_Objectives$ResQualCode=="DNQ"))]<-Hardness_W_Objectives$MDL[which(Hardness_W_Objectives$RL<
	Hardness_W_Objectives$Objective & (Hardness_W_Objectives$ResQualCode=="ND"|Hardness_W_Objectives$ResQualCode=="DNQ"))]*.5 #perfect scenario
	
	Hardness_W_Objectives$Result[which((Hardness_W_Objectives$RL<Hardness_W_Objectives$Objective)&is.na(Hardness_W_Objectives$MDL)&(Hardness_W_Objectives$ResQualCode=="ND"
	|Hardness_W_Objectives$ResQualCode=="DNQ"))]<-0 #When sample can be quantified, but is missing the MDL

	Hardness_W_Objectives$Result[which(Hardness_W_Objectives$RL>Hardness_W_Objectives$Objective
	|is.na(Hardness_W_Objectives$RL)&(Hardness_W_Objectives$ResQualCode=="ND"|Hardness_W_Objectives$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number




#Create table of samples thrown out due to quantitation limits
Hardness_Tossed<-filter(Hardness_W_Objectives, (Result==-100000000))
Hardness_Tossed$Type<-NULL
#Hardness_Tossed$RL<-NULL
#Hardness_Tossed$MDL<-NULL
#Hardness_Tossed$ResQualCode<-NULL
#Hardness_Tossed$QACode<-NULL
Hardness_Tossed$AveragingPeroid<-4


#Filter out the samples that did not meet quantitiation limitations
Hardness_W_Objectives<-filter(Hardness_W_Objectives, (Result!=-100000000))


###Remove replicate samples (collected on the same day and station) 
##from hardness analytes table where only total fraction metals were reported
Hardness_W_Objecitves<-tbl_df(as.data.table(Hardness_W_Objectives)[,mean(Result),list(AnalyteName,MatrixName
	,SampleDate,BeneficialUse,StationCode,ProjectName,UnitName,TargetLatitude
	,TargetLongitude,Waterbody,WBID,Wbtype,FractionName,Type)])
	names(Hardness_W_Objectives)[names(Hardness_W_Objectives)=="V1"]<-"Result"

#Remove unecessary fields from hardness data
#Hardness_W_Objectives$TargetLatitude<-NULL
#Hardness_W_Objectives$TargetLongitude<-NULL
#Hardness_W_Objectives$Wbtype<-NULL
Hardness_W_Objectives$Type<-NULL
Hardness_W_Objectives$AveragingPeroid<-4
Hardness_W_Objectives$AveragingPeroid[which(Hardness_W_Objectives$AnalyteName=="Silver")]<-1


#Create table of hardness adjusted data that have units other than ug/L
#If this table gets exported with any rows, we will need to add a conversion section to this code
HardnessUnitsTossedEnd<-subset(ExportedData,(AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc"))

HardnessUnitsTossedEnd<-filter(HardnessUnitsTossedEnd,!(UnitName=="ug/L"|UnitName=="mg/L"|UnitName=="pg/L"|UnitName=="ng/L"|UnitName=="g/L"))
HardnessUnitsTossedEnd$Issue<-"Issues with the units of Hardness dependent analytes.  Talk to Jacob about adding unit conversion to ReLEP"
AllExportedData<-rbind.data.frame(AllExportedData,HardnessUnitsTossedEnd)

#Cut rows with bad units out of Hardness adjusted data so that only ug/L remains
Hardness_W_Objectives<-Hardness_W_Objectives[Hardness_W_Objectives$UnitName=="ug/L",]

# Remove old values of Hardness dependent metals with BU of WA or CO from W_Waterbodies
W_Waterbodies<-filter(W_Waterbodies, !((AnalyteName=="Cadmium"|AnalyteName=="Copper"
	|AnalyteName=="Chromium"|AnalyteName=="Chromium VI"|AnalyteName=="Chromium III"|AnalyteName=="Lead"
	|AnalyteName=="Nickel"|AnalyteName=="Silver"|AnalyteName=="Zinc") & (BeneficialUse=="CO"|BeneficialUse=="WA"|BeneficialUse=="MA"|BeneficialUse=="ES")))


###########___________________End_Hardness_______________________##############################


#Combine Unionized Ammonia with the main data frame before converting units
W_Waterbodies<-rbind.data.frame(W_Waterbodies,Converted_Ammonia)

##############################Begin UNIT CONVERSION SECTION###################

# Convert Results, MDL, and RL that are in mg/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="mg/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="mg/L")]*1000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="mg/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="mg/L")]*1000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="mg/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="mg/L")]*1000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="mg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in pg/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="pg/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="pg/L")]*.000001
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="pg/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="pg/L")]*.000001
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="pg/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="pg/L")]*.000001
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="pg/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in ng/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ng/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="ng/L")]*.001
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ng/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="ng/L")]*.001
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ng/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="ng/L")]*.001
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="ng/L"]<-"ug/L"

# Convert Results, MDL, and RL that are in g/L to ug/L datasetwide and change UnitName to ug/L
W_Waterbodies$Result[which(W_Waterbodies$UnitName=="g/L")]<-W_Waterbodies$Result[which(W_Waterbodies$UnitName=="g/L")]*1000000
W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="g/L")]<-W_Waterbodies$MDL[which(W_Waterbodies$UnitName=="g/L")]*1000000
W_Waterbodies$RL[which(W_Waterbodies$UnitName=="g/L")]<-W_Waterbodies$RL[which(W_Waterbodies$UnitName=="g/L")]*1000000
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="g/L"]<-"ug/L"

#Change units for specific conductivity to be un uS/cm from umhos (even though its a 1:1 conversion)
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="umhos/cm"]<-"uS/cm"

#Change units for turbidity from NTRU to NTU (even though its a 1:1 conversion)
W_Waterbodies$UnitName[W_Waterbodies$UnitName=="NTRU"]<-"NTU"


UnitsTossed<-filter(ExportedData,!(UnitName=="ug/L"|UnitName=="mg/L"|UnitName=="pg/L"|UnitName=="ng/L"|UnitName=="g/L"|UnitName=="umhos/cm"|UnitName=="uS/cm"|UnitName=="NTU"|UnitName=="NTRU"))
UnitsTossed$Issue<-"Unit Issues.  Data reported in non-standard units."
AllExportedData<-rbind.data.frame(AllExportedData,UnitsTossed)


##########################___End Unit Conversion__################################################


#####################___________Start Pentachlorophenol Objective Calc_____#######################
#Create table of pentachlorophenol data then average for replicates
PentaData<-W_Waterbodies[W_Waterbodies$AnalyteName=="Pentachlorophenol"&(W_Waterbodies$BeneficialUse=="CO"|W_Waterbodies$BeneficialUse=="WA"),]


#Remove replicates from penta data
PentaData<-tbl_df(as.data.table(PentaData)[,mean(Result),list(AnalyteName,MatrixName
	,SampleDate,BeneficialUse,StationCode,ProjectName,UnitName,RL,MDL,QACode
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,ResQualCode,FractionName)])
	names(PentaData)[names(PentaData)=="V1"]<-"Result"

#Remove replicates from the pH data
PentapH<-tbl_df(as.data.table(PentapH)[,mean(pHResult),list(MatrixName,SampleDate
	,BeneficialUse,StationCode,ProjectName,TargetLatitude,TargetLongitude
	,Waterbody,WBID,Wbtype)])
	names(PentapH)[names(PentapH)=="V1"]<-"pHResult"


#Merge with pH data add a default pH value of 7.8 for all PentaChlorophenol results 
#that do not have coresponding pH result to use
PentaData<-tbl_df(merge(PentaData,PentapH,by=c("StationCode","ProjectName","SampleDate"
	,"TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse"
	,"MatrixName"), all.x=TRUE))
PentaData$pHResult[which(is.na(PentaData$pHResult))]<-7.8

#Calculate objective based on pH, then remove uncessary columns
Penta_W_Objectives<-PentaData
Penta_W_Objectives<-tbl_df(mutate(Penta_W_Objectives,Objective=(exp(1.005*(pHResult)-5.134))))
Penta_W_Objectives$pHResult<-NULL
Penta_W_Objectives$AveragingPeroid<-4

#Add objective language, and reference codes
Penta_W_Objectives$Evaluation_Guideline<-"NA"
Penta_W_Objectives$Eval_Ref_Number<-"NA"
Penta_W_Objectives$Objective_Language<-paste0("The Pentachlorophenol criterion continuous concentration (expressed as a 4-day average) to protect aquatic life in freshwater is pH dependent.  The criteria has a value of 15 ug/L when based on a default pH of 7.8. (California Toxics Rule, 2000).  ")
Penta_W_Objectives$Objective_Ref_Number<-"3714"

#Remove Penta data from the main data frame
W_Waterbodies<-filter(W_Waterbodies, !(AnalyteName=="Pentachlorophenol" & (BeneficialUse=="CO"|BeneficialUse=="WA")))

######################______________End Penta Objective Calc______#######################################

###############_____________________Begin Total Ammonia Criteria Calculation______###########

#Split out total ammonia so that the objective can be calculated
TotalAmmoniaObjectives<-W_Waterbodies[W_Waterbodies$AnalyteName=="Ammonia as N"&(W_Waterbodies$BeneficialUse=="CO"|W_Waterbodies$BeneficialUse=="WA"),]

#Merge ammonia data with pH and Temperature.
TotalAmmoniaObjectives<-tbl_df(merge(TotalAmmoniaObjectives,PentapH,by=c("StationCode","ProjectName"
	,"SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaObjectives<-tbl_df(merge(TotalAmmoniaObjectives,temp,by=c("StationCode","ProjectName"
	,"SampleDate","TargetLatitude","TargetLongitude","Waterbody","WBID","Wbtype","BeneficialUse","MatrixName")))

#Calculate objectives based on formula in EPA 2013
TotalAmmoniaObjectives<-mutate(TotalAmmoniaObjectives,Objective=1000*.8876*((.0278/(1+10^(7.688-pHResult)))+(1.1994/(1+10^(pHResult-7.688))))*(2.126*10^(.028*(20-TempResult))))

#Remove unecessary Columns
TotalAmmoniaObjectives$TempResult<-NULL
TotalAmmoniaObjectives$pHResult<-NULL


#Add objective language, and reference codes
TotalAmmoniaObjectives$Evaluation_Guideline<-"NA"
TotalAmmoniaObjectives$Eval_Ref_Number<-"NA"
TotalAmmoniaObjectives$Objective_Language<-paste0("The Total Ammonia criterion continuous concentration (expressed as a 30-day average) to protect aquatic life in freshwater is Temperature and pH dependent, and was calculated according to the formula listed in the Aquatic Life Ambient Water Quality Criteria for Ammonia - Freshwater 2013 document.")
TotalAmmoniaObjectives$Objective_Ref_Number<-"4107"
TotalAmmoniaObjectives$AveragingPeroid<-30
TotalAmmoniaObjectives<-tbl_df(TotalAmmoniaObjectives)


#Remove the rows of data from main data frame where the objectives were just calculated
W_Waterbodies<-filter(W_Waterbodies, !(AnalyteName=="Ammonia as N" & (BeneficialUse=="CO"|BeneficialUse=="WA")))


################_______________End Total Ammonia Objective Calc______##############################################

###################################################################################################################

###################################################################################################################

#############____________________Start Summing Pollutant Calculations________________________######################

#Create table of pollutants that need summing
SummingPollutantsSum<-tbl_df(merge(W_Waterbodies,SummingPollutants,by="AnalyteName"))


########
######
####
##
#Split out pyrethroids to calculate LC50 adjusted results
Pyrethroids<-SummingPollutantsSum[SummingPollutantsSum$SummingName=="Pyrethroids",]

#Join pyrehtroid data with the objective (which should be LC50s of the pollutant in question)
#Join to the CEDEN name not the summing name
Pyrethroids<-tbl_df(merge(Pyrethroids,Analytes,by=c("AnalyteName","BeneficialUse","UnitName")))

#Adjust for quantitation issues
Pyrethroids$Result[which(Pyrethroids$RL<Pyrethroids$Objective & (Pyrethroids$ResQualCode=="ND"|Pyrethroids$ResQualCode=="DNQ"))]<-0 #perfect scenario

Pyrethroids$Result[which((Pyrethroids$RL>Pyrethroids$Objective|is.na(Pyrethroids$RL))&(Pyrethroids$ResQualCode=="ND"|Pyrethroids$ResQualCode=="DNQ"))]<-0 #imperfect results in zero
Pyrethroids<-tbl_df(Pyrethroids)

#Remove replicates from the pyrethroid data
Pyrethroids<-as.data.table(Pyrethroids)[,mean(Result),list(AnalyteName,StationCode,ProjectName
,SampleDate,MatrixName,FractionName,UnitName,TargetLatitude
,TargetLongitude,Waterbody,WBID,Wbtype,BeneficialUse,Objective,SummingName)]
names(Pyrethroids)[names(Pyrethroids)=="V1"]<-"Result"

#Divide the result by the objective to get LC50 normalized data
Pyrethroids<-mutate(Pyrethroids,Result=Result/Objective)

#Remove uncessary fields
Pyrethroids<-subset(Pyrethroids,select=c("AnalyteName","BeneficialUse","UnitName"
,"StationCode","ProjectName","SampleDate","MatrixName","FractionName","Result"
,"Waterbody","WBID","Wbtype","SummingName","TargetLatitude","TargetLongitude"))

#Swap CEDEN analyte name for summed analyte name
Pyrethroids$AnalyteName<-NULL
names(Pyrethroids)[names(Pyrethroids)=="SummingName"]<-"AnalyteName"

#Add LC50 normalized results together so that all pyrethroid
#toxicity is summed for the sample day
Pyrethroids<-as.data.table(Pyrethroids)[,sum(Result),list(AnalyteName,BeneficialUse
,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName,Waterbody,WBID,Wbtype
,TargetLatitude,TargetLongitude)]
names(Pyrethroids)[names(Pyrethroids)=="V1"]<-"Result"
Pyrethroids<-tbl_df(Pyrethroids)

#add objectives back to the table
Pyrethroids<-tbl_df(merge(Pyrethroids, Analytes,by=c("AnalyteName","BeneficialUse","UnitName")))

##########
########
######
###
##
#Split out PAHs from summing pollutants table and multiply the result by appropriate TEFs
PAHs<-tbl_df(merge(SummingPollutantsSum,PAH_TEFs,by=c("AnalyteName")))

##Remove rows with summed names other than total PAHs as those may have
##their own objectives and their may be "sub isomers" that need to be summed
##and comapred to their objective in addition to the sum of all PAHs
PAHs<-PAHs[PAHs$SummingName=="PAHs (Polycyclic Aromatic Hydrocarbons)",]

#adjust result based on TEf by multiplying result by TEF
PAHs<-mutate(PAHs,Result=Result*TEF)
PAHs<-PAHs[which(PAHs$BeneficialUse=="MU"|PAHs$BeneficialUse=="CM"),]
#Remove TEF field form table
PAHs$TEF<-NULL

#Remove the PAH Data from the Summing pollutants table
SummingPollutantsSum<-anti_join(SummingPollutantsSum,PAHs,by=c("AnalyteName","StationCode"
	,"ProjectName","SampleDate","MatrixName","FractionName","Waterbody"
	,"WBID","Wbtype","BeneficialUse","SummingName"))

#Then add the adjusted PAH rows back in
SummingPollutantsSum<-rbind.data.frame(SummingPollutantsSum,PAHs)

#Change the names of the analytes columns so that we can look up
#objectives based on summing pollutant name instead of CEDEN name
names(SummingPollutantsSum)[names(SummingPollutantsSum)=="AnalyteName"]<-"CEDENName"
names(SummingPollutantsSum)[names(SummingPollutantsSum)=="SummingName"]<-"AnalyteName"


#Convert Aroclors data to total PCB data for aquatic life BUs, also remove congener data for 
#aquatic life PCBs because there is no objective for them
AquaticLifePCBs<-filter(SummingPollutantsSum,(AnalyteName=="Aroclor"&(BeneficialUse=="WA"|BeneficialUse=="CO"|BeneficialUse=="MA"|BeneficialUse=="ES")))
AquaticLifePCBs$AnalyteName<-"PCBs (Polychlorinated biphenyls)"
SummingPollutantsSum<-filter(SummingPollutantsSum,!((AnalyteName=="PCBs (Polychlorinated biphenyls)"|AnalyteName=="PCB, total Congeners")&(BeneficialUse=="CO"|BeneficialUse=="WA"|BeneficialUse=="ES"|BeneficialUse=="MA")))
SummingPollutantsSum<-rbind.data.frame(SummingPollutantsSum,AquaticLifePCBs)


#Look Up objectives based on summing pollutant name
Sum_No_reps<-tbl_df(merge(SummingPollutantsSum,Analytes,by=c("AnalyteName","UnitName","BeneficialUse")))

#Create a table of summing pollutants that do not have objectives
nosums<-anti_join(SummingPollutantsSum,Analytes,by=c("AnalyteName","UnitName","BeneficialUse"))


# Correct the results based on quantitation limits - I did this before removing 
#replicates to avoid averaging samples that we assesed with a different method

Sum_No_reps$Result[which(Sum_No_reps$RL<Sum_No_reps$Objective 
	& (Sum_No_reps$ResQualCode=="ND"|Sum_No_reps$ResQualCode=="DNQ"))]<-0 #perfect scenario

Sum_No_reps$Result[which((Sum_No_reps$RL>Sum_No_reps$Objective|is.na(Sum_No_reps$RL))
	&(Sum_No_reps$ResQualCode=="ND"|Sum_No_reps$ResQualCode=="DNQ"))]<-0 #imperfect results in very negative number
Sum_No_reps<-tbl_df(Sum_No_reps)


Sum_No_reps<-as.data.table(Sum_No_reps)[,mean(Result),list(AnalyteName,BeneficialUse
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,CEDENName,Objective,AveragingPeroid
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
	names(Sum_No_reps)[names(Sum_No_reps)=='V1']<-"Result"
	Sum_No_reps<-tbl_df(Sum_No_reps)


## Add Results field together
SummedSummingPollutants<-as.data.table(Sum_No_reps)[,sum(Result),list(AnalyteName,BeneficialUse
	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName,TargetLatitude,TargetLongitude
	,Waterbody,WBID,Wbtype,AveragingPeroid,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
	,Eval_Ref_Number)]
	names(SummedSummingPollutants)[names(SummedSummingPollutants)=='V1']<-"Result"
	SummedSummingPollutants<-tbl_df(SummedSummingPollutants)

#Split out PCBs and Aroclors from summed summing pollutants to calculate the max 
#sample day result for assessment


MaxPCBs<-SummedSummingPollutants[which((SummedSummingPollutants$AnalyteName=="Aroclor"
	|SummedSummingPollutants$AnalyteName=="PCB, total Congeners")|SummedSummingPollutants$AnalyteName=="PCBs (Polychlorinated biphenyls)"
	&(SummedSummingPollutants$BeneficialUse=="MU"|SummedSummingPollutants$BeneficialUse=="CM")),]

MaxPCBs$PCBName<-"PCBs (Polychlorinated biphenyls)"

#Calculate the max result for each station sample day Aroclors or PCBs
MaxPCBs<-as.data.table(MaxPCBs)[,max(Result),list(PCBName,BeneficialUse,UnitName,StationCode,ProjectName
	,SampleDate,MatrixName,FractionName,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype
	,Objective,AveragingPeroid,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
	MaxPCBs<-tbl_df(MaxPCBs)
	names(MaxPCBs)[names(MaxPCBs)=='V1']<-"Result"
	names(MaxPCBs)[names(MaxPCBs)=='PCBName']<-"AnalyteName"

#Cut out CM and MU PCB data and then put the adjusted results back in
SummedSummingPollutants<-filter(SummedSummingPollutants,!((AnalyteName=="Aroclor"
	|AnalyteName=="PCB, total Congeners")&(BeneficialUse=="MU"|BeneficialUse=="CM")))
	SummedSummingPollutants<-rbind(SummedSummingPollutants,MaxPCBs)


#Add pyrethroid data back into the summing table
SummedSummingPollutants<-rbind.data.frame(SummedSummingPollutants,Pyrethroids)


####Add back generic information for these fields that are no longer neeeded so that 
#####this table can be joined with the W_Objectives table that is about to be created
#######Adding these fields allows us to join the table back in before the station count
######and allows the data to go thoguht he quantitation check a second time without being 
#####modified.  Since the qunaittaiton check has already been run on this data, and since 
######there is no way of knowing what the RL, or MDL would be for the summed
####pollutants, the quantitation check does not need to be run a second time on this data.

SummedSummingPollutants$MDL<-0
SummedSummingPollutants$RL<-0
SummedSummingPollutants$QACode<-"None"
SummedSummingPollutants$ResQualCode<-"="


################################End summing pollutant Calculations###########################################################################
#############################################################################################################################################
#############################################################################################################################################

#Add objectives to saline ammonia data
SalineAmmonia_W_Objectives<-tbl_df(merge(W_Waterbodies,SalinityAmmoniaObjectives))

# Add Objectives table
W_Objectives<-merge(W_Waterbodies,Analytes,by=c("AnalyteName","BeneficialUse","UnitName"))
W_Objectives<-tbl_df(W_Objectives)
Missing_Analytes<-anti_join(ExportedData,Analytes,by="AnalyteName")
Missing_Analytes<-anti_join(Missing_Analytes,SummingPollutants)
Missing_Analytes$Issue<-"Analyte missing objective"
Missing_Analytes<-anti_join(Missing_Analytes,TotalAmmoniaObjectives,by=c("ProjectName","AnalyteName","SampleDate","TargetLatitude","TargetLongitude","StationCode"))
AllExportedData<-rbind.data.frame(AllExportedData,Missing_Analytes)


#Join Hardness adjusted objective rows to the main data table
W_Objectives<-rbind.data.frame(W_Objectives,Hardness_W_Objectives)

#Join Pentachlorophenol data to the main table
W_Objectives<-rbind.data.frame(W_Objectives,Penta_W_Objectives)

#Join Total Ammonia data to the main table
W_Objectives<-rbind.data.frame(W_Objectives,TotalAmmoniaObjectives)

#Join Saline Ammonia data to main table
W_Objectives<-rbind.data.frame(W_Objectives,SalineAmmonia_W_Objectives)

#Add Summed pollutants back into table
W_Objectives<-rbind.data.frame(W_Objectives,SummedSummingPollutants)


#Remove unecessary fields from summed pollutants
#then add the rows to the w-waterbodies table
#so that they can be joined into the station table
#and thus included in the LOEs
SummedSummingPollutants$Objective<-NULL
SummedSummingPollutants$Objective_Language<-NULL
SummedSummingPollutants$Evaluation_Guideline<-NULL
SummedSummingPollutants$Objective_Ref_Number<-NULL
SummedSummingPollutants$Eval_Ref_Number<-NULL
SummedSummingPollutants$AveragingPeroid<-NULL

#Add summedsumming pollutants table to the W_Waterbodies table
#So that we can calculate station information
W_Waterbodies<-rbind.data.frame(W_Waterbodies,SummedSummingPollutants)
W_Waterbodies<-rbind.data.frame(W_Waterbodies,NeedsConvertingForStations)

#Create a new table of station codes, waterbodies, analyte, and count of stations
Stations<-subset(W_Waterbodies,select=c("AnalyteName","StationCode","Waterbody","WBID","ProjectName","MatrixName","FractionName"))
Stations<-distinct(select(Stations,AnalyteName,Waterbody,StationCode,WBID,ProjectName,MatrixName,FractionName))
Stations<-Stations%>%group_by(AnalyteName,Waterbody,WBID,ProjectName,MatrixName,FractionName)%>%summarise(count=n(),
StationCode=paste(StationCode,collapse=", "))
names(Stations)<-c("AnalyteName","Waterbody","WBID","ProjectName","MatrixName","FractionName","StationCount","StationCode")


# Correct the results based on quantitation limits - I did this before removing replicates to avoid averaging samples that we assesed with a different method

W_Objectives$Result[which(W_Objectives$RL<W_Objectives$Objective & (W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<-W_Objectives$MDL[which(
	W_Objectives$RL<W_Objectives$Objective & (W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]*.5 #perfect scenario

W_Objectives$Result[which((W_Objectives$RL<W_Objectives$Objective)&is.na(W_Objectives$MDL)&(W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<-0 #When sample can be quantified, but is missing the MDL

W_Objectives$Result[which((W_Objectives$RL>W_Objectives$Objective|is.na(W_Objectives$RL))&(W_Objectives$ResQualCode=="ND"|W_Objectives$ResQualCode=="DNQ"))]<--100000000 #imperfect results in very negative number





#W_Objectives<-as.data.table(W_Objectives)[,mean(Result),list(AnalyteName,BeneficialUse
#	,UnitName,StationCode,ProjectName,SampleDate,MatrixName,FractionName
#	,TargetLatitude,TargetLongitude,Waterbody,WBID,Wbtype,Objective,AveragingPeroid
#	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
#	names(W_Objectives)[names(W_Objectives)=='V1']<-"Result"
#	W_Objectives<-tbl_df(W_Objectives)

#Create table of tossed samples and then combine them with the other tables of tossed samples due to quantitation limts
General_Tossed<-filter(W_Objectives,(Result==-100000000))

General_Tossed<-rbind.data.frame(General_Tossed,Hardness_Tossed)


# Calculate maximum and minimum date for each waterbody with data
General_Tossed$SampleDate<-as.Date(General_Tossed$SampleDate, "%m/%d/%Y")
Tossed_Max_Date<-as.data.table(General_Tossed)[,max(SampleDate),list(AnalyteName
	,BeneficialUse,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Tossed_Max_Date)[names(Tossed_Max_Date)=='V1']<-"MaxDate"
Tossed_Max_Date<-tbl_df(Tossed_Max_Date)
Tossed_Min_Date<-as.data.table(General_Tossed)[,min(SampleDate),list(AnalyteName
	,BeneficialUse,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language
	,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Tossed_Min_Date)[names(Tossed_Min_Date)=='V1']<-"MinDate"
Tossed_Min_Date<-tbl_df(Tossed_Min_Date)
Tossed_Date_Range<-tbl_df(merge(Tossed_Max_Date,Tossed_Min_Date,by=c("AnalyteName"
	,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BeneficialUse"
	,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))
General_Tossed<-tbl_df(merge(General_Tossed,Tossed_Date_Range))


#Convert General_Tossed into a list of samples thrown out with language specifying they were thrown out due to quantitation issues
Quantitation_Discards<-distinct(select(General_Tossed,AnalyteName,Waterbody,WBID,MatrixName
	,FractionName,ProjectName,BeneficialUse,Objective_Language,Evaluation_Guideline
	,Objective_Ref_Number,Eval_Ref_Number,MaxDate,MinDate,SampleDate))
Quantitation_Discards<-Quantitation_Discards%>%group_by(AnalyteName,Waterbody,WBID
	,ProjectName,MatrixName,FractionName,BeneficialUse
	,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number
	,MaxDate,MinDate)%>%summarise(count=n())
names(Quantitation_Discards)<-c("AnalyteName","Waterbody","WBID","ProjectName"
	,"MatrixName","FractionName","BeneficialUse","Objective_Language"
	,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"
	,"MaxDate","MinDate","TossedSampleCount")


W_Objectives<-filter(W_Objectives,(Result>-100000000))

#Create table of rows with 1 hour averaging peroid
CMC_Values<-W_Objectives[W_Objectives$AveragingPeroid==1,]

#Remove the 1 hour averaging peroid data form the main table
W_Objectives<-filter(W_Objectives,(AveragingPeroid!=1))


##################################################################################################
####################################Start 1 day MAX###############################################
##################################################################################################
CMC_Values$SampleDate2<-ymd(CMC_Values$SampleDate)

#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
CMC_Values <- CMC_Values %>% arrange(SampleDate)%>%
  group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
	,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 1)) %>% 
  group_by(interval, add = TRUE) %>% 
  summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = max(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
 ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
 ,Eval_Ref_Number,Result)
  

##############################End 1 Day Max########################################################
###################################################################################################
###################################################################################################


#Split out DO from W_Objectives
DO_Data<-W_Objectives[W_Objectives$AnalyteName=="Oxygen, Dissolved"|W_Objectives$AnalyteName=="Oxygen, Saturation",]

#Remove DO Data from W_Objectives
W_Objectives<-filter(W_Objectives,!(AnalyteName=="Oxygen, Dissolved"|AnalyteName=="Oxygen, Saturation"))


##################################################################################################
####################################Start 1 day MIN###############################################
##################################################################################################


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
DO_Data <- DO_Data %>% arrange(SampleDate)%>%
  group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
	,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 1)) %>% 
  group_by(interval, add = TRUE) %>% 
  summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = min(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
 ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
 ,Eval_Ref_Number,Result)
  

##############################End 1 Day Max########################################################
###################################################################################################
###################################################################################################




No_reps<-as.data.table(W_Objectives)[,mean(Result),list(AnalyteName,BeneficialUse,UnitName
	,StationCode,ProjectName,SampleDate,MatrixName,FractionName
	,Waterbody,WBID,Wbtype,Objective,AveragingPeroid,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
	,Eval_Ref_Number)]
names(No_reps)[names(No_reps)=='V1']<-"Result"
W_Objectives<-tbl_df(No_reps)


###############_____________________Begin Second Total Ammonia Criteria Calculation______###########

#Split out total ammonia so that the objective can be calculated
TotalAmmoniaObjectives2<-W_Objectives[W_Objectives$AnalyteName=="Ammonia as N"&(W_Objectives$BeneficialUse=="CO"|W_Objectives$BeneficialUse=="WA"),]


#Remove objective fields from the averaged ammonia data
TotalAmmoniaObjectives2$Evaluation_Guideline<-NULL
TotalAmmoniaObjectives2$Eval_Ref_Number<-NULL
TotalAmmoniaObjectives2$Objective_Language<-NULL
TotalAmmoniaObjectives2$Objective_Ref_Number<-NULL


#Average the pH and Temp data outside of objective application (this is less imporant with pH but temperature may not have any objective for CO)
PentapH2<-as.data.table(PentapH)[,mean(pHResult),list(BeneficialUse,StationCode,ProjectName,SampleDate,MatrixName,Waterbody,WBID,Wbtype)]
names(PentapH2)[names(PentapH2)=='V1']<-"pHResult"
PentapH2<-tbl_df(PentapH2)

#Average the pH and Temp data outside of objective application (this is less imporant with pH but temperature may not have any objective for CO)
Temp2<-as.data.table(temp)[,mean(TempResult),list(BeneficialUse,StationCode,ProjectName,SampleDate,MatrixName,Waterbody,WBID,Wbtype)]
names(Temp2)[names(Temp2)=='V1']<-"TempResult"
Temp2<-tbl_df(Temp2)

#Merge ammonia data with pH and Temperature.
TotalAmmoniaObjectives2<-tbl_df(merge(TotalAmmoniaObjectives2,PentapH2,by=c("StationCode"
	,"ProjectName","SampleDate","Waterbody","WBID"
	,"Wbtype","BeneficialUse","MatrixName")))
TotalAmmoniaObjectives2<-tbl_df(merge(TotalAmmoniaObjectives2,Temp2,by=c("StationCode"
	,"ProjectName","SampleDate","Waterbody","WBID"
	,"Wbtype","BeneficialUse","MatrixName")))

#Calculate objectives based on formula in EPA 2013
TotalAmmoniaObjectives2<-mutate(TotalAmmoniaObjectives2,Objective=1000*.8876*((.0278/(1+10^(7.688-pHResult)))+(1.1994/(1+10^(pHResult-7.688))))*(2.126*10^(.028*(20-TempResult))))

#Remove unecessary Columns
TotalAmmoniaObjectives2$TempResult<-NULL
TotalAmmoniaObjectives2$pHResult<-NULL
TotalAmmoniaObjectives$AveragingPeroid<-30

#Create a table that contains all the fields for the ammonia information
#that contains fake placeholder information to avoid getting errors when there
#is not ammonia data in the data set being assessed
rownames<-c("StationCode","ProjectName","SampleDate"
	,"Waterbody","WBID","Wbtype","BeneficialUse","MatrixName","AnalyteName","UnitName"
	,"FractionName","Objective","Result")
fakeinformation<-c("placeholder","Placeholder","1850-01-01","Placeholder"
	,"Placeholder","Placeholer","Placeholder","na/L","Placeholder","NA","NA","1","0")
placeholderinfo<-as.data.frame(rbind(rownames,fakeinformation))
colnames(placeholderinfo)<-rownames
row.names(placeholderinfo)<-NULL
placeholderinfo<-placeholderinfo[-1,]
placeholderinfo<-tbl_df(placeholderinfo)
placeholderinfo$Result<-as.numeric(placeholderinfo$Result)
placeholderinfo$Objective<-as.numeric(placeholderinfo$Objective)
placeholderinfo$Result<-0
placeholderinfo$AveragingPeroid<-0


TotalAmmoniaObjectives2<-tbl_df(rbind.data.frame(TotalAmmoniaObjectives2,placeholderinfo))



#Add objective language, and reference codes
TotalAmmoniaObjectives2$Evaluation_Guideline<-"NA"
TotalAmmoniaObjectives2$Eval_Ref_Number<-"NA"
TotalAmmoniaObjectives2$Objective_Language<-paste0("The Total Ammonia criterion continuous concentration (expressed as a 30-day average) to protect aquatic life in freshwater is Temperature and pH dependent, and was calculated according to the formula listed in the Aquatic Life Ambient Water Quality Criteria for Ammonia - Freshwater 2013 document.")
TotalAmmoniaObjectives2$Objective_Ref_Number<-"4107"


#Remove the rows of data from main data frame where the objectives were just calculated
W_Objectives<-filter(W_Objectives, !(AnalyteName=="Ammonia as N" & (BeneficialUse=="CO"|BeneficialUse=="WA")))


################_______________End Second Total Ammonia Objective Calc______######################################




##############################################################################################################

########SPLIT OUT POLLUTATNS WITH AVERAGING PEROIDS OTHER THAN 7 DAYS HERE####################################

##############################################################################################################

#Create table of 4 day avering pollutants
FourDayAverage<-W_Objectives[which(W_Objectives$AveragingPeroid==4),]

ThirtyDayAveraging<-(TotalAmmoniaObjectives2)


#Remove 4 day averagin gpollutants from main table
W_Objectives<-W_Objectives[which(W_Objectives$AveragingPeroid==7),]


##############################################################################################################

###########################Begin 7 Day Averaging peroid#######################################################

##############################################################################################################

W_Objectives$SampleDate<-ymd(W_Objectives$SampleDate)


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
W_Objectives2 <- W_Objectives %>% arrange(SampleDate)%>%
  group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName,Waterbody
	,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 7)) %>% 
  group_by(interval, add = TRUE) %>% 
  summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = mean(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName, FractionName
	,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number,Result)
  

###################################End Seven Day Average##################################

###########################################################################################
####################################Start 4 day average#################################
######################################################################################
FourDayAverage$SampleDate2<-ymd(FourDayAverage$SampleDate)

#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
FourDayAverage2 <- FourDayAverage %>% arrange(SampleDate)%>%
  group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName,FractionName
	,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 4)) %>% 
  group_by(interval, add = TRUE) %>% 
  summarise(SampleDate = max(SampleDate),Objective=mean(Objective), Result = mean(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName
 ,FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline,Objective_Ref_Number
 ,Eval_Ref_Number,Result)
  



##############################End Four Day Agerage##################################
###################################################################################
#############################Start 30 day average####################################

ThirtyDayAveraging$SampleDate<-ymd(ThirtyDayAveraging$SampleDate)


#Make a new dataframe based on filtering d for what we need and aggrgating from the mean
ThirtyDayAveraging2 <- ThirtyDayAveraging %>% arrange(SampleDate)%>%
  group_by(AnalyteName,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName 
  ,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
  ,Objective_Ref_Number,Eval_Ref_Number) %>% 
  mutate(interval = cumsum(SampleDate - lag(SampleDate, default = min(SampleDate)) >= 30)) %>% 
  group_by(interval, add = TRUE) %>% 
  summarise(SampleDate = max(SampleDate),Objective = mean(Objective), Result = mean(Result)) %>%
  select(AnalyteName,SampleDate,BeneficialUse,UnitName,StationCode,ProjectName, MatrixName 
  , FractionName,Waterbody,WBID,Objective,Objective_Language,Evaluation_Guideline
   ,Objective_Ref_Number,Eval_Ref_Number,Result)
  
###########################End 30 Day Averaging##############################################

###########################################################################################
##########################################################################################


#Combine all averaged data back together into a single data frame
No_reps_W_Objectives<-tbl_df(rbind.data.frame(W_Objectives2,FourDayAverage2,ThirtyDayAveraging2,CMC_Values,DO_Data))

No_reps_W_Objectives$N<-NULL
No_reps_W_Objectives$SampleDate<-as.Date(No_reps_W_Objectives$SampleDate, "%m/%d/%Y")

No_reps<-No_reps_W_Objectives

#Convert No_reps back into W_Objectives
W_Objectives<-tbl_df(No_reps)

W_Objectives_for_date_Range<-W_Objectives


####################################################################################################################
##########################################Dissolved Oxygen exceedance count#########################################
#Split out DO and Alkalinity Data and then calculate samples and exceedances
DOData<-W_Objectives[W_Objectives$AnalyteName=="Oxygen, Dissolved"|W_Objectives$AnalyteName=="Oxygen, Saturation"|W_Objectives$AnalyteName=="Alkalinity as CaCO3",]

DOData$Objective<-as.numeric(DOData$Objective)
DOOver<-as.data.table(DOData)[,sum(Result<Objective),list(AnalyteName,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(DOOver)[names(DOOver)=='V1']<-"Exceedances"
DOOver<-tbl_df(DOOver)

W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
DOTotal<-as.data.table(DOData)[,sum(Result>-.1),list(AnalyteName,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(DOTotal)[names(DOTotal)=='V1']<-"Total"
DOTotal<-tbl_df(DOTotal)

# Combine the two toegether
DOresults<-merge(DOOver,DOTotal,by=c("AnalyteName","BeneficialUse","ProjectName","MatrixName"
,"FractionName","Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
DOresults<-tbl_df(DOresults)

###################################################################################################################################
###################################################################################################################################
#################################_______________Begin pH Sample Count____________________########################################

####Split out pH for pH sample count and exceedances
pHdata<-W_Objectives2[W_Objectives2$AnalyteName=="pH",]

#Add alias back to pH data
# Load Region 2 objecives table and convert to tbl_df
Analytes<-read.delim(ObjectivesTable,header=TRUE,stringsAsFactors=FALSE)
Analytes<-tbl_df(Analytes)

#Remove numeric objective because it was previously averaged during the averaging peroid section
pHdata$Objective<-NULL
pHdata<-unique(pHdata)


#Add objective and alias back to data using Analyte, BU, Unit, and objective text to join back on
pHdata<-tbl_df(merge(pHdata,Analytes,by=c("AnalyteName","BeneficialUse","UnitName"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))

#Determine if there was an exceedance for each day by including SampleDate in the counting feature
#so that each day will have a max value of one

pHdata$Objective<-as.numeric(pHdata$Objective)
pHdataOver<-pHdata[pHdata$Alias=="High",]
pHdataOver<-as.data.table(pHdataOver)[,sum(Result>Objective),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,StationCode
,Waterbody,WBID,SampleDate,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataOver)[names(pHdataOver)=='V1']<-"Exceedances"
pHdataOver<-tbl_df(pHdataOver)

pHdata$Objective<-as.numeric(pHdata$Objective)
pHdataUnder<-pHdata[pHdata$Alias=="Low",]
pHdataUnder<-as.data.table(pHdataUnder)[,sum(Result<Objective),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,StationCode
,Waterbody,WBID,SampleDate,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataUnder)[names(pHdataUnder)=='V1']<-"Exceedances"
pHdataUnder<-tbl_df(pHdataUnder)


#Combine the two tables together with rbind then coutn weather or not there was an exceedance
pHdataExceedances<-rbind.data.frame(pHdataOver,pHdataUnder)

#Take max value. Each station day will be either a zero or a one
pHdataExceedances<-as.data.table(pHdataExceedances)[,max(Exceedances),list(AnalyteName,BeneficialUse
,ProjectName,MatrixName,FractionName,StationCode
,Waterbody,WBID,SampleDate,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataExceedances)[names(pHdataExceedances)=='V1']<-"Exceedances"
pHdataExceedances<-tbl_df(pHdataExceedances)


#Then finally count up exceedances for the waterbody by removing sample date
pHdataAllExceedances<-as.data.table(pHdataExceedances)[,sum(Exceedances),list(AnalyteName,BeneficialUse
,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataAllExceedances)[names(pHdataAllExceedances)=='V1']<-"Exceedances"
pHdataAllExceedances<-tbl_df(pHdataAllExceedances)

#Count up the number of pH samples for each waterbody independent of the objective

pHdata$Objective<-as.numeric(pHdata$Objective)
pHdataTotal<-pHdata[pHdata$Alias=="Low",]
pHdataTotal<-as.data.table(pHdataTotal)[,sum(Result>=0),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(pHdataTotal)[names(pHdataTotal)=='V1']<-"Total"
pHdataTotal<-tbl_df(pHdataTotal)


#Combine samples and exceedances together for pH data
pHResults<-tbl_df(merge(pHdataTotal,pHdataAllExceedances,by=c("AnalyteName","BeneficialUse","ProjectName"
,"MatrixName","FractionName","Waterbody","WBID","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number")))

#Change Sample number to zero in Region 6 waterbodies
pHResults$Exceedances[which(substr(pHResults$WBID,4,4)=="6")]<-0


##########################################################################################################################################
######################################____End pH sample count_______####################################################################
###########################################################################################################################################


###############################################################################################################################################
################################___________________Begin Chromium assessment______________________________#####################################
#############################################################################################################################################

ChromiumData<-W_Objectives[which(W_Objectives$AnalyteName=="Chromium"&(W_Objectives$BeneficialUse=="CO"|W_Objectives$BeneficialUse=="WA")),]

#Add Chromim VI to this data
ChromiumData$VIObjective<-11
CleanChromium<-tbl_df(as.data.table(ChromiumData)[,sum((Result<Objective)&(Result<VIObjective)),list(AnalyteName
,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)])
names(CleanChromium)[names(CleanChromium)=="V1"]<-"Total"
CleanChromium$Exceedances<-0
Chromiumresults<-tbl_df(CleanChromium)

TossedChromium<-as.data.table(ChromiumData)[,sum((Result>Objective)&(Result>VIObjective)),list(AnalyteName
,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
TossedChromium<-tbl_df(TossedChromium)

TossedChromium2<-as.data.table(ChromiumData)[,sum((Result<Objective)&(Result>VIObjective)),list(AnalyteName
,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
TossedChromium2<-tbl_df(TossedChromium2)

TossedChromium3<-rbind(TossedChromium,TossedChromium2)

TossedChromium4<-as.data.table(TossedChromium3)[,sum(V1),list(AnalyteName,BeneficialUse,ProjectName
,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
TossedChromium<-tbl_df(TossedChromium4)
names(TossedChromium4)[names(TossedChromium4)=="V1"]<-"Samples"


#Remove chromium data from main table
W_Objectives<-filter(W_Objectives, !(AnalyteName=="Chromium"&(BeneficialUse=="CO"|BeneficialUse=="WA")))


###############################______________End_Chromium_Assessment____________##############################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################



#Remove DO and pH data from main data frame
W_Objectives<-filter(W_Objectives, !(AnalyteName=="Oxygen, Dissolved" |AnalyteName=="Oxygen, Saturation"|AnalyteName=="pH"|AnalyteName=="Alkalinity as CaCO3"))



#############################################################################################################################################
#############################################################################################################################################
##################_______________________Start Exceedance Count and LOE Writing Processes_______________________#############################


W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
Over<-as.data.table(W_Objectives)[,sum(Result>Objective),list(AnalyteName,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Over)[names(Over)=='V1']<-"Exceedances"
Over<-tbl_df(Over)

W_Objectives$Objective<-as.numeric(W_Objectives$Objective)
Total<-as.data.table(W_Objectives)[,sum(Result>=0),list(AnalyteName,BeneficialUse,ProjectName,MatrixName,FractionName
,Waterbody,WBID,Objective_Language,Evaluation_Guideline,Objective_Ref_Number,Eval_Ref_Number)]
names(Total)[names(Total)=='V1']<-"Total"
Total<-tbl_df(Total)


# Combine the two toegether
results<-merge(Over,Total,by=c("AnalyteName","BeneficialUse","ProjectName","MatrixName","FractionName"
,"Waterbody","WBID","Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
results<-tbl_df(results)

#add the DO and pH samples and Exceedances together with rbind.data.frame
MoreResults<-results
MoreResults<-rbind.data.frame(MoreResults,DOresults)
MoreResults<-rbind.data.frame(MoreResults,pHResults)
MoreResults<-rbind.data.frame(MoreResults,Chromiumresults)

W_Objectives_for_date_Range$SampleDate<-as.Date(W_Objectives_for_date_Range$SampleDate, "%m/%d/%Y")
Max_Date<-as.data.table(W_Objectives_for_date_Range)[,max(SampleDate),list(AnalyteName,BeneficialUse
,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
names(Max_Date)[names(Max_Date)=='V1']<-"MaxDate"
Max_Date<-tbl_df(Max_Date)
Min_Date<-as.data.table(W_Objectives_for_date_Range)[,min(SampleDate),list(AnalyteName,BeneficialUse
,ProjectName,MatrixName,FractionName,Waterbody,WBID,Objective_Language,Evaluation_Guideline
,Objective_Ref_Number,Eval_Ref_Number)]
names(Min_Date)[names(Min_Date)=='V1']<-"MinDate"
Min_Date<-tbl_df(Min_Date)
Date_Range<-tbl_df(merge(Max_Date,Min_Date,by=c("AnalyteName","Waterbody","WBID","ProjectName"
,"MatrixName","FractionName","BeneficialUse"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))


# Combine results with max a min date range
R_W_Dates<-merge(MoreResults,Date_Range,by=c("Waterbody","WBID","AnalyteName","FractionName"
,"MatrixName","BeneficialUse","ProjectName","Objective_Language"
,"Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number"))
R_W_Dates<-tbl_df(R_W_Dates)



#Create table of LOEs where all data was thrown out from quantitation issues then add necessary fields to write LOEs
ZeroOfZero<-anti_join(Quantitation_Discards,R_W_Dates,by=c("Waterbody","WBID","AnalyteName"
,"FractionName","MatrixName","BeneficialUse","ProjectName","Objective_Language","Evaluation_Guideline"
,"Objective_Ref_Number","Eval_Ref_Number"))
ZeroOfZero$Total<-0
ZeroOfZero$Total<-as.integer(ZeroOfZero$Total)
ZeroOfZero$Exceedances<-0
ZeroOfZero$Exceedances<-as.integer(ZeroOfZero$Exceedances)

#Add temporal rep field to the zero of zero LOEs
ZeroOfZero$TEMPORAL_REP<-paste0("Date for this waterbody was collected over the date range ", ZeroOfZero$MinDate, " to ",ZeroOfZero$MaxDate)

#Add station information to the zero of zeros
ZeroOfZero<-tbl_df(merge(ZeroOfZero,Stations))

#Add the station information to the field SpatialRep with some generic text
ZeroOfZero$SPATIAL_REP<-paste0("Data was collected from ",ZeroOfZero$StationCount," station(s) (Station Code(s) ",ZeroOfZero$StationCode,").")
ZeroOfZero<-tbl_df(ZeroOfZero)

ZeroOfZero<-tbl_df(mutate(ZeroOfZero,TotalSamples=Total+TossedSampleCount))

#Keep this comman on a single line to avoid LOE export issues
ZeroOfZero$DATA_USED<-paste0("Water Board staff assessed ",ZeroOfZero$ProjectName," data for ",ZeroOfZero$Waterbody," to determine beneficial use support, and the results are as follows: ",ZeroOfZero$Exceedances, " of the ",ZeroOfZero$Total," samples exceeded the evaluation guideline for ",ZeroOfZero$AnalyteName,". Although a total of ",ZeroOfZero$TotalSamples," samples were collected, ", ZeroOfZero$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")


#Add the information from the tossed samples but remove max and min dates first
Quantitation_Discards_No_Range<-Quantitation_Discards
Quantitation_Discards_No_Range$MaxDate<-NULL
Quantitation_Discards_No_Range$MinDate<-NULL



Results_W_Date_Range_QD<-tbl_df(merge(R_W_Dates,Quantitation_Discards_No_Range,by=c("AnalyteName"
,"Waterbody","WBID","ProjectName","MatrixName","FractionName","BeneficialUse"
,"Objective_Language","Evaluation_Guideline","Objective_Ref_Number","Eval_Ref_Number")))

Results_W_Stations_Date_Range_QD<-tbl_df(merge(Results_W_Date_Range_QD,Stations))

#Add TotalSample Field
Results_W_Stations_Date_Range_QD<-tbl_df(mutate(Results_W_Stations_Date_Range_QD,TotalSamples=Total+TossedSampleCount))

	#Add the Data used field using the quantitation discarded samples
	#Keep this command on ONE line or it will cause issues with the LOE export
Results_W_Stations_Date_Range_QD$DATA_USED<-paste0("Water Board staff assessed ",Results_W_Stations_Date_Range_QD$ProjectName," data for ",Results_W_Stations_Date_Range_QD$Waterbody," to determine beneficial use support and the results are as follows: ",Results_W_Stations_Date_Range_QD$Exceedances, " of the ",Results_W_Stations_Date_Range_QD$Total," samples exceeded the water quality standard for ",Results_W_Stations_Date_Range_QD$AnalyteName,". Although a total of ",Results_W_Stations_Date_Range_QD$TotalSamples," samples were collected, ", Results_W_Stations_Date_Range_QD$TossedSampleCount," of these samples were not included in the assessment because the laboratory data reporting limit(s) was above the objective and therefore the results could not be quantified with the level of certainty required by the Listing Policy Section 6.1.5.5.")
	#Add Station information to the table to creat Spatial Rep field
Results_W_Stations_Date_Range_QD$SPATIAL_REP<-paste0("Data was collected from ",Results_W_Stations_Date_Range_QD$StationCount," station(s): (Station Codes ",Results_W_Stations_Date_Range_QD$StationCode,").")
	#Add temporal rep to table so you can join with zero of zero table
Results_W_Stations_Date_Range_QD$TEMPORAL_REP<-paste0("Date for this waterbody was collected over the date range ", Results_W_Stations_Date_Range_QD$MinDate, " to ",Results_W_Stations_Date_Range_QD$MaxDate)


#Add Zero of Zero and Results with QD together
ZeroOfZero<-tbl_df(rbind.data.frame(Results_W_Stations_Date_Range_QD,ZeroOfZero))

#Remove tossedsamplecount field
ZeroOfZero$TossedSampleCount<-NULL
ZeroOfZero$TotalSamples<-NULL
ZeroOfZero$MaxDate<-NULL
ZeroOfZero$MinDate<-NULL
ZeroOfZero<-tbl_df(ZeroOfZero)

#Remove the rows of data from main data frame that had data with quantitation issues
R_W_Language<-anti_join(R_W_Dates,Quantitation_Discards,by=c("Waterbody","WBID","AnalyteName","FractionName"
,"MatrixName","BeneficialUse","ProjectName","Objective_Language","Evaluation_Guideline","Objective_Ref_Number"))

#Merge list of stations with the LOEs
R_W_Language$DATA_USED<-paste0("Water Board staff assessed ",R_W_Language$ProjectName," data for ",R_W_Language$Waterbody," to determine beneficial use support and results are as follows: ", R_W_Language$Exceedances," of ",R_W_Language$Total," samples exceeded the water quality standard for ",R_W_Language$AnalyteName,".")

R_W_Language["TEMPORAL_REP"]<-NA
R_W_Language$TEMPORAL_REP<-paste0("The samples were collected between the dates of ", R_W_Language$MinDate, " and ", R_W_Language$MaxDate)

#Add station infomrnation to R_W_Language
R_W_Language<-merge(R_W_Language,Stations,by=c("AnalyteName","Waterbody","WBID","ProjectName"
,"MatrixName","FractionName"))

R_W_Language<-tbl_df(R_W_Language)
R_W_Language$SPATIAL_REP<-paste0("The samples were collected at ",R_W_Language$StationCount
," monitoring site(s) (",R_W_Language$StationCode,")")

R_W_Language$MinDate<-NULL
R_W_Language$MaxDate<-NULL
R_W_Language$Alias<-NULL


R_W_Language<-rbind.data.frame(R_W_Language,ZeroOfZero)
R_W_Language$StationCount<-NULL
R_W_Language$StationCode<-NULL


#Merge with the DataUsed Ref Number and QA ref number data then remove project code from table
R_W_Language<-merge(R_W_Language,DataReferences,by="ProjectName")
R_W_Language<-tbl_df(R_W_Language)
R_W_Language$ProjectName<-NULL

#Change Sample number to zero in Region 6 waterbodies
R_W_Language$Total[which(substr(R_W_Language$WBID,4,4)=="6"&R_W_Language$AnalyteName=="pH")]<-0
R_W_Language$DATA_USED[which(substr(R_W_Language$WBID,4,4)=="6"&R_W_Language$AnalyteName=="pH")]<-paste0(R_W_Language$DATA_USED," The objective for this pollutant requires background information that is currently unavailable, and therefore an assessment of water quality standards could not be made.")


#Change column names to match formatting required by CalWQA upload tool
colnames(R_W_Language)<-c("POLLUTANT","WB_SEGMENT","WBID","MATRIX","FRACTION","BU_CODE"
,"CRITERION/OBJECTIVE","EVAL_GUIDELINE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE_REFERENCES","EXCEEDANCE_COUNT","SAMPLE_COUNT","DATA_USED","TEMPORAL_REP"
,"SPATIAL_REP","DATA_USED_REFERENCES","QA_INFO_REFERENCES","QA_INFO","DATA_SOURCE")


 
R_W_Language<-tbl_df(R_W_Language)
R_W_Language$ASSESSMENT_STATUS<-"LOE In Progress"
R_W_Language$DATE_CREATED<-Sys.Date()

R_W_Language$MIGRATION_ID<-rownames(R_W_Language)

############################################
 ###Enter Region number and Username here#
      #############################
		  ###############
			#######
			  ###
			   #

	R_W_Language$REGION<-Region
	R_W_Language$AUTHOR<-Author


#########################################
###########################################
R_W_Language$MATRIX<-"Water"
R_W_Language$ENVIRONMENTAL_CONDITIONS<-""
R_W_Language$ASSESSOR_COMMENT<-""
R_W_Language$DATA_TYPE<-"PHYSICAL/CHEMICAL MONITORING"
R_W_Language$SUB_GROUP<-"Pollutant-Water"
#Reorder the rows of the LOEs so that it matches the template


#R_W_Language<-R_W_Language[,c(21,22,2,3,1,28,4,5,6,7,9,8,10,12,11,13,14,15
#,16,17,18,19,20,23,24,25,26,27)]





R_W_Language

#___________________________________________________________________________________________________________#

#Create unique list of data that was exported (and not assessed) by ReLEP, count the number of errors within each row of data that caused export
#and create a comma delimited list of these errors for review by getting a unique list of all fields in the data EXCEPT for the "Issue" field
AllExportedData<-AllExportedData%>%group_by(WQID,ProgramCode,ProgramName,ParentProjectCode,ParentProjectName,ProjectCode,ProjectName,ProjectDescr
,QAPPCode,QAPPName,PublicRelease,SampleDate,StationCode,StationName,StationDescr,TargetLatitude,TargetLongitude,Datum,RegionalBoardID
,WaterBodyType,SampleAgency,SampleAgencyCode,SampleComments,CollectionTime,PositionWaterColumn,LabCollectionComments,CollectionMethodName
,SampleTypeCode,SampleTypeName,Replicate,CollectionDeviceCode,CollectionDeviceName,CollectionDepth,UnitCollectionDepth,LabAgencyCode,LabAgencyName
,SubmittingAgency,LabSubmissionCode,LabBatchComments,MatrixCode,MatrixName,MethodCode,MethodName,AnalyteCode,AnalyteName,FractionCode,FractionName
,UnitCode,UnitName,MDL,RL,QACode,LabBatch,AnalysisDate,LabReplicate,Result,ResQualCode,LabResultComments,SampleLatitiude,SampleLongitude,SampleDatum
,SampleElevation,SampleUnitElevation,DataQuality,DataQualityIndicator,FieldWater)%>%summarise(count=n(),Issue=paste(Issue,collapse=", AND "))

AssessorsComments<-ungroup(AllExportedData)
AssessorsComments<-select(AssessorsComments,AnalyteName,FractionName,ProjectName,StationCode,MatrixName,Result,Issue,count,SampleDate)
AssessorsComments<-tbl_df(merge(AssessorsComments,Waterbodies))
AssessorsComments<-tbl_df(merge(AssessorsComments,DataReferences))
AssessorsComments<-AssessorsComments%>%group_by(AnalyteName,FractionName,Issue,Waterbody,WBID,BeneficialUse,MatrixName,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO)%>%summarise(count=n())
AssessorsComments<-ungroup(AssessorsComments)
colnames(AssessorsComments)<-c("POLLUTANT","FRACTION","Issue","WB_SEGMENT","WBID","BU_CODE","MATRIX","DATA_USED_REFERENCES","QA_INFO_REFERENCES","QA_INFO","count")
AssessorsComments$Reasons<-paste0(AssessorsComments$count," samples were thrown out because they had ",AssessorsComments$Issue)
AssessorsComments$Issue<-NULL
AssessorsComments$count<-NULL
AssessorsComments$MATRIX<-"Water"
AssessorsComments<-AssessorsComments%>%group_by(POLLUTANT,FRACTION,WB_SEGMENT,WBID,BU_CODE,MATRIX,DATA_USED_REFERENCES,QA_INFO_REFERENCES,QA_INFO)%>%summarise(Reasons=paste(Reasons,collapse=", "))



R_W_Language<-tbl_df(merge(R_W_Language,AssessorsComments,all.x=TRUE))
R_W_Language$ASSESSOR_COMMENT<-paste0(R_W_Language$Reasons)
R_W_Language$Reasons<-NULL

R_W_Language<-tbl_df(merge(R_W_Language,ReLEP_to_CalWQA_Lookup,all.x=TRUE))
R_W_Language$POLLUTANT<-NULL
names(R_W_Language)[names(R_W_Language)=="PollutantName_in_CalWQA"]<-"POLLUTANT"

R_W_Language$AQ_USE_CODE<-""
R_W_Language$UPDATED_BY<-""
R_W_Language$DATE_UPDATED<-""


#Re-Order Columns to match LOE uploader template
R_W_Language<-R_W_Language[,c("MIGRATION_ID","REGION","WB_SEGMENT","WBID","POLLUTANT"
,"SUB_GROUP","MATRIX","FRACTION","BU_CODE","AQ_USE_CODE","CRITERION/OBJECTIVE","CRITERION/OBJECTIVE_REFERENCES"
,"EVAL_GUIDELINE","EVAL_GUIDELINE_REFERENCES","SAMPLE_COUNT","EXCEEDANCE_COUNT","SPATIAL_REP","TEMPORAL_REP","QA_INFO"
,"QA_INFO_REFERENCES","DATA_TYPE","DATA_USED","DATA_USED_REFERENCES","ASSESSMENT_STATUS","DATA_SOURCE"
,"AUTHOR","DATE_CREATED","UPDATED_BY","DATE_UPDATED","ENVIRONMENTAL_CONDITIONS","ASSESSOR_COMMENT")]

#Flag LOEs as electornic generated LOEs
R_W_Language$ASSESSOR_COMMENT[R_W_Language$ASSESSOR_COMMENT=="NA"]<-""
R_W_Language$ASSESSOR_COMMENT<-paste0("(LOE written by ReLEP ", ReLEP_Version,") ", R_W_Language$ASSESSOR_COMMENT)


#Create file name for LOEs based on author, date and matrix
FileName<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_Water_LOEs_",FileNameInQuotations)
write.table(R_W_Language, FileName,sep="\t",row.names=FALSE)

ExportedDataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_AllTossed_Water_Data_",FileNameInQuotations)
write.table(AllExportedData,ExportedDataFileName,sep="\t",row.names=FALSE)

SSODataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_Data_W_SiteSpecific_Objectives_",FileNameInQuotations)
write.table(W_SSOs_for_Export,SSODataFileName,sep="\t",row.names=FALSE)


######################################################################################################################

#Create file name for LOEs based on author, date and matrix
FileName<-paste0("Outputs\\LOEs\\",Author,"_",Sys.Date(),"_LOEs_",FileNameInQuotations)


ExportedDataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_AllTossedData_",FileNameInQuotations)
write.table(AllExportedData,ExportedDataFileName,sep="\t",row.names=FALSE)

SSODataFileName<-paste0("Outputs\\",Author,"_",Sys.Date(),"_Data_W_SiteSpecific_Objectives_",FileNameInQuotations)
write.table(W_SSOs_for_Export,SSODataFileName,sep="\t",row.names=FALSE)


Program_Run_Time<-Sys.time()-Start_Time
Program_Run_Time<-paste0("ReLEP took ",Program_Run_Time," minutes to write ",nrow(R_W_Language)," Line(s) of Evidence (LOE's)")
Program_Run_Time


