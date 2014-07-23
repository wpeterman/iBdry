###############################################################
# Source R code to optimize analysis parameters for batch
# processing iButton files
#                                                             
# Written by Bill Peterman                                    
# November 2013                                                     
#
##############################################################

# You should not have to modify any lines in this file
# Open and modify the necessary lines of code in the "Execute_iButtonOptimization.R" file

# Check for, and install necessary R packages
libs=c("zoo","plyr","caret")
type=getOption("pkgType")                           
CheckInstallPackage <- function(packages, repos="http://cran.r-project.org") {
  installed=as.data.frame(installed.packages())
  for(p in packages) {
    if(is.na(charmatch(p, installed[,1]))) { 
      install.packages(p, repos=repos) 
    }
  }
} 
CheckInstallPackage(packages=libs)
###
INITIAL<-function(initial.state){if(Move.DryDate[1]==1){"Dry"} 
																 else {"Filled"}
}


require(zoo)
require(plyr)
require(caret)

# List all iButton files
# setwd(FOLDER)
ibut.files<-list.files(path=FOLDER,pattern = "\\.csv$") # Make list of all files with '.csv' extension
# ibut.names<-list.files(path=FOLDER,pattern = "\\.csv$") 
Day.O<-as.Date(Day.Out, "%m/%d/%y")
Day.I<-as.Date(Day.In,"%m/%d/%y")
ibut.dry_fill<-data.frame()

method<-c("mean","var")

EXPAND <- arrange(expand.grid(method=method,cd=cd, variance=variance,window=window),method)
EXPAND$Comb<-paste0(EXPAND$method,"_V",EXPAND$variance,"_C", EXPAND$cd,"_W",EXPAND$window)
COMBOS<-EXPAND$Comb
c.list<-as.list(COMBOS)
Comb.List<-NULL
DATE<-seq.Date(from=Day.O,to=Day.I,by=1)

r <-0 # This is a counter for the for loop
r2 <- 0

  for (m in method){
    for (c in cd){
      for (v in variance){
        for (w in window){
      VAR=v
      METHOD=m
      WINDOW=w
      c.Days=c
      
      POND.RESULTS <-data.frame()
         
      for (i in ibut.files){
        IBUTTON<-read.csv(paste0(FOLDER,i), skip=14,stringsAsFactors=FALSE)
        times = as.POSIXct(strptime(substr(IBUTTON$Date.Time,1,20),format = '%m/%d/%y %I:%M:%S %p'))
        Time <- as.character(substr(times,12,19))
        Date <- as.Date(substr(times,1,10))
        IBUTTON$Time <- Time
        IBUTTON$Date <- Date
        IBUTTON.s <- IBUTTON[which(IBUTTON$Date>=Day.O & IBUTTON$Date<=Day.I),]
        IBUTTON.v <- aggregate(Value~Date,FUN=var,data=IBUTTON.s)
        NA.array<-matrix(NA,nrow=(WINDOW-1))
        

  Move.var<-as.matrix(abs(rollapply(IBUTTON.v$Value,width=WINDOW,FUN=METHOD,align="left")))
  Move.var<-rbind(NA.array,Move.var)
  IBUTTON.v$var<-Move.var

  IBUTTON.v$Dry<-as.numeric(IBUTTON.v$var>=VAR) # Identify dry events with a "1"
  Move.DryDate<-as.numeric(rollapply(IBUTTON.v$Dry,width=c.Days,FUN=function(x) (sum(x)==c.Days),align="left"))  
  VALUES<-rep(x=Move.DryDate[WINDOW],times=c.Days-1)
  Move.DryDate[is.na(Move.DryDate)]<-Move.DryDate[WINDOW]
  Move.DryDate <- append(Move.DryDate,values=VALUES,after=1)
  
  
  COMB<-paste0(m,"_V",v,"_C",c, "_W",w)
  D_F<-as.matrix(Move.DryDate)
  colnames(D_F)<-COMB
  DATE<-as.Date(IBUTTON.v$Date)
  SITE<-NAME<-gsub(".csv","",x=i)  
  POND<-rep(x=SITE,times=length(DATE))
  Date.DF<-cbind.data.frame(DATE,POND,D_F)
  
  POND.RESULTS<-rbind(POND.RESULTS,Date.DF)
  POND.RESULTS[,3][POND.RESULTS[,3]==0]<-"Wet"
  POND.RESULTS[,3][POND.RESULTS[,3]==1]<-"Dry"
  r2=r2+1
  cat(paste0("Iteration ",r2,"/",length(c.list)*length(ibut.files),"--->","Working on ",SITE,"; combination ", m,"_c",c,"_var",v,"_w",w, "\n")) 
         } # Original loop
            r=r+1 # Add to counter to progress to next combination
            Comb.List[[r]]<-POND.RESULTS  # Store results
      }
    }
  }
}
names(Comb.List)<-COMBOS
DF.Event<-lapply(Comb.List, `[`, 3)
MERGED.DATA<-do.call(cbind,DF.Event)
DATE<-Comb.List[[1]][c(1,2)]
MERGED.DATA<-cbind.data.frame(DATE,MERGED.DATA)
MERGED.DATA$ID<-paste0(MERGED.DATA$POND,MERGED.DATA$DATE)


# write.table(x=MERGED.DATA,paste(OUT.DIR,"Dry_Fill_STATUS_ALL.csv",sep=""),sep=",",row.names=F,col.names=T)

# Work on Validation data
VALIDATION<-read.csv(VALIDATION,header=T)
VALIDATION[,2]=as.Date(VALIDATION[,2],"%m/%d/%Y")
VALIDATION$ID<-paste0(VALIDATION[,1],VALIDATION[,2])

Join.Tables<-join(VALIDATION,MERGED.DATA,"ID")

# write.table(x=Join.Tables,paste(OUT.DIR,"JOINED_VALIDATION.csv",sep=""),sep=",",row.names=F,col.names=T)

MAX<-7+length(Comb.List)-1
DATA<-Join.Tables[complete.cases(Join.Tables[7]),]
# DATA$Depth[which(DATA$Depth==0)]<-"Dry"
# DATA$Depth[which(DATA$Depth!="Dry")]<-"Wet"
# DATA[,7:MAX][DATA[,7:MAX]==0]<-"Wet"
# DATA[,7:MAX][DATA[,7:MAX]==1]<-"Dry"

#####################
# Sensitivity / Specificty

Test.Comb<-DATA[,c(1,3,7:MAX)]
col<-3:(3+length(Comb.List)-1)
RESULTS<-matrix(nrow=(length(col)),ncol=3)
colnames(RESULTS)<-c("Combination","Sensitivity","Specificity")
r=0

  for(c in col){
    r=r+1
    Sens<-sensitivity(data=as.factor(Test.Comb[,c]),reference=as.factor(Test.Comb[,2]),positive="Dry")
    Spec<-specificity(data=as.factor(Test.Comb[,c]),reference=as.factor(Test.Comb[,2]),negative="Wet")
    COMB<-colnames(Test.Comb[c])
    RESULTS[r,]<-cbind(COMB,Sens,Spec)
  }

RESULTS.all<-as.data.frame(RESULTS)
RESULTS.all$Specificity<-as.numeric(levels(RESULTS.all$Specificity))[RESULTS.all$Specificity]
RESULTS.all$Sensitivity<-as.numeric(levels(RESULTS.all$Sensitivity))[RESULTS.all$Sensitivity]

RESULTS.all$RANK<-RESULTS.all$Specificity+RESULTS.all$Sensitivity
RESULTS.all<-RESULTS.all[order(RESULTS.all[,'RANK'],decreasing=T),]
RESULTS.all$RANK<-1:(nrow(RESULTS.all))

write.table(x=RESULTS.all,file=paste(OUT.DIR,"Sensitivity_Specificity_OptimizationResults.csv"),sep=",",col.names=T,row.names=F)

