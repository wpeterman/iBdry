###############################################################
# Source R code to execute batch analysis of iButtons
#                                                             
# Written by Bill Peterman                                    
# July 2013                                                     
#
##############################################################

# You should not have to modify any lines in this file
# Open and modify the necessary lines of code in the "Execute_iButton.R"

# Check for, and install necessary R packages
libs=c("zoo","plyr")
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

# List all iButton files
# setwd(FOLDER)
ibut.files<-list.files(path=FOLDER,pattern = "\\.csv$",full.names=TRUE) # Make list of all files with '.csv' extension
ibut.names<-list.files(path=FOLDER,pattern = "\\.csv$") 
Day.O<-as.Date(Day.Out, "%m/%d/%y")
Day.I<-as.Date(Day.In,"%m/%d/%y")
ibut.dry_fill<-data.frame()

if(FOLDER==OUT.DIR) stop("Results cannot be stored in the same folder as raw iButton.csv files!!! Please change 'OUT.DIR' to a new folder and rerun")

# Clear output directories
invisible(suppressWarnings(do.call(file.remove,list(list.files(OUT.DIR,full.names=TRUE,recursive=TRUE )))))
cnt=0 # Counter
for (i in ibut.files){
  cnt=cnt+1
  IBUTTON<-read.csv(i, skip=14,stringsAsFactors=FALSE)
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
  
  # Identify positions of changes
  Change<-abs(rollapply(Move.DryDate,width=2,FUN=function(x) (diff(x)),align="left"))
  Change[1:(WINDOW-1+c.Days-1)]<-NA # The first values are NA b/c of the moving window analyses used
  SHIFT<-c.Days-1
#   Total.Changes <-count(Change)$freq[2]
#   Total.Changes <-1:Total.Changes %% 2
#   Shift.Change <-ifelse(Total.Changes==1,-SHIFT,SHIFT)
  Change.Pos <- (which(Change==1)+1)-SHIFT # Add 1  b/c the actual change point is 1 greater than value
#   Change.Pos <- Change.Pos + Shift.Change 

  EVENTS <- length(Change.Pos)
	Dry_Fill<-data.frame()
  initial.state<-INITIAL(Move.DryDate[WINDOW])
  SITE<-NAME<-gsub(".csv","",x=ibut.names[cnt])
  SITE<-cbind(SITE,initial.state)
  

##############################
 Dry_Fill<- {if(EVENTS==0){
{pdf(paste(PLOTS,NAME,".pdf",sep=""))
 par(mfrow=c(2,1))
 plot(IBUTTON.v$Date,IBUTTON.v$Value,xlab="Date",ylab="Daily variance",main=paste(NAME," assessed using ",METHOD,"; ", c.Days, " consecutive days",sep=""),las=1); abline(h=VAR, lty=2, col='red')
 plot(IBUTTON.v$Date,IBUTTON.v$var,xlab="Date",ylab=paste(WINDOW,"-day ", METHOD, " of daily variance",sep=""),main=paste(NAME," assessed using ",METHOD,"; ", c.Days, " consecutive days",sep=""),las=1); abline(h=VAR, lty=2, col='red')
  dev.off()
}
Dry_Fill<-rbind.fill(Dry_Fill,as.data.frame(SITE))
  } else {
    for (j in 1:EVENTS){
      Event<-as.character(IBUTTON.v$Date[Change.Pos[j]])
      Event.num<-paste("Event",j,sep="")
      SITE<-cbind(SITE,Event)
      colnames(SITE)[j+2]<-Event.num      
    }
    
    DATES<-as.list(as.Date(SITE[,c(-1,-2)]))
    
{ pdf(paste(PLOTS,NAME,".pdf",sep=""))
  par(mfrow=c(2,1))
  plot(IBUTTON.v$Date,IBUTTON.v$Value,xlab="Date",ylab="Daily variance",main=paste(NAME," assessed using ",METHOD,"; ", c.Days, " consecutive days",sep=""),las=1);abline(v=DATES);abline(h=VAR, lty=2, col='red')
  plot(IBUTTON.v$Date,IBUTTON.v$var,xlab="Date",ylab=paste(WINDOW,"-day ", METHOD, " of daily variance",sep=""),main=paste(NAME," assessed using ",METHOD,"; ", c.Days, " consecutive days",sep=""),las=1);abline(v=DATES);abline(h=VAR, lty=2, col='red')  
  dev.off()
    }
    Dry_Fill<-rbind.fill(Dry_Fill,as.data.frame(SITE))
  }
  }
##############################
  
  ibut.dry_fill<-rbind.fill(ibut.dry_fill,Dry_Fill)
  

  Export.File<-paste(OUT.DIR,NAME,".csv",sep="")
  write.table(x=IBUTTON,Export.File,sep=",",row.names=F,col.names=T)
    
}

CSV.files <- list.files(OUT.DIR,pattern="*.csv",full.names=TRUE)
CSV.names <- gsub(pattern="*.csv","",((list.files(OUT.DIR,pattern="*.csv"))))

COMBINED <-list()
for(i in 1:length(CSV.names)){
  CSV <- read.csv(CSV.files[i])
  iButton <- rep(CSV.names[i],times=nrow(CSV))
  CSV <- cbind(iButton,CSV)
  COMBINED[[i]] <- CSV
}

All_iButtons_Combined<-as.data.frame(do.call(rbind,COMBINED))
write.table(x=ibut.dry_fill,paste(OUT.DIR,"Dry_Fill_Events.csv",sep=""),sep=",",row.names=F,col.names=T)
par(mfrow=c(1,1))
write.table(All_iButtons_Combined,file=paste0(OUT.DIR,"All_iButtons_Combined.csv"),sep=",",row.names=F,col.names=T)
