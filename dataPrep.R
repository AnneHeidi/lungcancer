##############################
### Samler dataene fra Espen (spss og xls) 
### for så å sammenligne disse med mine gamle data 
### på pasientdataene 
##############################


## DATA
spss <- read.table("/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/NLCB_2016_spss.dat", sep = "\t", header =TRUE, quote = "", stringsAsFactors = FALSE)
mb <- read.table("/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/myblock.txt", header=TRUE, stringsAsFactors = FALSE)
xls <- read.table("/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/NLCB_2016_R2.txt", stringsAsFactors = FALSE, header = TRUE)
########

## SPSS: 
count <- c(1:445)
spss$ny <- count
nr <- rep("401_0000", 9)
nr2 <- rep("401_000", 91)
nr3 <- rep("401_00", 345)
n <- c(nr, nr2, nr3)
spss$ny2 <- n
spss$sampleID <- paste(spss$ny2, spss$ny, sep="")
colnames(spss)[6] <- "Tumor"
colnames(spss)[7] <- "Nodes"
colnames(spss)[8] <- "Metastasis"
#flytter om på rekkefølgen og fjerner overflødige kolonner som ny2 og EMA og EMB
nyS <- data.frame(spss$sampleID, spss$histology, spss$diagnosis, spss$Stadium, spss$Tumor, spss$Nodes, 
	spss$Metastasis, spss$nonLC, spss$smoking_status, spss$smoke_start, spss$smoke_stop, spss$duration, 
	spss$pack_year, spss$pack_year_lvl, spss$no_of_cigarettes, spss$cig_day, stringsAsFactors = FALSE)
colnames(nyS) <- sub(".....", "", colnames(nyS))
nyS <- nyS[order(nyS$sampleID),]
########


## XLS:
colnames(xls)[4] <- "inclusion"
xls$sampleID <- gsub("-", "_", xls$sampleID)
nyX <- data.frame(xls$sampleID, xls$sex, xls$born, xls$inclusion, xls$Dato, xls$comment, stringsAsFactors =FALSE)
colnames(nyX) <- sub("....", "", colnames(nyX))
colnames(nyX)[5] <- "died"
colnames(nyX)[2] <- "gender"

nyX <- nyX[order(nyX$sampleID),]

nyX$born <- gsub("\\.", "/", nyX$born)
nyX$inclusion <- gsub("\\.", "/", nyX$inclusion)
nyX$died <- gsub("\\.", "/", nyX$died)

nyX$born <- as.Date(nyX$born, format="%d/%m/%Y", origin = "1900-01-01")
nyX$inclusion <- as.Date(nyX$inclusion, format="%d/%m/%Y", origin = "1900-01-01")
nyX$died <- as.Date(nyX$died, format="%d/%m/%Y", origin = "1900-01-01")
########

## MB:
nlcb <- mb[mb$biobank == "NLCB",]
colnames(nlcb)[1] <- "sampleID"

nyN <- unique(data.frame(nlcb$sampleID, nlcb$gender, nlcb$birth, nlcb$inclusion_date, nlcb$death, nlcb$age, 
	nlcb$smoking_status, nlcb$pack_year, nlcb$no_of_cigarettes, nlcb$duration, nlcb$histology, nlcb$Stadium,  stringsAsFactors = FALSE))
colnames(nyN) <- sub(".....", "", colnames(nyN))
nyN <- nyN[order(nyN$sampleID),]

colnames(nyN)[3] <- "born"
colnames(nyN)[4] <- "inclusion"
colnames(nyN)[5] <- "died"

nyN$born <- as.Date(nyN$born, format="%m/%d/%Y", origin = "1900-01-01")
nyN$inclusion <- as.Date(nyN$inclusion, format="%m/%d/%Y", origin = "1900-01-01")
nyN$died <- as.Date(nyN$died)
nyN$pack_year <- gsub(",", ".", nyN$pack_year)
nyN$pack_year <- as.numeric(nyN$pack_year)
nyN$Stadium <- gsub("i", "I", nyN$Stadium)
nyN$histology <- gsub("carcinom", "carcinoma", nyN$histology)
nyN$Stadium <- gsub("a", "A", nyN$Stadium)
nyN$Stadium <- gsub("b", "B", nyN$Stadium)

########

## Sammenligner mb (nyN) og xls(nyX):
ifelse(nyN$sampleID == nyX$sampleID, "yes", "NO") #OK
ifelse(nyN$gender == nyX$gender, "yes", "NO") #OK
ifelse(nyN$born == nyX$born, "yes", "NO") #OK
ifelse(nyN$inclusion == nyX$inclusion, "yes", "NO") #OK
ifelse(nyN$died == nyX$died, "yes", "NO") #Her er nyX mer oppdatert enn nyN
summary(ifelse(nyN$died== nyX$died, "yes", "NO") == "yes")
########

## henter ut de jeg har brukt på chip (nyN) ifra (nyS)
S <- nyS[nyS$sampleID %in% nyN$sampleID,]

# Bruker Espens siste versjon (nS), inntil jeg får pratet og dobbelsjekket at alt stemmer:
nSX <- merge(nyX, S, by = "sampleID")
nSX$histology <- gsub(" ", "_", nSX$histology)
nSX$histology <- gsub("S", "s", nSX$histology)
nSX$histology <- gsub("C", "c", nSX$histology)
nSX$histology <- gsub("A", "a", nSX$histology)
nSX$histology <- gsub("scLc", "SCLC", nSX$histology)
nSX$histology <- gsub("_ca", "_carcinoma", nSX$histology)
nSX$histology <- gsub("N", "n", nSX$histology)
nSX$histology <- gsub("U", "u", nSX$histology)


nSX$diagnosis <- gsub(" ", "_", nSX$diagnosis)
nSX$smoking_status <- gsub(" ", "_", nSX$smoking_status)
nSX$smoking_status <- gsub("Earlier", "previous", nSX$smoking_status)
nSX$smoking_status <- gsub("S", "s", nSX$smoking_status)
nSX$smoking_status <- gsub("N", "n", nSX$smoking_status)
nSX$pack_year <- gsub(",", ".", nSX$pack_year)
###

## HISTOLOGY som ikke matcher: 
df <- merge(nSX, nyN, by ="sampleID")
df$histology <- ifelse(df$histology.x == df$histology.y, "yes", "NO") 
Hpatients <- df$sampleID[df$histology == "NO"]
Hdf <- df[df$sampleID %in% Hpatients,]
H <- Hdf[,c("sampleID", "histology.x","histology.y")]
###

## Stadium som ikke matcher:
df$Stadium <- ifelse(df$Stadium.x == df$Stadium.y, "yes", "NO") 
Spatients <- df$sampleID[df$Stadium == "NO"]
Sdf <- df[df$sampleID %in% Spatients,]
St <- Sdf[,c("sampleID", "Stadium.x","Stadium.y")]
###

## smoking_status som ikke matcher:
df$smoking_status <- ifelse(df$smoking_status.x == df$smoking_status.y, "yes", "NO") 
ss.patients <- df$sampleID[df$smoking_status == "NO"]
ss.df <- df[df$sampleID %in% ss.patients,]
ss <- ss.df[,c("sampleID", "smoking_status.x","smoking_status.y")]
###

## duration som ikke matcher:
df$duration <- ifelse(df$duration.x == df$duration.y, "yes", "NO") 
dur.patients <- df$sampleID[df$duration == "NO"]
dur.df <- df[df$sampleID %in% dur.patients,]
dur <- dur.df[,c("sampleID", "duration.x","duration.y")]
###

## pack_year som ikke matcher:
df$pack_year <- ifelse(df$pack_year.x == df$pack_year.y, "yes", "NO") 
packY.patients <- df$sampleID[df$pack_year == "NO"]
packY.df <- df[df$sampleID %in% packY.patients,]
packY <- packY.df[,c("sampleID", "pack_year.x","pack_year.y")]
###

## no_of_cigarettes som ikke matcher:
df$no_of_cigarettes <- ifelse(df$no_of_cigarettes.x == df$no_of_cigarettes.y, "yes", "NO") 
cigs.patients <- df$sampleID[df$no_of_cigarettes == "NO"]
cigs.df <- df[df$sampleID %in% cigs.patients,]
cigs <- cigs.df[,c("sampleID", "no_of_cigarettes.x","no_of_cigarettes.y")]
###
