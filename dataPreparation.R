##############################
### Samler alle NLCBdatafilene i en fil 
##############################

## LIBRARY

########

## DATA
spss <- read.table("/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/NLCB_2016_spss.dat", sep = "\t", header =TRUE, quote = "", stringsAsFactors = FALSE)
mb <- read.table("/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/myblock.txt", header=TRUE, stringsAsFactors = FALSE)
xls <- read.table("/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/NLCB_2016_R2.txt", stringsAsFactors = FALSE, header = TRUE)
########

## Bearbeider spss: 
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

## Bearbeider xls: 
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

## Bearbeider mb:
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

## Sammenligner mb(nyN) og spss(S)
ifelse(nyN$sampleID == S$sampleID, "yes", "NO") #OK

## Samler oppdatert info fra nyN og nyX:
final <- merge(nyN, nyX, by = "sampleID")

Fin <- data.frame(sampleID=final$sampleID, gender=final$gender.x, born=final$born.x, inclusion=final$inclusion.x, died=final$died.y,
	smoking_status=final$smoking_status ,pack_year=final$pack_year, no_of_cigarettes=final$no_of_cigarettes, 
	duration=final$duration, histology=final$histology, Stadium=final$Stadium, comment=final$comment,
	 stringsAsFactors =FALSE)

Fina <- data.frame(sampleID=final$sampleID, gender=final$gender.x, born=final$born.x, inclusion=final$inclusion.x, died=final$died.y,
	smoking_status=final$smoking_status,  S$smoking_status, pack_year=final$pack_year, S$pack_year, no_of_cigarettes=final$no_of_cigarettes, 
	S$no_of_cigarettes, duration=final$duration, S$duration, histology=final$histology, S$histology, Stadium=final$Stadium, S$Stadium, comment=final$comment,
	 stringsAsFactors =FALSE)

Fina$S.smoking_status <- gsub("S", "s", Fina$S.smoking_status)
Fina$S.smoking_status <- gsub(" ", "_", Fina$S.smoking_status)
Fina$S.smoking_status <- gsub("Earlier", "previous", Fina$S.smoking_status)
Fina$S.smoking_status <- gsub("N", "n", Fina$S.smoking_status)
Fina$test <- ifelse(Fina$smoking_status == Fina$S.smoking_status, "yes", "NO") # NB: 6 matcher ikke!

Fina$test <- ifelse(Fina$pack_year	 == Fina$S.pack_year, "yes", "NO") # NB: 22 matcher ikke!
noMatch <-Fina$sampleID[Fina$test=="NO"]
Fina[Fina$sampleID %in% noMatch,]

## Pål leter etter systematiske feil: 
summary(Fina)
table(Fina$S.histology)
table(Fina$histology)
Fina$histology %in% "SCLC"
table(Fina$S.histology)
Fina$histology %in% "SCLC" == Fina$S.histology %in% "SCLC"
Fina[!(Fina$histology %in% "SCLC" == Fina$S.histology %in% "SCLC"),]
table(Fina$S.histology)
table(Fina$histology)
Fina[!(Fina$histology %in% "adeno_carcinom" == Fina$S.histology %in% "Adeno Ca"),]
# Ser ut som om feilene kommer av ulik tolkning

# Bruker Espens siste versjon (S), inntil jeg får pratet og dobbelsjekket at alt stemmer:
df <- merge(nyX, S, by = "sampleID")
df$histology <- gsub(" ", "_", df$histology)
df$diagnosis <- gsub(" ", "_", df$diagnosis)
df$smoking_status <- gsub(" ", "_", df$smoking_status)

df <- df[-c(47), ] # FJERNER PASIENT 401_00105 da denne ikke hadde lc 

# dealer med tomme celler:
df$Tumor <- gsub(" ", NA, df$Tumor)
df$Nodes <- gsub(" ", NA, df$Nodes)
df$Metastasis <- gsub(" ", NA, df$Metastasis)
df$pack_year <- gsub(" ", NA, df$pack_year)
df$pack_year_lvl <- gsub(" ", NA, df$pack_year_lvl)
df$cig_day <- gsub(" ", NA, df$cig_day)
df$Stadium <- gsub(" ", NA, df$Stadium)


df$nonLC <- NULL  ## Fjerner denne foreløpig da den bare er rotete

## OUTPUT: 
# write.table(nyN,"nlcb_from_myblock.txt", row.names=FALSE, quote=FALSE)
# write.table(df,"/Users/anne_heidi/PhD_mac/Sammenligninger/ah/lungcancer/data/TEMP_nlcbPlusSpss.txt", col.names=TRUE, quote=FALSE)



