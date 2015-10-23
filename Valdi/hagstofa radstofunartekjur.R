#RENAMEA ÞETTA M.T.T. RÁÐSTÖFUNARTEKNA, EF ÞAÐ GENGUR EKKI AÐ NOTA PX

#dalkar eru
#	1	Artal
#	2	Heildareignir
#	3	Medaltal eigna
#	4	Midgildi eigna
#	5	Fasteignir
#	6	Okutaeki
#	7	Innlan
#	8	Verdbrjef
#	9	Adrar eignir
#	10	Heildarskuldir
#	11	Medaltal skulda
#	12	Midgildi skulda
#	13	Ibudarlan
#	14	Adrar skuldir
#	15	Heildar eigid fje
#	16	Egid fje i fasteign
#	17	Egid fje annad
#	18	Fjoldi i hop
#	19	Medaltal skulda skuldsettra
#	20	Fjoldi skuldsettra
#
dalknofn <- c("Ar", "Eignir alls", "Medaltal eigna", "Midgildi eigna", "Fasteignir", "Okutaeki", "Innlan", "Verdbrjef", "Adrar eignir", "Skuldir alls", "Medaltal skulda", "Midgildi skulda", "Ibudarlan", "Adrar skuldir", "Eigid fje alls", "Egid fje i fasteign", "Eigid fje annad", "Fjoldi i hop", "Medaltal skulda skuldsettra", "Fjoldi skuldsettra")

#breytilegt eftir aÃ°stÃ¦Ã°um
folderpath <- "Documents/hagstofa/HagstofaXlSkjol/SkuldirEignirOgEiginfjarstadaEftirFjolskyldugerdAldriOgBusetu/"

filename <- ""

dataimport <- function(folderpath, filename) {
	#import data from file
	tempframe <- read.csv2(paste(c(folderpath, filename), collapse = ""), header = TRUE, dec=",", as.is=TRUE)
	
	#prepare return frame
	frame <- data.frame(matrix(0, nrow=17, ncol=20))
	
	#fix data
	for(i in 1:17) {
		frame[i,] <- do.call(rbind, strsplit(tempframe[i,1],","))
	}
	
	#str to num, give columns names.
	frame <- sapply(frame, as.numeric)
	colnames(frame) <- dalknofn
	
	return(frame)
}


#--------------------------
#
#aldur undir 24

filename <- "HagstofaUndir24.csv"
aldur24 <- dataimport(folderpath, filename)


#--------------------------
#
#aldur milli 25 og 29

filename <- "HagstofaA25-29.csv"
aldur2529 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 30 og 34

filename <- "HagstofaA30-34.csv"
aldur3034 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 30 og 34

filename <- "HagstofaA35-39.csv"
aldur3539 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 40 og 44

filename <- "HagstofaA40-44.csv"
aldur4044 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 45 og 49

filename <- "HagstofaA45-49.csv"
aldur4549 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 50 og 54

filename <- "HagstofaA50-54.csv"
aldur5054 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 55 og 59

filename <- "HagstofaA55-59.csv"
aldur5559 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur milli 60 og 66

filename <- "HagstofaA60-66.csv"
aldur6066 <- dataimport(folderpath, filename)


#-----------------------------
#
#aldur yfir 67

filename <- "HagstofaYfir67.csv"
aldur67 <- dataimport(folderpath, filename)


#-----------------------------
#
#Einstaed foreldri

filename <- "HagstofaEinstaettForeldri.csv"
einstaed <- dataimport(folderpath, filename)


#-----------------------------
#
#Einstaklingur

filename <- "HagstofaEinstaklingur.csv"
einstaklingur <- dataimport(folderpath, filename)


#-----------------------------
#
#Hjon An Barna

filename <- "HagstofaHjonAnBarna.csv"
anbarna <- dataimport(folderpath, filename)


#-----------------------------
#
#Hjon Med Born

filename <- "HagstofaHjonMedBorn.csv"
medborn <- dataimport(folderpath, filename)


#-----------------------------
#
#Hofudborgarsvaedid

filename <- "HagstofaHofudborg.csv"
hofudborg <- dataimport(folderpath, filename)


#-----------------------------
#
#Landsbyggd

filename <- "HagstofaLandsbyggd.csv"
landsbyggd <- dataimport(folderpath, filename)


#-----------------------------
#
#Allt landiÃ°

filename <- "HagstofaAlls.csv"
alls <- dataimport(folderpath, filename)


#----------------------