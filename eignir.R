

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
eignadalkar <- c("Ar", "Eigniralls", "MedaltalEigna", "MidgildiEigna", "Fasteignir", "Okutaeki", "Innlan", "Verdbrjef", "AdrarEignir", "Skuldiralls", "MedaltalSkulda", "MidgildiSkulda", "Ibudarlan", "Adrarskuldir", "Eigidfje", "EgidFjeiFasteign", "Eigidfje.annad", "Fjoldiihop", "MedaltalSkuldaSkuldsettra", "FjoldiSkuldsettra")

#breytilegt eftir a?st??um
folderpath <- "SkuldirEignirOgEiginfjarstadaEftirFjolskyldugerdAldriOgBusetu/"

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
	frame <- data.frame(sapply(frame, as.numeric))
	colnames(frame) <- eignadalkar
	
	return(frame)
}


#--------------------------
#
#aldur undir 25

filename <- "HagstofaUndir24.csv"
u24 <- dataimport(folderpath, filename)


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
#aldur 67 og yfir

filename <- "HagstofaYfir67.csv"
y67 <- dataimport(folderpath, filename)


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
#Allt landi?

filename <- "HagstofaAlls.csv"
alls <- dataimport(folderpath, filename)


#----------------------


landshlutaeignir = list(hofudborg, landsbyggd)

eignir <- list(u24,aldur2529,aldur3034,aldur3539,aldur4044,aldur4549,aldur5054,aldur5559,aldur6066,y67)



