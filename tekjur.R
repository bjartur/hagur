
#dalkar eru
#	1	Ar
#	2	Heildartekjur
#	3	Medaltekjur
#	4	MidgildiTekna
#	5	Atvinnutekjur
#	6	Fjármagnstekjur
#	7	AdrarTekjur
#	8	Skattar
#	9	VaxtaOgBarnabaetur
#	10	Heildarskattar(SkattarVaxtaOgBarnabaetur)
#	11	VaxtagjoldV/Ibudalana
#	12	OnnurVaxtagjold
#	13	Radstofunartekjur(Heildartekjur-Heildarskattar)
#	14	Radstofunartekjur2
#	15	Fjoldiihop
#
dalknofn <- c("Ar", "Heildartekjur", "Medaltekjur", "MidgildiTekna", "Atvinnutekjur", "Fjarmagnstekjur", "AdrarTekjur", "Skattar", "VaxtaOgBarnabaetur", "Heildarskattar(SkattarVaxtaOgBarnabaetur)", "VaxtagjoldV/Ibudalana", "OnnurVaxtagjold", "Radstofunartekjur(Heildartekjur-Heildarskattar)", "Radstofunartekjur2", "Fjoldiihop")

#breytilegt eftir aðstæðum
folderpath <- "TekjurGjold/Gognin/"

filename <- c("u24.csv", "Aldur2529.csv", "Aldur3034.csv", "Aldur3539.csv", "Aldur4044.csv", "Aldur4549.csv", "Aldur5054.csv", "Aldur5559.csv", "Aldur6066.csv", "y67.csv", "Alls.csv", "EinstaettForeldri.csv", "Einstaklingur.csv", "HjonAnBarna.csv", "HjonMedBorn.csv", "Hofudborg.csv", "Landsbyggd.csv")

dataimport <- function(folderpath, filename, numrow, numcol) {
	#import data from file
	tempframe <- read.csv2(paste(c(folderpath, filename), collapse = ""), header = TRUE, dec=",", as.is=TRUE)
	
	#prepare return frame
	frame <- data.frame(matrix(0, nrow= numrow, ncol= numcol))
	
	#fix data
	for(i in 1:numrow) {
		frame[i,] <- do.call(rbind, strsplit(tempframe[i,1],","))
	}
	
	#str to num, give columns names.
	frame <- sapply(frame, as.numeric)
	colnames(frame) <- dalknofn
	
	return(frame)
}

#	tekjurOgGjold[[1]]	undir 24
#	tekjurOgGjold[[2]]	25-29
#	tekjurOgGjold[[3]]	30-34
#	tekjurOgGjold[[4]]	35-39
#	tekjurOgGjold[[5]]	40-44
#	tekjurOgGjold[[6]]	45-49
#	tekjurOgGjold[[7]]	50-54
#	tekjurOgGjold[[8]]	55-59
#	tekjurOgGjold[[9]]	60-66
#	tekjurOgGjold[[10]]	yfir 67
#	tekjurOgGjold[[11]]	Alls
#	tekjurOgGjold[[12]]	Einstaett foreldri
#	tekjurOgGjold[[13]]	Einstaklingur
#	tekjurOgGjold[[14]]	Hjon An Barna
#	tekjurOgGjold[[15]]	Hjon Med Born
#	tekjurOgGjold[[16]]	Hofudborg
#	tekjurOgGjold[[17]]	Landsbyggd
#	
#	
tekjurOgGjold <- list(dataimport(folderpath, filename[1], 17, 15), dataimport(folderpath, filename[2], 17, 15), dataimport(folderpath, filename[3], 17, 15), dataimport(folderpath, filename[4], 17, 15), dataimport(folderpath, filename[5], 17, 15), dataimport(folderpath, filename[6], 17, 15), dataimport(folderpath, filename[7], 17, 15), dataimport(folderpath, filename[8], 17, 15), dataimport(folderpath, filename[9], 17, 15), dataimport(folderpath, filename[10], 17, 15), dataimport(folderpath, filename[11], 17, 15), dataimport(folderpath, filename[12], 17, 15), dataimport(folderpath, filename[13], 17, 15), dataimport(folderpath, filename[15], 17, 15), dataimport(folderpath, filename[15], 17, 15), dataimport(folderpath, filename[16], 17, 15), dataimport(folderpath, filename[17], 17, 15))


dalknofn[1] <- "Hopur"

stokartol <- function(artal, tekjurOgGjold) {
	line <- artal - 1996
	
	frame <- data.frame(matrix(0, nrow= length(tekjurOgGjold), ncol= dim(tekjurOgGjold[[1]])[2]))
	
	for(i in 1:length(tekjurOgGjold)) {
		frame[i,] = tekjurOgGjold[[i]][line,]
		frame[i,1] = i
	}
	
	colnames(frame) <- dalknofn
	
	return(frame)
}

artolSkuldir <- list(stokartol(1997, tekjurOgGjold), stokartol(1998, tekjurOgGjold), stokartol(1999, tekjurOgGjold), stokartol(2000, tekjurOgGjold), stokartol(2001, tekjurOgGjold), stokartol(2002, tekjurOgGjold), stokartol(2003, tekjurOgGjold), stokartol(2004, tekjurOgGjold), stokartol(2005, tekjurOgGjold), stokartol(2006, tekjurOgGjold), stokartol(2007, tekjurOgGjold), stokartol(2008, tekjurOgGjold), stokartol(2009, tekjurOgGjold), stokartol(2010, tekjurOgGjold), stokartol(2011, tekjurOgGjold), stokartol(2012, tekjurOgGjold), stokartol(2013, tekjurOgGjold))

#til ad fa data frame fyrir artal x kallar madur a artolSkuldir[[(x-1996)]]



