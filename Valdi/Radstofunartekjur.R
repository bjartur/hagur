#ATH 1. tiund er tekjuLAEGSTA tiund,
#10. tiund er tekjuheasta

#dalkar eru
#	1	Artal
#	2	Heildareignir
#	3	Fasteignir
#	4	Okutaeki
#	5	Innlan
#	6	Verdbrjef
#	7	Adrar Eignir
#	8	Heildarskuldir
#	9	Ibudalan
#	10	Adrar skuldir
#	11	Heildar eigid fje
#	12	eigid fje i fasteign
#	13	Eigid fje annad
#	14	Atvinnutekjur
#	15	Fjarmagnstekjur
#	16	Adrar tekjur
#	17	Heildartekjur
#	18	Skattar
#	19	Vaxta- og barnabaetur
#	20	Skattar alls
#	21	Vaxtagjold v/ ibudalana
#	22	Onnur Vaxtagjold
#	23	Radstofunartekjur (Tekjur alls - Skattar alls)
#	24	Radstofunartekjur (Tekjur alls - Skattar alls - Vaxtagjold)
#	25	Fjoldi i hop
#
dalknofn <- c("Artal", "Heildareignir", "Fasteignir", "Okutaeki", "Innlan", "Verdbrjef", "AdrarEignir", "Heildarskuldir", "Ibudalan", "Adrarskuldir", "Heildareigidfje", "Eigidfjeifasteign", "Eigidfjeannad", "Atvinnutekjur", "Fjarmagnstekjur", "Adrartekjur", "Heildartekjur", "Skattar", "Vaxta-ogbarnabaetur", "Skattaralls", "Vaxtagjoldv/ibudalana", "OnnurVaxtagjold", "Radstofunartekjur1", "Radstofunartekjur2", "Fjoldiihop")

#breytilegt eftir aðstæðum
folderpath <- "./radstofun/"

filename <- c("1.csv", "2.csv", "3.csv", "4.csv", "5.csv", "6.csv", "7.csv", "8.csv", "9.csv", "10.csv", "11.csv")

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


tiundir <- list(dataimport(folderpath, filename[1], 17, 25), dataimport(folderpath, filename[2], 17, 25), dataimport(folderpath, filename[3], 17, 25), dataimport(folderpath, filename[4], 17, 25), dataimport(folderpath, filename[5], 17, 25), dataimport(folderpath, filename[6], 17, 25), dataimport(folderpath, filename[7], 17, 25), dataimport(folderpath, filename[8], 17, 25), dataimport(folderpath, filename[9], 17, 25), dataimport(folderpath, filename[10], 17, 25), dataimport(folderpath, filename[11], 17, 25))

#til ad kalla a data frame fyrir tiund i er
#skipunin tiundir[[i]]
#fyrir stak (p,q) i tiund i, tiundir[[i]][p,q]
#stak 11 i tiundir er allt landid


dalknofn[1] <- "tiund"

stokartol <- function(artal, tiundir) {
	line <- artal - 1996
	
	frame <- data.frame(matrix(0, nrow= length(tiundir), ncol= dim(tiundir[[1]])[2]))
	
	for(i in 1:length(tiundir)) {
		frame[i,] = tiundir[[i]][line,]
		frame[i,1] = i
	}
	
	colnames(frame) <- dalknofn
	
	return(frame)
}

artol <- list(stokartol(1997, tiundir), stokartol(1998, tiundir), stokartol(1999, tiundir), stokartol(2000, tiundir), stokartol(2001, tiundir), stokartol(2002, tiundir), stokartol(2003, tiundir), stokartol(2004, tiundir), stokartol(2005, tiundir), stokartol(2006, tiundir), stokartol(2007, tiundir), stokartol(2008, tiundir), stokartol(2009, tiundir), stokartol(2010, tiundir), stokartol(2011, tiundir), stokartol(2012, tiundir), stokartol(2013, tiundir))

#til ad fa data frame fyrir artal x kallar madur a artol[[(x-1996)]]
#ath! ellefta lina er heildartala, en ekki ellefta tiund.









