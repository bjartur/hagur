# Plottum r??st?funartekjur a m?ti ?rum, fyrir alls kyns tiundir

source('radstofunartekjur.R')

# Radstofunartekjur.R hefur veri? keyrt.

arin = 1997:2013

# Notkun: x = nunakronur(upphaed, ar)
# Fyrir: upphaed er upph?? ? millj?num kr?na sem m?ld var ?ri? "ar"  (m? vera ? milli 1997-2013)
# Eftir: x er upph??in ? 2015 ? millj?num krona
nunakronur = function(upphaed, ar)
{
  
  # V?sitala neysluver?s fyrir arin 1998-2013:
  visitala = c(180.3, 183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
  visisept = 430.6 # v?sitala neysluver?s ? september 2015
  hlutfallsvisitala = visitala/visisept
  
  return(upphaed/hlutfallsvisitala[ar-1996])
  
}

tekjur = list(1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17) # ra?st?funartekjur t?unda (tekjur - skattar - vextir). 
fjoldi =  c(1:17) # fj?ldi ? tiundum er eins ? t?undum, en breytist milli ?ra. 
medaltekjur = list(1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17) # me?altekju

for(j in 1:10)
{ # listar yfir heildar r??st?funartekjur fyrstu j tiundanna hafaa veri? f?r?ar inn ? tekjur[1:j]
  # fj?ldinn 

  for(k in 1:17)
  { # ra?st?funartekjur f?lks ? tiund j ? ?ri 1996+k hefur veri? f?r?ur inn ? fjoldi[1:j][1:k]
    if(j == 1)
    { # hef fj?ldann i 3. t?und sem fj?ldann ? t?undum. 17 ?r.
      fjoldi[k] = as.data.frame(tiundir[j])["Fjoldiihop"][,1][k]
    }
    
    tekjur[[j]][k] = as.data.frame(tiundir[j])["Radstofunartekjur2"][,1][k]
    medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[k]
    #medaltekjur[[j]][k] = tekjur[[j]][k]/fjoldi[k]

  }
}  


# ?arf si?an a? plotta allt a sama graf
# m?rk grafsins ?kvar?ast af tekjum h?stu tiundarinnar.
# plottum fyrst efstu t?undirnar, svo l?gri tiundirnar - hli? vi? hli? 
# 
# if(FALSE)
# {
#   # fyrir tiundir 10 ni?ur i 8:
#   plot(arin, medaltekjur[[10]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "?r", ylab = "Me?al ra?st?funartekjur ? ?ri  [millj?nir]", ylim = c(min(medaltekjur[[8]]), max(medaltekjur[[10]])))
#   litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue")
#   for(i in 9:8)
#   { # myndir fyrir 10-i tekjuh?stu t?undirnar hafa veri? s?ndar
#     lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
#     
#   }
#   
#   
#   # fyrir t?undir 7 ni?ur ? 4:
#   plot(arin, medaltekjur[[7]], type = "b", col = litir[i], xaxp = c(arin[1],arin[17], 8), xlab = "?r", ylab = "Me?al ra?st?funartekjur ? ?ri  [millj?nir]", ylim = c(min(medaltekjur[[4]]), max(medaltekjur[[7]])))
#   for(i in 7:4)
#   { # myndir fyrir 10-i tekjuh?stu t?undirnar hafa veri? s?ndar
#     lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
#     
#   }
#   
#   # fyrir t?undir 3 ni?ur ? 1:
#   plot(arin, medaltekjur[[3]], type = "b", col = litir[i], xaxp = c(arin[1],arin[17], 8), xlab = "?r", ylab = "Me?al ra?st?funartekjur ? ?ri  [millj?nir]", ylim = c(min(medaltekjur[[1]]), max(medaltekjur[[3]])))
#   for(i in 3:1)
#   { # myndir fyrir 10-i tekjuh?stu t?undirnar hafa veri? s?ndar
#     lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
#     
#   }
#   
#   
#   
#   # Allt plotta? ? eina mynd:
#   
#   plot(arin, medaltekjur[[10]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "?r", ylab = "Me?al ra?st?funartekjur ? ?ri  [millj?nir]", ylim = c(min(medaltekjur[[1]]), max(medaltekjur[[10]])))
#   
#   for(i in 9:1)
#   { # myndir fyrir 10-i tekjuh?stu t?undirnar hafa veri? s?ndar
#     lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
#   }
# 
# }

# Notkun: tekjudreifing(medalT, mintiund, maxtiund)
# Fyrir: medalT er listi af listum ?ar sem undirlistarnir eru me? 17 st?k t.d. me?altal r??st?funartekna yfir ?rin 1998-2013 
#   mintiund og maxtiund eru t?undir, 1 <= mintiund < maxtiund <= 10
# Eftir: medalT hefur veri? teikna? ? m?ti arunum 1997-2013 fyrir tiundirnar a milli mintiund og maxtiund

# Til a? alh?fa yfir ? a?rar st?r?ir en ra?st?funartekjur ?arf a? breyta ylabel
tekjudreifing = function(medalT, tiundir)
{
  if(is.null(tiundir))
    tiundir = 1:10
  tiundir = as.numeric(tiundir)
  
  # ymork = c(min(medalT[[min(tiundir)]]), max(medalT[[max(tiundir)]]))
  ymork = c(0, max(medalT[[max(tiundir)]]))
  
  litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
  plot(arin, medalT[[max(tiundir)]], type = "l", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "?r", ylab = "Me?al ra?st?funartekjur ? ?ri  [millj?nir]", ylim = ymork)


  for(i in tiundir)
    { # myndir fyrir 10-i tekjuh?stu t?undirnar hafa veri? s?ndar
      lines(arin, medalT[[i]], type = "l", col = litir[i])
    }
}

tekjudreifing(medaltekjur, 1:9) # 90%


# N? ?tla ?g a? plotta eigi?f?, skuldir og eignir ? m?ti ?rum fyrir landsbygg?ina annarsvegar og h?fu?borgarsv??i? hinsvegar
  # landshlutar mega b??a

eigidfjeAldurs = aldurs("Eigidfje")

# Afmörkum, titlum og stillum glugga til að teikna í
xmork = c(arin[1],arin[17])
ymork = c(min(eigidfjeAldurs), max(eigidfjeAldurs))
litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")

plot(c(), c(), type = "l", xlab = "Ár", ylab = "Eigið fé að meðaltali [m.kr.]", xaxp = c(arin[1],arin[17], 8), xlim=xmork, ylim = ymork)

sapply(seq_along(aldursflokkar), function(i) {
  lines(arin, eigidfjeAldurs[,i], type="l", col=litir[i])
})


if(FALSE) {
# Teikna ?? eigi?f?, skuldir og eignir fyrir landsbygg?ina og h?f?uborgarsv??i?

# FJARL?GI BIL ? D?LKAN?FNUM FYRIR ALLT

minborgbyggd = min(c(-hofudborg$Skuldiralls/hofudborg$Fjoldiihop, -landsbyggd$Skuldiralls/landsbyggd$Fjoldiihop))
maxborgbyggd = max(c(hofudborg$Eigniralls/hofudborg$Fjoldiihop, landsbyggd$Eigniralls/landsbyggd$Fjoldiihop))
plot(arin, hofudborg$EigidFje/hofudborg$Fjoldiihop, type = "b", xlab = "?r", ylab = "Eigi?f? a? me?altali [millj?nir krona]", xaxp = c(arin[1],arin[17], 8), col = "blue", ylim = c(minborgbyggd, maxborgbyggd))
lines(arin, landsbyggd$EigidFje/landsbyggd$Fjoldiihop, type = "b", col = "red")
# b?ti vi? skuldum og eignum fyrir sitt hvort
  # dashed l?nu fyrir skuldir, dotted l?nu fyrir eignir, litirnir haldast
lines(arin, -hofudborg$Skuldiralls/hofudborg$Fjoldiihop, lty = 2, col = "blue")
lines(arin, hofudborg$Eigniralls/hofudborg$Fjoldiihop, lty = 3, col = "blue")

lines(arin, -landsbyggd$Skuldiralls/landsbyggd$Fjoldiihop, lty = 2, col = "red")
lines(arin, landsbyggd$Eigniralls/landsbyggd$Fjoldiihop, lty = 3, col = "red")

# 0 l?nan

lines(arin, rep(0,17))

}


# Teikna ?? eignir ? m?ti aldursh?pum, me? mismunandi ferla eftir ?rum (speglun)

