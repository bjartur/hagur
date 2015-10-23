# 5. okt?ber 
# Teikna ?a? sem ?g er kominn me?


setwd("C:/Users/Eggert??r/Documents/Sk?linn/Reiknigreind/Project 1/")
arin = c(1997:2013)
litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")


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

# Notkun: medalT = medaltekjur(tafla, dalkurmedal, fjoldiihop, fjoldihopa,)
# Fyrir: 
#   tafla er listi af (listum sem breyta m? i dataframe)
#   dalkurstaerd er strengur, nafni? ? d?lkinum fyrir ?a? sem er fundi? me?altal af ("Radstofunartekjur2") - upph?? ? millj?num kr?na
#   dalkurfjoldi er strengur, nafni? ? d?lkinum fyrir fj?lda i h?pnum sem finna a me?altal af ("Fjoldiihop")
#   fjoldihopa er heiltala, fj?ldi h?pa    (t.d. 10 fyrir t?undirnar)
# Eftir: medalT inniheldur me?altal st?r?arinnar ? dalkurmedal fyrir h?pana  ?r tafla yfir ?rin 1997-2013
#        m?lt ? 2015 kr?num.
medalTek = function(tafla, dalkurstaerd, dalkurfjoldi, fjoldihopa)
{
    tekjur = list(1:17) # 17 ?r
    medaltekjur = list(1:17)
    fjoldi = list(1:17)
    
    for(i in 1:(fjoldihopa-1))
    { # i 17 staka listar hafa veri? sm??a?ir til a? geyma hvert af eftirfarandi: tekjur, me?altekjur og fj?lda
        
        tekjur = c(tekjur, list(1:17))  
        medaltekjur = c(medaltekjur, list(1:17))  
        fjoldi = c(fjoldi, list(1:17))
        
    }
    # fjoldihopa 17 staka listar hafa veri? sm??a?ir fyrir hvert af eftirfarandi: tekjur, me?altekjur og fj?lda
 
    
    # j er n?mer h?ps, k er ?rtali?-1996
    for(j in 1:fjoldihopa)
    { # listar yfir me?alr??st?funartekjur fyrstu j h?panna yfir arin 1997-2013 hafa veri? f?r?ar inn ? medaltekjur[1:j][1:k]
        
        for(k in 1:17)
        {# me?altekjur f?lks ? h?pi j ? ?ri 1996+k hefur veri? f?r?ur inn ? medaltekjur[[1:j-1]] og medaltekjur[[j]][1:k]
            
            fjoldi[[j]][k] = as.data.frame(tafla[j])[dalkurfjoldi][,1][k]   # fj?ldi ? h?p j ?ri? 1996+k
            
            tekjur[[j]][k] = as.data.frame(tafla[j])[dalkurstaerd][,1][k]   # tekjur ?eirra ? h?p j ari? 1996+k
            

            medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[[j]][k]      # me?altekjur ?eirra ? h?p j ?ri? 1996+k ? 2015 kr?num

            
        }    
    }  

    return(medaltekjur)
}


# Notkun: tekjudreifing(medalT, mintiund, maxtiund)
# Fyrir: medalT er listi af listum ?ar sem undirlistarnir eru me? 17 st?k t.d. me?altal r??st?funartekna yfir ?rin 1998-2013 
#   mintiund og maxtiund eru t?undir, 1 <= mintiund < maxtiund <= 10
# breytaplottud er strengur, nafni? ? ?v? sem veri? er a? 
# Eftir: medalT hefur veri? teikna? ? m?ti arunum 1997-2013 fyrir tiundirnar a milli mintiund og maxtiund

# Til a? alh?fa yfir ? a?rar st?r?ir en ra?st?funartekjur ?arf a? breyta ylabel
tekjudreifing = function(medalT, mintiund, maxtiund, breytaplottud)
{
    litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
    arin = c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)
    plot(arin, medalT[[maxtiund]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "?r", ylab = paste("Me?al ", breytaplottud, " [millj?nir kr.]"), ylim = c(min(medalT[[mintiund]]), max(medalT[[maxtiund]])), )
    
    title("Eftir tekjut?undum")
    
    if( mintiund < maxtiund)
    {
        for(i in (maxtiund-1):mintiund)
        { # myndir fyrir 10-i tekjuh?stu t?undirnar hafa veri? s?ndar
            lines(arin, medalT[[i]], type = "b", col = litir[i])
        }            
    }

    
}  


# Notkun: ndir = ntvenndir(n)
# Fyrir: n er heiltala >= 2
# Eftir: ndir er (2*n)x17 fylki sem getur geymt gildi ? n tvenndum yfir ?rin 17
ntvenndir = function(n)
{
    tvennd = c(1:2)
    ndir = list(rep(tvennd,n))
    for( i in 2:17)
    {
        ndir = c(ndir, list(rep(tvennd,n)))
    }
    
    return(ndir)
}

tiundaferdalag = function(valtiundir, ar1, ar2)
{
    
    # h?fum liti fyrir ?rin 17. fyrstu 10 litirnir eru endurn?ttir ?r tekjudreifingarmyndinni
    litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
    
    # Getur lines readjusta? xlim og ylim? Nei. ?arf a? skilgreina m?rkin ? asunum fyrirfram.
    # vel l?gmarki? og h?marki? ?r tekjunum og skuldunum fyrir valdnar tiundir, yfir ?ll arin, til a? geta plotta?.
    tekjos = c()
    skuldos = c()
    for( p in valtiundir ) 
    {   # p t?knar valdna tiund
        for(s in c((ar1-1996):(ar2-1996)))
        {   # s t?knar ar
            tekjos = c(tekjos, tiutek[[p]][s])
            skuldos = c(skuldos, tiuskuld[[p]][s])            
        }
        
    }        
    # tekjos og skuldos geyma tekjur og skuldir valinna tiunda a v?ldum arum
    # svo vi? getum stillt ?sana i plotinu
    for(j in (ar1-1996):(ar2-1996))    
    {   # hlaupum yfir arin og teiknum tekju/skulda tvenndir fyrir valdnar t?undir fyrir arin
        
        # pr?fum ?etta ?ar sem ar1 = ar2 = 1997
      
        tekjur = rep(0,10) # b? til vigur fyrir tekjurnar og set inn ? fyrir ??r tiundir sem sko?a skal
        skuldir = rep(0,10) # b? til vigur fyrir skuldirnar og set inn ? fyrir valtiundirnar
        j
        # fyrir hvert ?r, ?? set ?g tekjur/skuldir inn ? valdnar t?undir
        for(i in valtiundir)
        {   # hle? inn ?eim skuldum og tekjum sem ver?a nota?ar
            k = 2*i-1
            tekjur[[i]] = tiugogn[[j]][k]
            k = k+1
            skuldir[[i]] = tiugogn[[j]][k]
            
        }
        # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir ?rin fr? ar1 og upp ?anga? sem komi? er 
        
        
        
        # sett upp mynd fyrir fyrstu v?ldu t?undina
        # (breyta ?arf labelunum ? ?sana ef breytunum er breytt)
        
        if(j == (ar1-1996))
        {   # geri bara plot ? fyrsta skipti?, eftir ?a? baralines.
            plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldos), max(skuldos)), xlim = c(min(tekjos), max(tekjos)),  xlab = "Me?al r??st?funartekjur [millj?nir kr. / ?r]", ylab = "Me?al ?b??askuldir [millj?nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
            
        }
        else
        {
            
            lines(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Me?al r??st?funartekjur [millj?nir kr. / ?r]", ylab = "Me?al ?b??askuldir [millj?nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
            # vil segmenta saman fyrstu valdna t?und (4) ? ari? 1998 og ?rinu ? undan
            # hugsa ?t fra tiugogn
            segments(tiugogn[[j-1]][2*valtiundir[1]-1], tiugogn[[j-1]][2*valtiundir[1]],   tiugogn[[j]][2*valtiundir[1]-1], tiugogn[[j]][2*valtiundir[1]], col = litir[j])
        }
        
        
        # og b?ti svo vi? restinni:
        if(length(valtiundir) > 1)
        {   # vil geta teikna? staka t?und ?n ?ess a? fa villu
            for(k in valtiundir[2:length(valtiundir)])
            {
                
                lines( tekjur[k], skuldir[k], type = "b", col = litir[j], pch = k)
                # er ?? b?inn a? b?ta vi? punktum fyrir hinar tiundirnar
                # ?arf a? b?ta vi? segments sem tengjast s??astli?nu ?ri 
                if(j > 1)
                {  # ???ir l?ti? a? teikna strik ? ari? ? undan ? fyrsta ?rinu
                    segments(tiugogn[[j-1]][2*k-1], tiugogn[[j-1]][2*k],   tiugogn[[j]][2*k-1], tiugogn[[j]][2*k], col = litir[j])    
                }
                
            }                                
        }
        
    }
}

# Nota aldurs ? t.d. EigidFje til a? f? ?a? sta?la? m.t.t. ver?bolgu. Byggir ? leidretta
aldurs <- function(lysistaerd, deila=TRUE) {
    if(deila)
        sapply(aldursflokkar,
               function(aldur) {
                   leidretta(t(aldur[lysistaerd]/aldur$Fjoldiihop))
               }
        )
    else
        sapply(aldursflokkar,
               function(aldur) {
                   leidretta(t(aldur[lysistaerd]))
               }
        )
}
    
# Notkun: leidretta(upphaedir)
# Fyrir: upphaedir er 17 talna listi.
#           Hver tala er krónutala á verðlagi samsvarandi árs, á bilinu 1997–2013,
#           þegar ártölunum er raðað ???? vaxandi röð.
# Skilagildi: Sömu upphæðir, nema allar á verðlagi septembers 2015
leidretta = function(upphaedir) {
    sapply(seq_along( verdlag ),
           function(i) {
               nuvirdi(upphaedir[i], 1997+(i-1))
           })
}






# R??st?funartekjur t?unda eftir arum. (y-?s: r??st?funartekjur, x-?s: ?r, gagnvirkni: t?undir )
# (? eftir a? b?ta inn gagnvirkninni)

# F? g?gnin sem unni? er me? ?r Radstofunartekjur.R 
# Er svo me? 3 f?ll; til a? sta?la m.t.t. ver?b?lgu, til a? finna me?alr??st?funartekjur og til a? teikna ??r fyrir tiundirnar

source("Radstofunartekjur.R")  


    litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
    # 10 litir fyrir 10 h?pa (t?undir, e?a aldursh?par - virkar fyrir b??i)
    medalrst =     medalTek(tiundir,"Radstofunartekjur2", "Fjoldiihop", 10)
    medaleigidfje = medalTek(tiundir, "Heildareigidfje", "Fjoldiihop", 10)   
    medaleigidfjefasteign = medalTek(tiundir, "Eigidfjeifasteign", "Fjoldiihop", 10)
    medaleigidfjeannad = medalTek(tiundir, "Eigidfjeannad", "Fjoldiihop", 10)
    medalskattar = medalTek(tiundir, "Skattar", "Fjoldiihop", 10)
    
    tekjudreifing(medalrst,1,10, "r??st?funartekjur")

    tekjudreifing(medalrst,1,4)

    tekjudreifing(medalrst,4,4)
    
    tekjudreifing(medaleigidfje,1,4, "eigi? f?")

    par(mfrow=c(2,1))
    tekjudreifing(medalrst,1,4, "r??st?funartekjur ? ?ri")
    tekjudreifing(medaleigidfje,1,4, "eigi? f?")

    par(mfrow=c(2,1))
    tekjudreifing(medaleigidfje, 6,10, "eigi? f?")
    tekjudreifing(medaleigidfje, 1,5, "eigi? f?")

    par(mfrow=c(1,1))
    tekjudreifing(medaleigidfje, 1,10, "eigi? f?")

    par(mfrow=c(2,1))
    tekjudreifing(medalfasteign, 1,10, "eigi? f? ? fasteign")
    tekjudreifing(medalrst,1,10, "r??st?funartekjur ? ?ri")

    tekjudreifing(medalrst,4,6, "r??st?funartekjur ? ?ri")
    tekjudreifing(medalskattar,4,6, "skattar greiddir ? ?ri")


# Sko?um eigi? f? fyrir ne?ri tiundirnar og greinum ?a? ? tvennt

    par(mfrow = c(3,1))
    tekjudreifing(medaleigidfje, 1, 5, "eigi? f?")
    tekjudreifing(medaleigidfjefasteign, 1, 5, "eigi? f? ? fasteign")
    tekjudreifing(medaleigidfjeannad, 1, 5, "eigi? f? anna?")

	
# xxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxx

# Eigi? f? aldursh?pa eftir ?rum (y-?s: eigi? f?, x-?s: ?r,  gagnvirkni: aldursh?par)) 

# G?gn: keyri

source("hagstofa.R")   
# Er ?? me? 17 data frame fyrir ?j??f?lagsh?pana 17



    hopar = c(2:5)
    
    arin = c(1997:2013)
    
    allt = list(u24$EigidFje/u24$Fjoldiihop, aldur2529$EigidFje/aldur2529$Fjoldiihop, aldur3034$EigidFje/aldur3034$Fjoldiihop, aldur3539$EigidFje/aldur3539$Fjoldiihop, aldur4044$EigidFje/aldur4044$Fjoldiihop,aldur4549$EigidFje/aldur4549$Fjoldiihop, aldur5054$EigidFje/aldur5054$Fjoldiihop, aldur5559$EigidFje/aldur5559$Fjoldiihop, aldur6066$EigidFje/aldur6066$Fjoldiihop, y67$EigidFje/y67$Fjoldiihop  )    
    alltstadlad = rep(list(1:17),10)
    for(i in 1:10)
    {
       alltstadlad[[i]] = leidretta(allt[[i]]) 
       
    }
    alltstadlad

    fjem = c()  # einn langur vigur sem geymir eigi?f? fyrir h?pana sem sko?a skal 
    for(j in hopar)
    {
        fjem = c(fjem,alltstadlad[[j]])
    }
    fjem
    minfjem = min(fjem)
    maxfjem = max(fjem)
    
 
    plot(arin, leidretta(aldur2529$EigidFje)/aldur2529$Fjoldiihop, type = "b", xlab = "?r", ylab = "Eigi? fe a? me?altali [millj?nir kr?na]", xaxp = c(arin[1],arin[17], 8), ylim = c(minfjem, maxfjem))
    
    #b?ti vi? l?nu sem t?knar 0-i?
    
    lines(arin, rep(0, 17))
    
    # l?nur fyrir alla hina aldursh?pana b?tast vi?:
    lines(arin, leidretta(aldur2529$EigidFje)/aldur2529$Fjoldiihop, type = "b", col = litir[1])
    lines(arin, leidretta(aldur3034$EigidFje)/aldur3034$Fjoldiihop, type = "b",col = litir[2])
    lines(arin, leidretta(aldur3539$EigidFje)/aldur3539$Fjoldiihop,type = "b", col = litir[3])
    lines(arin, leidretta(aldur4044$EigidFje)/aldur4044$Fjoldiihop,type = "b", col = litir[4])
    lines(arin, leidretta(aldur4549$EigidFje)/aldur4549$Fjoldiihop,type = "b", col = litir[5])
    lines(arin, leidretta(aldur5054$EigidFje)/aldur5054$Fjoldiihop, type = "b",col = litir[6])
    lines(arin, leidretta(aldur5559$EigidFje)/aldur5559$Fjoldiihop,type = "b", col = litir[7])
    lines(arin, leidretta(aldur6066$EigidFje)/aldur6066$Fjoldiihop, type = "b",col = litir[8])
    lines(arin, leidretta(y67$EigidFje)/y67$Fjoldiihop, type = "b",col = litir[9])
    
    # breyta er t.d. "EigidFje", 
    # hopar er talnavigur ?ar sem c(1:4) eru yngstu 4 tiundirnar
    eignastadaaldurshopa = function(breyta, hopar)
    {
      # ?arf a? keyra leidretta e?a aldurs ? ?etta til a? lei?r?tta m.t.t. ver?bolgu
        aldurslisti = list(u24,aldur2529, aldur3034, aldur3539, aldur4044, aldur4549,aldur5054,aldur5559,aldur6066,y67)
        aldurslisti[2][[]]
        length(aldurslisti)
        allt = list(u24[[breyta]]/u24$Fjoldiihop, aldur2529[[breyta]]/aldur2529$Fjoldiihop, aldur3034[[breyta]]/aldur3034$Fjoldiihop, aldur3539[[breyta]]/aldur3539$Fjoldiihop, aldur4044[[breyta]]/aldur4044$Fjoldiihop,aldur4549[[breyta]]/aldur4549$Fjoldiihop, aldur5054[[breyta]]/aldur5054$Fjoldiihop, aldur5559[[breyta]]/aldur5559$Fjoldiihop, aldur6066[[breyta]]/aldur6066$Fjoldiihop, y67[[breyta]]/y67$Fjoldiihop  )    
        alltstadlad = rep(list(1:17),10)
    
        for(i in 1:10)
        {
            alltstadlad[[i]] = leidretta(allt[[i]]) 
        }
        alltstadlad 
        hopar = c(2:10)
        hopar
        fjem = c()  # einn st?r listi sem geymir ?ll g?gnin fyrir valdna h?pa    
        for(k in hopar)
        {
            fjem = c(fjem,alltstadlad[[k]])
        }
        fjem #allt einum vigri
        minfjem = min(fjem)
        maxfjem = max(fjem)
        minfjem
        maxfjem
        hopar = c(2:4)
        breyta = "EigidFje"
        plot(arin, leidretta(aldurslisti[[hopar[1]]][[breyta]])/aldurslisti[[hopar[1]]][["Fjoldiihop"]], type = "b", col = litir[hopar[1]], xlab = "?r", ylab = paste(breyta, "a? me?altali [millj?nir kr?na]"), xaxp = c(arin[1],arin[17], 8), ylim = c(minfjem, maxfjem))
        
        lines(arin, rep(0,17))    
        for(j in hopar[2:length(hopar)])
        {   
            j = 5
            lines(arin, leidretta(aldurslisti[[ hopar[j] ]] [[breyta]])/aldurslisti[[hopar[j]]][["Fjoldiihop"]], type = "b", col = litir[hopar[j]])            
        }
    }

eignastadaaldurshopa("EigidFje", c(1:4))


# xxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxx

# Eigi? f?, skuldir og eignir eftir landshlutum  (y-?s: eigi?f? og skuldir og eignir, x-?s: ?r, gagnvirkni: engin)

# Gagnvirknin ?tti a? vera a? velja hva? ma?ur vill sj? 
    
    # allt a? me?altali
    hbskuldir = leidretta(-hofudborg$Skuldiralls)/hofudborg$Fjoldiihop
    hbeignir = leidretta(hofudborg$Eigniralls)/hofudborg$Fjoldiihop
    hbeigid = leidretta(hofudborg$EigidFje)/hofudborg$Fjoldiihop
    
    lbeignir = leidretta(landsbyggd$Eigniralls)/landsbyggd$Fjoldiihop
    lbskuldir = leidretta(-landsbyggd$Skuldiralls)/landsbyggd$Fjoldiihop
    lbeigid = leidretta(landsbyggd$EigidFje)/landsbyggd$Fjoldiihop

    minborgbyggd = min(c(hbskuldir, lbskuldir))
    maxborgbyggd = max(c(hbeignir, lbeignir))
    plot(arin, hbeigid, type = "b", xlab = "?r", ylab = "Fj?rhagssta?a a? me?altali [millj?nir krona]", xaxp = c(arin[1],arin[17], 8),  col = "blue", ylim = c(minborgbyggd, maxborgbyggd))
    lines(arin, lbeigid, type = "b", col = "red")
    title("Einstaklingar eftir landshlutum")
    # b?ti vi? skuldum og eignum fyrir sitt hvort
    # dashed l?nu fyrir skuldir og eignir, litirnir haldast
    lines(arin, hbskuldir, lty = 2, col = "blue")
    lines(arin, hbeignir, lty = 2, col = "blue")
    
    lines(arin, lbskuldir, lty = 2, col = "red")
    lines(arin, lbeignir, lty = 2, col = "red")
    
    # 0 l?nan
    
    lines(arin, rep(0,17))
    
    title("H?fu?borgarsv??i og landsbygg?")

    # B?tum inn r??st?funartekjum fyrir h?fu?borgina og landsbygg?ina 
    # Nota leidretta til a? f? lista sta?la?a m.t.t. ver?b?lgu 
    hbtekjur =  leidretta(as.data.frame(tekjurOgGjold[16])$Radstofunartekjur2) / as.data.frame(tekjurOgGjold[16])$FjoldiIHop
    lbtekjur =  leidretta(as.data.frame(tekjurOgGjold[17])$Radstofunartekjur2) / as.data.frame(tekjurOgGjold[17])$FjoldiIHop

    lines(arin, hbtekjur, lty = 1, col = "blue")
    lines(arin, lbtekjur, lty = 1, col = "red")
    

    # V?sir a? fallv??ingu:

    # valdarbreytur er listi af strengjum 
    # t.d. valdarbreytur = c("EigidFje", "Skuldiralls", "Eigniralls")
    # hopar er listi sem tilgreinir hva?a h?pa skal sko?a, t.d. c(4,5,6) fyrir mi?aldra e?a c(1:4) fyrir unga
    # e?a c(16,17) fyrir landshlutana

    # 1. safna saman ?eim g?gnum sem sko?a skal ? langan vigur til a? finna min og max til a? stilla ?sana
    hopalisti = list(u24,aldur2529, aldur3034, aldur3539, aldur4044, aldur4549,aldur5054,aldur5559,aldur6066,y67, 11,12,13,14,15,hofudborg,landsbyggd)
    alltstadlad = rep(list(1:17),10)

    for(i in 1:17)
    {
        alltstadlad[[i]] = leidretta(hopalisti[[i]]) 
        
    }
    alltstadlad
    
    fjem = c()  # einn langur vigur sem geymir eigi?f? fyrir h?pana sem sko?a skal 
    for(j in hopar)
    {
        fjem = c(fjem,alltstadlad[[j]])
    }
    fjem
    # 2. plotta vi?eigandi breytur fyrir vi?eigandi h?pa
    # litirnir eru litir[1..10] fyrir aldursh?pana, litir[16..17] fyrir landshlutana

    # plotta 0-l?nuna
    plot(arin, rep(0,17),type = "b", xlab = "?r", ylab = "Fj?rhagssta?a a? me?altali [millj?nir krona]", xaxp = c(arin[1],arin[17], 8),  col = "blue", ylim = c(minborgbyggd, maxborgbyggd))   

    for(j in hopar)
    {
        lines(arin, hopalisti[[]])
        
    }
    


# xxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxx

# ?b??al?naskuldir ? m?ti r??st?funartekjum fyrir landshluta (y-?s: ?b??al?naskuldir, x-?s: r??st?funartekjur, gagnvirkni: ?rin) 

    # G?gnin:
    source("hagstofa3.0tekjuroggjold.R")
    
    # H?r vantar falli? ntvenndir
    
    # Notkun: ndir = ntvenndir(n)
    # Fyrir: n er heiltala >= 2
    # Eftir: ndir er (2*n)x17 fylki sem getur geymt gildi ? n tvenndum yfir ?rin 17
    ntvenndir = function(n)
    {
        tvennd = c(1:2)
        ndir = list(rep(tvennd,n))
        for( i in 2:17)
        {
            ndir = c(ndir, list(rep(tvennd,n)))
        }
        
        return(ndir)
    }
    
    
    landshlutar = list(hofudborg, landsbyggd)
    
    ibskuldirLands <- sapply(landshlutar, function(hluti) 
    {
        t(leidretta(hluti$Ibudarlan/hluti$Fjoldiihop)) #listi eigi? f? tiltekna aldurh?psins hvert ?r t?mabilsins
    }
    )
    
    
    
    # plotta heildar fer?alag landshlutanna tveggja:
    
    medaltekjur = medalTek(tekjurOgGjold, "Radstofunartekjur2", "FjoldiIHop",17)
    
    mrstlandid = list(1:2) # 1 er h?fu?borg, 2 er landsbyggd
    mrstlandid[1] = medaltekjur[16]
    mrstlandid[2] = medaltekjur[17]
    
    
    # er me? ?b??al?naskuldirnar ? ibskuldirLands og me?altekjurnar i mrstlandid
    
    minibskuldir = min(ibskuldirLands[,1], ibskuldirLands[,2])
    maxibskuldir = max(ibskuldirLands[,1], ibskuldirLands[,2])
    xmin = min( c(mrstlandid[[1]], mrstlandid[[2]])  )
    xmax = max( c(mrstlandid[[1]], mrstlandid[[2]])  )
    plot(mrstlandid[[1]], ibskuldirLands[,1], type = "b", col = "blue", ylim = c(minibskuldir, maxibskuldir),  xlab = "Me?al r??st?funartekjur [millj?nir kr.]", ylab = "Me?al ?b??askuldir [millj?nir kr.]")
    lines(mrstlandid[[2]], ibskuldirLands[,2], type = "b", col = "red")
    
    
    
    # plotta einn ? einu, m.?.a.
    
    ndir = ntvenndir(2)
    
    for(i in 1:(17))  # 17 
    {
        ndir[[i]][1] = mrstlandid[[1]][i]
        ndir[[i]][2] = ibskuldirLands[,1][i]
        ndir[[i]][3] = mrstlandid[[2]][i]
        ndir[[i]][4] = ibskuldirLands[,2][i]
    }
    
    # ndir hefur 17 ra?ir sem hver inniheldur 4 gildi, fyrstu tv? eru tvennd h?fu?borgarinnar, seinni tv? landsbygg?arinnar
    # Vil b?ta ?eim inn einni ? einu ? mynd.
    
    # Set fyrst upp fyrir ari? 1997:
    rvktvennd1997 = ndir[[1]][c(1:2)]
    lbtvennd1997 = ndir[[1]][c(3:4)]
    plot(c(rvktvennd1997[1]), c(rvktvennd1997[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Me?al r??st?funartekjur [millj?nir kr. / ?r]", ylab = "Me?al ?b??askuldir [millj?nir kr.]",main ="Skulda- og tekna fer?alag")
    lines(c(lbtvennd1997[1]), c(lbtvennd1997[2]), type = "b", col = "blue")
    
    
    # Teiknum ??r eina ? einu:
    
    # forsenda: ?ri? 1997 hefur veri? teikna? (hefur veri? gert h?r a? ofan)
    i = 2
    
    # endurt?kum svo eftirfarandi 5 l?nur til a? b?ta inn einni tvennd ? einu:
    lines(c(ndir[[i]][1]), c(ndir[[i]][2]), type = "b", col = "red")
    lines(c(ndir[[i]][3]), c(ndir[[i]][4]), type = "b", col = "blue")
    segments( c(ndir[[i-1]][1]), c(ndir[[i-1]][2]), c(ndir[[i]][1]), c(ndir[[i]][2]), lty = 2, col = "red") # teiknar fyrir RVK
    segments( c(ndir[[i-1]][3]), c(ndir[[i-1]][4]), c(ndir[[i]][3]), c(ndir[[i]][4]), lty = 2, col = "blue") # teiknar fyrir RVK
    i = i+1
    
    # Viljum sj? a? lokum heildarfer?alagi?. ?.e. b?ta inn striki ? milli 1. og 17. tvenndar
    segments( c(ndir[[1]][1]), c(ndir[[1]][2]), c(ndir[[17]][1]), c(ndir[[17]][2]), col = "red") # teiknar fyrir RVK
    segments( c(ndir[[1]][3]), c(ndir[[1]][4]), c(ndir[[17]][3]), c(ndir[[17]][4]), col= "blue") # teiknar fyrir landsbygg?ina
    
    
    
    
    
    # hallatala striksins:
    # RVK:
    (ndir[[17]][2] - ndir[[1]][2])/(ndir[[17]][1] - ndir[[1]][1])
    # LB:
    (ndir[[17]][4] - ndir[[1]][4])/(ndir[[17]][3] - ndir[[1]][3])
    
    # RVK:
    # ?ri? 1998 var hlutfall ?b??askulda a m?ti tekjum a? me?altali:
    lantekjur98 = ndir[[1]][2]/ndir[[1]][1]
    # ?ri? 2013 var hlutfall ?b??askulda ? m?ti tekjum a? me?altali:
    lantekjur13 = ndir[[17]][2]/ndir[[17]][1]
    # Hlutfalli? ?ar a milli:
    lantekjur13/lantekjur98
    # LB:
    # ?ri? 1998 var hlutfall ?b??askulda a m?ti tekjum a? me?altali:
    lantekjur98 = ndir[[1]][4]/ndir[[1]][3]
    lantekjur98
    # ?ri? 2013 var hlutfall ?b??askulda ? m?ti tekjum a? me?altali:
    lantekjur13 = ndir[[17]][4]/ndir[[17]][3]
    lantekjur
    # Hlutfalli? ?ar a milli:
    lantekjur13/lantekjur98
    


# xxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxx

# ?b??al?naskuldir ? m?ti r??st?funartekjum fyrir t?undir (y-?s: ?b??al?naskuldir, x-?s: r??st?funartekjur, gagnvirkni: t?undir og ?rin ) 


    
    tiutek = medalTek(tiundir,"Radstofunartekjur2", "Fjoldiihop", 10)
    tiuskuld = medalTek(tiundir,"Ibudalan", "Fjoldiihop", 10)
    
    tiugogn = ntvenndir(10) # hl??um g?gnunum hinga? inn
    
    
    for (j in 1:17)
    {   # Hleyp yfir ?rin. ? ?ri 1996+j er tvenndin fyrir tiund i ? s?tum 2i-1 (tekjur) og 2i (skuld)
        # sj? d?mi a? ofan
        
        for(i in 1:10)
        {
            k = 2*i-1
            tiugogn[[j]][k] = tiutek[[i]][j]
            k = k+1
            tiugogn[[j]][k] = tiuskuld[[i]][j]
            
        }
        
    }
    tiugogn
    
    
    
    # g.r.f. a? tiugogn hafi veri? hla?i? inn. geri einnig r?? fyrir a? tiutek og tiuskuld hafi veri? hla?i? inn
    # t.d. ar1 = 1997, ar2 = 2013  og tekjur = c(4,5,6)
    tiundaferdalag = function(valtiundir, ar1, ar2)
    {
        
        # h?fum liti fyrir ?rin 17. fyrstu 10 litirnir eru endurn?ttir ?r tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
        
        # Getur lines readjusta? xlim og ylim? Nei. ?arf a? skilgreina m?rkin ? asunum fyrirfram.
        # vel l?gmarki? og h?marki? ?r tekjunum og skuldunum fyrir valdnar tiundir, yfir ?ll arin, til a? geta plotta?.
        tekjos = c()
        skuldos = c()
        for( p in valtiundir ) 
        {   # p t?knar valdna tiund
            for(s in c((ar1-1996):(ar2-1996)))
            {   # s t?knar ar
                tekjos = c(tekjos, tiutek[[p]][s])
                skuldos = c(skuldos, tiuskuld[[p]][s])            
            }
            
        }        
        # tekjos og skuldos geyma tekjur og skuldir valinna tiunda a v?ldum arum
        # svo vi? getum stillt ?sana i plotinu
        for(j in (ar1-1996):(ar2-1996))    
            {   # hlaupum yfir arin og teiknum tekju/skulda tvenndir fyrir valdnar t?undir fyrir arin
                
                # pr?fum ?etta ?ar sem ar1 = ar2 = 1997
               
                tekjur = rep(0,10) # b? til vigur fyrir tekjurnar og set inn ? fyrir ??r tiundir sem sko?a skal
                skuldir = rep(0,10) # b? til vigur fyrir skuldirnar og set inn ? fyrir valtiundirnar
                j
                # fyrir hvert ?r, ?? set ?g tekjur/skuldir inn ? valdnar t?undir
                for(i in valtiundir)
                {   # hle? inn ?eim skuldum og tekjum sem ver?a nota?ar
                    k = 2*i-1
                    tekjur[[i]] = tiugogn[[j]][k]
                    k = k+1
                    skuldir[[i]] = tiugogn[[j]][k]
                    
                }
                # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir ?rin fr? ar1 og upp ?anga? sem komi? er 
                
                
                
                # sett upp mynd fyrir fyrstu v?ldu t?undina
                # (breyta ?arf labelunum ? ?sana ef breytunum er breytt)
                
                if(j == (ar1-1996))
                {   # geri bara plot ? fyrsta skipti?, eftir ?a? bara lines.
                    plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldos), max(skuldos)), xlim = c(min(tekjos), max(tekjos)),  xlab = "Me?al r??st?funartekjur [millj?nir kr. / ?r]", ylab = "Me?al ?b??askuldir [millj?nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                    
                }
                else
                {
                    
                    lines(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Me?al r??st?funartekjur [millj?nir kr. / ?r]", ylab = "Me?al ?b??askuldir [millj?nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                    # vil segmenta saman fyrstu valdna t?und (4) ? ari? 1998 og ?rinu ? undan
                    # hugsa ?t fra tiugogn
                    segments(tiugogn[[j-1]][2*valtiundir[1]-1], tiugogn[[j-1]][2*valtiundir[1]],   tiugogn[[j]][2*valtiundir[1]-1], tiugogn[[j]][2*valtiundir[1]], col = litir[j])
                }
                
                
                # og b?ti svo vi? restinni:
                if(length(valtiundir) > 1)
                {   # vil geta teikna? staka t?und ?n ?ess a? fa villu
                    for(k in valtiundir[2:length(valtiundir)])
                    {
                        
                        lines( tekjur[k], skuldir[k], type = "b", col = litir[j], pch = k)
                        # er ?? b?inn a? b?ta vi? punktum fyrir hinar tiundirnar
                        # ?arf a? b?ta vi? segments sem tengjast s??astli?nu ?ri 
                        if(j > 1)
                        {  # ???ir l?ti? a? teikna strik ? ari? ? undan ? fyrsta ?rinu
                            segments(tiugogn[[j-1]][2*k-1], tiugogn[[j-1]][2*k],   tiugogn[[j]][2*k-1], tiugogn[[j]][2*k], col = litir[j])    
                        }
                        
                    }                                
                }
                
            }
    }
    
    
    tiundaferdalag(c(4,5,6,7), 1997,2009)     
    tiundaferdalag(c(8,9), 1997,2009)
    tiundaferdalag(c(1,2,3),1997,2013)

	
# xxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxx	
	
# R??st?funartekjur aldursh?pa eftir ?rum (y-?s: rst ? ?ri, x-?s: ?r, gagnvirkni: h?par)

    # hef falli? leidretta til a? fa i n?nakronum
    
    
    
    
    # vel h?pa, t.d. c(5:6) fyrir folk a milli fertugs og fimmtugs
    
    # Fyrir: tekjurOgGjold hefur veri? loada?, ?samt lei?r?tta
    #   valhopar er listi af t?lum fra 1 upp ? 10 yfir valdna aldursh?pa
    #   lykillinn er eftirfarandi:
    
    #   tekjurOgGjold[[1]]    undir 24
    #	tekjurOgGjold[[2]]	25-29
    #	tekjurOgGjold[[3]]	30-34
    #	tekjurOgGjold[[4]]	35-39
    #	tekjurOgGjold[[5]]	40-44
    #	tekjurOgGjold[[6]]	45-49
    #	tekjurOgGjold[[7]]	50-54
    #	tekjurOgGjold[[8]]	55-59
    #	tekjurOgGjold[[9]]	60-66
    #	tekjurOgGjold[[10]]	yfir 67
    
    # t.d. gefur valhopar = c(3:6) f?lk a milli ?r?tugs og fimmtugs.
    
    tekjuraldurshopa = function(valhopar)
    {    
     
        medalrsthopar = rep(list(1:17),10)
        for(i in 1:10)
        {
            medalrsthopar[[i]] = leidretta(as.data.frame(tekjurOgGjold[i])$Radstofunartekjur2) / as.data.frame(tekjurOgGjold[i])$FjoldiIHop
            
        }
        
        # Finn m?rk ? y-?sin
        valhopar
        mintekjur = min(medalrsthopar[[valhopar[1]]])
        maxtekjur = max(medalrsthopar[[valhopar[1]]])
        for(j in valhopar[2:length(valhopar)])
        {
            mintekjur = min(c(mintekjur,medalrsthopar[[j]]))
            maxtekjur = max(c(maxtekjur,medalrsthopar[[j]]))
        }
        mintekjur
        maxtekjur
    
        
        plot(arin, medalrsthopar[[valhopar[1]]], type = "b", col = litir[valhopar[1]], xaxp = c(arin[1],arin[17], 8), ylim = c(mintekjur,maxtekjur), xlab = "?r", ylab = "Me?al r??st?funartekjur [millj?nir kr?na / ?r]")
        
        for(i in valhopar[2:length(valhopar)])
        {
            lines(arin, medalrsthopar[[i]], type = "b", col = litir[i])
        }
    }
    
    
    tekjuraldurshopa(1:3)
    
    # Hermi gagnvirknina og hle? ?llu inn a eina mynd
    
    medalrsthopar
    
    
    mintekjur = min(medalrsthopar[[1]])
    maxtekjur = max(medalrsthopar[[1]])
    for(j in 2:10)
    {
        mintekjur = min(c(mintekjur,medalrsthopar[[j]]))
        maxtekjur = max(c(maxtekjur,medalrsthopar[[j]]))
    }
    plot(arin, medalrsthopar[[1]], type = "b", col = litir[1], xaxp = c(arin[1],arin[17], 8), ylim = c(mintekjur,maxtekjur), xlab = "?r", ylab = "Me?al r??st?funartekjur [millj?nir kr?na / ?r]")
    
    i = 5
    
    lines(arin, medalrsthopar[[i]], type = "b", col = litir[i])
    i = i+1




# xxxxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxx	
	
# ?b??al?naskuldir ? m?ti ra?st?funartekjum fyrir aldursh?pa 

# Tekjurnar eru ? medalrsthopar
# ?bu?askuldirnar eru ? t.d. aldur2529$Ibudarlan
# en ?a? ? eftir a? lei?r?tta m.t.t. ver?b?lgu
# Er me? aldurs til a? lei?r?tta.
    
    
    medalrsthopar = rep(list(1:17),10)
    for(i in 1:10)
    {
        medalrsthopar[[i]] = leidretta(as.data.frame(tekjurOgGjold[i])$Radstofunartekjur2) / as.data.frame(tekjurOgGjold[i])$FjoldiIHop
        
    }
    
    # Er kominn me? me?alra?st?funartekjurnar - ??r eru ? medalrsthopar[[1..10]]
    
    # ?arf a? n? i ?b??askuldirnar yfir ?rin fyrir h?pana 10
    
    medalibskuld = rep(list(1:17),10)
    aldurshopar = list(u24)
    aldurshopar = c(aldurshopar, list(aldur2529))
    aldurshopar = c(aldurshopar, list(aldur3034))
    aldurshopar = c(aldurshopar, list(aldur3539))
    aldurshopar = c(aldurshopar, list(aldur4044))
    aldurshopar = c(aldurshopar, list(aldur4549))
    aldurshopar = c(aldurshopar, list(aldur5054))
    aldurshopar = c(aldurshopar, list(aldur5559))
    aldurshopar = c(aldurshopar, list(aldur6066))
    aldurshopar = c(aldurshopar, list(y67))
    
    
    medalibskuld[[1]] = leidretta(u24$Ibudarlan)
    for(i in 1:10)
    {
        medalibskuld[[i]] = leidretta(aldurshopar[[i]]$Ibudarlan)/aldurshopar[[i]]$Fjoldiihop
    }
    
    medalibskuld
    
    # Er kominn me? me?al?b??askuldirnar, ??r eru ? medalibskuld
    
    # ?arf a? safna t?u tvenndum fyrir hvert ?r, sbr. tiundaferdalag
    
    hopagogn = ntvenndir(10)
    for (j in (1):17)
    {   # Hleyp yfir ?rin. ? ?ri 1996+j er tvenndin fyrir aldursh?p i ? s?tum 2i-1 (tekjur) og 2i (skuld)
        # sj? d?mi a? ofan
        
        for(i in 1:10)
        {
            k = 2*i-1
            hopagogn[[j]][k] = medalrsthopar[[i]][j]
            k = k+1
            hopagogn[[j]][k] = medalibskuld[[i]][j]
            
        }
        
    }
    hopagogn
    
    
    hopaferdalag = function(valhopar, ar1, ar2)  # hleyp ekki alltaf yfir arin 17
    {
        # h?fum liti fyrir arin 17
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
        
        # adjusta ?sana fyrir ?rin sem sko?u? eru. safna ?llum tekjunum og ?llum skuldunum ? lista 
        # sem min() og max() af akvar?a m?rkin ? ?sunum (eftir ?vi hva? er vali?)
        tekjos = c()
        skuldos = c()
        for(p in valhopar)
        {
            # hleyp yfir arin
            for(j in 1:17)
            {
                tekjos = c(tekjos, medalrsthopar[[p]][j])  
                skuldos = c(skuldos, medalibskuld[[p]][j])
                
            }
            
            
        }
                
        for(j in (ar1-1996):(ar2-1997))
        {
            tekjur = rep(0,10)
            skuldir = rep(0,10)
            
            
            for(i in valhopar)
            {
                k = 2*i-1
                tekjur[[i]] = hopagogn[[j]][k]
                k = k+1
                skuldir[[i]] = hopagogn[[j]][k]            
            }
            
            # set upp plot fyrir fyrsta ?ri?
            if(j == (ar1-1996))
            {   # geri bara plot ? fyrsta skipti?, eftir ?a? baralines.
                plot(tekjur[valhopar[1]], skuldir[valhopar[1]], type = "b", col = litir[j], pch = valhopar[1], ylim = c(min(skuldos), max(skuldos)), xlim = c(min(tekjos), max(tekjos)),  xlab = "Me?al r??st?funartekjur [m kr./ ?r]", ylab = "Me?al ?b??askuldir [m kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                
            }
            else
            {
                
                lines(tekjur[valhopar[1]], skuldir[valhopar[1]], type = "b", col = litir[j], pch = valhopar[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Me?al r??st?funartekjur [millj?nir kr. / ?r]", ylab = "Me?al ?b??askuldir [millj?nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                # vil segmenta saman fyrstu valdna t?und (4) ? ari? 1998 og ?rinu ? undan
                # hugsa ?t fra hopagogn
                segments(hopagogn[[j-1]][2*valhopar[1]-1], hopagogn[[j-1]][2*valhopar[1]],   hopagogn[[j]][2*valhopar[1]-1], hopagogn[[j]][2*valhopar[1]], col = litir[j])
            }
            
            # B?ti svo vi? restinni
            
            if(length(valhopar) > 1)
            {   # vil geta teikna? stakan h?p ?n ?ess a? fa villu
                for(k in valhopar[2:length(valhopar)])
                {
                    
                    lines( tekjur[k], skuldir[k], type = "b", col = litir[j], pch = k)
                    # er ?? b?inn a? b?ta vi? punktum fyrir hinar tiundirnar
                    # ?arf a? b?ta vi? segments sem tengjast s??astli?nu ?ri 
                    if(j > 1)
                    {  # ???ir l?ti? a? teikna strik ? ari? ? undan ? fyrsta ?rinu
                        segments(hopagogn[[j-1]][2*k-1], hopagogn[[j-1]][2*k],   hopagogn[[j]][2*k-1], hopagogn[[j]][2*k], col = litir[j])    
                    }
                    
                }                                
            }
        }
    
    }


hopaferdalag(c(1,2,3), 1997,2013)

hopaferdalag(c(4,5),1997,2013)

hopaferdalag(c(1:10), 1997,2013)