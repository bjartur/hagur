    # Plottum ráðstöfunartekjur a móti árum, fyrir alls kyns tiundir
    source("Radstofunartekjur.R")
    # Radstofunartekjur.R hefur verið keyrt.
    
    
    # Notkun: x = nunakronur(upphaed, ar)
    # Fyrir: upphaed er upphæð í milljónum króna sem mæld var árið "ar"  (má vera á milli 1997-2013)
    # Eftir: x er upphæðin í 2015 í milljónum krona
    nunakronur = function(upphaed, ar)
    {
      
      # Vísitala neysluverðs fyrir arin 1998-2013:
      visitala = c(180.3, 183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
      visisept = 430.6 # vísitala neysluverðs í september 2015
      hlutfallsvisitala = visitala/visisept
      
      return(upphaed/hlutfallsvisitala[ar-1996])
      
    }
    
    tekjur = list(1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17) # raðstöfunartekjur tíunda (tekjur - skattar - vextir). 
    fjoldi =  c(1:17) # fjöldi í tiundum er eins í tíundum, en breytist milli ára. 
    medaltekjur = list(1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17) # meðaltekju
    
    for(j in 1:10)
    { # listar yfir meðalráðstöfunartekjur fyrstu j tiundanna  yfir arin 1997-2013 hafa verið færðar inn í medaltekjur[1:j]
      # fjöldinn 
    
      for(k in 1:17)
      { # meðalraðstöfunartekjur fólks í tiund j á ári 1996+k hefur verið færður inn í fjoldi[1:j][1:k]
        if(j == 1)
        { # hef fjöldann i 3. tíund sem fjöldann í tíundum. 17 ár.
          fjoldi[k] = as.data.frame(tiundir[j])["Fjoldiihop"][,1][k]
        }
        
        tekjur[[j]][k] = as.data.frame(tiundir[j])["Radstofunartekjur2"][,1][k]
      
        medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[k]
        #medaltekjur[[j]][k] = tekjur[[j]][k]/fjoldi[k]
    
      }
    }  
      
    medaltekjur
    
    # þarf siðan að plotta allt a sama graf
    # mörk grafsins ákvarðast af tekjum hæstu tiundarinnar.
    # plottum fyrst efstu tíundirnar, svo lægri tiundirnar - hlið við hlið 
    
    if(FALSE)
    {
      # fyrir tiundir 10 niður i 8:
      plot(arin, medaltekjur[[10]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "Ár", ylab = "Meðal raðstöfunartekjur á ári  [milljónir]", ylim = c(min(medaltekjur[[8]]), max(medaltekjur[[10]])))
      litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue")
      for(i in 9:8)
      { # myndir fyrir 10-i tekjuhæstu tíundirnar hafa verið sýndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
        
      }
      
      
      # fyrir tíundir 7 niður í 4:
      plot(arin, medaltekjur[[7]], type = "b", col = litir[i], xaxp = c(arin[1],arin[17], 8), xlab = "Ár", ylab = "Meðal raðstöfunartekjur á ári  [milljónir]", ylim = c(min(medaltekjur[[4]]), max(medaltekjur[[7]])))
      for(i in 7:4)
      { # myndir fyrir 10-i tekjuhæstu tíundirnar hafa verið sýndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
        
      }
      
      # fyrir tíundir 3 niður í 1:
      plot(arin, medaltekjur[[3]], type = "b", col = litir[i], xaxp = c(arin[1],arin[17], 8), xlab = "Ár", ylab = "Meðal raðstöfunartekjur á ári  [milljónir]", ylim = c(min(medaltekjur[[1]]), max(medaltekjur[[3]])))
      for(i in 3:1)
      { # myndir fyrir 10-i tekjuhæstu tíundirnar hafa verið sýndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
        
      }
      
      
      
      # Allt plottað á eina mynd:
      
      plot(arin, medaltekjur[[10]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "Ár", ylab = "Meðal raðstöfunartekjur á ári  [milljónir]", ylim = c(min(medaltekjur[[1]]), max(medaltekjur[[10]])))
      
      for(i in 9:1)
      { # myndir fyrir 10-i tekjuhæstu tíundirnar hafa verið sýndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
      }
    
    }
    
    # Notkun: tekjudreifing(medalT, mintiund, maxtiund)
    # Fyrir: medalT er listi af listum þar sem undirlistarnir eru með 17 stök t.d. meðaltal ráðstöfunartekna yfir árin 1998-2013 
    #   mintiund og maxtiund eru tíundir, 1 <= mintiund < maxtiund <= 10
    # Eftir: medalT hefur verið teiknað á móti arunum 1997-2013 fyrir tiundirnar a milli mintiund og maxtiund
    
    # Til að alhæfa yfir á aðrar stærðir en raðstöfunartekjur þarf að breyta ylabel
    tekjudreifing = function(medalT, mintiund, maxtiund)
    {
      litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
      arin = c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)
      plot(arin, medalT[[maxtiund]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "Ár", ylab = "Meðal raðstöfunartekjur á ári  [milljónir]", ylim = c(min(medalT[[mintiund]]), max(medalT[[maxtiund]])), )
      
      title("Eftir tekjutíundum")
    
      for(i in (maxtiund-1):mintiund)
        { # myndir fyrir 10-i tekjuhæstu tíundirnar hafa verið sýndar
          lines(arin, medalT[[i]], type = "b", col = litir[i])
        }
      
    }  
    
    tekjudreifing(medaltekjur, 1, 10) # allt í einni
    
    tekjudreifing(medaltekjur, 8,10) # hæstu
    tekjudreifing(medaltekjur, 1, 7) # restin
    
    
    
    # Nú ætla ég að plotta eigiðfé, skuldir og eignir á móti árum fyrir landsbyggðina annarsvegar og höfuðborgarsvæðið hinsvegar
      # landshlutar mega bíða
    
    # Byrja á að skoða bara 25-29 ára.
    
    minfje = min(c(u24$EigidFje, aldur3034$EigidFje, aldur3539$EigidFje,aldur4044$EigidFje,aldur4549$EigidFje, aldur5054$EigidFje, aldur5559$EigidFje, aldur6066$EigidFje, y67$EigidFje  ))
    maxfje = max(c(u24$EigidFje, aldur3034$EigidFje, aldur3539$EigidFje,aldur4044$EigidFje,aldur4549$EigidFje, aldur5054$EigidFje, aldur5559$EigidFje, aldur6066$EigidFje, y67$EigidFje  ))
    litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
    
    plot(arin, aldur2529$EigidFje, type = "b", xlab = "Ár", ylab = "Eigið fe [milljónir króna]", xaxp = c(arin[1],arin[17], 8), ylim = c(minfje, maxfje))
    
    #bæti við línu sem táknar 0-ið
    
    lines(arin, rep(0, 17))
    
    # línur fyrir alla hina aldurshópana bætast við:
    lines(arin, u24$EigidFje, type = "b", col = litir[1])
    lines(arin, aldur3034$EigidFje, type = "b",col = litir[2])
    lines(arin, aldur3539$EigidFje,type = "b", col = litir[3])
    lines(arin, aldur4044$EigidFje,type = "b", col = litir[4])
    lines(arin, aldur4549$EigidFje,type = "b", col = litir[5])
    lines(arin, aldur5054$EigidFje, type = "b",col = litir[6])
    lines(arin, aldur5559$EigidFje,type = "b", col = litir[7])
    lines(arin, aldur6066$EigidFje, type = "b",col = litir[8])
    lines(arin, y67$EigidFje, type = "b",col = litir[9])
    
    
    # Þetta er i heildina, það á eftir að athuga meðaltalið:
    
    # Þarf að endurnefna þ.a. dálkanöfnin innihaldi ekki bil (breyti "Fjöldi i hop" í "Fjoldiihop" í hagstofa2.R)
    
    # Geri þá allt sama aftur(línur 120-140) nema deili með fjölda i hóp hverju sinni:
    # Bæti við 'm' fyrir aftan breytunöfn til að merkja meðal eða mean
    
    minfjem = min(c(u24$EigidFje/u24$Fjoldiihop, aldur3034$EigidFje/aldur3034$Fjoldiihop, aldur3539$EigidFje/aldur3539$Fjoldiihop, aldur4044$EigidFje/aldur4044$Fjoldiihop,aldur4549$EigidFje/aldur4549$Fjoldiihop, aldur5054$EigidFje/aldur5054$Fjoldiihop, aldur5559$EigidFje/aldur5559$Fjoldiihop, aldur6066$EigidFje/aldur6066$Fjoldiihop, y67$EigidFje/y67$Fjoldiihop  ))
    maxfjem = max(c(u24$EigidFje/u24$Fjoldiihop, aldur3034$EigidFje/aldur3034$Fjoldiihop, aldur3539$EigidFje/aldur3539$Fjoldiihop, aldur4044$EigidFje/aldur4044$Fjoldiihop,aldur4549$EigidFje/aldur4549$Fjoldiihop, aldur5054$EigidFje/aldur5054$Fjoldiihop, aldur5559$EigidFje/aldur5559$Fjoldiihop, aldur6066$EigidFje/aldur6066$Fjoldiihop, y67$EigidFje/y67$Fjoldiihop  ))
    
    
    plot(arin, aldur2529$EigidFje/aldur2529$Fjoldiihop, type = "b", xlab = "Ár", ylab = "Eigið fe að meðaltali [milljónir króna]", xaxp = c(arin[1],arin[17], 8), ylim = c(minfjem, maxfjem))
    
    #bæti við línu sem táknar 0-ið
    
    lines(arin, rep(0, 17))
    
    # línur fyrir alla hina aldurshópana bætast við:
    lines(arin, u24$EigidFje/u24$Fjoldiihop, type = "b", col = litir[1])
    lines(arin, aldur3034$EigidFje/aldur3034$Fjoldiihop, type = "b",col = litir[2])
    lines(arin, aldur3539$EigidFje/aldur3539$Fjoldiihop,type = "b", col = litir[3])
    lines(arin, aldur4044$EigidFje/aldur4044$Fjoldiihop,type = "b", col = litir[4])
    lines(arin, aldur4549$EigidFje/aldur4549$Fjoldiihop,type = "b", col = litir[5])
    lines(arin, aldur5054$EigidFje/aldur5054$Fjoldiihop, type = "b",col = litir[6])
    lines(arin, aldur5559$EigidFje/aldur5559$Fjoldiihop,type = "b", col = litir[7])
    lines(arin, aldur6066$EigidFje/aldur6066$Fjoldiihop, type = "b",col = litir[8])
    lines(arin, y67$EigidFje/y67$Fjoldiihop, type = "b",col = litir[9])
    
    
    # Teikna þá eigiðfé, skuldir og eignir fyrir landsbyggðina og höfðuborgarsvæðið
    
    # FJARLÆGI BIL Í DÁLKANÖFNUM FYRIR ALLT
    
    minborgbyggd = min(c(-hofudborg$Skuldiralls/hofudborg$Fjoldiihop, -landsbyggd$Skuldiralls/landsbyggd$Fjoldiihop))
    maxborgbyggd = max(c(hofudborg$Eigniralls/hofudborg$Fjoldiihop, landsbyggd$Eigniralls/landsbyggd$Fjoldiihop))
    plot(arin, hofudborg$EigidFje/hofudborg$Fjoldiihop, type = "b", xlab = "Ár", ylab = "Eigiðfé að meðaltali [milljónir krona]", xaxp = c(arin[1],arin[17], 8), col = "blue", ylim = c(minborgbyggd, maxborgbyggd))
    lines(arin, landsbyggd$EigidFje/landsbyggd$Fjoldiihop, type = "b", col = "red")
    # bæti við skuldum og eignum fyrir sitt hvort
      # dashed línu fyrir skuldir, dotted línu fyrir eignir, litirnir haldast
    lines(arin, -hofudborg$Skuldiralls/hofudborg$Fjoldiihop, lty = 2, col = "blue")
    lines(arin, hofudborg$Eigniralls/hofudborg$Fjoldiihop, lty = 3, col = "blue")
    
    lines(arin, -landsbyggd$Skuldiralls/landsbyggd$Fjoldiihop, lty = 2, col = "red")
    lines(arin, landsbyggd$Eigniralls/landsbyggd$Fjoldiihop, lty = 3, col = "red")
    
    # 0 línan
    
    lines(arin, rep(0,17))
    
    
    
    
    # Teikna þá eignir á móti aldurshópum, með mismunandi ferla eftir árum (speglun)
    
    # Bjartur ser um það
    
    
    
    # Teikna skuldir (íbúðaskuldir) á móti meðalraðstöfunartekjum
    # með mismunandi tákn fyrir meðlimi mismunandi hópa (t.d. x fyrir u24, o fyrir 25-29 ára) 
    # sem eru valnir úr hópagerð sem bera skal saman (aldurshópar eða tekjutíundir eða landshlutar) 
    # Mismunandi litir fyrir mismunandi ár
    
    
    # Ásarnir eru stilltir þannig að allir hóparnir liggi á línunni f(x) = x árið 1997.
    
    # Byrja á að teikna landshlutana, höfuðborgarsvæði vs. landsbyggð - þríhyrningar vs. hringir
    # 17 litir fyrir 17 ár 
    # skoðum svo hvernig hóparnir breytast milli ára - skoðum ferla þeirra um state spaceið
    
    
    
    
    # Endurnýja kóðann frá línu 20-41 til að finna meðaltekjurnar fyrir landshlutana
    # Til að finna meðalráðstöfunartekjur eftir landshlutum/aldurshópa vantar töfluna
    # Tekjur og gjöld einstaklinga eftir fjölskyldugerð, aldri og búsetu 1997-2013
    
    # G.r.f. að það verði listi hopar af hópum sem varpa má i dataframe
    # sbr. hvernig tiundir er listi af tíundunm sem varpa má í dataframe
    # sjá Radstofunartekjur.R þar sem tiund = list(...) og 
    
    
    # breytur: 
    
    # fjöldi undirhópa (10 í tilfelli tiundanna, 2 í tilfelli landshluta) - fjoldihopa
    # data frameið sem fer inn (tiundir, landsbyggd) - tafla   (segir til um hvaða hóp er verið að vi
    # nafnið á dálkinum fyrir það sem er fundið meðaltal af ("Radstofunartekjur2")
    # nafið á dálkinum með meðaltalið("Fjoldiihop")
    
    
    # Notkun: medalT = medaltekjur(tafla, dalkurmedal, fjoldiihop, fjoldihopa,)
    # Fyrir: 
    #   tafla er listi af (listum sem breyta má i dataframe)
    #   dalkurstaerd er strengur, nafnið á dálkinum fyrir það sem er fundið meðaltal af ("Radstofunartekjur2") - upphæð í milljónum króna
    #   dalkurfjoldi er strengur, nafnið á dálkinum fyrir fjölda i hópnum sem finna a meðaltal af ("Fjoldiihop")
    #   fjoldihopa er heiltala, fjöldi hópa    (t.d. 10 fyrir tíundirnar)
    # Eftir: medalT inniheldur meðaltal stærðarinnar í dalkurmedal fyrir hópana  úr tafla yfir árin 1997-2013
    #        mælt í 2015 krónum.
    medalTek = function(tafla, dalkurstaerd, dalkurfjoldi, fjoldihopa)
    {
      tekjur = list(1:17) # 17 ár
      medaltekjur = list(1:17)
      fjoldi = list(1:17)
    
      for(i in 1:(fjoldihopa-1))
      { # i 17 staka listar hafa verið smíðaðir til að geyma hvert af eftirfarandi: tekjur, meðaltekjur og fjölda
        
        tekjur = c(tekjur, list(1:17))  
        medaltekjur = c(medaltekjur, list(1:17))  
        fjoldi = c(fjoldi, list(1:17))
        
      }
      # fjoldihopa 17 staka listar hafa verið smíðaðir fyrir hvert af eftirfarandi: tekjur, meðaltekjur og fjölda
    
      #print(tekjur)
      #tekjur[[1]][5] = "lel"
      #tekjur[[1]][8] = "lel"
      #tekjur[[4]][2] = "lol"
      #tekjur[[4]][11] = "lol"
      #print(tekjur)
      
      # j er númer hóps, k er ártalið-1996
      for(j in 1:fjoldihopa)
      { # listar yfir meðalráðstöfunartekjur fyrstu j hópanna yfir arin 1997-2013 hafa verið færðar inn í medaltekjur[1:j][1:k]
        
        for(k in 1:17)
        {# meðaltekjur fólks í hópi j á ári 1996+k hefur verið færður inn í medaltekjur[[1:j-1]] og medaltekjur[[j]][1:k]
          
          fjoldi[[j]][k] = as.data.frame(tafla[j])[dalkurfjoldi][,1][k]   # fjöldi í hóp j árið 1996+k
          
          tekjur[[j]][k] = as.data.frame(tafla[j])[dalkurstaerd][,1][k]   # tekjur þeirra í hóp j arið 1996+k
          
    
          #print(tekjur[[j]][k])
          medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[[j]][k]      # meðaltekjur þeirra í hóp j árið 1996+k í 2015 krónum
          #medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)  #nunatekjur
          
          #print(class(fjoldi[k]))
          
        }    
      }  
      #print(fjoldi)
     # print("  woooo ")
      return(medaltekjur)
    }
    
    
    medalTek(tiundir,"Radstofunartekjur2", "Fjoldiihop", 10)
    
    # Skilar örlítið öðruvísi niðurstöðu en upprunalegi reikningurinn. hér er eg ekki að gera ráð fyrir að það séu jafn margir í öllum tíundum og tíund 3
    
    
    # 
    medalTek(tekjurOgGjold, "Radstofunartekjur2", "FjoldiIHop",17)
    
    
    medalTek(tekjurOgGjold, "Heildartekjur", "FjoldiIHop",17)
    
    
    # Get þá reiknað út meðaltekjur eða meðalgjöld einstaklinga i mismunandi þjóðfelagshópum.
    
    # Vil geta reiknað út meðaleignir eða meðalskuldir - fæ þau gögn á sama form og "tiundir" og "tekjurOgGjold"
    # Bjartur sér um aðferðina til að finna meðalskuldirnar
    
    source('hagstofa.R')
    source('núþákrónur.R') 
    
    aldursflokkar <- list(u24,aldur2529,aldur3034,aldur3539,aldur4044,aldur4549,aldur5054,aldur5559,aldur6066,y67)
    
    # Eigið fé eftir aldri
    eigidfjeAldurs <- sapply(aldursflokkar, 
                             function(aldur) {
      t(
        leidretta(aldur$Eigidfje/aldur$Fjoldiihop) #listi eigið fé tiltekna aldurhópsins hvert ár tímabilsins
       )
    }
    )
    
    # Höfum núvirði eiginfés í eigidfjeAldurs
    
    
    # Íbúðalán eftir aldri
    ibskuldirAldurs <- sapply(aldursflokkar, function(aldur) 
      {
                 t(leidretta(aldur$Ibudarlan/aldur$Fjoldiihop)) #listi eigið fé tiltekna aldurhópsins hvert ár tímabilsins
       }
    )
    
    landshlutar = list(hofudborg, landsbyggd)
    
    ibskuldirLands <- sapply(landshlutar, function(hluti) 
    {
      t(leidretta(hluti$Ibudarlan/hluti$Fjoldiihop)) #listi eigið fé tiltekna aldurhópsins hvert ár tímabilsins
    }
    )
    
    
    
    # Vil geta teiknað t.d. skuldir á móti t.d. meðalraðstöfunartekjum
    # með mismunandi tákn fyrir meðlimi mismunandi hópa (t.d. x fyrir u24, o fyrir 25-29 ára) 
    # sem eru valnir úr hópagerð sem bera skal saman (aldurshópar eða tekjutíundir eða landshlutar) 
    # Mismunandi litir fyrir mismunandi ár
    
    
    # Ásarnir eru stilltir þannig að allir hóparnir liggi á línunni f(x) = x árið 1997.
    
    # Byrja á að teikna landshlutana, höfuðborgarsvæði vs. landsbyggð - þríhyrningar vs. hringir
    # 17 litir fyrir 17 ár  - byrja á að gera fyrir arið 2013
    # skoðum svo hvernig hóparnir breytast milli ára - skoðum ferla þeirra um state spaceið
    
    # Geri data frame sem geymir meðalraðstöfunartekjur mismunandi hópa, með lista yfir medalrst í $nafnhops
    #   byrja á að gera bara lista fyrir höfuðborg og landsbyggð
    medaltekjur = medalTek(tekjurOgGjold, "Radstofunartekjur2", "FjoldiIHop",17)
    
    mrstlandid = list(1:2) # 1 er höfuðborg, 2 er landsbyggd
    mrstlandid[1] = medaltekjur[16]
    mrstlandid[2] = medaltekjur[17]
    
    
    # er með íbúðalánaskuldirnar í ibskuldirLands
    
    minibskuldir = min(ibskuldirLands[,1], ibskuldirLands[,2])
    maxibskuldir = max(ibskuldirLands[,1], ibskuldirLands[,2])
    xmin = min( c(mrstlandid[[1]], mrstlandid[[2]])  )
    xmax = max( c(mrstlandid[[1]], mrstlandid[[2]])  )
    plot(mrstlandid[[1]], ibskuldirLands[,1], type = "b", ylim = c(minibskuldir, maxibskuldir),  xlab = "Meðal ráðstöfunartekjur [milljónir kr.]", ylab = "Meðal íbúðaskuldir [milljónir kr.]")
    lines(mrstlandid[[2]], ibskuldirLands[,2], type = "b", col = "red")
    
    # höfum teiknað samband tekna og íbúðaskulda (sem fylgja verðlagi)
    
    # vil þá teikna eina tvennd í einu (gagnvirknin) tekjur/skuldir landshlutanna 2002, tekjur/skuldir landshlutanna 2003
    # með þríhyrning og kassa fyrir sitt hvorn landshlutann
    
    # fáum tvenndirnar úr sitt hvorum.
    
    # vil fá 2x17 fylki þar sem fyrra stakið í hverri röð er tvennd höfuðborgarinnar og síðari er tvennd landsbyggðarinnar
    # (eða nx17 og n-d, þegar það er verið að skoða n aldurshópa)
     
    
    
    tvennd = c(1:2) # 2 því þetta eru tvenndir
    ndir = list(rep(tvennd,2))
    
    
    #tvenndir = list(1:2)  
    for(i in 2:(17))  # 17 
    {
      ndir = c(ndir, list(rep(tvennd,2))) 
    
    }
    
    ndir
    # ndir er t.d. 4x17 fylki, 17 raðir, fyrir hvert ar. Tvær tvenndir á hverju ári því breytan tekur 2 gildi (2 landshlutar).
    
    
    
    
    #og svo restinni, í lykkju (öllum í lykkju býst ég við)
    
    for(i in 1:(17))  # 17 
    {
      ndir[[i]][1] = mrstlandid[[1]][i]
      ndir[[i]][2] = ibskuldirLands[,1][i]
      ndir[[i]][3] = mrstlandid[[2]][i]
      ndir[[i]][4] = ibskuldirLands[,2][i]
    }
    
    # ndir hefur 17 raðir sem hver inniheldur 4 gildi, fyrstu tvö eru tvennd höfuðborgarinnar, seinni tvö landsbyggðarinnar
    # Vil bæta þeim inn einni í einu á mynd.
    
    # Set fyrst upp fyrir arið 1997:
    rvktvennd1997 = ndir[[1]][c(1:2)]
    lbtvennd1997 = ndir[[1]][c(3:4)]
    plot(c(rvktvennd1997[1]), c(rvktvennd1997[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]",main ="Skulda- og tekna ferðalag")
    lines(c(lbtvennd1997[1]), c(lbtvennd1997[2]), type = "b", col = "blue")
    
    
    tekjurmin = min(c(ndir[[1]][1]),c(ndir[[1]][3]) )
    tekjurmax = max(c(ndir[[1]][1]),c(ndir[[1]][3]) )
    for(j in 2:17)
    {
      tekjurmin = min(c(tekjurmin,ndir[[j]][1]),c(ndir[[j]][3]) )
      tekjurmax = max(c(tekjurmax,ndir[[j]][1]),c(ndir[[j]][3]) )
      
    }
    tekjurmin = floor(tekjurmin)
    tekjurmax = ceiling(tekjurmax)
    
    rvktvennd2013 = ndir[[17]][c(1:2)]
    lbtvennd2013 = ndir[[17]][c(3:4)]
    plot(c(rvktvennd2013[1]), c(rvktvennd2013[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]", main = "2013")
    lines(c(lbtvennd2013[1]), c(lbtvennd2013[2]), type = "b", col = "blue")
    
    
    i = 1
    # TEIKNAR TVENND FYRIR TVENND fra 1998 til 2013
    for(i in 2:17)
    { # 1996+i er árið
      # rauður er höfuðborgarsvæðið, blár er landsbyggðin
        lines(c(ndir[[i]][1]), c(ndir[[i]][2]), type = "b", col = "red")
        lines(c(ndir[[i]][3]), c(ndir[[i]][4]), type = "b", col = "blue")
    }
    
    # Teiknum þær eina í einu:
    
    # forsenda: árið 1997 hefur verið teiknað (línur 420-423)
    i = 2
    # endurtökum svo eftirfarandi 3 línur til að bæta inn einni tvennd í einu:
    lines(c(ndir[[i]][1]), c(ndir[[i]][2]), type = "b", col = "red")
    lines(c(ndir[[i]][3]), c(ndir[[i]][4]), type = "b", col = "blue")
    segments( c(ndir[[i-1]][1]), c(ndir[[i-1]][2]), c(ndir[[i]][1]), c(ndir[[i]][2]), lty = 2, col = "red") # teiknar fyrir RVK
    segments( c(ndir[[i-1]][3]), c(ndir[[i-1]][4]), c(ndir[[i]][3]), c(ndir[[i]][4]), lty = 2, col = "blue") # teiknar fyrir RVK
    i = i+1
    
    # Viljum sjá að lokum heildarferðalagið. Þ.e. bæta inn striki á milli 1. og 17. tvenndar
    segments( c(ndir[[1]][1]), c(ndir[[1]][2]), c(ndir[[17]][1]), c(ndir[[17]][2]), col = "red") # teiknar fyrir RVK
    segments( c(ndir[[1]][3]), c(ndir[[1]][4]), c(ndir[[17]][3]), c(ndir[[17]][4]), col= "blue") # teiknar fyrir landsbyggðina
    
    
    
    # hallatala striksins:
    # RVK:
    (ndir[[17]][2] - ndir[[1]][2])/(ndir[[17]][1] - ndir[[1]][1])
    # LB:
    (ndir[[17]][4] - ndir[[1]][4])/(ndir[[17]][3] - ndir[[1]][3])
    
    # RVK:
      # árið 1998 var hlutfall íbúðaskulda a móti tekjum að meðaltali:
      lantekjur98 = ndir[[1]][2]/ndir[[1]][1]
      # árið 2013 var hlutfall íbúðaskulda á móti tekjum að meðaltali:
      lantekjur13 = ndir[[17]][2]/ndir[[17]][1]
      # Hlutfallið þar a milli:
      lantekjur13/lantekjur98
    # LB:
      # árið 1998 var hlutfall íbúðaskulda a móti tekjum að meðaltali:
      lantekjur98 = ndir[[1]][4]/ndir[[1]][3]
      lantekjur98
      # árið 2013 var hlutfall íbúðaskulda á móti tekjum að meðaltali:
      lantekjur13 = ndir[[17]][4]/ndir[[17]][3]
      lantekjur
      # Hlutfallið þar a milli:
      lantekjur13/lantekjur98
    
    # Endurtökum svo sama fyrir tekjutíundirnar! Sér i lagi meðalmanninn.
    
    
    
    # Bý til fylkið fyrir tvenndirnar(línur 387-400), nema fyrir 10 breytur (tíundir) í stað 2ja
    
    tvennd = c(1:2) # 2 því þetta eru tvenndir
    ndir = list(rep(tvennd,10)) 
    
    
    #tvenndir = list(1:2)  
    for(i in 2:(17))  # 17 
    { #c(ndir, list(rep(nd,m))) skapar m tvenndir
      
      ndir = c(ndir, list(rep(tvennd,10))) 
      
    }
    # ndir er 20x17 fylki, hver röð taknar ár, það eru 10 gildi á hverju ári.
    ndir
    
    # Alhæfi yfir í fall sem byr til fylki fyrir n tvenndir
    
    # Notkun: ndir = ntvenndir(n)
    # Fyrir: n er heiltala >= 2
    # Eftir: ndir er (2*n)x17 fylki sem getur geymt gildi á n tvenndum yfir árin 17
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
    
    ntvenndir(2)
    ntvenndir(10)
    
    
    # Þarf svo að safna saman gögnunum sem á að vinna ur:
    # meðaltekjur tiundanna, íbúðaskuldir tiundanna (bæði úr sama gagnasafni)
    # meðaltekjur aldurshópanna, íbuðaskuldir aldurshópanna (úr mismunandi söfnum.)
    # Auðvelt að skoða annað en íbúðaskuldir - alhæfi í fall þegar þetta sértilfelli er komið.
    
    
    # Næ í tíundagögnin.
    
    
    
    tiutek = medalTek(tiundir,"Radstofunartekjur2", "Fjoldiihop", 10)
    tiuskuld = medalTek(tiundir,"Ibudalan", "Fjoldiihop", 10)
    
    tiugogn = ntvenndir(10) # hlöðum gögnunum hingað inn
    tiugogn
    
    
    
    # byrja a að hlaða gögnunum handvirkt, profa mig áfram:
    
    tiugogn # tíugögn inniheldur 17 10 staka lista
    tiutek # tiutek inniheldur 10 17 staka lista
    tiugogn[[1]] # fyrsta arið
    tiugogn[[1]][1] = tiutek[[1]][1] # tekjur fyrsta ars fyrstu tiundinnar
    tiugogn[[1]][2] = tiuskuld[[1]][1] # skuldir fyrsta ars fyrstu tíundinnar
    tiugogn[[1]][3] = tiutek[[2]][1]  # tekjur fyrsta ars annarar tíundarinnar
    tiugogn[[1]][4] = tiuskuld[[2]][1]
    # ...
    tiugogn[[2]][9] = tiutek[[5]][2]
    tiugogn[[2]][10] = tiuskuld[[5]][2]
    
    tiugogn[[j]][k] = tiuskuld[[i]][j]] # j er ár, i er tiund, k ræðst af i
    
    # k = 2*i-1 og 2*i
    
    # tek þetta svo saman í lykkju:
    
    for (j in 1:17)
    {   # Hleyp yfir árin. Á ári 1996+j er tvenndin fyrir tiund i í sætum 2i-1 (tekjur) og 2i (skuld)
        # sjá dæmi að ofan
        
        for(i in 1:10)
        {
            k = 2*i-1
            tiugogn[[j]][k] = tiutek[[i]][j]
            k = k+1
            tiugogn[[j]][k] = tiuskuld[[i]][j]
            
        }
        
    }
    tiugogn
    #tiugogn inniheldur þá, fyrir tekjutíundirnar, (tekjur,skuldir) fyrir tiundirnar tíu fyrir hvert ar
    
    # Vil fá þetta á mynd
    
    # Vil fá fall sem velur tíundir til að skoða, t.d. 4., 5. og 6. tíund
    # Byrja a að teikna þær fyrir arið 1997
    
    tiundir1997 = tiugogn[[1]]
    # vil hafa mismunandi tákn fyrir tiundirnar. Bý til lista af 10 táknum sem einkenna þá tíundirnar.
    # use the pch= option to specify symbols,  pch = 1 er tiund i, pch = i er tiund i, 1<=i<=10
    # og mismunandi liti eftir árum
    
    # hleyp yfir tiundirnar sem eru valdnar.
    
    # g.r.f. að 4., 5. og 6. hafi verið valdnar. Þá er parameter í fallið listinn c(4,5,6)
    # tiundir = c(4,5,6)
    
    #samanber:
#    plot(c(rvktvennd2013[1]), c(rvktvennd2013[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]", main = "2013")
    
    # þetta býr til mynd fyrir bara árið 1997 - en árið er 1996+j, j getur orðið annað en 1 og þá er þetta almennara
    # g.r.f. að tiugogn hafi verið hlaðið inn. geri einnig ráð fyrir að tiutek og tiuskuld hafi verið hlaðið inn
    # t.d. ar1 = 1997, ar2 = 2013  og tekjur = c(4,5,6)
    tiundaferdalag = function(valtiundir, ar1, ar2)
    {
        print(ar1)
        print(ar2)
        # höfum liti fyrir árin 17. fyrstu 10 litirnir eru endurnýttir úr tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
       
        # Getur lines readjustað xlim og ylim? Nei. Þarf að skilgreina mörkin á asunum fyrirfram.
        # vel lágmarkið og hámarkið úr tekjunum og skuldunum fyrir valdnar tiundir, yfir öll arin, til að geta plottað.
         {   # hlaupum yfir arin og teiknum tekju/skulda tvenndir fyrir valdnar tíundir fyrir arin
            
            # prófum þetta þar sem ar1 = ar2 = 1997
            
            tekjur = rep(0,10) # bý til vigur fyrir tekjurnar og set inn í fyrir þær tiundir sem skoða skal
            skuldir = rep(0,10) # bý til vigur fyrir skuldirnar og set inn í fyrir valtiundirnar
        
            # fyrir hvert ár, þá set ég tekjur/skuldir inn í valdnar tíundir
            for(i in valtiundir)
            {   # hleð inn þeim skuldum og tekjum sem verða notaðar
                k = 2*i-1
                tekjur[[i]] = tiugogn[[j]][k]
                k = k+1
                skuldir[[i]] = tiugogn[[j]][k]
                
            }
            # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir árin frá ar1 og upp þangað sem komið er 

    
            
            # sett upp mynd fyrir fyrstu völdu tíundina
                # (breyta þarf labelunum á ásana ef breytunum er breytt)

            if(j == (ar1-1996))
            {   # geri bara plot í fyrsta skiptið, eftir það baralines.
                plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldos), max(skuldos)), xlim = c(min(tekjos), max(tekjos)),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                
            }
            else
            {
                
                lines(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                # vil segmenta saman fyrstu valdna tíund (4) á arið 1998 og árinu á undan
                # hugsa út fra tiugogn
                segments(tiugogn[[j-1]][2*valtiundir[1]-1], tiugogn[[j-1]][2*valtiundir[1]],   tiugogn[[j]][2*valtiundir[1]-1], tiugogn[[j]][2*valtiundir[1]], col = litir[j])
            }

   
            # og bæti svo við restinni:
            if(length(valtiundir) > 1)
            {   # vil geta teiknað staka tíund án þess að fa villu
                for(k in valtiundir[2:length(valtiundir)])
                {
                    
                    lines( tekjur[k], skuldir[k], type = "b", col = litir[j], pch = k)
                    # er þá búinn að bæta við punktum fyrir hinar tiundirnar
                    # þarf að bæta við segments sem tengjast síðastliðnu ári 
                    if(j > 1)
                    {  # þýðir lítið að teikna strik á arið á undan á fyrsta árinu
                        segments(tiugogn[[j-1]][2*k-1], tiugogn[[j-1]][2*k],   tiugogn[[j]][2*k-1], tiugogn[[j]][2*k], col = litir[j])    
                    }
                    
                }                                
            }

        }
    }

    
    tiundaferdalag(c(4,5,6,7), 1997,2009)       
        # valtiundir[1] er fyrsta valdna tíundin - þurfum svo að velja yfir árin sem (ar1,ar2)

        
        tekjos = c()
        skuldos = c()
        for( p in valtiundir ) 
        {   # p táknar valdna tiund
            for(s in c((ar1-1996):(ar2-1996)))
            {   # s táknar ar
                tekjos = c(tekjos, tiutek[[p]][s])
                skuldos = c(skuldos, tiuskuld[[p]][s])            
            }

        }        
        # tekjos og skuldos geyma tekjur og skuldir valinna tiunda a völdum arum
        # svo við getum stillt ásana i plotinu

        for(j in (ar1-1996):(ar2-1996))

    # þetta fall dugar eiginlega ekki til að teikna ferðalagið
    # því það plottar yfir fyrir hvert ár sem bætist við. 
    
    # hugsa um 1 ár í einu, geri fall sem teiknar tekjutiundirnar fyrir stakt ár
    # býr þá til plot fyrir það ár og by til annað fall sem tekur inn árabil, gerir lines fyrir árin og bætir þeim inn
    # "tiundaferðalag" getur þá kallað á þau
    
    
    
    tiundirarid = function(valtiundir, arid)
    {
        
        # höfum liti fyrir árin 17. fyrstu 10 litirnir eru endurnýttir úr tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
        

            
            # prófum þetta þar sem ar1 = ar2 = 1997
            
            tekjur = rep(0,10) # bý til vigur fyrir tekjurnar og set inn í fyrir þær tiundir sem skoða skal
            skuldir = rep(0,10) # bý til vigur fyrir skuldirnar og set inn í fyrir valtiundirnar
            
            for(i in valtiundir)
            {
                k = 2*i-1
                tekjur[[i]] = tiugogn[[arid-1996]][k]
                k = k+1
                skuldir[[i]] = tiugogn[[arid-1996]][k]
                
            }
            # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir árin frá ar1 og upp þangað sem komið er 
            
            # sett upp mynd fyrir fyrstu völdu tíundina
            # (breyta þarf labelunum á ásana ef breytunum er breytt)
            plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[arid-1996], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]", main = as.character(arid) )
            # og bæti svo við restinni
            if(length(valtiundir) > 1)
            {   # vil geta teiknað staka tíund án þess að fa villu
                for(k in valtiundir[2:length(valtiundir)])
                {
                    
                    lines( tekjur[k], skuldir[k], type = "b", col = litir[arid-1996], pch = k)
                    
                }                                
            }
            
        
    }    
    
    tiundirarid(c(4,5,6), 1998)
    par(new=TRUE)
    tiundirarid(c(4,5,6), 1999)
    # sökkar
    
    # get fyrst plottað og svo lineað, það ætti að virka. geri allt fyrir fyrsta árið, hleyp svo yfir restina af árunum.
    
    tiundirarin = function(valtiundir, arid)
    {
        
        # höfum liti fyrir árin 17. fyrstu 10 litirnir eru endurnýttir úr tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
        
        
        
        # Byrja a að teikna fyrir fyrsta arið
        
        tekjur = rep(0,10) # bý til vigur fyrir tekjurnar og set inn í fyrir þær tiundir sem skoða skal
        skuldir = rep(0,10) # bý til vigur fyrir skuldirnar og set inn í fyrir valtiundirnar
        
        for(i in valtiundir)
        {
            k = 2*i-1
            tekjur[[i]] = tiugogn[[arid-1996]][k]
            k = k+1
            skuldir[[i]] = tiugogn[[arid-1996]][k]
            
        }
        # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir árin frá ar1 og upp þangað sem komið er 
        
        # sett upp mynd fyrir fyrstu völdu tíundina
        # (breyta þarf labelunum á ásana ef breytunum er breytt)
        plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[arid-1996], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Meðal ráðstöfunartekjur [milljónir kr. / ár]", ylab = "Meðal íbúðaskuldir [milljónir kr.]", main = as.character(arid) )
        # og bæti svo við restinni
        if(length(valtiundir) > 1)
        {   # vil geta teiknað staka tíund án þess að fa villu
            for(k in valtiundir[2:length(valtiundir)])
            {
                
                lines( tekjur[k], skuldir[k], type = "b", col = litir[arid-1996], pch = k)
                
            }                                
        }
        
        
    }   
    
    
    
    # get siðan yfirfært yfir í aldurshopaferdalag.
    
    
    
    
    
