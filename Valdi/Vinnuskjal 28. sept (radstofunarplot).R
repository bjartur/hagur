    # Plottum r��st�funartekjur a m�ti �rum, fyrir alls kyns tiundir
    source("Radstofunartekjur.R")
    # Radstofunartekjur.R hefur veri� keyrt.
    
    
    # Notkun: x = nunakronur(upphaed, ar)
    # Fyrir: upphaed er upph�� � millj�num kr�na sem m�ld var �ri� "ar"  (m� vera � milli 1997-2013)
    # Eftir: x er upph��in � 2015 � millj�num krona
    nunakronur = function(upphaed, ar)
    {
      
      # V�sitala neysluver�s fyrir arin 1998-2013:
      visitala = c(180.3, 183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
      visisept = 430.6 # v�sitala neysluver�s � september 2015
      hlutfallsvisitala = visitala/visisept
      
      return(upphaed/hlutfallsvisitala[ar-1996])
      
    }
    
    tekjur = list(1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17) # ra�st�funartekjur t�unda (tekjur - skattar - vextir). 
    fjoldi =  c(1:17) # fj�ldi � tiundum er eins � t�undum, en breytist milli �ra. 
    medaltekjur = list(1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17,1:17) # me�altekju
    
    for(j in 1:10)
    { # listar yfir me�alr��st�funartekjur fyrstu j tiundanna  yfir arin 1997-2013 hafa veri� f�r�ar inn � medaltekjur[1:j]
      # fj�ldinn 
    
      for(k in 1:17)
      { # me�alra�st�funartekjur f�lks � tiund j � �ri 1996+k hefur veri� f�r�ur inn � fjoldi[1:j][1:k]
        if(j == 1)
        { # hef fj�ldann i 3. t�und sem fj�ldann � t�undum. 17 �r.
          fjoldi[k] = as.data.frame(tiundir[j])["Fjoldiihop"][,1][k]
        }
        
        tekjur[[j]][k] = as.data.frame(tiundir[j])["Radstofunartekjur2"][,1][k]
      
        medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[k]
        #medaltekjur[[j]][k] = tekjur[[j]][k]/fjoldi[k]
    
      }
    }  
      
    medaltekjur
    
    # �arf si�an a� plotta allt a sama graf
    # m�rk grafsins �kvar�ast af tekjum h�stu tiundarinnar.
    # plottum fyrst efstu t�undirnar, svo l�gri tiundirnar - hli� vi� hli� 
    
    if(FALSE)
    {
      # fyrir tiundir 10 ni�ur i 8:
      plot(arin, medaltekjur[[10]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "�r", ylab = "Me�al ra�st�funartekjur � �ri  [millj�nir]", ylim = c(min(medaltekjur[[8]]), max(medaltekjur[[10]])))
      litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue")
      for(i in 9:8)
      { # myndir fyrir 10-i tekjuh�stu t�undirnar hafa veri� s�ndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
        
      }
      
      
      # fyrir t�undir 7 ni�ur � 4:
      plot(arin, medaltekjur[[7]], type = "b", col = litir[i], xaxp = c(arin[1],arin[17], 8), xlab = "�r", ylab = "Me�al ra�st�funartekjur � �ri  [millj�nir]", ylim = c(min(medaltekjur[[4]]), max(medaltekjur[[7]])))
      for(i in 7:4)
      { # myndir fyrir 10-i tekjuh�stu t�undirnar hafa veri� s�ndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
        
      }
      
      # fyrir t�undir 3 ni�ur � 1:
      plot(arin, medaltekjur[[3]], type = "b", col = litir[i], xaxp = c(arin[1],arin[17], 8), xlab = "�r", ylab = "Me�al ra�st�funartekjur � �ri  [millj�nir]", ylim = c(min(medaltekjur[[1]]), max(medaltekjur[[3]])))
      for(i in 3:1)
      { # myndir fyrir 10-i tekjuh�stu t�undirnar hafa veri� s�ndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
        
      }
      
      
      
      # Allt plotta� � eina mynd:
      
      plot(arin, medaltekjur[[10]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "�r", ylab = "Me�al ra�st�funartekjur � �ri  [millj�nir]", ylim = c(min(medaltekjur[[1]]), max(medaltekjur[[10]])))
      
      for(i in 9:1)
      { # myndir fyrir 10-i tekjuh�stu t�undirnar hafa veri� s�ndar
        lines(arin, medaltekjur[[i]], type = "b", col = litir[i])
      }
    
    }
    
    # Notkun: tekjudreifing(medalT, mintiund, maxtiund)
    # Fyrir: medalT er listi af listum �ar sem undirlistarnir eru me� 17 st�k t.d. me�altal r��st�funartekna yfir �rin 1998-2013 
    #   mintiund og maxtiund eru t�undir, 1 <= mintiund < maxtiund <= 10
    # Eftir: medalT hefur veri� teikna� � m�ti arunum 1997-2013 fyrir tiundirnar a milli mintiund og maxtiund
    
    # Til a� alh�fa yfir � a�rar st�r�ir en ra�st�funartekjur �arf a� breyta ylabel
    tekjudreifing = function(medalT, mintiund, maxtiund)
    {
      litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
      arin = c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)
      plot(arin, medalT[[maxtiund]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "�r", ylab = "Me�al ra�st�funartekjur � �ri  [millj�nir]", ylim = c(min(medalT[[mintiund]]), max(medalT[[maxtiund]])), )
      
      title("Eftir tekjut�undum")
    
      for(i in (maxtiund-1):mintiund)
        { # myndir fyrir 10-i tekjuh�stu t�undirnar hafa veri� s�ndar
          lines(arin, medalT[[i]], type = "b", col = litir[i])
        }
      
    }  
    
    tekjudreifing(medaltekjur, 1, 10) # allt � einni
    
    tekjudreifing(medaltekjur, 8,10) # h�stu
    tekjudreifing(medaltekjur, 1, 7) # restin
    
    
    
    # N� �tla �g a� plotta eigi�f�, skuldir og eignir � m�ti �rum fyrir landsbygg�ina annarsvegar og h�fu�borgarsv��i� hinsvegar
      # landshlutar mega b��a
    
    # Byrja � a� sko�a bara 25-29 �ra.
    
    minfje = min(c(u24$EigidFje, aldur3034$EigidFje, aldur3539$EigidFje,aldur4044$EigidFje,aldur4549$EigidFje, aldur5054$EigidFje, aldur5559$EigidFje, aldur6066$EigidFje, y67$EigidFje  ))
    maxfje = max(c(u24$EigidFje, aldur3034$EigidFje, aldur3539$EigidFje,aldur4044$EigidFje,aldur4549$EigidFje, aldur5054$EigidFje, aldur5559$EigidFje, aldur6066$EigidFje, y67$EigidFje  ))
    litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
    
    plot(arin, aldur2529$EigidFje, type = "b", xlab = "�r", ylab = "Eigi� fe [millj�nir kr�na]", xaxp = c(arin[1],arin[17], 8), ylim = c(minfje, maxfje))
    
    #b�ti vi� l�nu sem t�knar 0-i�
    
    lines(arin, rep(0, 17))
    
    # l�nur fyrir alla hina aldursh�pana b�tast vi�:
    lines(arin, u24$EigidFje, type = "b", col = litir[1])
    lines(arin, aldur3034$EigidFje, type = "b",col = litir[2])
    lines(arin, aldur3539$EigidFje,type = "b", col = litir[3])
    lines(arin, aldur4044$EigidFje,type = "b", col = litir[4])
    lines(arin, aldur4549$EigidFje,type = "b", col = litir[5])
    lines(arin, aldur5054$EigidFje, type = "b",col = litir[6])
    lines(arin, aldur5559$EigidFje,type = "b", col = litir[7])
    lines(arin, aldur6066$EigidFje, type = "b",col = litir[8])
    lines(arin, y67$EigidFje, type = "b",col = litir[9])
    
    
    # �etta er i heildina, �a� � eftir a� athuga me�altali�:
    
    # �arf a� endurnefna �.a. d�lkan�fnin innihaldi ekki bil (breyti "Fj�ldi i hop" � "Fjoldiihop" � hagstofa2.R)
    
    # Geri �� allt sama aftur(l�nur 120-140) nema deili me� fj�lda i h�p hverju sinni:
    # B�ti vi� 'm' fyrir aftan breytun�fn til a� merkja me�al e�a mean
    
    minfjem = min(c(u24$EigidFje/u24$Fjoldiihop, aldur3034$EigidFje/aldur3034$Fjoldiihop, aldur3539$EigidFje/aldur3539$Fjoldiihop, aldur4044$EigidFje/aldur4044$Fjoldiihop,aldur4549$EigidFje/aldur4549$Fjoldiihop, aldur5054$EigidFje/aldur5054$Fjoldiihop, aldur5559$EigidFje/aldur5559$Fjoldiihop, aldur6066$EigidFje/aldur6066$Fjoldiihop, y67$EigidFje/y67$Fjoldiihop  ))
    maxfjem = max(c(u24$EigidFje/u24$Fjoldiihop, aldur3034$EigidFje/aldur3034$Fjoldiihop, aldur3539$EigidFje/aldur3539$Fjoldiihop, aldur4044$EigidFje/aldur4044$Fjoldiihop,aldur4549$EigidFje/aldur4549$Fjoldiihop, aldur5054$EigidFje/aldur5054$Fjoldiihop, aldur5559$EigidFje/aldur5559$Fjoldiihop, aldur6066$EigidFje/aldur6066$Fjoldiihop, y67$EigidFje/y67$Fjoldiihop  ))
    
    
    plot(arin, aldur2529$EigidFje/aldur2529$Fjoldiihop, type = "b", xlab = "�r", ylab = "Eigi� fe a� me�altali [millj�nir kr�na]", xaxp = c(arin[1],arin[17], 8), ylim = c(minfjem, maxfjem))
    
    #b�ti vi� l�nu sem t�knar 0-i�
    
    lines(arin, rep(0, 17))
    
    # l�nur fyrir alla hina aldursh�pana b�tast vi�:
    lines(arin, u24$EigidFje/u24$Fjoldiihop, type = "b", col = litir[1])
    lines(arin, aldur3034$EigidFje/aldur3034$Fjoldiihop, type = "b",col = litir[2])
    lines(arin, aldur3539$EigidFje/aldur3539$Fjoldiihop,type = "b", col = litir[3])
    lines(arin, aldur4044$EigidFje/aldur4044$Fjoldiihop,type = "b", col = litir[4])
    lines(arin, aldur4549$EigidFje/aldur4549$Fjoldiihop,type = "b", col = litir[5])
    lines(arin, aldur5054$EigidFje/aldur5054$Fjoldiihop, type = "b",col = litir[6])
    lines(arin, aldur5559$EigidFje/aldur5559$Fjoldiihop,type = "b", col = litir[7])
    lines(arin, aldur6066$EigidFje/aldur6066$Fjoldiihop, type = "b",col = litir[8])
    lines(arin, y67$EigidFje/y67$Fjoldiihop, type = "b",col = litir[9])
    
    
    # Teikna �� eigi�f�, skuldir og eignir fyrir landsbygg�ina og h�f�uborgarsv��i�
    
    # FJARL�GI BIL � D�LKAN�FNUM FYRIR ALLT
    
    minborgbyggd = min(c(-hofudborg$Skuldiralls/hofudborg$Fjoldiihop, -landsbyggd$Skuldiralls/landsbyggd$Fjoldiihop))
    maxborgbyggd = max(c(hofudborg$Eigniralls/hofudborg$Fjoldiihop, landsbyggd$Eigniralls/landsbyggd$Fjoldiihop))
    plot(arin, hofudborg$EigidFje/hofudborg$Fjoldiihop, type = "b", xlab = "�r", ylab = "Eigi�f� a� me�altali [millj�nir krona]", xaxp = c(arin[1],arin[17], 8), col = "blue", ylim = c(minborgbyggd, maxborgbyggd))
    lines(arin, landsbyggd$EigidFje/landsbyggd$Fjoldiihop, type = "b", col = "red")
    # b�ti vi� skuldum og eignum fyrir sitt hvort
      # dashed l�nu fyrir skuldir, dotted l�nu fyrir eignir, litirnir haldast
    lines(arin, -hofudborg$Skuldiralls/hofudborg$Fjoldiihop, lty = 2, col = "blue")
    lines(arin, hofudborg$Eigniralls/hofudborg$Fjoldiihop, lty = 3, col = "blue")
    
    lines(arin, -landsbyggd$Skuldiralls/landsbyggd$Fjoldiihop, lty = 2, col = "red")
    lines(arin, landsbyggd$Eigniralls/landsbyggd$Fjoldiihop, lty = 3, col = "red")
    
    # 0 l�nan
    
    lines(arin, rep(0,17))
    
    
    
    
    # Teikna �� eignir � m�ti aldursh�pum, me� mismunandi ferla eftir �rum (speglun)
    
    # Bjartur ser um �a�
    
    
    
    # Teikna skuldir (�b��askuldir) � m�ti me�alra�st�funartekjum
    # me� mismunandi t�kn fyrir me�limi mismunandi h�pa (t.d. x fyrir u24, o fyrir 25-29 �ra) 
    # sem eru valnir �r h�pager� sem bera skal saman (aldursh�par e�a tekjut�undir e�a landshlutar) 
    # Mismunandi litir fyrir mismunandi �r
    
    
    # �sarnir eru stilltir �annig a� allir h�parnir liggi � l�nunni f(x) = x �ri� 1997.
    
    # Byrja � a� teikna landshlutana, h�fu�borgarsv��i vs. landsbygg� - �r�hyrningar vs. hringir
    # 17 litir fyrir 17 �r 
    # sko�um svo hvernig h�parnir breytast milli �ra - sko�um ferla �eirra um state spacei�
    
    
    
    
    # Endurn�ja k��ann fr� l�nu 20-41 til a� finna me�altekjurnar fyrir landshlutana
    # Til a� finna me�alr��st�funartekjur eftir landshlutum/aldursh�pa vantar t�fluna
    # Tekjur og gj�ld einstaklinga eftir fj�lskylduger�, aldri og b�setu 1997-2013
    
    # G.r.f. a� �a� ver�i listi hopar af h�pum sem varpa m� i dataframe
    # sbr. hvernig tiundir er listi af t�undunm sem varpa m� � dataframe
    # sj� Radstofunartekjur.R �ar sem tiund = list(...) og 
    
    
    # breytur: 
    
    # fj�ldi undirh�pa (10 � tilfelli tiundanna, 2 � tilfelli landshluta) - fjoldihopa
    # data framei� sem fer inn (tiundir, landsbyggd) - tafla   (segir til um hva�a h�p er veri� a� vi
    # nafni� � d�lkinum fyrir �a� sem er fundi� me�altal af ("Radstofunartekjur2")
    # nafi� � d�lkinum me� me�altali�("Fjoldiihop")
    
    
    # Notkun: medalT = medaltekjur(tafla, dalkurmedal, fjoldiihop, fjoldihopa,)
    # Fyrir: 
    #   tafla er listi af (listum sem breyta m� i dataframe)
    #   dalkurstaerd er strengur, nafni� � d�lkinum fyrir �a� sem er fundi� me�altal af ("Radstofunartekjur2") - upph�� � millj�num kr�na
    #   dalkurfjoldi er strengur, nafni� � d�lkinum fyrir fj�lda i h�pnum sem finna a me�altal af ("Fjoldiihop")
    #   fjoldihopa er heiltala, fj�ldi h�pa    (t.d. 10 fyrir t�undirnar)
    # Eftir: medalT inniheldur me�altal st�r�arinnar � dalkurmedal fyrir h�pana  �r tafla yfir �rin 1997-2013
    #        m�lt � 2015 kr�num.
    medalTek = function(tafla, dalkurstaerd, dalkurfjoldi, fjoldihopa)
    {
      tekjur = list(1:17) # 17 �r
      medaltekjur = list(1:17)
      fjoldi = list(1:17)
    
      for(i in 1:(fjoldihopa-1))
      { # i 17 staka listar hafa veri� sm��a�ir til a� geyma hvert af eftirfarandi: tekjur, me�altekjur og fj�lda
        
        tekjur = c(tekjur, list(1:17))  
        medaltekjur = c(medaltekjur, list(1:17))  
        fjoldi = c(fjoldi, list(1:17))
        
      }
      # fjoldihopa 17 staka listar hafa veri� sm��a�ir fyrir hvert af eftirfarandi: tekjur, me�altekjur og fj�lda
    
      #print(tekjur)
      #tekjur[[1]][5] = "lel"
      #tekjur[[1]][8] = "lel"
      #tekjur[[4]][2] = "lol"
      #tekjur[[4]][11] = "lol"
      #print(tekjur)
      
      # j er n�mer h�ps, k er �rtali�-1996
      for(j in 1:fjoldihopa)
      { # listar yfir me�alr��st�funartekjur fyrstu j h�panna yfir arin 1997-2013 hafa veri� f�r�ar inn � medaltekjur[1:j][1:k]
        
        for(k in 1:17)
        {# me�altekjur f�lks � h�pi j � �ri 1996+k hefur veri� f�r�ur inn � medaltekjur[[1:j-1]] og medaltekjur[[j]][1:k]
          
          fjoldi[[j]][k] = as.data.frame(tafla[j])[dalkurfjoldi][,1][k]   # fj�ldi � h�p j �ri� 1996+k
          
          tekjur[[j]][k] = as.data.frame(tafla[j])[dalkurstaerd][,1][k]   # tekjur �eirra � h�p j ari� 1996+k
          
    
          #print(tekjur[[j]][k])
          medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[[j]][k]      # me�altekjur �eirra � h�p j �ri� 1996+k � 2015 kr�num
          #medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)  #nunatekjur
          
          #print(class(fjoldi[k]))
          
        }    
      }  
      #print(fjoldi)
     # print("  woooo ")
      return(medaltekjur)
    }
    
    
    medalTek(tiundir,"Radstofunartekjur2", "Fjoldiihop", 10)
    
    # Skilar �rl�ti� ��ruv�si ni�urst��u en upprunalegi reikningurinn. h�r er eg ekki a� gera r�� fyrir a� �a� s�u jafn margir � �llum t�undum og t�und 3
    
    
    # 
    medalTek(tekjurOgGjold, "Radstofunartekjur2", "FjoldiIHop",17)
    
    
    medalTek(tekjurOgGjold, "Heildartekjur", "FjoldiIHop",17)
    
    
    # Get �� reikna� �t me�altekjur e�a me�algj�ld einstaklinga i mismunandi �j��felagsh�pum.
    
    # Vil geta reikna� �t me�aleignir e�a me�alskuldir - f� �au g�gn � sama form og "tiundir" og "tekjurOgGjold"
    # Bjartur s�r um a�fer�ina til a� finna me�alskuldirnar
    
    source('hagstofa.R')
    source('n���kr�nur.R') 
    
    aldursflokkar <- list(u24,aldur2529,aldur3034,aldur3539,aldur4044,aldur4549,aldur5054,aldur5559,aldur6066,y67)
    
    # Eigi� f� eftir aldri
    eigidfjeAldurs <- sapply(aldursflokkar, 
                             function(aldur) {
      t(
        leidretta(aldur$Eigidfje/aldur$Fjoldiihop) #listi eigi� f� tiltekna aldurh�psins hvert �r t�mabilsins
       )
    }
    )
    
    # H�fum n�vir�i eiginf�s � eigidfjeAldurs
    
    
    # �b��al�n eftir aldri
    ibskuldirAldurs <- sapply(aldursflokkar, function(aldur) 
      {
                 t(leidretta(aldur$Ibudarlan/aldur$Fjoldiihop)) #listi eigi� f� tiltekna aldurh�psins hvert �r t�mabilsins
       }
    )
    
    landshlutar = list(hofudborg, landsbyggd)
    
    ibskuldirLands <- sapply(landshlutar, function(hluti) 
    {
      t(leidretta(hluti$Ibudarlan/hluti$Fjoldiihop)) #listi eigi� f� tiltekna aldurh�psins hvert �r t�mabilsins
    }
    )
    
    
    
    # Vil geta teikna� t.d. skuldir � m�ti t.d. me�alra�st�funartekjum
    # me� mismunandi t�kn fyrir me�limi mismunandi h�pa (t.d. x fyrir u24, o fyrir 25-29 �ra) 
    # sem eru valnir �r h�pager� sem bera skal saman (aldursh�par e�a tekjut�undir e�a landshlutar) 
    # Mismunandi litir fyrir mismunandi �r
    
    
    # �sarnir eru stilltir �annig a� allir h�parnir liggi � l�nunni f(x) = x �ri� 1997.
    
    # Byrja � a� teikna landshlutana, h�fu�borgarsv��i vs. landsbygg� - �r�hyrningar vs. hringir
    # 17 litir fyrir 17 �r  - byrja � a� gera fyrir ari� 2013
    # sko�um svo hvernig h�parnir breytast milli �ra - sko�um ferla �eirra um state spacei�
    
    # Geri data frame sem geymir me�alra�st�funartekjur mismunandi h�pa, me� lista yfir medalrst � $nafnhops
    #   byrja � a� gera bara lista fyrir h�fu�borg og landsbygg�
    medaltekjur = medalTek(tekjurOgGjold, "Radstofunartekjur2", "FjoldiIHop",17)
    
    mrstlandid = list(1:2) # 1 er h�fu�borg, 2 er landsbyggd
    mrstlandid[1] = medaltekjur[16]
    mrstlandid[2] = medaltekjur[17]
    
    
    # er me� �b��al�naskuldirnar � ibskuldirLands
    
    minibskuldir = min(ibskuldirLands[,1], ibskuldirLands[,2])
    maxibskuldir = max(ibskuldirLands[,1], ibskuldirLands[,2])
    xmin = min( c(mrstlandid[[1]], mrstlandid[[2]])  )
    xmax = max( c(mrstlandid[[1]], mrstlandid[[2]])  )
    plot(mrstlandid[[1]], ibskuldirLands[,1], type = "b", ylim = c(minibskuldir, maxibskuldir),  xlab = "Me�al r��st�funartekjur [millj�nir kr.]", ylab = "Me�al �b��askuldir [millj�nir kr.]")
    lines(mrstlandid[[2]], ibskuldirLands[,2], type = "b", col = "red")
    
    # h�fum teikna� samband tekna og �b��askulda (sem fylgja ver�lagi)
    
    # vil �� teikna eina tvennd � einu (gagnvirknin) tekjur/skuldir landshlutanna 2002, tekjur/skuldir landshlutanna 2003
    # me� �r�hyrning og kassa fyrir sitt hvorn landshlutann
    
    # f�um tvenndirnar �r sitt hvorum.
    
    # vil f� 2x17 fylki �ar sem fyrra staki� � hverri r�� er tvennd h�fu�borgarinnar og s��ari er tvennd landsbygg�arinnar
    # (e�a nx17 og n-d, �egar �a� er veri� a� sko�a n aldursh�pa)
     
    
    
    tvennd = c(1:2) # 2 �v� �etta eru tvenndir
    ndir = list(rep(tvennd,2))
    
    
    #tvenndir = list(1:2)  
    for(i in 2:(17))  # 17 
    {
      ndir = c(ndir, list(rep(tvennd,2))) 
    
    }
    
    ndir
    # ndir er t.d. 4x17 fylki, 17 ra�ir, fyrir hvert ar. Tv�r tvenndir � hverju �ri �v� breytan tekur 2 gildi (2 landshlutar).
    
    
    
    
    #og svo restinni, � lykkju (�llum � lykkju b�st �g vi�)
    
    for(i in 1:(17))  # 17 
    {
      ndir[[i]][1] = mrstlandid[[1]][i]
      ndir[[i]][2] = ibskuldirLands[,1][i]
      ndir[[i]][3] = mrstlandid[[2]][i]
      ndir[[i]][4] = ibskuldirLands[,2][i]
    }
    
    # ndir hefur 17 ra�ir sem hver inniheldur 4 gildi, fyrstu tv� eru tvennd h�fu�borgarinnar, seinni tv� landsbygg�arinnar
    # Vil b�ta �eim inn einni � einu � mynd.
    
    # Set fyrst upp fyrir ari� 1997:
    rvktvennd1997 = ndir[[1]][c(1:2)]
    lbtvennd1997 = ndir[[1]][c(3:4)]
    plot(c(rvktvennd1997[1]), c(rvktvennd1997[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]",main ="Skulda- og tekna fer�alag")
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
    plot(c(rvktvennd2013[1]), c(rvktvennd2013[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]", main = "2013")
    lines(c(lbtvennd2013[1]), c(lbtvennd2013[2]), type = "b", col = "blue")
    
    
    i = 1
    # TEIKNAR TVENND FYRIR TVENND fra 1998 til 2013
    for(i in 2:17)
    { # 1996+i er �ri�
      # rau�ur er h�fu�borgarsv��i�, bl�r er landsbygg�in
        lines(c(ndir[[i]][1]), c(ndir[[i]][2]), type = "b", col = "red")
        lines(c(ndir[[i]][3]), c(ndir[[i]][4]), type = "b", col = "blue")
    }
    
    # Teiknum ��r eina � einu:
    
    # forsenda: �ri� 1997 hefur veri� teikna� (l�nur 420-423)
    i = 2
    # endurt�kum svo eftirfarandi 3 l�nur til a� b�ta inn einni tvennd � einu:
    lines(c(ndir[[i]][1]), c(ndir[[i]][2]), type = "b", col = "red")
    lines(c(ndir[[i]][3]), c(ndir[[i]][4]), type = "b", col = "blue")
    segments( c(ndir[[i-1]][1]), c(ndir[[i-1]][2]), c(ndir[[i]][1]), c(ndir[[i]][2]), lty = 2, col = "red") # teiknar fyrir RVK
    segments( c(ndir[[i-1]][3]), c(ndir[[i-1]][4]), c(ndir[[i]][3]), c(ndir[[i]][4]), lty = 2, col = "blue") # teiknar fyrir RVK
    i = i+1
    
    # Viljum sj� a� lokum heildarfer�alagi�. �.e. b�ta inn striki � milli 1. og 17. tvenndar
    segments( c(ndir[[1]][1]), c(ndir[[1]][2]), c(ndir[[17]][1]), c(ndir[[17]][2]), col = "red") # teiknar fyrir RVK
    segments( c(ndir[[1]][3]), c(ndir[[1]][4]), c(ndir[[17]][3]), c(ndir[[17]][4]), col= "blue") # teiknar fyrir landsbygg�ina
    
    
    
    # hallatala striksins:
    # RVK:
    (ndir[[17]][2] - ndir[[1]][2])/(ndir[[17]][1] - ndir[[1]][1])
    # LB:
    (ndir[[17]][4] - ndir[[1]][4])/(ndir[[17]][3] - ndir[[1]][3])
    
    # RVK:
      # �ri� 1998 var hlutfall �b��askulda a m�ti tekjum a� me�altali:
      lantekjur98 = ndir[[1]][2]/ndir[[1]][1]
      # �ri� 2013 var hlutfall �b��askulda � m�ti tekjum a� me�altali:
      lantekjur13 = ndir[[17]][2]/ndir[[17]][1]
      # Hlutfalli� �ar a milli:
      lantekjur13/lantekjur98
    # LB:
      # �ri� 1998 var hlutfall �b��askulda a m�ti tekjum a� me�altali:
      lantekjur98 = ndir[[1]][4]/ndir[[1]][3]
      lantekjur98
      # �ri� 2013 var hlutfall �b��askulda � m�ti tekjum a� me�altali:
      lantekjur13 = ndir[[17]][4]/ndir[[17]][3]
      lantekjur
      # Hlutfalli� �ar a milli:
      lantekjur13/lantekjur98
    
    # Endurt�kum svo sama fyrir tekjut�undirnar! S�r i lagi me�almanninn.
    
    
    
    # B� til fylki� fyrir tvenndirnar(l�nur 387-400), nema fyrir 10 breytur (t�undir) � sta� 2ja
    
    tvennd = c(1:2) # 2 �v� �etta eru tvenndir
    ndir = list(rep(tvennd,10)) 
    
    
    #tvenndir = list(1:2)  
    for(i in 2:(17))  # 17 
    { #c(ndir, list(rep(nd,m))) skapar m tvenndir
      
      ndir = c(ndir, list(rep(tvennd,10))) 
      
    }
    # ndir er 20x17 fylki, hver r�� taknar �r, �a� eru 10 gildi � hverju �ri.
    ndir
    
    # Alh�fi yfir � fall sem byr til fylki fyrir n tvenndir
    
    # Notkun: ndir = ntvenndir(n)
    # Fyrir: n er heiltala >= 2
    # Eftir: ndir er (2*n)x17 fylki sem getur geymt gildi � n tvenndum yfir �rin 17
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
    
    
    # �arf svo a� safna saman g�gnunum sem � a� vinna ur:
    # me�altekjur tiundanna, �b��askuldir tiundanna (b��i �r sama gagnasafni)
    # me�altekjur aldursh�panna, �bu�askuldir aldursh�panna (�r mismunandi s�fnum.)
    # Au�velt a� sko�a anna� en �b��askuldir - alh�fi � fall �egar �etta s�rtilfelli er komi�.
    
    
    # N� � t�undag�gnin.
    
    
    
    tiutek = medalTek(tiundir,"Radstofunartekjur2", "Fjoldiihop", 10)
    tiuskuld = medalTek(tiundir,"Ibudalan", "Fjoldiihop", 10)
    
    tiugogn = ntvenndir(10) # hl��um g�gnunum hinga� inn
    tiugogn
    
    
    
    # byrja a a� hla�a g�gnunum handvirkt, profa mig �fram:
    
    tiugogn # t�ug�gn inniheldur 17 10 staka lista
    tiutek # tiutek inniheldur 10 17 staka lista
    tiugogn[[1]] # fyrsta ari�
    tiugogn[[1]][1] = tiutek[[1]][1] # tekjur fyrsta ars fyrstu tiundinnar
    tiugogn[[1]][2] = tiuskuld[[1]][1] # skuldir fyrsta ars fyrstu t�undinnar
    tiugogn[[1]][3] = tiutek[[2]][1]  # tekjur fyrsta ars annarar t�undarinnar
    tiugogn[[1]][4] = tiuskuld[[2]][1]
    # ...
    tiugogn[[2]][9] = tiutek[[5]][2]
    tiugogn[[2]][10] = tiuskuld[[5]][2]
    
    tiugogn[[j]][k] = tiuskuld[[i]][j]] # j er �r, i er tiund, k r��st af i
    
    # k = 2*i-1 og 2*i
    
    # tek �etta svo saman � lykkju:
    
    for (j in 1:17)
    {   # Hleyp yfir �rin. � �ri 1996+j er tvenndin fyrir tiund i � s�tum 2i-1 (tekjur) og 2i (skuld)
        # sj� d�mi a� ofan
        
        for(i in 1:10)
        {
            k = 2*i-1
            tiugogn[[j]][k] = tiutek[[i]][j]
            k = k+1
            tiugogn[[j]][k] = tiuskuld[[i]][j]
            
        }
        
    }
    tiugogn
    #tiugogn inniheldur ��, fyrir tekjut�undirnar, (tekjur,skuldir) fyrir tiundirnar t�u fyrir hvert ar
    
    # Vil f� �etta � mynd
    
    # Vil f� fall sem velur t�undir til a� sko�a, t.d. 4., 5. og 6. t�und
    # Byrja a a� teikna ��r fyrir ari� 1997
    
    tiundir1997 = tiugogn[[1]]
    # vil hafa mismunandi t�kn fyrir tiundirnar. B� til lista af 10 t�knum sem einkenna �� t�undirnar.
    # use the pch= option to specify symbols,  pch = 1 er tiund i, pch = i er tiund i, 1<=i<=10
    # og mismunandi liti eftir �rum
    
    # hleyp yfir tiundirnar sem eru valdnar.
    
    # g.r.f. a� 4., 5. og 6. hafi veri� valdnar. �� er parameter � falli� listinn c(4,5,6)
    # tiundir = c(4,5,6)
    
    #samanber:
#    plot(c(rvktvennd2013[1]), c(rvktvennd2013[2]), type = "b", col = "red", ylim = c(minibskuldir, maxibskuldir), xlim = c(xmin,xmax),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]", main = "2013")
    
    # �etta b�r til mynd fyrir bara �ri� 1997 - en �ri� er 1996+j, j getur or�i� anna� en 1 og �� er �etta almennara
    # g.r.f. a� tiugogn hafi veri� hla�i� inn. geri einnig r�� fyrir a� tiutek og tiuskuld hafi veri� hla�i� inn
    # t.d. ar1 = 1997, ar2 = 2013  og tekjur = c(4,5,6)
    tiundaferdalag = function(valtiundir, ar1, ar2)
    {
        print(ar1)
        print(ar2)
        # h�fum liti fyrir �rin 17. fyrstu 10 litirnir eru endurn�ttir �r tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
       
        # Getur lines readjusta� xlim og ylim? Nei. �arf a� skilgreina m�rkin � asunum fyrirfram.
        # vel l�gmarki� og h�marki� �r tekjunum og skuldunum fyrir valdnar tiundir, yfir �ll arin, til a� geta plotta�.
         {   # hlaupum yfir arin og teiknum tekju/skulda tvenndir fyrir valdnar t�undir fyrir arin
            
            # pr�fum �etta �ar sem ar1 = ar2 = 1997
            
            tekjur = rep(0,10) # b� til vigur fyrir tekjurnar og set inn � fyrir ��r tiundir sem sko�a skal
            skuldir = rep(0,10) # b� til vigur fyrir skuldirnar og set inn � fyrir valtiundirnar
        
            # fyrir hvert �r, �� set �g tekjur/skuldir inn � valdnar t�undir
            for(i in valtiundir)
            {   # hle� inn �eim skuldum og tekjum sem ver�a nota�ar
                k = 2*i-1
                tekjur[[i]] = tiugogn[[j]][k]
                k = k+1
                skuldir[[i]] = tiugogn[[j]][k]
                
            }
            # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir �rin fr� ar1 og upp �anga� sem komi� er 

    
            
            # sett upp mynd fyrir fyrstu v�ldu t�undina
                # (breyta �arf labelunum � �sana ef breytunum er breytt)

            if(j == (ar1-1996))
            {   # geri bara plot � fyrsta skipti�, eftir �a� baralines.
                plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldos), max(skuldos)), xlim = c(min(tekjos), max(tekjos)),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                
            }
            else
            {
                
                lines(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[j], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]", main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                # vil segmenta saman fyrstu valdna t�und (4) � ari� 1998 og �rinu � undan
                # hugsa �t fra tiugogn
                segments(tiugogn[[j-1]][2*valtiundir[1]-1], tiugogn[[j-1]][2*valtiundir[1]],   tiugogn[[j]][2*valtiundir[1]-1], tiugogn[[j]][2*valtiundir[1]], col = litir[j])
            }

   
            # og b�ti svo vi� restinni:
            if(length(valtiundir) > 1)
            {   # vil geta teikna� staka t�und �n �ess a� fa villu
                for(k in valtiundir[2:length(valtiundir)])
                {
                    
                    lines( tekjur[k], skuldir[k], type = "b", col = litir[j], pch = k)
                    # er �� b�inn a� b�ta vi� punktum fyrir hinar tiundirnar
                    # �arf a� b�ta vi� segments sem tengjast s��astli�nu �ri 
                    if(j > 1)
                    {  # ���ir l�ti� a� teikna strik � ari� � undan � fyrsta �rinu
                        segments(tiugogn[[j-1]][2*k-1], tiugogn[[j-1]][2*k],   tiugogn[[j]][2*k-1], tiugogn[[j]][2*k], col = litir[j])    
                    }
                    
                }                                
            }

        }
    }

    
    tiundaferdalag(c(4,5,6,7), 1997,2009)       
        # valtiundir[1] er fyrsta valdna t�undin - �urfum svo a� velja yfir �rin sem (ar1,ar2)

        
        tekjos = c()
        skuldos = c()
        for( p in valtiundir ) 
        {   # p t�knar valdna tiund
            for(s in c((ar1-1996):(ar2-1996)))
            {   # s t�knar ar
                tekjos = c(tekjos, tiutek[[p]][s])
                skuldos = c(skuldos, tiuskuld[[p]][s])            
            }

        }        
        # tekjos og skuldos geyma tekjur og skuldir valinna tiunda a v�ldum arum
        # svo vi� getum stillt �sana i plotinu

        for(j in (ar1-1996):(ar2-1996))

    # �etta fall dugar eiginlega ekki til a� teikna fer�alagi�
    # �v� �a� plottar yfir fyrir hvert �r sem b�tist vi�. 
    
    # hugsa um 1 �r � einu, geri fall sem teiknar tekjutiundirnar fyrir stakt �r
    # b�r �� til plot fyrir �a� �r og by til anna� fall sem tekur inn �rabil, gerir lines fyrir �rin og b�tir �eim inn
    # "tiundafer�alag" getur �� kalla� � �au
    
    
    
    tiundirarid = function(valtiundir, arid)
    {
        
        # h�fum liti fyrir �rin 17. fyrstu 10 litirnir eru endurn�ttir �r tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
        

            
            # pr�fum �etta �ar sem ar1 = ar2 = 1997
            
            tekjur = rep(0,10) # b� til vigur fyrir tekjurnar og set inn � fyrir ��r tiundir sem sko�a skal
            skuldir = rep(0,10) # b� til vigur fyrir skuldirnar og set inn � fyrir valtiundirnar
            
            for(i in valtiundir)
            {
                k = 2*i-1
                tekjur[[i]] = tiugogn[[arid-1996]][k]
                k = k+1
                skuldir[[i]] = tiugogn[[arid-1996]][k]
                
            }
            # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir �rin fr� ar1 og upp �anga� sem komi� er 
            
            # sett upp mynd fyrir fyrstu v�ldu t�undina
            # (breyta �arf labelunum � �sana ef breytunum er breytt)
            plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[arid-1996], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]", main = as.character(arid) )
            # og b�ti svo vi� restinni
            if(length(valtiundir) > 1)
            {   # vil geta teikna� staka t�und �n �ess a� fa villu
                for(k in valtiundir[2:length(valtiundir)])
                {
                    
                    lines( tekjur[k], skuldir[k], type = "b", col = litir[arid-1996], pch = k)
                    
                }                                
            }
            
        
    }    
    
    tiundirarid(c(4,5,6), 1998)
    par(new=TRUE)
    tiundirarid(c(4,5,6), 1999)
    # s�kkar
    
    # get fyrst plotta� og svo linea�, �a� �tti a� virka. geri allt fyrir fyrsta �ri�, hleyp svo yfir restina af �runum.
    
    tiundirarin = function(valtiundir, arid)
    {
        
        # h�fum liti fyrir �rin 17. fyrstu 10 litirnir eru endurn�ttir �r tekjudreifingarmyndinni
        litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")
        
        
        
        # Byrja a a� teikna fyrir fyrsta ari�
        
        tekjur = rep(0,10) # b� til vigur fyrir tekjurnar og set inn � fyrir ��r tiundir sem sko�a skal
        skuldir = rep(0,10) # b� til vigur fyrir skuldirnar og set inn � fyrir valtiundirnar
        
        for(i in valtiundir)
        {
            k = 2*i-1
            tekjur[[i]] = tiugogn[[arid-1996]][k]
            k = k+1
            skuldir[[i]] = tiugogn[[arid-1996]][k]
            
        }
        # tekjur og skuldir geyma tekjur og skuldir valinna tiunda fyrir �rin fr� ar1 og upp �anga� sem komi� er 
        
        # sett upp mynd fyrir fyrstu v�ldu t�undina
        # (breyta �arf labelunum � �sana ef breytunum er breytt)
        plot(tekjur[valtiundir[1]], skuldir[valtiundir[1]], type = "b", col = litir[arid-1996], pch = valtiundir[1], ylim = c(min(skuldir), max(skuldir)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Me�al r��st�funartekjur [millj�nir kr. / �r]", ylab = "Me�al �b��askuldir [millj�nir kr.]", main = as.character(arid) )
        # og b�ti svo vi� restinni
        if(length(valtiundir) > 1)
        {   # vil geta teikna� staka t�und �n �ess a� fa villu
            for(k in valtiundir[2:length(valtiundir)])
            {
                
                lines( tekjur[k], skuldir[k], type = "b", col = litir[arid-1996], pch = k)
                
            }                                
        }
        
        
    }   
    
    
    
    # get si�an yfirf�rt yfir � aldurshopaferdalag.
    
    
    
    
    
