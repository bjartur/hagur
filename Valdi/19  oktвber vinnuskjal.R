

arin = c(1997:2013)
litir = c("red", "green", "blue", "black", "orange", "brown", "pink", "purple", "royalblue", "orchid4", "orange", "pink", "salmon4", "oldlace", "ivory4", "black", "green")

pchtakn = c("hringur", "þríhyrningur", "plús", "kross", "tígull", "þríhyrningur niður", "fernings kross", "stjarna", "tíguls plús", "hring plús")

aldurshopanofn = c("u24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-66", "y67")

landshlutanofn = c("höfuðborgarsvæði", "landsbyggð")

source("Radstofunartekjur.R")
source("../eignir.R", encoding="UTF-8")
source("../tekjur.R", encoding="UTF-8")

aldursflokkar <- lapply(seq_along(eignir), function(i) {
    merge(eignir[i], tekjurOgGjold[i], by=c("Ar","Fjoldiihop"))
})

landshlutaeignir = list(hofudborg, landsbyggd)

landshlutar  <- lapply(seq_along(landshlutaeignir), function(i) {
    merge(landshlutaeignir[i], tekjurOgGjold[i+15], by=c("Ar","Fjoldiihop"))
})


# Notað í fjarhagsstadatiunda 
tiundirdataf = rep(list(1), 10)
for(i in 1:10)
{
    tiundirdataf[[i]] = as.data.frame(tiundir[[i]])
}


# Þau dálkanöfn sem ekki þarf að deila með:
# MedaltalEigna, MidgildiEigna, MedaltalSkulda, MidgildiSkulda, Fjoldiihop, MedaltalSkuldaSkuldsettra, Medaltekjur, MidgildiTekna 

ekkideila = c("MedaltalEigna", "MidgildiEigna", "MedaltalSkulda", "MidgildiSkulda", "Fjoldiihop", "MedaltalSkuldaSkuldsettra", "Medaltekjur", "MidgildiTekna")




# Aðal föllin eru efst,  


# Fyrir:  tiundirdataf er listi af dataframeum fyrir tiundirnar, sbr. kóðinn að ofan 
# max 3 breytur skoðaðar í einu 
# 
fjarhagsstadatiunda = function(hoparvaldir, breyturvaldar)
{
	
	if(length(breyturvaldar) > 3)
	{
		return
	}
    breytunumer = 1:length(breyturvaldar)
    ymin = min( stadla(tiundirdataf, breyturvaldar[1])[,hoparvaldir] )
    ymax = max( stadla(tiundirdataf, breyturvaldar[1])[,hoparvaldir] )
    
    if(length(breyturvaldar) > 1)
    {
        for(j in breytunumer[2:length(breytunumer)])
        {
            ymin = min( c(ymin, stadla(tiundirdataf, breyturvaldar[j])[,hoparvaldir])  )
            ymax = max( c(ymax, stadla(tiundirdataf, breyturvaldar[j])[,hoparvaldir])  )
        }
    }


    
    plot(arin, rep(0,17), xlab = "Ár", type = "l", ylab = "Meðaltal[m. króna]", xaxp = c(arin[1],arin[17], 16), yaxp = c(floor(ymin),ceiling(ymax), 12), ylim = c(ymin,ymax) ) 
    
    usr = par("usr")
    # merki fyrst fyrir fyrsta hópinn
    sma = (usr[2]-usr[1])/30
    x0 = usr[1]+ sma 
    x1 = x0 + sma
    y0 = usr[4] - (usr[4]-usr[3])/20
    
    segments(x0,y0, x1,y0, col = litir[hoparvaldir[1]])
    text(x1 + sma, y0, paste(" : ", "tíund", as.character(hoparvaldir[1]) ) )
    
    if(length(hoparvaldir) > 1)
    {    
        tel = 2
        for(t in hoparvaldir[2:length(hoparvaldir)])
        {   # svo fyrir restina af völdum hópum
            y0 = y0 - (usr[4]-usr[3])/20
            segments(x0,y0, x1,y0, col = litir[t])
            text(x1 + sma, y0, paste(" : ", "tíund", as.character(hoparvaldir[tel]) ) )
            tel = tel+1
            
        }
    }
    
    undirtitill = breyturvaldar[1]

    if(length(breyturvaldar) > 1)
    {
        for(j in 2:length(breyturvaldar))
        {
            if(j == 2)
            {
                undirtitill = paste(undirtitill, "," ,breyturvaldar[j], "(strik)")    
            }else if ( j ==3)
            {
                undirtitill = paste(undirtitill, "," ,breyturvaldar[j], "(punktar)")    
            }else
			{
				undirtitill = paste(undirtitill, "," ,breyturvaldar[j])    
			}
            
            
        }
    }
    title(main = "Tíundir", sub= undirtitill)   

    for(i in 1:length(breyturvaldar))
    {  # 
        
        stadladgildi = stadla(tiundirdataf, breyturvaldar[i])    
        
        # gildin á fyrstu i breytunum hafa verið staðlaðar m.t.t. verðbolgu OG deilt með fjölda í hverjum hóp!  XXXX
        # og er nýjasta breytan sem unnið er með í dataframeinu stadladgildi
        # stadladgildi ætti að geyma valdna breytu staðlaða m.t.t. verðbólgu yfir árin 17 fyrir alla hópana
        # stadladgildi[,j] ætti þá að geyma valdna breytu staðlaða m.t.t. yfir arin 17 fyrir hóp j
        # j er í hoparvaldir
        
        for(j in hoparvaldir)
        {
            lines(arin, stadladgildi[,j],   lty = i, col = litir[j])
        }
        
    }
}


# Notkun: fjarhagsstadahopa = function(hoparvaldir, breyturvaldar, aldurstrue)
# Fyrir:
# hopar er talnavigur sem merkir hóparna sem skoða skal, þar sem c(1:10) eru allir aldurshóparnir, c(16:17) eru landshlutarnir
    # c(1:4) eru yngstu 4 aldurshóparnir
#  breyturvaldar er strengjavigur sem inniheldur dálkanöfnin úr hagstofa.R á breytunum sem setja skal á myndina 
    # ferlanir sem samsvara þeim verða plottaðir
    # t.d. breyturvaldar = c("EigidFje", "EigidFjeiFasteign", "EigidFjeAnnad)
    # breytur er 3ja staka hlutvigur i´eftirfarandi:
    # c( "Eigniralls",  "Fasteignir", "Okutaeki", "Innlan", "Verdbrjef", "AdrarEignir", "Skuldiralls",  "Ibudarlan", 
    # "Adrarskuldir", "EigidFje", "EgidFjeiFasteign", "Eigidfje annad", 
    # "Fjoldiihop", "MedaltalEigna", "MidgildiEigna", "MedaltalSkulda", "MidgildiSkulda", "Fjoldiihop", "MedaltalSkuldaSkuldsettra", "Medaltekjur", "MidgildiTekna")

# aldurstrue er boolean sem er TRUE ef aldurshóparnir eru skoðaðir, FALSE ef landshlutarnir eru skoðaðir



# Eftir: ferlarnir fyrir breyturnar í breytur hafa verið teiknaðir upp a sömu mynd, á móti árunum 1997-2013. 
#   Ásarnir á myndinni eru stilltir þ.a. þeir sýna fra minnsta að hæsta gildi þess sem er teiknað
#   Gildin á y-ásnum hafa verið stöðluð m.t.t. verðbolgu og allt er synt í sept 2015 krónum.   
# Litnirir samsvara hópunum sem hafa verið valdir. i-ti hópurinn hefur i-ta litinn úr 'litir'

fjarhagsstadahopa = function(hoparvaldir,breyturvaldar, aldurstrue)
{
    
    
    breytunumer = 1:length(breyturvaldar) # til að hlaupa yfir
   
    
    if(aldurstrue)
    {

        
        if(breyturvaldar[1] %in% ekkideila)
        {
            ymin = min( stadla(aldursflokkar, breyturvaldar[1], FALSE)[,hoparvaldir] )
            ymax = max( stadla(aldursflokkar, breyturvaldar[1], FALSE)[,hoparvaldir] ) 
        } else
        {
            ymin = min( stadla(aldursflokkar, breyturvaldar[1])[,hoparvaldir] )
            ymax = max( stadla(aldursflokkar, breyturvaldar[1])[,hoparvaldir] )
        }      
        # þarf að setja mörk á asana
        # vil fá dataframe fyrir allar breyturnar. fæ valda hópa með stadla(aldursflokkar,breyturvaldar[i])[,hopar]

        
        if(length(breyturvaldar) > 1)
        {
            for(j in breytunumer[2:length(breytunumer)])
            {
                
                
                if(breyturvaldar[1] %in% ekkideila)
                {
                    ymin = min( c(ymin, stadla(aldursflokkar, breyturvaldar[j], FALSE)[,hoparvaldir])  )
                    ymax = max( c(ymax, stadla(aldursflokkar, breyturvaldar[j], FALSE)[,hoparvaldir])  )
                }
                else
                {
                                        
                    ymin = min( c(ymin, stadla(aldursflokkar, breyturvaldar[j])[,hoparvaldir])  )
                    ymax = max( c(ymax, stadla(aldursflokkar, breyturvaldar[j])[,hoparvaldir])  )
                }
            }
		}
    # set upp ramma til að teikna á. þarf fyrst að finna hæsta og lægsta gildið til að stilla rammann
		plot(arin, rep(0,17), xlab = "Ár", type = "l", ylab = "Meðaltal [m. króna]", xaxp = c(arin[1],arin[17], 16),yaxp = c(floor(ymin),ceiling(ymax), 12), ylim = c(ymin,ymax) ) 
			
		usr = par("usr")
		# merki fyrst fyrir fyrsta hópinn
		sma = (usr[2]-usr[1])/30
		x0 = usr[1]+ sma 
		x1 = x0 + sma
		y0 = usr[4] - (usr[4]-usr[3])/20
		
		segments(x0,y0, x1,y0, col = litir[hoparvaldir[1]])
		text(x1 + sma, y0, paste(" ", aldurshopanofn[hoparvaldir[1]] ) )		
	    if(length(hoparvaldir) > 1)
		{    
			tel = 2
			for(t in hoparvaldir[2:length(hoparvaldir)])
			{   # svo fyrir restina af völdum hópum
				y0 = y0 - (usr[4]-usr[3])/20
				segments(x0,y0, x1,y0, col = litir[t])
				text(x1 + sma, y0, paste(" ", aldurshopanofn[hoparvaldir[tel]] ) )
				tel = tel+1
			}
		}			
			
		undirtitill = breyturvaldar[1]	
		if(length(breyturvaldar) > 1)
		{           
			for(j in 2:length(breyturvaldar))
			{
				if(j == 2)
				{
					undirtitill = paste(undirtitill, "," ,breyturvaldar[j], "(strik)") 
				}else if ( j ==3)
				{
					undirtitill = paste(undirtitill, "," ,breyturvaldar[j], "(punktar)")    
				}else
				{
					undirtitill = paste(undirtitill, "," ,breyturvaldar[j])    
				}
			
			}
		}
        title(main = "Aldurshópar", sub= undirtitill)   
    
# Þarf að hafa mismunandi utlit a ferlunum fyrir mismunandi breytur
# MAX 3 mismunandi breytur i einu. type = "l", "b", "p"

        for(i in breytunumer)
        {  # 
            if(breyturvaldar[i] %in% ekkideila)
			{
				stadladgildi = stadla(aldursflokkar, breyturvaldar[i], FALSE)    
			}
			else
            {
				stadladgildi = stadla(aldursflokkar, breyturvaldar[i])    
            }
            # gildin á fyrstu i breytunum hafa verið staðlaðar m.t.t. verðbolgu OG deilt með fjölda í hverjum hóp!  XXXX
            # og er nýjasta breytan sem unnið er með í dataframeinu stadladgildi
            # stadladgildi ætti að geyma valdna breytu staðlaða m.t.t. verðbólgu yfir árin 17 fyrir alla hópana
            # stadladgildi[,j] ætti þá að geyma valdna breytu staðlaða m.t.t. yfir arin 17 fyrir hóp j
            # j er í hoparvaldir

            for(j in hoparvaldir)
            {
         
                lines(arin, stadladgildi[,j],   lty = i, col = litir[j])
            }
            
        }

    }
    
    else
    {
		hoparvaldir = 1:2  # landshlutar er 2ja lista listi, 16:17 passa ekki sem vísar 
		
        # þarf að setja mörk á asana
        # vil fá dataframe fyrir allar breyturnar. fæ valda hópa með stadla(landshlutar,breyturvaldar[i])[,hopar]
       if(breyturvaldar[1] %in% ekkideila)
        {
            ymin = min( stadla(landshlutar, breyturvaldar[1], FALSE)[,hoparvaldir] )
            ymax = max( stadla(landshlutar, breyturvaldar[1], FALSE)[,hoparvaldir] ) 
        } else
        {
            ymin = min( stadla(landshlutar, breyturvaldar[1])[,hoparvaldir] )
            ymax = max( stadla(landshlutar, breyturvaldar[1])[,hoparvaldir] )
        }      
        # þarf að setja mörk á asana
        # vil fá dataframe fyrir allar breyturnar. fæ valda hópa með stadla(landshlutar,breyturvaldar[i])[,hopar]

        
        if(length(breyturvaldar) > 1)
        {
            for(j in breytunumer[2:length(breytunumer)])
            {	# hleyp yfir breyturnar 
                
                
                if(breyturvaldar[j] %in% ekkideila)
                {
                    ymin = min( c(ymin, stadla(landshlutar, breyturvaldar[j], FALSE)[,hoparvaldir])  )
                    ymax = max( c(ymax, stadla(landshlutar, breyturvaldar[j], FALSE)[,hoparvaldir])  )
                }
                else
                {
                                        
                    ymin = min( c(ymin, stadla(landshlutar, breyturvaldar[j])[,hoparvaldir])  )
                    ymax = max( c(ymax, stadla(landshlutar, breyturvaldar[j])[,hoparvaldir])  )
                }
            }
		}
		

		
        # set upp ramma til að teikna á. þarf fyrst að finna hæsta og lægsta gildið til að stilla rammann
        plot(arin, rep(0,17), xlab = "Ár", type = "l", ylab = "Meðaltal [m. króna]", xaxp = c(arin[1],arin[17], 16), ylim = c(ymin,ymax) ) 
        
		usr = par("usr")
		# merki fyrst fyrir fyrsta hópinn
		sma = (usr[2]-usr[1])/30
		x0 = usr[1]+ sma 
		x1 = x0 + sma
		y0 = usr[4] - (usr[4]-usr[3])/20
		
		# svo fyrir hinn hópinn (landsbyggð)
		segments(x0,y0, x1,y0, col = litir[16])
		text(x1 + 1.5*sma, y0, paste("  ", landshlutanofn[1] ) )		
	    
		y0 = y0 - (usr[4]-usr[3])/20
		segments(x0,y0, x1,y0, col = litir[17 ])
		text(x1 + sma, y0, paste("  ", landshlutanofn[2] ) )
		
		
		
        undirtitill = breyturvaldar[1]
		if(length(breyturvaldar) > 1)
		{
		
			for(j in 2:length(breyturvaldar))
			{
					if(j == 2)
					{
						undirtitill = paste(undirtitill, "," ,breyturvaldar[j], "(strik)") 
					}else if ( j ==3)
					{
						undirtitill = paste(undirtitill, "," ,breyturvaldar[j], "(punktar)")    
					}else
					{
						undirtitill = paste(undirtitill, "," ,breyturvaldar[j])    
					}
				
			}
		}
		
        title(main = "Landshlutar - höfuðborgarsvæði vs. landsbyggð", sub = undirtitill)   
            
        for(i in breytunumer)
        {  
            if(breyturvaldar[i] %in% ekkideila)
			{
				stadladgildi = stadla(landshlutar, breyturvaldar[i], FALSE)    
			}
			else
            {
				stadladgildi = stadla(landshlutar, breyturvaldar[i])    
            }
            # gildin á fyrstu i breytunum hafa verið staðlaðar m.t.t. verðbolgu OG deilt með fjölda í hverjum hóp!  XXXX
            # og er nýjasta breytan sem unnið er með í dataframeinu stadladgildi
            # stadladgildi ætti að geyma valdna breytu staðlaða m.t.t. verðbólgu yfir árin 17 fyrir alla hópana
            # stadladgildi[,j] ætti þá að geyma valdna breytu staðlaða m.t.t. yfir arin 17 fyrir hóp j
            # j er í hoparvaldir
            
            for(j in 1:2)
            {
 
                lines(arin, stadladgildi[,j],   lty = i, col = litir[j+15])
            }
            
        }

    }

    
}


# Notkun: tiundaferdalag(valtiundir, valbreyta, ar1, ar2)
# Fyrir: 
	#	valtiundir eru valdar tíundir, c(i:j) fyrir tíundir i til j, 1 <= i <= j <= 10
	#	valbreyta er valin breyta sem verður á lóðrétta ásinum, dálkanafn úr töflunni tiundir
	# 1997 <= ar1 <= ar2 <= 2013 
	
# Eftir: 
	# valbreyta hefur verið plottuð á móti ráðstöfunartekjum fyrir valtiundir frá ar1 til ar2. Samfelldur ferill hefur verið teiknaður 
tiundaferdalag = function(valtiundir, valbreyta, ar1, ar2)
{
	tiutek = medalTek(tiundir, "Radstofunartekjur2", "Fjoldiihop", 10)
	tiubreyta = medalTek(tiundir, valbreyta, "Fjoldiihop", 10)
	
	tiugogn = ntvenndir(10) # hlöðum gögnunum hingað inn
       
    for (j in 1:17)
    {   # Hleyp yfir árin. Á ári 1996+j er tvenndin fyrir tiund i í sætum 2i-1 (tekjur) og 2i (breyta)
        
        for(i in 1:10)
        {
            k = 2*i-1
            tiugogn[[j]][k] = tiutek[[i]][j]
            k = k+1
            tiugogn[[j]][k] = tiubreyta[[i]][j]
            
        }
	}

	
	tekjos = c()
	breytos = c()
	for( p in valtiundir ) 
	{   # p táknar valdna tiund
		for(s in c((ar1-1996):(ar2-1996)))
		{   # s táknar ar
			tekjos = c(tekjos, tiutek[[p]][s])
			breytos = c(breytos, tiubreyta[[p]][s])            
		}
		
	}                
	
	# tekjos geymir ráðstöfunartekjur valtiunda frá ar1 til ar2
	# breytos geymir valbreyta valtiunda frá ar1 til ar2
	# (gert til að stilla ásana í myndinni, min og max)
		
	for(j in (ar1-1996):(ar2-1996))    
		{   # hlaupum yfir arin og teiknum tekju/breytu tvenndir fyrir valdnar tíundir fyrir arin
		# ath. upprunalega fallið 
			
		   
			tekjur = rep(10) # bý til vigur fyrir tekjurnar og set inn í fyrir þær tiundir sem skoða skal
			breytan = rep(10) # bý til vigur fyrir breytuna sem skoða skal og set inn í fyrir valtiundirnar
			
			# fyrir hvert ár, þá set ég tekjur/breytan inn í valdnar tíundir
			for(i in valtiundir)
			{   # hleð inn þeim breytu og tekjum sem verða notaðar
				k = 2*i-1
				tekjur[[i]] = tiugogn[[j]][k]
				k = k+1
				breytan[[i]] = tiugogn[[j]][k]
				
			}
			# tekjur og breytan geyma tekjur og valbreyta valinna tiunda fyrir árin frá ar1 og upp þangað sem komið er (1996+j held ég)
			
			xmin = floor(min(tekjos))
			xmax = ceiling(max(tekjos))
			
			ymin = floor(min(breytos))
			ymax = ceiling(max(breytos))
		
   
            
		
			# xaxp segir til um fjölda ticks í grafið fyrir x-ás, yaxp fyrir y-ás
			# t.d. xaxp = c(arin[1],arin[17], 8)
			
			# sett upp mynd fyrir fyrstu völdu tíundina
			# (breyta þarf labelunum á ásana ef breytunum er breytt)
			
			if(j == (ar1-1996))
			{   # geri bara plot í fyrsta skiptið, eftir það bara lines.
				plot(tekjur[valtiundir[1]], breytan[valtiundir[1]], type = "b", pch = valtiundir[1], ylim = c(min(breytos), max(breytos)), col = "red", xlim = c(min(tekjos), max(tekjos)),  xlab = "Meðal ráðstöfunartekjur [m. kr. / ár]", ylab = paste("Meðal ", valbreyta, " [m. kr.]"), xaxp = c(xmin, xmax, 16 ), yaxp = c(ymin, ymax, 20) , main = paste(as.character(ar1), " - ", as.character(ar2)) )                

				usr = par("usr")
				
				p = 1
				for(t in valtiundir)
				{   
				    text(usr[1]+ (usr[2]-usr[1])/14, usr[4] - p*(usr[4]-usr[3])/20 , paste(pchtakn[t], ": tíund", as.character(t) ))                 
				    p = p+1
				}                
                
			}
			else
			{
				
				lines(tekjur[valtiundir[1]], breytan[valtiundir[1]], type = "b", pch = valtiundir[1], ylim = c(min(breytan), max(breytan)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Meðal ráðstöfunartekjur [m kr. / ár]", ylab = paste("Meðal ", valbreyta, " [m. kr.]"), main = paste(as.character(ar1), " - ", as.character(ar2)) )                
				# vil segmenta saman fyrstu valdna tíund (4) á arið 1998 og árinu á undan
				# hugsa út fra tiugogn
				segments(tiugogn[[j-1]][2*valtiundir[1]-1], tiugogn[[j-1]][2*valtiundir[1]],   tiugogn[[j]][2*valtiundir[1]-1], tiugogn[[j]][2*valtiundir[1]])
			}
            

			
			# og bæti svo við restinni:
			if(length(valtiundir) > 1)
			{   # vil geta teiknað staka tíund án þess að fa villu
				for(k in valtiundir[2:length(valtiundir)])
				{
					if(j == 1)
					{
					    lines( tekjur[k], breytan[k], type = "b",  pch = k, col = "red")
					}
                    else
                    {
                        lines( tekjur[k], breytan[k], type = "b",  pch = k)                        
                    }
                    
				
					
                    # er þá búinn að bæta við punktum fyrir hinar tiundirnar
					# þarf að bæta við segments sem tengjast síðastliðnu ári 
					if(j > 1)
					{  # þýðir lítið að teikna strik á arið á undan á fyrsta árinu
						segments(tiugogn[[j-1]][2*k-1], tiugogn[[j-1]][2*k],   tiugogn[[j]][2*k-1], tiugogn[[j]][2*k])    
					}
					
				}                                
			}
			
		}	
	




}




hopaferdalag = function(valhopar, valbreyta, ar1, ar2, aldurstrue)
{
    
    if(aldurstrue)
    {
        n = 10 # 10 hópar mögulegir 
        
        # hleð fyrst gögnunum inn 
        
        medalrsthopar = medalTek(aldursflokkar, "Radstofunartekjur2", "Fjoldiihop", 10)
       
        hopabreyta = medalTek(aldursflokkar, valbreyta, "Fjoldiihop", 10)
        
        # Bý svo til stað til að geyma gögn fyrir valda hópa 
        
        hopagogn = ntvenndir(10)
        for (j in (1):17)
        {   # Hleyp yfir árin. Á ári 1996+j er tvenndin fyrir aldurshóp i í sætum 2i-1 (tekjur) og 2i (skuld)
            # sjá dæmi að ofan
            
            for(i in 1:10)
            {
                k = 2*i-1
                hopagogn[[j]][k] = medalrsthopar[[i]][j]
                k = k+1
                hopagogn[[j]][k] = hopabreyta[[i]][j]
                
            }
            
        }
        
        
        # Hleð svo inn gögnum fyrir valda hópa, til að stilla ásana
        
     
        
        
        tekjos = c()
        breytos = c()
        for( p in valhopar ) 
        {   # p táknar valinn hóp 
            for(s in c((ar1-1996):(ar2-1996)))
            {   # s táknar ar
                tekjos = c(tekjos, medalrsthopar[[p]][s])
                breytos = c(breytos, hopabreyta[[p]][s])            
            }
            
        }                
        
        # tekjos geymir ráðstöfunartekjur valtiunda frá ar1 til ar2
        # breytos geymir valbreyta valtiunda frá ar1 til ar2
        # (gert til að stilla ásana í myndinni, min og max)
        
        for(j in (ar1-1996):(ar2-1996))    
        {   # hlaupum yfir arin og teiknum tekju/breytu tvenndir fyrir valdnar tíundir fyrir arin
            # ath. upprunalega fallið 
            
            
            tekjur = rep(0,10) # bý til vigur fyrir tekjurnar og set inn í fyrir þær tiundir sem skoða skal
            breytan = rep(0,10) # bý til vigur fyrir breytuna sem skoða skal og set inn í fyrir valhoparnar
            
            # fyrir hvert ár, þá set ég tekjur/breytan inn í valdnar tíundir
            for(i in valhopar)
            {   # hleð inn þeim breytu og tekjum sem verða notaðar
                k = 2*i-1
                tekjur[[i]] = hopagogn[[j]][k]
                k = k+1
                breytan[[i]] = hopagogn[[j]][k]
                
            }
            # tekjur og breytan geyma tekjur og valbreyta valinna tiunda fyrir árin frá ar1 og upp þangað sem komið er (1996+j held ég)
            
            xmin = floor(min(tekjos))
            xmax = ceiling(max(tekjos))
            
            ymin = floor(min(breytos))
            ymax = ceiling(max(breytos))
            
            
            
            xaxp = c(xmin, xmax, 2*(xmax-xmin) )
            
            
            
            yaxp = c(ymin, ymax, 2*(ymax-ymin))
            
            # xaxp segir til um fjölda ticks í grafið fyrir x-ás, yaxp fyrir y-ás
            # t.d. xaxp = c(arin[1],arin[17], 8)
            
            # sett upp mynd fyrir fyrstu völdu tíundina
            # (breyta þarf labelunum á ásana ef breytunum er breytt)
            
            if(j == (ar1-1996))
            {   # geri bara plot í fyrsta skiptið, eftir það bara lines.
                plot(tekjur[valhopar[1]], breytan[valhopar[1]], type = "b", pch = valhopar[1], ylim = c(min(breytos), max(breytos)), col = "red", xlim = c(min(tekjos), max(tekjos)),  xlab = "Meðal ráðstöfunartekjur [m. kr. / ár]", ylab = paste("Meðal ", valbreyta, " [m. kr.]"), xaxp = c(xmin, xmax, 16 ), yaxp = c(ymin, ymax, 20) , main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                
                usr = par("usr")
                
                p = 1
                for(t in valhopar)
                {   
                    text(usr[1]*1.05+ (usr[2]-usr[1])/16, usr[4] - p*(usr[4]-usr[3])/20 , paste(pchtakn[t], ": ", aldurshopanofn[t] ))                 
                    p = p+1
                }                
            }
            else
            {
                
                lines(tekjur[valhopar[1]], breytan[valhopar[1]], type = "b", pch = valhopar[1], ylim = c(min(breytan), max(breytan)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Meðal ráðstöfunartekjur [m kr. / ár]", ylab = paste("Meðal ", valbreyta, " [m. kr.]"), main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                # vil segmenta saman fyrstu valdna tíund (4) á arið 1998 og árinu á undan
                # hugsa út fra hopagogn
                segments(hopagogn[[j-1]][2*valhopar[1]-1], hopagogn[[j-1]][2*valhopar[1]],   hopagogn[[j]][2*valhopar[1]-1], hopagogn[[j]][2*valhopar[1]])
            }
            
            
            # og bæti svo við restinni:
            if(length(valhopar) > 1)
            {   # vil geta teiknað staka tíund án þess að fa villu
                for(k in valhopar[2:length(valhopar)])
                {
                    if(j == 1)
                    {
                        lines( tekjur[k], breytan[k], type = "b",  pch = k, col = "red")
                    }
                    else
                    {
                        lines( tekjur[k], breytan[k], type = "b",  pch = k)                        
                    }
                    
                    # er þá búinn að bæta við punktum fyrir hinar tiundirnar
                    # þarf að bæta við segments sem tengjast síðastliðnu ári 
                    if(j > 1)
                    {  # þýðir lítið að teikna strik á arið á undan á fyrsta árinu
                        segments(hopagogn[[j-1]][2*k-1], hopagogn[[j-1]][2*k],   hopagogn[[j]][2*k-1], hopagogn[[j]][2*k])    
                    }
                    
                }                                
            }
            
        }
    }
    
    else
    {
    
        
        # hleð fyrst gögnunum inn 
        
        medalrsthopar = medalTek(landshlutar, "Radstofunartekjur2", "Fjoldiihop", 2)
        hopabreyta = medalTek(landshlutar, valbreyta, "Fjoldiihop", 2)
        
        # Bý svo til stað til að geyma gögn fyrir valda hópa 
        
        hopagogn = ntvenndir(2)
        for (j in (1):17)
        {   # Hleyp yfir árin. Á ári 1996+j er tvenndin fyrir aldurshóp i í sætum 2i-1 (tekjur) og 2i (skuld)
            # sjá dæmi að ofan
            
            for(i in 1:2)
            {
                k = 2*i-1
                hopagogn[[j]][k] = medalrsthopar[[i]][j]
                k = k+1
                hopagogn[[j]][k] = hopabreyta[[i]][j]
                
            }
            
        }
        
        
        # Hleð svo inn gögnum fyrir valda hópa, til að stilla ásana
        
     
        
        
        tekjos = c()
        breytos = c()
        for( p in valhopar ) 
        {   # p táknar valdna tiund
            for(s in c((ar1-1996):(ar2-1996)))
            {   # s táknar ar
                tekjos = c(tekjos, medalrsthopar[[p-15]][s])
                breytos = c(breytos, hopabreyta[[p-15]][s])            
            }
            
        }                
        
        # tekjos geymir ráðstöfunartekjur valtiunda frá ar1 til ar2
        # breytos geymir valbreyta valtiunda frá ar1 til ar2
        # (gert til að stilla ásana í myndinni, min og max)
        
        for(j in (ar1-1996):(ar2-1996))    
        {   # hlaupum yfir arin og teiknum tekju/breytu tvenndir fyrir valdnar tíundir fyrir arin
            # ath. upprunalega fallið 
            
            
            tekjur = rep(0,2) # bý til vigur fyrir tekjurnar og set inn í fyrir þær tiundir sem skoða skal
            breytan = rep(0,2) # bý til vigur fyrir breytuna sem skoða skal og set inn í fyrir valhoparnar
            
            # fyrir hvert ár, þá set ég tekjur/breytan inn í valdnar tíundir
            for(i in 1:2)
            {   # hleð inn þeim breytu og tekjum sem verða notaðar
                k = 2*i-1
                tekjur[[i]] = hopagogn[[j]][k]
                k = k+1
                breytan[[i]] = hopagogn[[j]][k]
                
            }
            # tekjur og breytan geyma tekjur og valbreyta valinna tiunda fyrir árin frá ar1 og upp þangað sem komið er (1996+j held ég)
            
            xmin = floor(min(tekjos))
            xmax = ceiling(max(tekjos))
            
            ymin = floor(min(breytos))
            ymax = ceiling(max(breytos))
            
            
            
            
            # xaxp segir til um fjölda ticks í grafið fyrir x-ás, yaxp fyrir y-ás
            # t.d. xaxp = c(arin[1],arin[17], 8)
            
            # sett upp mynd fyrir fyrstu völdu tíundina
            # (breyta þarf labelunum á ásana ef breytunum er breytt)
            
            if(j == (ar1-1996))
            {   # geri bara plot í fyrsta skiptið, eftir það bara lines.
                plot(tekjur[valhopar[1]-15], breytan[valhopar[1]-15], type = "b", pch = valhopar[1], ylim = c(min(breytos), max(breytos)), col = "red", xlim = c(min(tekjos), max(tekjos)),  xlab = "Meðal ráðstöfunartekjur [m. kr. / ár]", ylab = paste("Meðal ", valbreyta, " [m. kr.]"), xaxp = c(xmin, xmax, 16 ), yaxp = c(ymin, ymax, 20) , main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                
                usr = par("usr")
                
                p = 1
                for(t in 1:2)
                {   
                    text(usr[1]*1.05+ (usr[2]-usr[1])/16, usr[4] - p*(usr[4]-usr[3])/20 , paste(pchtakn[t], ": ", landshlutanofn[t] ))                 
                    p = p+1
                }                                
            }
            else
            {
                
                lines(tekjur[valhopar[1]-15], breytan[valhopar[1]-15], type = "b", pch = valhopar[1], ylim = c(min(breytan), max(breytan)), xlim = c(min(tekjur), max(tekjur)),  xlab = "Meðal ráðstöfunartekjur [m kr. / ár]", ylab = paste("Meðal ", valbreyta, " [m. kr.]"), main = paste(as.character(ar1), " - ", as.character(ar2)) )                
                # vil segmenta saman fyrstu valdna tíund (4) á arið 1998 og árinu á undan
                # hugsa út fra hopagogn
                segments(hopagogn[[j-1]][1], hopagogn[[j-1]][2],   hopagogn[[j]][1], hopagogn[[j]][2])
            }
            
            
            # og bæti svo við restinni:
            if(length(valhopar) > 1)
            {   # vil geta teiknað staka tíund án þess að fa villu
                for(k in valhopar[2:length(valhopar)])
                {
                    if(j == 1)
                    {
                        lines( tekjur[k-15], breytan[k-15], type = "b",  pch = k, col = "red")
                    }
                    else
                    {
                        lines( tekjur[k-15], breytan[k-15], type = "b",  pch = k)                        
                    }
                    
                    # er þá búinn að bæta við punktum fyrir hinar tiundirnar
                    # þarf að bæta við segments sem tengjast síðastliðnu ári 
                    if(j > 1)
                    {  # þýðir lítið að teikna strik á arið á undan á fyrsta árinu
                        segments(hopagogn[[j-1]][3], hopagogn[[j-1]][4],   hopagogn[[j]][3], hopagogn[[j]][4], col = "green")    
                    }
                    
                }                                
            }
            
        }
    }
    






}




# úr 5. október vinnuskjal 

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
    
    
    # j er númer hóps, k er ártalið-1996
    for(j in 1:fjoldihopa)
    { # listar yfir meðalráðstöfunartekjur fyrstu j hópanna yfir arin 1997-2013 hafa verið færðar inn í medaltekjur[1:j][1:k]
        
        for(k in 1:17)
        {# meðaltekjur fólks í hópi j á ári 1996+k hefur verið færður inn í medaltekjur[[1:j-1]] og medaltekjur[[j]][1:k]
            
            fjoldi[[j]][k] = as.data.frame(tafla[j])[dalkurfjoldi][,1][k]   # fjöldi í hóp j árið 1996+k
            
            tekjur[[j]][k] = as.data.frame(tafla[j])[dalkurstaerd][,1][k]   # tekjur þeirra í hóp j arið 1996+k
            
            
            medaltekjur[[j]][k] = nunakronur(tekjur[[j]][k], 1996+k)/fjoldi[[j]][k]      # meðaltekjur þeirra í hóp j árið 1996+k í 2015 krónum
            
            
        }    
    }  
    
    return(medaltekjur)
}


# Notkun: tekjudreifing(medalT, mintiund, maxtiund)
# Fyrir: medalT er listi af listum þar sem undirlistarnir eru með 17 stök t.d. meðaltal ráðstöfunartekna yfir árin 1998-2013 
#   mintiund og maxtiund eru tíundir, 1 <= mintiund < maxtiund <= 10
# breytaplottud er strengur, nafnið á því sem verið er að 
# Eftir: medalT hefur verið teiknað á móti arunum 1997-2013 fyrir tiundirnar a milli mintiund og maxtiund

# Til að alhæfa yfir á aðrar stærðir en raðstöfunartekjur þarf að breyta ylabel
tekjudreifing = function(medalT, mintiund, maxtiund, breytaplottud)
{
    litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4")
    arin = c(1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013)
    plot(arin, medalT[[maxtiund]], type = "b", col = "salmon4", xaxp = c(arin[1],arin[17], 8), xlab = "Ár", ylab = paste("Meðal ", breytaplottud, " [milljónir kr.]"), ylim = c(min(medalT[[mintiund]]), max(medalT[[maxtiund]])), )
    
    title("Eftir tekjutíundum")
    
    if( mintiund < maxtiund)
    {
        for(i in (maxtiund-1):mintiund)
        { # myndir fyrir 10-i tekjuhæstu tíundirnar hafa verið sýndar
            lines(arin, medalT[[i]], type = "b", col = litir[i])
        }            
    }
    
    
}  


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

source("../núþákrónur.R", encoding="UTF-8")
# Notkun: leidretta(upphaedir)
# Fyrir: upphaedir er 17 talna listi.
#           Hver tala er krÃ³nutala Ã¡ verÃ°lagi samsvarandi Ã¡rs, Ã¡ bilinu 1997â€“2013,
#           Ã¾egar Ã¡rtÃ¶lunum er raÃ°aÃ° Ã??? vaxandi rÃ¶Ã°.
# Skilagildi: SÃ¶mu upphÃ¦Ã°ir, nema allar Ã¡ verÃ°lagi septembers 2015
leidretta = function(upphaedir) {
    sapply(seq_along( verdlag ),
           function(i) {
               nuvirdi(upphaedir[i], 1997+(i-1))
           })
}



# Notkun: stadla(lista, lysistaerd)
# lista: (listi yfir datatframe) annadhvort breytan aldursflokkar eda landshlutar
# lysistaerd: (strengur) nafnid a breytistaerdinni innan dataframe sem a ad saekja
# skilar: lista af listum, stadlad m.t.t. verðbólgu 
stadla <- function(lista, lysistaerd, deila=TRUE) {
    
    
        if(deila)
            sapply(lista,
                   function(aldur) {
                       leidretta(t(aldur[lysistaerd]/aldur$Fjoldiihop))
                   }
            )
        else
            sapply(lista,
                   function(aldur) {
                       leidretta(t(aldur[lysistaerd]))
                   }
            )    
    

    
}

