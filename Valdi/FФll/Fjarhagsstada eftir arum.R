# Eignastaða vs. raðstöfunartekjur fyrir aldurshópa eða landshluta
# Velja má eignastöðu breytu til að skoða - t.d. eigið fe eða skuldir 


# Byggir á þeirri forsendu að fallið hans Bjarts "stadlar" sem staðlar m.t.t. verðbolgu sé til staðar.

# Úr miniplön.txt fra 8. október:  
if(FALSE)
{
    
    Nota R Shiny til að fá samræmda lista hoparvaldir og litirvaldir
    hopaval er listi yfir TRUE/FALSE eftir því hvaða hópa notandi velur
    hoparvaldir =  hopar[hopaval]
    litirvaldir =  litir[hopaval]
    
    stadlad = sapply(breyturvaldar, function(breytunafn) {
        stadla(hoparvaldir, breytunafn)
    }
    
    ymork = c( min(stadlad), max(stadlad) )
    sapply(stadlad, function(hopar) {
        for(i in seq_along()
            sapply(hopar, function(hopur) {
                lines(arin, )
            }
    }
    
            )
    
    
}

litir = c("red", "green", "blue", "black", "maroon", "brown", "violet", "purple", "royalblue", "salmon4", "orange", "pink", "orchid4", "oldlace", "ivory4", "cyan", "burlywood3")

# ÞAÐ A EFTIR AÐ BÚA TIL listann landshlutar og SETJA INN Í FALLIÐ
landshlutar = list(hofudborg, landsbyggd)

# Notkun: stadla(lista, lysistaerd)
# lista: (listi yfir datatframe) annadhvort breytan aldursflokkar eda landshlutar
# lysistaerd: (strengur) nafnid a breytistaerdinni innan dataframe sem a ad saekja
# skilar: lista af listum, stadlad
stadla <- function(lista, lysistaerd, deila=TRUE) {
    
    if(identical(lista, aldursflokkar))
    {
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
    
    else
    {   # g.r.f. að fallið se notað rett
        if(deila)
            sapply(landshlutar,
                   function(landshluti) {
                       leidretta(t(landshluti[lysistaerd]/landshluti$Fjoldiihop))
                   }
            )
        else
            sapply(landshlutar,
                   function(landshluti) {
                       leidretta(t(landshluti[lysistaerd]))
                   }
            )            
    }
    
}

seigidfje = stadla(aldursflokkar,"EigidFje")
slandsheigidfje = stadla(landshlutar, "EigidFje")
min(seigidfje)
hopar = c(1:4)
min(seigidfje[,hopar])
# seigidfje[,i] gefur eigiðfé i-tu tiundarinnar yfir árin 1997-2013


# Notkun: fjarhagsstadahopa = function(hoparvaldir, breyturvaldar, aldurstrue)
# Fyrir:
# hopar er talnavigur sem merkir hóparna sem skoða skal, þar sem c(1:10) eru allir aldurshóparnir, c(16:17) eru landshlutarnir
    # c(1:4) eru yngstu 4 aldurshóparnir
#  breyturvaldar er strengjavigur sem inniheldur nöfnin úr hagstofa.R á breytunum sem setja skal á myndina 
    # ferlanir sem samsvara þeim verða plottaðir
    # t.d. breyturvaldar = c("EigidFje", "EigidFjeiFasteign", "EigidFjeAnnad)
    # breytur er 3ja staka hlutvigur i´eftirfarandi:
    # c( "Eigniralls",  "Fasteignir", "Okutaeki", "Innlan", "Verdbrjef", "AdrarEignir", "Skuldiralls",  "Ibudarlan", 
    # "Adrarskuldir", "EigidFje", "EgidFjeiFasteign", "Eigidfje annad", 
    # "Fjoldiihop")
    # (sleppum meðaltölunum og þeim dalkum sem ekki eru mældir i milljónum króna)
# aldurstrue er boolean sem er TRUE ef aldurshóparnir eru skoðaðir, FALSE ef landshlutarnir eru skoðaðir

#ATH max 3 breytur

# Eftir: ferlarnir fyrir breyturnar í breytur hafa verið teiknaðir upp a sömu mynd, á móti árunum 1997-2013. 
#   Ásarnir á myndinni eru stilltir þ.a. þeir sýna fra minnsta að hæsta gildi þess sem er teiknað
#   Gildin á y-ásnum hafa verið stöðluð m.t.t. verðbolgu og allt er synt í sept 2015 krónum.   
# Litnirir samsvara hópunum sem hafa verið valdir. i-ti hópurinn hefur i-ta litinn úr 'litir'

fjarhagsstadahopa = function(hoparvaldir,breyturvaldar, aldurstrue)
{
    
    #hopalisti = list(u24,aldur2529, aldur3034, aldur3539, aldur4044, aldur4549,aldur5054,aldur5559,aldur6066,y67, 11,12,13,14,15,hofudborg,landsbyggd)
    if(length(breyturvaldar) > 3){ return }
    
    breytunumer = 1:length(breyturvaldar) # til að hlaupa yfir
   
    
# hoparvaldir = c(1:4)
# breyturvaldar = c("EigidFje", "Okutaeki", "Innlan")

    if(aldurstrue)
    {

        # þarf að setja mörk á asana
        # vil fá dataframe fyrir allar breyturnar. fæ valda hópa með stadla(aldursflokkar,breyturvaldar[i])[,hopar]
        ymin = min( stadla(aldursflokkar, breyturvaldar[1])[,hoparvaldir] )
        ymax = max( stadla(aldursflokkar, breyturvaldar[1])[,hoparvaldir] )
        
        if(length(breyturvaldar) > 1)
        {
            for(j in breytunumer[2:length(breytunumer)])
            {
                ymin = min( c(ymin, stadla(aldursflokkar, breyturvaldar[j])[,hoparvaldir])  )
                ymax = max( c(ymax, stadla(aldursflokkar, breyturvaldar[j])[,hoparvaldir])  )
            }
        }
# set upp ramma til að teikna á. þarf fyrst að finna hæsta og lægsta gildið til að stilla rammann
        plot(arin, rep(0,17), xlab = "Ár", type = "l", ylab = "Fjárhagsstaða að meðaltali [m. króna]", xaxp = c(arin[1],arin[17], 8), ylim = c(ymin,ymax) ) 
        
        
        undirtitill = breyturvaldar[1]
        for(j in 2:length(breyturvaldar))
        {
            undirtitill = paste(undirtitill, "," ,breyturvaldar[j])
        }
    
        title(main = "Aldurshópar", sub= undirtitill)   
    
# Þarf að hafa mismunandi utlit a ferlunum fyrir mismunandi breytur
# MAX 3 mismunandi breytur i einu. type = "l", "b", "p"

        for(i in breytunumer)
        {  
            
            stadladgildi = stadla(aldursflokkar, breyturvaldar[i])    
            
            # gildin á fyrstu i breytunum hafa verið staðlaðar m.t.t. verðbolgu OG deilt með fjölda í hverjum hóp!  XXXX
            # og er nýjasta breytan sem unnið er með í dataframeinu stadladgildi
            # stadladgildi ætti að geyma valdna breytu staðlaða m.t.t. verðbólgu yfir árin 17 fyrir alla hópana
            # stadladgildi[,j] ætti þá að geyma valdna breytu staðlaða m.t.t. yfir arin 17 fyrir hóp j
            # j er í hoparvaldir
            
            for(j in hoparvaldir)
            {
                lines(arin, stadladgildi[,j], type = "b", lty = i, col = litir[j])
            }
            
        }

    }
    
    else
    {
        # þarf að setja mörk á asana
        # vil fá dataframe fyrir allar breyturnar. fæ valda hópa með stadla(aldursflokkar,breyturvaldar[i])[,hopar]
        ymin = min( stadla(landshlutar, breyturvaldar[1])[,hoparvaldir] )
        ymax = max( stadla(landshlutar, breyturvaldar[1])[,hoparvaldir] )
        for(j in breytunumer[2:length(breytunumer)])
        {
            ymin = min( c(ymin, stadla(landshlutar, breyturvaldar[j])[,hoparvaldir])  )
            ymax = max( c(ymax, stadla(landshlutar, breyturvaldar[j])[,hoparvaldir])  )
        }
        # set upp ramma til að teikna á. þarf fyrst að finna hæsta og lægsta gildið til að stilla rammann
        plot(arin, rep(0,17), xlab = "Ár", type = "l", ylab = "Fjárhagsstaða að meðaltali [m. króna]", xaxp = c(arin[1],arin[17], 8), ylim = c(ymin,ymax) ) 
        
        undirtitill = breyturvaldar[1]
        for(j in 2:length(breyturvaldar))
        {
            undirtitill = paste(undirtitill, "," ,breyturvaldar[j])
        }
        
        title(main = "Landshlutar - höfuðborgarsvæði vs. landsbyggð", sub = undirtitill)   
            
        for(i in breytunumer)
        {  
            stadladgildi = stadla(landshlutar, breyturvaldar[i])    
            
            # gildin á fyrstu i breytunum hafa verið staðlaðar m.t.t. verðbolgu OG deilt með fjölda í hverjum hóp!  XXXX
            # og er nýjasta breytan sem unnið er með í dataframeinu stadladgildi
            # stadladgildi ætti að geyma valdna breytu staðlaða m.t.t. verðbólgu yfir árin 17 fyrir alla hópana
            # stadladgildi[,j] ætti þá að geyma valdna breytu staðlaða m.t.t. yfir arin 17 fyrir hóp j
            # j er í hoparvaldir
            
            for(j in hoparvaldir)
            {
                lines(arin, stadladgildi[,j], type = "b", lty = i, col = litir[j])
            }
            
        }

    }

    
}

fjarhagsstadahopa(4:6,breyturvaldar,TRUE)

breyturvaldar = c("EigidFje", "Innlan", "Fasteignir", "Skuldiralls")

breyturvaldar = c("Eigniralls", "Innlan", "Fasteignir")

fjarhagsstadahopa(4:6,breyturvaldar,TRUE)

# Þarf að bæta við myndina nöfnunum á völdum breytum! eitthvað svipað og title("")


breyturvaldar