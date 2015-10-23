## Project 1 - Reiknigreind
## inniheldur nunakronur og þakronur

# Notkun: x = nunakronur(upphaed, ar)
# Fyrir: upphaed er upphæð í milljónum króna sem mæld var árið "ar"  (má vera á milli 1998-2013)
# Eftir: x er upphæðin í 2015 kronum
nunakronur = function(upphaed, ar)
{
  
  # Vísitala neysluverðs fyrir arin 1998-2013:
  visitala = c(183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
  visisept = 430.6 # vísitala neysluverðs í september 2015
  hlutfallsvisitala = visitala/visisept
  
  return(upphaed*10^6/hlutfallsvisitala[ar-1997])
  
}


# Notkun: x = þakronur(upphaed, ar, kronuar)
# Fyrir: upphaed er fjárhæð í milljónum á árinu ar, kronuar er árið sem við viljum staðla m.t.t.
# ar og kronuar eru á milli 1998 og 2013.
# Eftir: x er upphæðin mæld í krónum arið kronuar
þakronur = function(upphaed, ar, kronuar)
{
  visitala = c(183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
  hlutfallsvisitala = visitala/visitala[kronuar-1997]
  
  return(upphaed*10^6/hlutfallsvisitala[ar-1997])
  
  
  
}