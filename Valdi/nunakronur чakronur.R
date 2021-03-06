## Project 1 - Reiknigreind
## inniheldur nunakronur og �akronur

# Notkun: x = nunakronur(upphaed, ar)
# Fyrir: upphaed er upph�� � millj�num kr�na sem m�ld var �ri� "ar"  (m� vera � milli 1998-2013)
# Eftir: x er upph��in � 2015 kronum
nunakronur = function(upphaed, ar)
{
  
  # V�sitala neysluver�s fyrir arin 1998-2013:
  visitala = c(183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
  visisept = 430.6 # v�sitala neysluver�s � september 2015
  hlutfallsvisitala = visitala/visisept
  
  return(upphaed*10^6/hlutfallsvisitala[ar-1997])
  
}


# Notkun: x = �akronur(upphaed, ar, kronuar)
# Fyrir: upphaed er fj�rh�� � millj�num � �rinu ar, kronuar er �ri� sem vi� viljum sta�la m.t.t.
# ar og kronuar eru � milli 1998 og 2013.
# Eftir: x er upph��in m�ld � kr�num ari� kronuar
�akronur = function(upphaed, ar, kronuar)
{
  visitala = c(183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
  hlutfallsvisitala = visitala/visitala[kronuar-1997]
  
  return(upphaed*10^6/hlutfallsvisitala[ar-1997])
  
  
  
}