## Project 1 - Reiknigreind
## inniheldur föll til að núvirða og þávirða upphæðir í íslenskum krónum


# Vísitala neysluverðs árin 1998–2013:
verdlag = c(180.3, 183.3, 189.6, 199.1, 212.4, 222.6, 227.3, 234.6, 244.1, 260.6, 273.7, 307.7, 344.6, 363.2, 377.7, 397.3, 412.7)
visitala = function(artal) {
  verdlag[1+(artal-1997)]
}

# Notkun: nuvirdi(upphaed, ar)
# Fyrir: upphaed er gömul krónutala (t.d. í milljónum króna) á verðlagi ársins ar.
#           ar er tala sem geymir ártal á bilinu 1997–2013.
# Skilagildi: Sama upphæð, nema á verðlagi ársins 2015. Verðlagi septembers 2015, nánar tiltekið.
nuvirdi = function(upphaed, ar)
{
  sept2015 = 430.6 # v?sitala neysluver?s ? september 2015
  uppsofnud_verdbolga = sept2015/visitala(ar)
  
  return(upphaed*uppsofnud_verdbolga)
}

# Notkun: leidretta(upphaedir)
# Fyrir: upphaedir er 17 talna listi.
#           Hver tala er krónutala á verðlagi samsvarandi árs, á bilinu 1997–2013,
#           þegar ártölunum er raðað í vaxandi röð.
# Skilagildi: Sömu upphæðir, nema allar á verðlagi septembers 2015
leidretta = function(upphaedir) {
  sapply(seq_along( verdlag ),
         function(i) {
           nuvirdi(upphaedir[i], 1997+(i-1))
         })
}

# Notkun: x = ?akronur(upphaed, ar, kronuar)
# Fyrir: upphaed er fj?rh?? frá ?rinu ar, kronuar er ?ri? sem vi? viljum sta?la m.t.t.
# ar og kronuar eru ? milli 1997 og 2013.
# Eftir: x er upph??in m?ld ? kr?num ari? kronuar
thavirdi = function(upphaed, gamla, nyja)
{
  uppsofnud_verdbolga = visitala(nyja)/visitala(gamla)
  
  return(upphaed*uppsofnud_verdbolga)
  
  
  
}