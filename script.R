### ------------------------------------------------------------------------
# Web scraping with R - A short intro 
# Written by Eduard Szöcs
# @R-User Group Rhein-Neckar, 25. Feb. 2016 , "Dezernat 16" 
# License: CC-BY-NC



# Load --------------------------------------------------------------------
require(xml2)
require(httr)
require(rvest)
# devtools::install_github("hrbrmstr/xmlview")
require(xmlview)

# need for output
require(ggplot2)
require(reshape2)
require(xtable)



# Scraping structured web pages -------------------------------------------

# Election results in RLP

# 0 - look at the source
# https://de.wikipedia.org/wiki/Landtagswahlen_in_Rheinland-Pfalz

# 1 - download web-page
h <- read_html('https://de.wikipedia.org/wiki/Landtagswahlen_in_Rheinland-Pfalz')

# 2 - extract tables <table> ... </table>
html_table(h)
# this extracts all tables and parses them to a data.frame
# we are interested only in the first one..
# Note that we need to change the decimal
df <- html_table(h, dec = ',')[[1]]
df

# 3 - cleaning
# still need to clean the citations (wont show)
df$FDP <- as.numeric(                      # make numeric
  gsub(",", ".",                           # replace comma with dot
    gsub('\\[.*\\]', "", df$FDP)))         # remove all square brackets
# set date 
df$Wahltag <- as.Date(df$Wahltag, format = '%d.%m.%Y')

# plot
dfm <- melt(df[ , c('Wahltag', 'SPD', 'CDU', 'FDP', 'Grüne')], id.vars = 'Wahltag')

p <- ggplot(dfm) +
  geom_line(aes(x = Wahltag, y = value, col = variable)) +
  scale_color_manual('Party', breaks = unique(dfm$variable), 
                     values = c("red", "black", "orange", "green")) +
  theme_bw()
p
# ggsave('p1.pdf', p, width = 7)



# Scraping unstructured web pages -----------------------------------------

h <- read_html('http://webbook.nist.gov/cgi/cbook.cgi?ID=50-00-0&Units=SI')

# view in RStudio
xml_view(h)
xml_view(h, add_filter = TRUE)

# basic PAth
xml_find_all(h, '//a') # Selects all a elements (=links) no matter where they are in the document
xml_find_all(h, '//a[@title="IUPAC definition of empirical formula"]') # Selects link with attribute class = external

# -------------------------------
# extract InChIkey
# may different ways to specify xpath
# depends on how "stable" the webpage is across different compounds
xml_find_all(h, '/html/body/ul[1]/li[4]/span')
# xpath from firebug
# this xpath relies on same order (might be not so robust)
xml_find_all(h, './/li/strong[contains(., "IUPAC Standard InChIKey:")]/following-sibling::span')
# this xpath relies on the text "IUPAC Stand..." and extracts the next span

# xml_text extracts the text element between tags.
inchikey <- xml_text(xml_find_all(h, '/html/body/ul[1]/li[4]/span'))



# -------------------------------
# extract Molecular weight
# xpath from firebug
xml_find_all(h, '/html/body/ul[1]/li[2]')

xml_find_all(h, './/li/strong[contains/following-sibling::text()')
xml_find_all(h , './/li/strong[contains(., "Molecular weight")]')
# all text that follows li/strong
xml_find_all(h, './/li/strong[contains(., "Molecular weight")]/following-sibling::text()')
# all text that follows li/strong that contains "Molecular weight"
mw <- xml_text(xml_find_all(h, './/li/strong[contains(., "Molecular weight")]/following-sibling::text()'), trim = TRUE)


# extract data from attributes
# this is the link
nd <- xml_find_all(h, './/li/strong[contains(., "Molecular weight")]/a')
# extract link url
xml_attr(nd, 'href')



# Automatisation ----------------------------------------------------------

# Some CAS-Numbers for which we want information
cas <- c('50-00-0', '126-86-3', '28159-98-0', '1461-25-2',
         '120-18-3', '25637-99-4')

# wrap into function
foo <- function(cas){
  # build url
  qurl <- paste0('http://webbook.nist.gov/cgi/cbook.cgi?ID=', cas, '&Units=SI')
  # download url
  h <- read_html(qurl)
  # extract inchikey
  inchikey <- xml_text(xml_find_all(h, './/li/strong[contains(., "IUPAC Standard InChIKey:")]/following-sibling::span'))
  # extract molecular weight
  mw <- xml_text(xml_find_all(h, '//li/strong[contains(., "Molecular ")]/following-sibling::text()'))
  mw <- as.numeric(mw)120-18-3
  # return list
  out <- list(inchikey = inchikey, mw = mw)
}

sapply(cas, foo)
t(sapply(cas, foo))

# need error handling (not found..)


