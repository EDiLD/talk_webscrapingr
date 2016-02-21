### ----------------------------------------------------------------------------
# Web scraping with R - A short intro 
# Written by Eduard Szöcs
# @R-User Group Rhein-Neckar, 25. Feb. 2016 , "Dezernat 16" 
# License: CC-BY-NC
### ----------------------------------------------------------------------------


# Load --------------------------------------------------------------------
require(xml2)
require(httr)
require(rvest)
# devtools::install_github("hrbrmstr/xmlview")
require(xmlview)

require(ggplot2)
require(reshape2)


# Scraping structured web pages -------------------------------------------
# Election results in RLP from

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
dfm <- melt(df[ , c(1, 3:6)], id.vars = 'Wahltag')
p <- ggplot(dfm) +
  geom_line(aes(x = Wahltag, y = value, col = variable)) +
  scale_color_manual('Party', breaks = unique(dfm$variable), 
                     values = c("red", "black", "green", "yellow")) +
  theme_bw()
p
# ggsave('p1.pdf', p, width = 7)


# Scraping unstructured web pages -----------------------------------------
