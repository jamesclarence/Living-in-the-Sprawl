library("htmltab")

url <- "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")

df <- htmltab(doc = url, header = c(1:2))
df$season <- rep("14-15", times = length(df))

url2 <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings", 
          "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
          "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")

df <- data.frame()
x <- for (i in url2) {
    sub1 <- gsub('http://www.d3hoops.com/conf/NCAC/men/20',"",i)
    sub2 <- gsub('/standings',"",sub1)
    x <- rep(sub2, times = 11)
    print(x)
}

df <- data.frame()
for (i in url2)  {
    df <- htmltab(doc = i, header = c(1:2))
    print(df)
}


htmltab(doc = "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings", header = c(1:2))


for i in list {
    url2 <- c("http://www.d3hoops.com/conf/NCAC/men/2006-07/standings", 
              "http://www.d3hoops.com/conf/NCAC/men/2007-08/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2008-09/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2010-11/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2011-12/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2012-13/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2013-14/standings",
              "http://www.d3hoops.com/conf/NCAC/men/2014-15/standings")
    list <- lapply(url2, htmltab)
    rep("14-15", 2) 
}