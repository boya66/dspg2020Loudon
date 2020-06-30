library("readxl")
dat <- read_excel("data/DataDownload2015.xlsx", sheet = 3)
dat <- dat[dat$State == "Virginia",]
dat <- dat[dat$County == "Loudoun",]
dat$LA1and10
