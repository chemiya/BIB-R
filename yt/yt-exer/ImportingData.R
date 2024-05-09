

library(datasets) 


# CSV
rio_csv <- read.csv("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\ImportingData_Datasets\\mbb.csv")
head(rio_csv)
# TXT
rio_txt <- read.csv("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\ImportingData_Datasets\\mbb.txt", sep = "\t")
head(rio_txt)
# Excel XLSX
library(readxl)
rio_xlsx <- read_excel("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\ImportingData_Datasets\\mbb.xlsx")
head(rio_xlsx)
#ver datos cargados
View(rio_csv)
#tabla
r_txt1 <- read.table("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\ImportingData_Datasets\\mbb.txt", header = TRUE, 
                     sep = "\t")
r_txt1
