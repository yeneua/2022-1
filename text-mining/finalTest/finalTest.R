#2017447 김예나
library(tm)
library(stringr)
library(ggplot2)

# 문제1
bts <- file.choose()

str(bts)
library(xlsx)
library(readxl)
bts <- read.csv(file="news_comment_BTS.csv", header=T)
getwd()
x <- read.csv("news_comment_BTS.csv",header=TRUE,seq=",")
x <- read.csv("C:/Users/Administrator/Desktop/example.csv",header=TRUE)


file <- choose.files()
bts <- read.csv(file, header=T)
bts
str(bts)
