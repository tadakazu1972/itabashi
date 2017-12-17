#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
setwd("~/Desktop/itabashi")

#必要なファイル読込
shape <- st_read("h27ka13119.shp") #地図境界線、人口

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km, main="板橋区　人口")
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.0005, labels=shape$MOJI, cex=0.4)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0009, labels=shape$JINKO, cex=0.5)

###################################################################
#csvデータを読み込みで描画する場合
#前処理1：１行目を削除（後で色付けの際にヘッダーの文字が混在しているとエラー）
#前処理2：CITYの項目名をマッチングさせるためMOJIに変更
#前処理3：エディタで色付け計算のためにｘや-は0置換
data1 <- read_csv("xxxx.csv")

#シェープファイルと結合
data <- left_join(shape, data1 by="MOJI")

#地図　描画
par(family="HiraKakuProN-W3")
col_km <- data$共同住宅 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"YlGnBu"))
plot(data[4], col=col_km, main="板橋区　共同住宅　平成27年国勢調査")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]+0.0005, labels=data$MOJI, cex=0.4)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0009, labels=data$共同住宅, cex=0.5)

###################################################################
#主成分分析

#ファイル読み込み
data1 <- read_csv("xxxx.csv")
data2 <- read_csv("yyyy.csv")

#ファイル結合
data <- left_join(data1, data2, by="MOJI")

#一旦、結合したデータをcsvファイルに書き出し
write.csv(data, "zzzz.csv")

#クレンジング作業
#いらない列を削除
#NAを0置換

#再度ファイルを読み込み、この時はread.csvを使わないと項目名変換でエラーになる
data <- read.csv("data.csv")

#町丁目名を項目名に置き換え(４番目の項目がMOJI)
row.names(data) = data[,1]
data = data[,2:20]


#描画
par(xpd=TRUE, family="HiraKakuProN-W3")
biplot(prcomp(data, scale=TRUE))

#主成分にかかる固有ベクトルがわかる
result <- prcomp(data, scale=TRUE)
print(result)

#寄与率と累積寄与率
summary(result)

#主成分得点を見る
result$x
