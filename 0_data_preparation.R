#' ---
#' title: "EDA and Data Preprocessing"
#' author: "Tzu"
#' date: "2/7/2021"
#' output:
#'   html_document:
#'     df_print: paged
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 4
#'     toc_float: yes
#'   word_document: default
#'   pdf_document:
#'     toc: yes
#'     toc_depth: '4'
#' ---
#' In this project, we collaborate with AsiaYo, an online B&B booking platform company headquartered in Taiwan, to work together on solving their business problem using various statistical analysis methods. One challenge facing AsiaYo is customer retention, they want to find a better segmentation method for making more effective marketing communication. Considering each customer has their frequency for booking, we create RFM variables and then define a new variable called RT-Ratio(divide recency by frequency). Finally, we use the data of old customers with RFM and other multiple features to build:
#' 
#' (1) Multiple Linear Regression to predict RT-Ratio of new customers
#' (2) GLM model to predict the probability of new customers being alive 
#' 
#' ![](/Users/chengtzuhsuan/Statistical-Data-Analysis-for-Business-and-Management/p1_flow.png)
#' 
#' The data used in this project is generated from the booking transaction during 2018-01-01~2019-11-09 and includes 36 features such as the number of guests, the number of nights, the number of rooms, the amount paid, and so on. 
#' 
#' However, our goal is to predict the retention of new customers. For the purposes of this project, the following pre-processing steps have been made for creating the individual dataset: 
#' 
#' 1. Transform the original transaction dataset into individual-based dataset, which means the row data should be one row per customer now instead of one row per order
#' 2. Considering each customer makes different purchases, some of features would be calculated by the proportion of accepted orders, for more details please check on the final report file.
#' 3. Generate RFM variables based on our individual-based new data
#' 
#' ---
#' 
## ---- warning = FALSE, message = FALSE--------------------------------------------------------------------------------
library(tidyverse)
library(feather) # for open feather file
library(DescTools)
library(gridExtra) 

#' 
#' # Transaction data
#' In this section, we will make a cursory investigation about the customer transactioin data.
#' 
## ---------------------------------------------------------------------------------------------------------------------
path1 = "my_data.feather"
df.old <- read_feather(path1)

#' 
#' Transform to the correct data type
## ---------------------------------------------------------------------------------------------------------------------
## Categorical variables
cols <- c(1,11,12,13,14,15,16,19,20,21,22,23,24,25,26,27,29,30,31,32,33,34,35,36)
df.old[cols] <- lapply(df.old[cols], factor)  

## Datetime. Also combine order_date and order_time to oder_time2
df.old$order_time2 = paste(df.old$order_date, df.old$order_time, sep = " ")
df.old[c(2,3,4)] = lapply(df.old[c(2,3,4)], as.POSIXct)
df.old[37] = lapply(df.old[37], as.POSIXct, format = "%Y-%m-%d %H:%M:%S")
cols2 = c(2,3,4,5,37)

## Numerical variables
df.old[c(6,7,8,9,17,18)] = apply(df.old[c(6,7,8,9,17,18)], 2, as.numeric)

summary(df.old)

#' 
#' - 對 AsiaYo 提供的訂單資料進行初步分析,共 201,694 筆訂單,不重複會員(user_id)共 146,261 位,訂單成
#' 立日期(order_date)區間為 2018-01-01 至 2019-11-09。
#' 
#' - 經進步檢視, 發現資料中訂單編號(order_id)編碼方式為 AsiaYo 開頭的訂單共 2,766 筆,但只有 4 位不重複會員,且分別對應到 4個b2b_partner欄位,查詢後發現4個b2b_partner皆為機票飯店預訂平台,故本組推測即使是不同的用戶,若從同個預訂平台連結進 AsiaYo 下訂單都會是相同的 user_id,因無法分辨個別會員購物型態, 故將該部分訂單予以排除。
#' 
## ---------------------------------------------------------------------------------------------------------------------
# remove 
df.old = df.old[1:198928,]
df.old = df.old[!(df.old$platform == "pre_booking"),]
nrow(df.old)

#' 
#' ## Frequency Distribution of categorical variables
#' ### B&B type, country, platform, user location
## ---------------------------------------------------------------------------------------------------------------------
df.old %>%
  select(platform, user_locale, bnb_type, bnb_country) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 1) +
  theme(axis.text.x = element_text(angle = 90))

#' 
#' - bnb_country: 旅宿所在國家絕大部份為台灣,接著依序為日本、韓國與泰國,可能因為AsiaYo作為台灣原生的自由行訂房平台,當旅客要在台灣地區旅遊訂房時,會優先選擇AsiaYo平台,抑或平台多數使用者本身旅遊地就以台灣為主。
#' 
#' - bnb_type: Bnb/Apartment訂購數量最多,接著依序為EconomyHotel與Hostel。可能由於AsiaYo本身平台合作的旅宿類型本就以 Bnb/Apartment 為主,或者平台使用者偏好旅宿類型為 Bnb/Apartment。
#' 
#' - platform: Android 與 iOS為使用App訂購,Mobile則為使用手機版網頁訂購。從圖中可以看出,其實整體使用行動裝置(手機與平板)訂購的人數略多於利用PC訂購者。且用戶在使用行動裝置訂購時,直接用行動裝置的網頁訂購的人數明顯高於使用 App 的人數,或許可推測,用戶訂購時傾向不會為了「訂房」而下載App,或者是AsiaYo的App使用介面設計不良,導致使用者使用意願較低。
#' 
#' - user_locale: 可以看出訂購人所在地區主要集中於台灣地區,同時呼應bnb_country以台灣數量眾多,可能為訂購人更傾向在所在地區旅遊(如國內旅遊次數或許會多於國外旅遊)。
#' 
#' ### book status, pay currency
## ---------------------------------------------------------------------------------------------------------------------
df.old %>%
  select(book_status, pay_currency) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 90))

#' 
#' - book_status: 可以看出約七成訂單狀態為 Accepted,然而 Cancelled 和 Reject 的數量也不少,前者可能由於消費者會同時使用多個平台進行比較,因此會猶豫或改變主意,或者是AsiaYo 平台推出免費取消服務後,導致此比例的攀升。Reject則可能與房東偏好的消費者不同有關,如某些房東只收女性住客但男性消費者誤訂了。
#' 
#' - pay_currency: 貨幣使用次數,很明顯台幣使用量最高,與台灣人為AsiaYo平台主要使用者有關。而其次則是港幣與美金。
#' 
#' ## Boxplots, Histogram of numerical variables
#' ### Nights, Guests, Rooms
## ---------------------------------------------------------------------------------------------------------------------
p4.1 = df.old %>%
  ggplot(aes("nights",as.numeric(nights))) +
  geom_boxplot() 

p4.2 = df.old %>%
  ggplot(aes("guests", guests)) +
  geom_boxplot()

p4.3 = df.old %>%
  ggplot(aes("rooms", rooms)) +
  geom_boxplot()

p4.4 = grid.arrange(p4.1, p4.2, p4.3, ncol = 3)

options(scipen = 999) # remove science symbol

#' ### Amount paid
## ---------------------------------------------------------------------------------------------------------------------
# amount_paid
p5 = df.old %>%
  ggplot(aes(twd_amount)) +
  geom_histogram(bins = 100)

p5.1 = df.old %>% 
  ggplot(aes(factor("amount_paid"), twd_amount)) +
  geom_boxplot()


grid.arrange(p5, p5.1, ncol = 2)

#' 
#' twd_amount 的 histogram,呈現右尾分布,而每筆訂單金額多分布於萬元以下,但仍有許多離群值有
#' 待下步的探討。
#' 
#' ### Average paid
## ---------------------------------------------------------------------------------------------------------------------
# avg_paid
df.old$avg_paid = (as.numeric(df.old$twd_amount) / as.numeric(df.old$nights))
options(scipen = 999)

p5.2 = df.old %>% 
  ggplot(aes(avg_paid)) +
  geom_histogram(bins = 100)

p5.3 = df.old %>% 
  ggplot(aes(factor("avg_paid"), avg_paid)) +
  geom_boxplot()


grid.arrange(p5.2, p5.3, ncol = 2)

#' 
#' 接著我們想知道 AsiaYo 平台使用者偏好每晚房間價格為何,因此以twd_amount除以nights計算出avg_paid。圖中可看出每晚價位亦呈現右尾分布,大部分平均價位集中在幾千元左右以下,價位相對較低,可能與AsiaYo主打的客群有關,抑或在網路的世代中比價非常容易,使得在沒有任何忠誠、VIP會員制度的情況下,使用者會透過比價至最低價的平台上訂購房間,讓平台上每晚金額偏低。
#' 
#' ### More on Average Paid
#' 進一步將每晚平均價位再與各變數分析
## ---------------------------------------------------------------------------------------------------------------------
# avg_paid vs bnb_country
p8 = df.old %>%
  ggplot(aes(bnb_country, avg_paid)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

# avg_paid vs platform
p8.1 = df.old %>%
  ggplot(aes(platform, avg_paid)) +
  geom_boxplot() 

# avg_paid vs bnb_type
p8.2 = df.old %>%
  ggplot(aes(bnb_type, avg_paid)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

# avg_paid vs user locale
p8.3 = df.old %>%
  ggplot(aes(user_locale, avg_paid)) +
  geom_boxplot() 

grid.arrange(p8, p8.1, p8.2, p8.3)

#' 
#' 
#' - avg_paid 對 bnb_country 作圖:可以看出房間所在地各國家平均價位,台灣的離群值較多,之後會再多做後續分析了解是否為 outliers。日本與韓國的價位相較其他國家略高、馬來西亞的價位略低。
#' 
#' - avg_paid 對 platform 作圖:除了Mobile有個較離群的值外,剩下的平台大致有相似分布情形,無法從圖中看出平均數、中位數的額外差異。
#' 
#' - avg_paid 對 bnb_type作圖:可以看出Hostel有個離群值待作後續分析,而Villa的平均價位全距則較大,價位分布較廣。
#' 
#' - avg_paid 對 user_locale 作圖:是使用者居住地平均價位,看出台灣有較多離群值,而美國與馬來西亞的全距較大,可能與各地物價水準有關。
#' 
#' ---
#' 
#' # Individual data
## ---------------------------------------------------------------------------------------------------------------------
path1 = "Data Set for TMed 2019-12-01.feather"
tmed = read_feather(path1) %>% as_tibble()

#' Quick Summary
#' 
#' `Effective booked order`: 所有status的訂單（cancelled, rejected, accepted) 應會大於Freq
#' 
#' `PctCan`: Cancelled/EffBkOrder
#' 
#' `TtlContri`: 20個月內總貢獻金額
#' 
#' `Monetary`: Monetary = TtlContri/Frequency （有四捨五入至整數）
#' 
#' `TMed`: NaN為 frequency為0，其餘為訂單與訂單間的中位數
#' 
#' `RTratio`: Recency/ TMed
#' 
#' `Recency`: 距離上次購買時間的天數，距離11/10（為0點）
#' 
## ---------------------------------------------------------------------------------------------------------------------
print(length(tmed))
head(tmed)

#' 
## ---------------------------------------------------------------------------------------------------------------------
## Categorical variables
cols <- c(1,13,14)
tmed[cols] <- apply(tmed[cols], 2, as.factor)  

## Numerical variables
tmed[c(2:12, 15:160)] = apply(tmed[c(2:12, 15:160)], 2, as.numeric)

#' 
#' ### Country
## ---------------------------------------------------------------------------------------------------------------------
p7 = tmed %>%
  select(Country) %>% 
  ggplot(aes(Country)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90))

p7

p7.1 = tmed %>%
  select(Country) %>%
  filter(Country != "Taiwan") %>%
  ggplot(aes(Country)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 2) +
  theme(axis.text.x = element_text(angle = 90))

p7.1

#' 
#' 以上兩張圖為會員國籍 bar plot,左圖包含台灣,但因為國籍為台灣的會員人數過高會使其他國家的會員人數
#' 差異難以判別,故剔除掉台灣後重新繪製如右圖。可以從圖中看出 AsiaYo 會員主要為台灣人,次之為香港,緊接著
#' 為馬來西亞、韓國、新加坡、澳門,主要分佈於亞洲地區。值得注意的是,會員國籍中日本已排到第六名,但在旅
#' 宿國家中日本排名第二,往後分析中本組或許可針對各個國家別的會員,分析其旅宿國家分布情形。
#' 
#' ### Frequency
## ---------------------------------------------------------------------------------------------------------------------
# frequency = 1 vs frquency > 1

tmed$f1vf2 = ifelse(tmed$Frequency == 1, "1", ">1")
p9.1 = tmed %>%
  ggplot(aes(as.factor(f1vf2))) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 3) +
  labs(x = "Frequency")

# pie
data <- tmed %>% 
  group_by(f1vf2) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(f1vf2))
data$label <- scales::percent(data$per)

p9.2 = ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=f1vf2), stat="identity", width = 1)+
  coord_polar("y", start=0, direction = -1)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label)) +
  scale_fill_grey() +  
  theme(axis.text.x=element_blank()) 

grid.arrange(p9.1, p9.2, ncol=2)

#' 我們定義Frequency = 1 為新客，其佔比高達88%，舊客資料為Frequency>1的顧客，占比約12%。後續我們將利用舊客的資料預測新客是否回購。
#' 
#' # Data Preprocessing
#' For better knowing customer preference, we would like to transform some probability and numerical variables into categorical variables. The levels of each category is decided by the domain knowledge first, and then we conduct the multiple comparison as a mesurement to merge the similar levels as our final variables:
#' 
#' `NightType`: 用戶訂房偏好停留幾晚
#' 
#' `ChildType`: 用戶是否有帶小孩出遊
#' 
#' `GusetNum`: 用戶偏好出遊的團體大小
#' 
#' `Device`: 用戶偏好使用何種管道在該平台訂房
#' 
#' `BnbType`: 用戶偏好訂購何種房型
#' 
#' `DoW`: 用戶偏好的訂房時間
#' 
#' `OrderPre`: 用戶偏好在入住時間多少天前訂房
#' 
#' `Season`: 用戶出遊季節偏好
#' 
#' `Travel_dist`: 用戶偏好的旅途距離⾧短
#' 
#' ## NightType 
## ---------------------------------------------------------------------------------------------------------------------
tmed$NightType <- "other"
tmed$NightType [(tmed$Night1 + tmed$Night2 + tmed$Night3 > 50)] <- "most_1~3"
tmed$NightType [100-(tmed$Night1 + tmed$Night2 + tmed$Night3) > 50] <- "most_4up"

table(tmed$NightType)

#' 
#' ## ChildType
## ---------------------------------------------------------------------------------------------------------------------
tmed$ChildType <- "other"
tmed$ChildType[tmed$PctChild0 > 99] <- "xa_child" # 都沒有小孩
tmed$ChildType[tmed$PctChild0 == 0] <- "a_child" # 100%有小孩
tmed$ChildType[(tmed$PctChild0 < 100) & (tmed$PctChild0 > 0)] <- "xa_child" # 可能有小孩

table(tmed$ChildType)

#' 
#' ## GuestNum
## ---------------------------------------------------------------------------------------------------------------------
tmed$GuestNum <- NA
tmed$GuestNum[tmed$PctGuest1>=60] <- "single"
tmed$GuestNum[is.na(tmed$GuestNum)] <- "nonsingle"

table(tmed$GuestNum)

#' 
#' ## Device
## ---------------------------------------------------------------------------------------------------------------------
tmed$Device <- NA
tmed$Device[tmed$iOS+tmed$Android>=55] <- "app"
tmed$Device[tmed$Mobile>=55] <- "mobile"
tmed$Device[tmed$PC>=55] <- "pc"
tmed$Device[is.na(tmed$Device)] <- "neutral"

table(tmed$Device)

#' 
#' ## BnbType
## ---------------------------------------------------------------------------------------------------------------------
tmed$BnbType <- NA
tmed$BnbType[tmed$PctBnbTypAPT>50] <- "apt"
tmed$BnbType[tmed$PctBnbTypVilla+tmed$PctBnbTypHot+tmed$PctBnbTypSAPT+tmed$PctBnbTypEHot+tmed$PctBnbTypHost>50] <- "else.apt"
tmed$BnbType[is.na(tmed$BnbType)] <- "neutral"

#tmed$BnbType[tmed$PctBnbTypEHot>50] <- "ehot"
#tmed$BnbType[tmed$PctBnbTypHost>50] <- "host"
#tmed$BnbType[tmed$PctBnbTypHot>50] <- "hot"
#tmed$BnbType[tmed$PctBnbTypSAPT>50] <- "sapt"
#tmed$BnbType[tmed$PctBnbTypVilla+tmed$PctBnbTypHot+tmed$PctBnbTypSAPT>50] <- "sapt/hot/villa"

table(tmed$BnbType)

#' 
#' ## DoW
## ---------------------------------------------------------------------------------------------------------------------
# tmed$DoW[tmed$PctOrdMon + tmed$PctOrdTue + tmed$PctOrdWed +tmed$PctOrdThu + tmed$PctOrdFri >50] <- 'weekday'
# tmed$DoW[tmed$PctOrdFri >50] <- 'tgif' # special friday
# tmed$DoW[tmed$PctOrdSat +tmed$PctOrdSun >50] <- 'weekend'
# tmed$DoW[is.na(tmed$DoW)] <- 'no_special'

tmed$DoW[tmed$PctOrdMon + tmed$PctOrdTue + tmed$PctOrdWed +tmed$PctOrdThu  >50] <- 'weekday'
tmed$DoW[tmed$PctOrdFri + tmed$PctOrdSat +tmed$PctOrdSun >50] <- 'weekend' 
tmed$DoW[is.na(tmed$DoW)] <-  'xspecial'

table(tmed$DoW)

#' 
#' ## OrderPre
## ---------------------------------------------------------------------------------------------------------------------
tmed$OrderPre['tmed$PctOrd3-'>50] <- 'supernear'
tmed$OrderPre['tmed$PctOrd4+'>50] <- 'near'
tmed$OrderPre[tmed$'PctOrd14+'>50] <- 'normal'
tmed$OrderPre[tmed$'PctOrd30+'>50] <- 'normal+'
tmed$OrderPre[tmed$'PctOrd60+'>50] <- 'early'
tmed$OrderPre[tmed$'PctOrd90+'>50] <- 'early+'
tmed$OrderPre[is.na(tmed$OrderPre)] <- 'neutral'

table(tmed$OrderPre)

#' 
#' ## Season
## ---------------------------------------------------------------------------------------------------------------------
tmed$Season[tmed$PctCheckinDec + tmed$PctCheckinJan + tmed$PctCheckinFeb >50] <- 'winter'
tmed$Season[tmed$PctCheckinMar + tmed$PctCheckinApr + tmed$PctCheckinMay >50] <- 'spring'
tmed$Season[tmed$PctCheckinJun + tmed$PctCheckinJul + tmed$PctCheckinAug + tmed$PctCheckinSep >50] <- 'summer'
tmed$Season[tmed$PctCheckinOct + tmed$PctCheckinNov >50] <- 'autumn'
tmed$Season[is.na(tmed$Season)] <- 'no_special'

table(tmed$Season)

#' 
#' ## TravelDist
## ---------------------------------------------------------------------------------------------------------------------
tmed$Area[tmed$Country=="Taiwan"|tmed$Country=="Hong Kong"|tmed$Country=="Korea"|tmed$Country=="Japan"|tmed$Country=="Macau"|tmed$Country=="Korea-North"]="EastAsia"
tmed$Area[tmed$Country=="Malaysia"|tmed$Country=="Singapore"|tmed$Country=="Thailand"|tmed$Country=="Vietnam"]="EastSouthAsia"
tmed$Area[tmed$Country!="Taiwan"&tmed$Country!="Malaysia"&tmed$Country!="Hong Kong"&tmed$Country!="Korea"&tmed$Country!="Japan"&tmed$Country!="Macau"&tmed$Country!="Korea-North"&tmed$Country!="Singapore"&tmed$Country!="Thailand"&tmed$Country!="Vietnam"]="Others"

## distance
tmed$TravelDist[tmed$Area=="EastAsia" & tmed$PctHK+tmed$PctJP+tmed$PctKR+tmed$PctTW>50]="P_Short"
tmed$TravelDist[tmed$Area=="EastAsia" & tmed$PctMY+tmed$PctSG+tmed$PctTH>50]="P_Medium"
tmed$TravelDist[tmed$Area=="EastSouthAsia" & tmed$PctHK+tmed$PctJP+tmed$PctKR+tmed$PctTW>50]="P_Medium"
tmed$TravelDist[tmed$Area=="EastSouthAsia" & tmed$PctMY+tmed$PctSG+tmed$PctTH>50]="P_Short"
tmed$TravelDist[tmed$Area=="Others"]="P_Long"

##
tmed$TravelDist[tmed$Country=="Taiwan" & tmed$PctTW>50]="P_Local"
tmed$TravelDist[tmed$Country=="Hong Kong" & tmed$PctHK>50]="P_Local"
tmed$TravelDist[tmed$Country=="Malaysia" & tmed$PctMY>50]="P_Local"
tmed$TravelDist[tmed$Country=="Japan" & tmed$PctJP>50]="P_Local"
tmed$TravelDist[tmed$Country=="Korea" & tmed$PctKR>50]="P_Local"
tmed$TravelDist[tmed$Country=="Singapore" & tmed$PctSG>50]="P_Local"
tmed$TravelDist[tmed$Country=="Thailand" & tmed$PctTH>50]="P_Local"

tmed$TravelDist[is.na(tmed$TravelDist)]="No Tendency"

table(tmed$TravelDist)

#' 
#' - TravelDist2
## ---------------------------------------------------------------------------------------------------------------------
tmed$TravelDist2 = tmed$TravelDist
tmed$TravelDist2[tmed$TravelDist2 != "P_Local"] = "N_Local"

table(tmed$TravelDist2)

#' 
#' - TravelDist3
## ---------------------------------------------------------------------------------------------------------------------
tmed$TravelDist3[tmed$Frequency>1 & tmed$Area=="EastAsia" &
                   tmed$PctHK+tmed$PctJP+tmed$PctKR+tmed$PctTW>50]="P_Short"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Area=="EastAsia" &
                   tmed$PctMY+tmed$PctSG+tmed$PctTH>50]="P_Far"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Area=="EastSouthAsia" &
                   tmed$PctHK+tmed$PctJP+tmed$PctKR+tmed$PctTW>50]="P_Far"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Area=="EastSouthAsia" &
                   tmed$PctMY+tmed$PctSG+tmed$PctTH>50]="P_Short"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Area=="Others"]="P_Far"
##
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Taiwan" &
                   tmed$PctTW>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Hong Kong" &
                   tmed$PctHK>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Malaysia" &
                   tmed$PctMY>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Japan" &
                   tmed$PctJP>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Korea" &
                   tmed$PctKR>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Singapore" &
                   tmed$PctSG>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & tmed$Country=="Thailand" &
                   tmed$PctTH>50]="P_Local"
tmed$TravelDist3[tmed$Frequency>1 & is.na(tmed$TravelDist3)]="No Tendency"

table(tmed$TravelDist3)

#' 
## ---------------------------------------------------------------------------------------------------------------------
colnames(tmed)

#' 
## ---------------------------------------------------------------------------------------------------------------------
df = tmed%>% select(c(1:12, 15:21, 161:172))
df$tmedlog = log(df$TMed)
colnames(df)

#' 
#' ## Write to CSV
## ---------------------------------------------------------------------------------------------------------------------
# write.csv(df,"data_1216.csv", row.names = FALSE)

#' 
#' Check the missing values of new data 
## ---------------------------------------------------------------------------------------------------------------------
t = read.csv("data_1216.csv")
sapply(t , function(x) sum(is.na(x)))

#' 
#' OK, so our final dataset is prepared!! Next, we're going to build our models.
#' 
