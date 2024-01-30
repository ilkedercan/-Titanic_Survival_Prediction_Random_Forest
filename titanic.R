**3.Veri Setini Yükleme (Tidy Süreci)**

library(readr)
df <- read_csv("/home/ilke/Downloads/train.csv")


**Verinin değişkenleri**
colnames(df)


df %>% 
  glimpse()

## **3.1 Degisken Donusumleri ve Turlerinin Ayarlanmasi**


df <- df %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)


str(df)

**test train ayrımı yapmadan veriye bakalım**
glimpse(df)
profiling_num(df)
plot_num(df)

**sürekli değişkenlerin tümünün min,median,range vs vs detaylı istatistikleri**

library(pastecs)
stat.desc(df)  #sürekli değişkenlerin tümünün min,median,range vs vs detaylı data.frame olarak verir

## **3.2 test-train ayrımı**


set.seed(8367325)
trainIndex <- sample(1:nrow(df), size = round(0.8*nrow(df)), replace=FALSE)
train <- df[trainIndex ,]
test <- df[-trainIndex ,]

train <- train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)


# **4.Feature engineering**
## **4.1 Keşifsel Veri Analizi**
**Train veri setine genel olarak bakalım**

summary(train)


**sütunlardaki çeşitlere bakalım**

apply(train,2, function(x) length(unique(x)))


data <- c(433, 280)
data_percent <- prop.table(data) * 100
library(ggplot2)
data_df <- data.frame(Experience = c("0", "1"), count = c(433, 280))

ggplot(data = data_df, aes(x = "", y = count, fill = Experience)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) + 
  ggtitle("Survived Pie Chart") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_void() + 
  labs(fill = "Survived", title = "Survived Pie Chart") +
  geom_text(aes(label = paste0(round(data_percent), "%")), position = position_stack(vjust = 0.5))


**nicel değişkenlerin histogramlarına bakalım**

plot_num(train)

# Age vs Survived
ggplot(train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  theme_few() +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Survived")

**grafikten kadınların erkeklere oranla hayatta kalma şansının daha fazla olduğunu söyleyebiliriz**

# Sex vs Survived
ggplot(train, aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  theme_few() +
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")

scatter_plot <- ggplot(train, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point() +
  labs(x = "Yaş", y = "Ücret", color = "Hayatta Kalma Durumu") +
  ggtitle("Yaş ve Ücret İlişkisi")

scatter_plot_interactive <- ggplotly(scatter_plot)

scatter_plot_interactive


**1. sınıf yolcu olmanın size daha fazla hayatta kalma şansı verdiğine dair net bir eğilim var.**

ggplot(train, mapping = aes(x = Pclass, fill = Survived, colour = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Pclass') +
  theme(legend.position = "none")

ggplot(train, aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  theme_few()+
  theme(legend.title = element_blank())+
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81))


**Pclass 3'e doğru gidildikçe yaş ortalaması azalmaktadır. 2. ve 3. Pclass'larda aykırı değerler vardır.**
 
ggplot(train[!is.na(train$Age),], aes(x = Pclass, y = Age, fill=Sex )) +
  geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()


train %>%
  ggplot(aes(Fare, fill=Pclass)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  facet_wrap(~ Survived, ncol = 1)

ggplot(data = train, aes(x = SibSp, fill = as.factor(Survived))) + 
  geom_bar(position = 'fill') + 
  labs(y = "Survival Probability", fill = "Survival Probability")

ggplot(data = train, aes(x = Parch, fill = as.factor(Survived))) + 
  geom_bar(position = "fill") + 
  labs(y = "Survival Probability", fill = "Survival Probability")

**Gemide 1-3 kardeş/eş/ebeveyn/çocuk olması (SibSp = 1-2, Parch = 1-3), yalnız kalmaktan (SibSp + Parch = 0) veya geniş bir aileye sahip olmaktan orantılı olarak daha iyi hayatta kalma sayıları önerir.**
  **Geniş aile sahip veya yalnız kişiler daha şanssızdr**
  

scatter_3d <- ggplot(train, aes(x = Age, y = Parch, z = factor(Survived), color = factor(Survived))) +
  geom_point() +
  labs(x = "yaş", y = "Parch", z = "Hayatta Kalma Durumu", color = "Hayatta Kalma Durumu") +
  ggtitle("Yaş, Parch ve Hayatta Kalma Durumu")

scatter_3d_interactive <- ggplotly(scatter_3d)

scatter_3d_interactive


## **4.2 eksik verilerin yapısını inceleme ve Eksik veri sorununu giderme**

**ho: eksik veriler rastgele dağılmıştır.**
  
  **h1: eksik veriler rastgele dağılmamıştır.**

train[train == ""] <- NA #NA girilmemiş boş gözlem olma ihtimaline karşı

test1<- mcar_test(train)
test1  

**p.value<0.5 ho kabul edilemez.
sonuç: %5 anlamlılık düzeyinde eksik veriler rastgele dağılmamıştır.**
  
  **testin sonucunun doğruluğunu anlayabilmek ve çözüm bulabilmek için aşağıdaki adımlar izlendi.**
  
plot_intro(df)


**eksikliklerin hangi sütunlarda olduğunu görelim**

colSums(is.na(train)) 



**DEĞİŞKENLER İÇİN EKSİK DEĞER ORANI**

df_na <- df_status(df)

df_na[,c("variable","q_na","p_na")]


library(mice) 

md.pattern(df) 


**cabin için 77 oranında eksik gözlem var. Bu oran çok fazla. Yani eksiklikten ziyade yokluk olduğunu görüyruz.Bu yüzden bu değişkeni silmek en iyi çözümdür.**

train <- subset(train, select = -Cabin)



**Embarked için eksiklik gözardı edilecek kadar azdır. Bu yüzden en çok tekrar eden ile basit atama yöntemi ile doldurabiliriz**

table(train$Embarked)
train$Embarked[is.na(train$Embarked)] <- "S"


**Age değişkeni için eksiklik oranı 19.87'dir. Yaş değişkeni bizim için önemli olduğundan ve oran yüksek olmadığından bu eksiklikler doldurulmalıdır.**
**Age değişkeninin dağılımına bakmalıyız.**

ggplot(train, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, color = "black", fill = "#F8BBD0") +
  geom_density(color = "red") +
  labs(title = "Age Dağılımı", x = "Yaş", y = "Yoğunluk")


**Age değişkenindeki eksik değerleri randomForest ile doldurmaya karar verdim**
**Age değişkeni için eksik değer sorunu çözüldü**

library(randomForest)

# Eksik verileri doldurmak için kullanılacak değişkenleri seçin
features <- c("Pclass", "SibSp", "Parch", "Fare", "Embarked")

# Eksik verileri içermeyen ve içeren alt veri kümelerini oluşturun
data_without_missing <- train[!is.na(train$Age), ]
data_with_missing <- train[is.na(train$Age), ]

# Random Forest modelini eğitin
rf_model <- randomForest(x = data_without_missing[features], y = data_without_missing$Age, ntree = 100)


# Eksik verileri tahmin et
imputed_ages <- predict(rf_model, newdata = data_with_missing[features])

# Eksik yaş değerlerini doldur
train$Age[is.na(train$Age)] <- imputed_ages
sum(is.na(train$Age))

**Eksik değerleri doldurduktan sonra dağılımına tekrar bakalım**

ggplot(train, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, color = "black", fill = "#F8BBD0") +
  geom_density(color = "red") +
  labs(title = "Age Dağılımı", x = "Yaş", y = "Yoğunluk")



# **5. Model Kurma**

**Target değişkeni olarak Survived'ı kullanarak Random Forest modeli kuracağım**

target_variable <- train$Survived



train_data <- train[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]

rf_model3 <- randomForest(x = train_data, y = target_variable, ntree = 100)


## **5.1 Train Tahmin**
**Modeli kurduktan sonra ilk olarak Train veri seti üzerinden tahmin yaptım.**

# Train veri seti üzerinde tahmin yapma
train_predictions <- predict(rf_model3, newdata = train_data)

# Confusion Matrix oluşturma
confusion_matrix <- table(Actual = target_variable, Predicted = train_predictions)
print(confusion_matrix)

# Doğruluk (Accuracy) hesaplama
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

**Doğruluk 0.91 bulunmuştur.**
  
  ## **5.2 Test Tahmini**
  
  
  **Kurduğum model ile test veri seti üzerinde de tahmin yapacağım. Bunun için ilk olarak test verisi üzerinde de düzenlemeler yapıldı**

library(randomForest)

# Eksik verileri doldurmak için kullanılacak değişkenleri seçin
features <- c("Pclass", "SibSp", "Parch", "Fare", "Embarked")

# Eksik verileri içermeyen ve içeren alt veri kümelerini oluşturun
data_without_missing <- test[!is.na(test$Age), ]
data_with_missing <- test[is.na(test$Age), ]

# Random Forest modelini eğitin
rf_model1 <- randomForest(x = data_without_missing[features], y = data_without_missing$Age, ntree = 100)

# Eksik verileri tahmin et
imputed_ages <- predict(rf_model1, newdata = data_with_missing[features])

# Eksik yaş değerlerini doldur
test$Age[is.na(test$Age)] <- imputed_ages
sum(is.na(test$Age))


test_data <- test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]


test_predictions <- predict(rf_model3, newdata = test_data)



**Kurduğum model ile test verisi üzerinde yaptığım tahminler ve gerçek değerler karşılaştırılarak Confusion Matrix oluşturdum. Ve Accuracy'im 0.87dir**

# Gerçek değerler
test_target_variable <- test$Survived

# Confusion Matrix oluşturma
confusion_matrix <- table(Actual = test_target_variable, Predicted = test_predictions)
print(confusion_matrix)

# Doğruluk (Accuracy) hesaplama
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))



# Gerçek ve tahmin edilen sınıfları kullanarak sınıflandırma raporunu oluşturma
classification_report <- caret::confusionMatrix(data = test_target_variable, reference = test_predictions)
print(classification_report)


**Gerçek değerler ve tahminlerin bir arada olduğu iki sütundan oluşan bir veriseti oluşturdum.Bu veri setinin ilk 5 satırı şu şekildedir.**

# Gerçek değerler ve tahminlerin olduğu yeni bir veri çerçevesi oluşturma
results <- data.frame(Actual = test_target_variable, Predicted = test_predictions)

# Yanyana gösterme
head(results)



## **5.3 Hatanı Matrisinin Görselleştirilmesi**

**Hata matrisi oluşturarak onu görselleştirdim. Hatalı tahminler kırmızı renkte gösteriliyor ve doğru tahminler mavi renkte görüntüleniyor.**


# Hata matrisini oluşturma
error_matrix <- matrix(ifelse(test_target_variable != test_predictions, "1", "0"), nrow = length(test_target_variable))

# Hata matrisi görselleştirmesi
library(ggplot2)

df_error_matrix <- data.frame(x = rep(1:length(test_target_variable), length(test_target_variable)),
                              y = rep(1:length(test_target_variable), each = length(test_target_variable)),
                              error = as.vector(error_matrix))

ggplot(df_error_matrix, aes(x = x, y = y, fill = error)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Hata Matrisi", x = "Gerçek Değerler", y = "Tahminler") +
  theme_minimal()





