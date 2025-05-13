A1 <- read.csv("C:/Users/hacer/Downloads/harcama.csv", header = TRUE, sep = ";")

str(A1)

head(A1)



library(ggplot2)
library(scales)


renkler <- c(
  "Ilkokul" = "#FF69B4",      
  "Okul_oncesi" = "#8B0000",    
  "Ortaogretim" = "#C71585",    
  "Ortaokul" = "#FF0000", 
  "Yuksekogretim" = "#001F54"   
)

ggplot(A1, aes(x = Yil, y = Ogrenci_Basina_Dusen_Harcama_TL, fill = Egitim_duzeyi)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = renkler) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Egitim Duzeylerine Gore Yillik Ogrenci Basina Dusen Harcama", 
    x = "Yil",  
    y = "Ogrenci Basina Dusen Harcama (TL)", 
    fill = "Egitim Duzeyi"  
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










A2<- read.csv("C:/Users/hacer/Downloads/yuksekogretim_verileri.csv", header = TRUE, sep = ";")

str(A2)

head(A2)



library(ggplot2)
library(tidyr)
library(scales)  


A2_long <- pivot_longer(A2, 
                        cols = c("Yuksekogretim_ogrenci", "Yuksekogretim_Mezun"),
                        names_to = "Kategori", 
                        values_to = "Sayi")

ggplot(A2_long, aes(x = Ogretim_yili, y = Sayi, fill = Kategori)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Yuksekogretim_ogrenci" = "#e91e63",
                               "Yuksekogretim_Mezun" = "#1a237e")) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Y??llara Gore Yukseko??retimde  O??renci ve Mezun Say??s??",
       x = "????retim Y??l??",
       y = "Ki??i Say??s??",
       fill = "Kategori") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







library(ggplot2)
library(reshape2)


A3 <- read.csv("C:/Users/hacer/Downloads/tamamlama_orani.csv", header = TRUE, sep = ";")

A3_melted <- melt(A3, id.vars = "Yil")

A3_melted$value <- as.numeric(gsub(",", ".", A3_melted$value))

renk_paleti <- c(
  "#f5a5b8",  
  "#f44336",  
  "#e91e63",  
  "#9b2d20",  
  "#1a237e",  
  "#607d8b"   
)

ggplot(A3_melted, aes(x = as.factor(Yil), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = renk_paleti) +
  labs(
    title = "Egitim Seviyesi ve Cinsiyete Gore Tamamlama Orani",
    x = "Yil",
    y = "Tamamlama Orani (%)",
    fill = "Kategori"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13, face = "bold", color = "#444444"),
    plot.title = element_text(size = 15, face = "bold", color = "#b03060"),
    legend.title = element_text(face = "bold"),
    legend.position = "top"
  )









A4 <- read.csv("C:/Users/hacer/Downloads/okuma_yazma_bilmeyen.csv", header = TRUE, sep = ";")

str(A4)

head(A4)



library(ggplot2)
library(tidyr)
library(scales)

A4_long <- pivot_longer(A4, cols = c("Erkek", "Kadin"), names_to = "Cinsiyet", values_to = "Sayi")

ggplot(A4_long, aes(x = Yil, y = Sayi, color = Cinsiyet)) +
  geom_line(size = 1.2) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Kadin" = "#C71585",
                                "Erkek" = "#000080")) +  
  labs(title = "Y??llara Gore Okuma Yazma Bilmeyen Say??s??",
       x = "Y??l",
       y = "Okuma Yazma Bilmeyen Ki??i Say??s??",
       color = "Cinsiyet") +
  theme_minimal()











A5 <- read.csv("C:/Users/hacer/Downloads/okuma_yazma_bilen.csv", header = TRUE, sep = ";")

str(A5)

head(A5)



library(ggplot2)
boxplot(A5$Okuma_yazma_bilen_fakat_bir_okul_bitirmeyen,
      horizontal = TRUE,
      col = "deeppink",   
      border = "navy",  
      outline = FALSE     
      )
        
        
        
        
        