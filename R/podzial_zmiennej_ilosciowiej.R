install.packages("Information")
install.packages("rpart")
install.packages("DT")
library(Information)
library(gtools)
library(rpart)
library(data.table)
library(DT)

### PRZYK�AD

quanted_feature = quantcut(extract$age) #  bezpieczne
DT::datatable(as.data.frame.matrix(prop.table(table(quanted_feature,extract$pali),margin = 1)))  # powinny by� znacz�co inne rozk�ady

kmeans_feature = kmeans(x = extract$age,centers = 4) # bardzo podatny na warto�ci skrajne, najpierw usuniemy
DT::datatable(as.data.frame.matrix(prop.table(table(kmeans_feature$cluster,extract$pali),margin = 1)))

drzewo = rpart::rpart(pali~age,data = as.data.frame(extract),control = rpart::rpart.control(maxdepth = 2,cp=0.00)) # minsplit = 4000, je�eli nie chcemy zaw
DT::datatable(as.data.frame.matrix(prop.table(table(drzewo$where,extract$pali),margin = 1)))
print(drzewo) # st�d mo�na odczyta� jakie przedzia�y
drzewo$splits


### Kt�ry podzia� jest najlepszy? 
# mo�emy wyliczy� wska�niki, albo na oko, tych zmiennych nie ma tak du�o z tego co widzia�em

