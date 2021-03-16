install.packages("Information")
install.packages("rpart")
install.packages("DT")
library(Information)
library(gtools)
library(rpart)
library(data.table)
library(DT)

### PRZYK£AD

quanted_feature = quantcut(extract$age) #  bezpieczne
DT::datatable(as.data.frame.matrix(prop.table(table(quanted_feature,extract$pali),margin = 1)))  # powinny byæ znacz¹co inne rozk³ady

kmeans_feature = kmeans(x = extract$age,centers = 4) # bardzo podatny na wartoœci skrajne, najpierw usuniemy
DT::datatable(as.data.frame.matrix(prop.table(table(kmeans_feature$cluster,extract$pali),margin = 1)))

drzewo = rpart::rpart(pali~age,data = as.data.frame(extract),control = rpart::rpart.control(maxdepth = 2,cp=0.00)) # minsplit = 4000, je¿eli nie chcemy zaw
DT::datatable(as.data.frame.matrix(prop.table(table(drzewo$where,extract$pali),margin = 1)))
print(drzewo) # st¹d mo¿na odczytaæ jakie przedzia³y
drzewo$splits


### Który podzia³ jest najlepszy? 
# mo¿emy wyliczyæ wskaŸniki, albo na oko, tych zmiennych nie ma tak du¿o z tego co widzia³em