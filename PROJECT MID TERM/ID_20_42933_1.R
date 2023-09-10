library(readxl)
Titanic_Modified<- read_excel("D:/INTRODUCTION TO DATA SCIENCE/Project/Titanic - Modified.xlsx")

summary(Titanic_Modified)


colSums(is.na(Titanic_Modified))


which(is.na(Titanic_Modified$age))
which(is.na(Titanic_Modified$gender))
which(is.na(Titanic_Modified$embarked))
which(is.na(Titanic_Modified$class))

Titanic_Modified$age[is.na(Titanic_Modified$age)] <- mean(Titanic_Modified$age, na.rm = TRUE)

Titanic_Modified$gender[is.na(Titanic_Modified$gender)] <- 'NULL'
Titanic_Modified$embarked[is.na(Titanic_Modified$embarked)] <- 'NULL'
Titanic_Modified$class[is.na(Titanic_Modified$class)] <- 'NULL'


Titanic_Modified$gender <- ifelse(Titanic_Modified$gender <1, "Male", "Female")
Titanic_Modified$age <- ifelse(Titanic_Modified$age > 120, Titanic_Modified$age/10, Titanic_Modified$age)

Titanic_Modified$alone <- ifelse(Titanic_Modified$alone == "FALL", "FALSE", Titanic_Modified$alone)

rdata = Titanic_Modified$age
print(rdata)
print(max(rdata, na.rm=TRUE))
print(min(rdata, na.rm=TRUE))
print(max(rdata, na.rm=TRUE))-print(min(rdata, na.rm=TRUE))


fdata = Titanic_Modified$fare
print(fdata)
print(max(fdata, na.rm=TRUE))
print(min(fdata, na.rm=TRUE))
print(max(fdata, na.rm=TRUE))-print(min(fdata, na.rm=TRUE))


vc <- table(Titanic_Modified$age)
max_vc <- max(vc)
age_modes <- as.numeric(names(vc[vc == max_vc]))
print(age_modes)

vf <- table(Titanic_Modified$fare)
max_vf <- max(vf)
fare_modes <- as.numeric(names(vf[vf == max_vf]))
print(fare_modes)

vs <- table(Titanic_Modified$survived)
max_vs <- max(vs)
surv_modes <- as.numeric(names(vs[vs == max_vs]))
print(surv_modes)

vsb <- table(Titanic_Modified$sibsp)
max_vsb <- max(vsb)
sib_modes <- as.numeric(names(vsb[vsb == max_vsb]))
print(sib_modes)

vp <- table(Titanic_Modified$parch)
max_vp <- max(vp)
parch_modes <- as.numeric(names(vp[vp == max_vp]))
print(parch_modes)



x <- Titanic_Modified$age
sd(x)
y <- Titanic_Modified$fare
sd(y)
z <- Titanic_Modified$survived
sd(z)
a <- Titanic_Modified$parch
sd(a)
b <- Titanic_Modified$sibsp
sd(b)

age <- Titanic_Modified$age
hist(age, xlab = "No.of Human ",
     col = "blue", border = "white")

fare <- Titanic_Modified$fare
hist(fare, xlab = "Ticket price values ",
     col = "blue", border = "white")

survived <- Titanic_Modified$survived
hist(survived, xlab = "Survived peoples ",
     col = "blue", border = "white")


parch <- Titanic_Modified$parch
hist(parch, xlab = "Parents and Children ",
     col = "blue", border = "white")


sibsp <- Titanic_Modified$sibsp
hist(sibsp, xlab = "Siblings ",
     col = "blue", border = "white")


