### Preparación de los datos

lifetab <- read.csv2("~/Desktop/Universidad/Máster Estadistica Investigación Operativa/Segon Semestre/Estadistica Actuarial/LIFETAB.csv")

library(lifecontingencies)

lifetabH <- lifetab[, -3]

names(lifetabH)<- c("x", "qx")

lifetabHLt <- probs2lifetable(lifetabH[, "qx"], type = "qx", name = "Tabla Hombres")

lifetabF <- lifetab[,-2]
names(lifetabF) <- c("x", "qx")
lifetabFLt <- probs2lifetable(lifetabF[,"qx"], type = "qx", name = "Tabla Mujeres")

### Ejercicio 1. Creamos la tabla de vida conjunta en R

lifetabHF <- list(lifetabHLt, lifetabFLt)

### Ejercicio 2. Para una asegurada de 36 años, calcula las siguientes probabilidades:

#### a) Probabilidad de que fallezca antes de cumplir los 38 años.

prob_muerte_antes_38 <- qxt(lifetabFLt, x = 36, t = 2)

#### b) Probabilidad de que fallezca entre los 38 y los 41 años.

prob_muerte_entre_38_41 <- qxt(lifetabFLt, x = 36, t = 5) - qxt(lifetabFLt, x = 36, t = 2)

pxt(lifetabFLt, 36, 2)*qxt(lifetabFLt, 38, 3)

#### c) Calcula el APV (actuarial present value, o prima pura única) de la siguiente póliza: la asegurada tiene 36 años y los beneficiarios cobrarían las siguientes cantidades en los siguientes casos:

r <- 1.75/100
v <- 1/(1+r)

##### Si fallece antes de cumplir los 38 años, los beneficiarios cobrarían 80000 euros el día en que la asegurada hubiese cumplido los 38 años.

APV_antes_38 <- 80000 * prob_muerte_antes_38 * v^2

##### Si fallece entre los 38 y los 41 años, entonces los beneficiarios cobrarían 100000 euros el día en que la asegurada hubiese cumplido los 41 años.

APV_entre_38_41 <- 100000 * prob_muerte_entre_38_41 * v^5

##### ¿Cuál es la probabilidad de que la compañía aseguradora no tenga que pagar ninguna indemnización?

pxt(lifetabFLt,36,5)

### Ejercicio 3. Calcula el APV (actuarial present value, o prima pura única) de la siguiente póliza: la asegurada es una mujer de 36 años de edad, y, en caso de que fallezca, la indemnización se pagará a sus beneficiarios al final del año en que ocurra el fallecimiento. El periodo de cobertura es de 10 años. Si fallece durante el primer año, la indemnización será de 30000 euros. A partir de ese momento, la indemnización se incrementará a razón de 5000 euros al año.

#### (mi sol) profe dice que tambien lo podemos hacer asi si resulta mas fácil
x <- 36
n <- 10

tabla_act <- new("actuarialtable",
                 x = lifetabFLt@x,
                 lx = lifetabFLt@lx,
                 interest = r,
                 name = "Tabla Mujeres Actuarial")

APV <- 25000 * Axn(tabla_act, x = x, n = n) + 5000 * IAxn(tabla_act, x = x, n = n)
APV

### Apartado 4. Calcula el APV (actuarial present value, o prima pura única) de las siguientes pólizas:

#### a. Seguro que contrata una pareja de 33 años el hombre y 31 la mujer. Se paga una indemnización a los beneficiarios en caso de que ocurra la disolución de la pareja. La cobertura es de 8 años. La indemnización es de 100000 euros, y el pago se realiza al final del año en que ocurre la disolución.

### Datos
beneficio <- 100000

tabla_act_h <- new("actuarialtable",
                   x = lifetabHLt@x,
                   lx = lifetabHLt@lx,
                   interest = r,
                   name = "Hombres")

tabla_act_m <- new("actuarialtable",
                   x = lifetabFLt@x,
                   lx = lifetabFLt@lx,
                   interest = r,
                   name = "Mujeres")

JointAct <- list(tabla_act_h, tabla_act_m)

APV_disolucion <- beneficio *
  Axyzn(JointAct, x = c(33, 31), i = i, n = 8, status = "joint")

APV_disolucion


### Ej 4 sol profe mejor mirar este

## a)

1000000*Axyzn(lifetabHF, x=c(33,31), i=r, n=8, status = "joint")



#### b) Seguro que contrata una pareja de 41 años el hombre y 40 la mujer. Se paga una indemnización a los beneficiarios en caso de que ocurra la extinción de la pareja. La cobertura es de 12 años. La indemnización es de 150000 euros, y el pago se realiza al final del año en que ocurre la extinción.

150000*Axyzn(lifetabHF, x=c(41,40), i=r, n=12, status = "last")



