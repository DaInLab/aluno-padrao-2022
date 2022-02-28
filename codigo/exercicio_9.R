# Exercício 9
# Em um hospital foram coletados os salários (em salários mínimos) de 36 funcionários.
# Os resultados estão dispostos na planilha Excel “exercicio9.xls”.
# Construa o programa em R que calcule e gere a distribuição de frequências em intervalos de amplitude 2 e o histograma correspondente.

# Instalando biblioteca readxl para importar para o R arquivos com extensão .xls ou .xlsx.
if (!("readxl") %in% installed.packages()) install.packages("readxl")

# Carregando a biblioteca
library(readxl)

# Importando o arquivo do exercício 1
df.ex9 <- read_excel("./dados/exercicio9.xls")

df.ex9$salarios <- df.ex9$Salários
df.ex9$Salários <- NULL

quebras <- table(cut(df.ex9$salarios, breaks = seq(3,26,by=2),right=FALSE)) # Frequencias por categorias
quebras
tabela <- rbind(quebras, p_fi = 100*prop.table(quebras)) # Frequencias relativas em %
tabela
#quebras de linha apenas ilustrativas para facilitar a leitura
tabela <- as.data.frame(
  t(cbind(
    tabela,
    c(sum(tabela[1,]),sum(tabela[2,])
    ))),row.names =c(colnames(tabela),"Total")) #Construção da tabela
tabela_display<-transform(tabela,p_fi=round(p_fi,digits=2))
tabela_display

hist(df.ex9$salarios, breaks = seq(3,26,by=2))
