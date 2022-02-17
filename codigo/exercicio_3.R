# Exercício 3
# Numa pesquisa realizada com 100 famílias, levantaram-se as seguintes informações:
# Número de filhos 0  1  2  3 4 5 Mais de 5
# Famílias        17 20 28 19 7 4 5
# Construir a planilha em Excel ou Calc (LibreOffice) com as informações acima.
# Faça um programa em R para calcular e gerar os gráficos para: 1. A mediana do número de filhos; e
# 2. A moda do número de filhos

# A planilha foi digitada no Calc do software LibreOffice
# Portanto precisamos da biblioteca readODS
if(!("readODS") %in% installed.packages()) install.packages("readODS")
library(readODS)

# Importação dos dados da planilha no formato .ODS (OpenDocument Spreadsheet - Planilha OpenDocument)
df.ex3 <- read_ods("./dados/exercicio_3.ods")

# Cálculo da mediana
total_familias = sum(df.ex3$qtde_familias)
# Cálculo da freqência acumulada das famílias
freq_acumulada <- cumsum(df.ex3$qtde_familias)
# Adicionando o número de filhos correspondente à frequencia acumulada
names(freq_acumulada) = df.ex3$qtde_filhos
# Mediana: a mediana (fórmula) é dada por Md = (X1[n/2] + X2[n/2 + 1])/2, onde x é o número de filhos e n a somatória da qtde de famílias
x1 = freq_acumulada[I(total_familias/2 <= freq_acumulada)]
x2 = freq_acumulada[I((total_familias/2) + 1 <= freq_acumulada)]
# Pegar o primeiro menor dos dois casos
mediana = (as.numeric(names(x1[1])) + as.numeric(names(x2[1]))) /2
print(paste0("Médiana do número de filhos = ", mediana))

# Cálculo do moda
## A moda é o valor da classe que mais se repete. 
maior_qtde_familias <- max(df.ex3$qtde_familias)
# Descobrir o índice correspondente a este número 
indice <- which (df.ex3$qtde_familias == maior_qtde_familias)[[1]]
moda <- df.ex3$qtde_filhos [indice]
print(paste0("Moda do número de filhos = ", moda))

# Cálculo da média: como não se sabe a quantidade de filhos da última classe (5 famílias tem MAIS de 5 filhos)
# Não dá para calculá-la.
# Neste exemplo, estamos supondo que MAIS de 5 filhos = NA 
# Neste caso, a média seria calculada através da fórmula de "média ponderada"
total_filhos = df.ex3$qtde_familias * df.ex3$qtde_filhos
total_familias = sum(df.ex3$qtde_familias, na.rm = T)
media_pond_filhos = sum(total_filhos, na.rm = T) / total_familias
print(paste0("Média de filhos = ", round(media_pond_filhos,0)))

barplot(data = df.ex3, 
        main = "Pesquisa famílias e número de filhos",
        xlab = "Número de Famílias",
        ylab = "Quantidade de Filhos",
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
          qtde_filhos ~ qtde_familias)

