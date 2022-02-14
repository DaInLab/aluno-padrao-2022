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

