if(!("readODS") %in% installed.packages()) install.packages("readODS")
library(readODS)
df.ex3 <- read_ods("./dados/exercicio_3.ods")
total_filhos = df.ex3$qtde_familias * df.ex3$qtde_filhos
total_familias = sum(df.ex3$qtde_familias)
media_pond_filhos = sum(total_filhos) / total_familias
print(paste0("Média ponderada de filhos = ", round(media_pond_filhos,0)))


barplot(data = df.ex3, 
        main = "Pesquisa famílias e número de filhos",
        xlab = "Número de Famílias",
        ylab = "Quantidade de Filhos",
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
          qtde_filhos ~ qtde_familias)

