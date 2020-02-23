# Machine Learning - Regressão 
# Prevendo Despesas Hospitalares

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
setwd("~/project1-data-prediction")
getwd()

# Problema de Negócio: Previsão de Despesas Hospitalares

# Para esta análise, vamos usar um conjunto de dados simulando despesas médicas hipotéticas 
# para um conjunto de pacientes espalhados por 4 regiões do Brasil.
# Esse dataset possui 1.338 observações e 7 variáveis.

# Etapa 1 - Coletando os dados
library(readr)
despesas <- read_csv(file.choose())
View(despesas)

# Etapa 2: Explorando e Preparando os Dados
# Visualizando as variáveis
str(despesas)

# Medias de Tendência Central da variável gastos
summary(despesas$gastos)

# Construindo um histograma
hist(despesas$gastos, main = 'Histograma', xlab = 'Gastos')

# Tabela de contingência das regiões
table(despesas$regiao)

# Explorando relacionamento entre as variáveis: Matriz de Correlação
cor(despesas[c("idade", "bmi", "filhos", "gastos")])

# Nenhuma das correlações na matriz é considerada forte, mas existem algumas associações interessantes. 
# Por exemplo, a idade e o bmi (IMC) parecem ter uma correlação positiva fraca, o que significa que 
# com o aumento da idade, a massa corporal tende a aumentar. Há também uma correlação positiva 
# moderada entre a idade e os gastos, além do número de filhos e os gastos. Estas associações implicam 
# que, à media que idade, massa corporal e número de filhos aumenta, o custo esperado do seguro saúde sobe. 

# Visualizando relacionamento entre as variáveis: Scatterplot
# Perceba que não existe um claro relacionamento entre as variáveis
pairs(despesas[c("idade", "bmi", "filhos", "gastos")])

# Scatterplot Matrix
install.packages("psych")
library(psych)

# Este gráfico fornece mais informações sobre o relacionamento entre as variáveis
pairs.panels(despesas[c("idade", "bmi", "filhos", "gastos")])

# Etapa 3: Treinando o Modelo (usando os dados de treino)
?lm
modelo <- lm(gastos ~ idade + filhos + bmi + sexo + fumante + regiao, data = despesas)

# Similar ao item anterior
modelo <- lm(gastos ~ ., data = despesas)

# Visualizando os coeficientes
modelo

# Prevendo despesas médicas 
?predict

# Aqui verificamos os gastos previstos pelo modelo que devem ser iguais aos dados de treino
previsao1 <- predict(modelo)
View(previsao1)

# Prevendo os gastos com Dados de teste
despesasteste <- read_csv(file.choose())
View(despesasteste)
previsao2 <- predict(modelo, despesasteste)
View(previsao2)



# Etapa 4: Avaliando a Performance do Modelo
# Mais detalhes sobre o modelo
summary(modelo)


# ****************************************************
# *** Estas informa??es abaixo ? que far?oo de voc? ***
# *** um verdadeiro conhecedor de Machine Learning ***
# ****************************************************

# Equa??o de Regress?o
# y = a + bx (simples)
# y = a + b0x0 + b1x1 (m?ltipla)

# Res?duos
# Diferen??a entre os valores observados de uma vari?vel e seus valores previstos
# Seus res?duos devem se parecer com uma distribui??o normal, o que indica
# que a m?dia entre os valores previstos e os valores observados ? pr?ximo de 0 (o que ? bom)

# Coeficiente - Intercept - a (alfa)
# Valor de a na equa??o de regress?o

# Coeficientes - Nomes das vari?veis - b (beta)
# Valor de b na equa??o de regress?o

# Obs: A quest?o ? que lm() ou summary() t?m diferentes conven??es de 
# rotulagem para cada vari?vel explicativa. 
# Em vez de escrever slope_1, slope_2, .... 
# Eles simplesmente usam o nome da vari?vel em qualquer sa?da para 
# indicar quais coeficientes pertencem a qual vari?vel.

# Erro Padr?o
# Medida de variabilidade na estimativa do coeficiente a (alfa). O ideal ? que este valor 
# seja menor que o valor do coeficiente, mas nem sempre isso irá ocorrer.

# Asteriscos 
# Os asteriscos representam os n?veis de signific?ncia de acordo com o p-value.
# Quanto mais estrelas, maior a signific?ncia.
# Aten??o --> Muitos astericos indicam que ? improv?vel que n?o exista 
# relacionamento entre as vari?veis.

# Valor t
# Define se coeficiente da vari?vel ? significativo ou n?o para o modelo. 
# Ele ? usado para calcular o p-value e os n?veis de signific?ncia.

# p-value
# O p-value representa a probabilidade que a vari?vel n?o seja relevante. 
# Deve ser o menor valor poss?vel. 
# Se este valor for realmente pequeno, o R ir? mostrar o valor 
# como nota??o cient?fica

# Signific?ncia
# S?o aquelas legendas pr?ximas as suas vari?veis
# Espa?o em branco - ruim
# Pontos - razo?vel
# Asteriscos - bom
# Muitos asteriscos - muito bom

# Residual Standar Error
# Este valor representa o desvio padr?o dos res?duos

# Degrees of Freedom
# ? a diferen?a entre o n?mero de observa??es na amostra de treinamento 
# e o n?mero de vari?veis no seu modelo

# R-squared (coeficiente de determina??o - R^2)
# Ajuda a avaliar o n?vel de precis?o do nosso modelo. 
# Quanto maior, melhor, sendo 1 o valor ideal.

# F-statistics
# ? o teste F do modelo. Esse teste obt?m os par?metros do nosso modelo 
# e compara com um modelo que tenha menos par?metros.
# Em teoria, um modelo com mais par?metros tem um desempenho melhor. 

# Se o seu modelo com mais par?metros N?O tiver perfomance
# melhor que um modelo com menos par?metros, o valor do p-value ser? bem alto. 

# Se o modelo com mais par?metros tiver performance
# melhor que um modelo com menos par?metros, o valor do p-value ser? mais baixo.

# Lembre-se que correla??o n?o implica causalidade



# Etapa 5: Otimizando a Performance do Modelo

# Adicionando uma vari?vel com o dobro do valor das idades
despesas$idade2 <- despesas$idade ^ 2

# Adicionando um indicador para BMI >= 30
despesas$bmi30 <- ifelse(despesas$bmi >= 30, 1, 0)

View(despesas)

# Criando o modelo final
modelo_v2 <- lm(gastos ~ idade + idade2 + filhos + bmi + sexo +
                   bmi30 * fumante + regiao, data = despesas)

summary(modelo_v2)

# Dados de teste
despesasteste <- read_csv(file.choose())
View(despesasteste)
previsao <- predict(modelo, despesasteste)
class(previsao)
View(previsao)


