#########################
#      PROMETHEE        #
#########################
?PROMETHEE

# Pacotes
if(!require(PROMETHEE)) {install.packages("PROMETHEE"); require(PROMETHEE)}

vetor <- rep("Lienar", times = 577)

linhas <- "5570"

### --- Etapa 1 ----

# DADOS (alternativas e critérios)
# Alternativas: linhas
# Critérios: colunas
dataset <- dados
head(dataset)


# Extraindo as informações relacionadas à matriz de avaliação
# Retirar colunas que não serão avaliadas
glimpse(dataset)

dataset <- dataset %>% select(casos19, difcasos_1, difcasos_2)
head(dataset)

### --- Etapa 2 ----

# PREFERÊNCIAS
# Declarar as preferências, com base nas alternativas
# isso envolve definir uma função de preferência e os limites
# de preferência/indiferença de acordo

# FUNÇÃO DE PREFERÂNCIA
# Funções: Level, Linear, V-shape e Gaussian -> futuramente Usual e U-shape

casos19 <- rep("V-shape", times = linhas)
difcasos_1 <- rep("V-shape", times = linhas)
difcasos_2 <- rep("V-shape", times = linhas)
difmenor_1 <- rep("V-shape", times = linhas)
difmenor_2 <- rep("V-shape", times = linhas)
difexam_1 <- rep("V-shape", times = linhas)
difexam_2 <- rep("V-shape", times = linhas)
difgif_1 <- rep("V-shape", times = linhas)
difgif_2 <- rep("V-shape", times = linhas)


PreferenceF <- cbind(casos19, 
                     difcasos_1, 
                     difcasos_2,
                     difmenor_1,
                     difmenor_2,
                     difexam_1,
                     difexam_2,
                     difgif_1,
                     difgif_2)

# carregando a matriz de funções de preferência (aninhadas a um comando as.matrix)
PreferenceF <- read.csv()
head(PreferenceF)


# LIMITE DE PREFERÊNCIA
# Definir os limites de preferência, ou seja...
# Afirmar a diferença (sob uma função específica) entre as 
# alternativas em cada critério


# decis
quantile(bd_g3$difgif_2, seq(0.10, 0.9, 0.1))


casos19 <- rep(51, times = linhas)
difcasos_1 <- rep(-30, times = linhas)
difcasos_2 <- rep(-17, times = linhas)
difmenor_1 <- rep(-2, times = linhas)
difmenor_2 <- rep(-1, times = linhas)
difexam_1 <- rep(-70, times = linhas)
difexam_2 <- rep(-170, times = linhas)
difgif_1 <- rep(10, times = linhas)
difgif_2 <- rep(128, times = linhas)


PreferenceT <- cbind(casos19, 
                     difcasos_1,
                     difcasos_2,
                     difmenor_1,
                     difmenor_2,
                     difexam_1,
                     difexam_2,
                     difgif_1,
                     difgif_2)

# Carregando a matriz limites de preferência (aninhada e, um 
# comando data.matrix)
PreferenceT <- read.csv()
head(PreferenceT)


# LIMITE DE INDIFERENÇA
# O limiar de indiferença indica o oposto, ou seja, a diferença 
# entre duas alternativas (sob uma função específica)
# que torna insignificante a comparação entre alternativas em um critério específico

casos19 <- rep(1, times = linhas)
difcasos_1 <- rep(-1, times = linhas)
difcasos_2 <- rep(-1, times = linhas)
difmenor_1 <- rep(0, times = linhas)
difmenor_2 <- rep(0, times = linhas)
difexam_1 <- rep(0, times = linhas)
difexam_2 <- rep(0, times = linhas)
difgif_1 <- rep(0, times = linhas)
difgif_2 <- rep(0, times = linhas)


IndifferenceT <- cbind(casos19, 
                       difcasos_1, 
                       difcasos_2,
                       difmenor_1,
                       difmenor_2,
                       difexam_1,
                       difexam_2,
                       difgif_1,
                       difgif_2)

# Carregando a matriz de limites de indiferença (aninhados em um comando data.matrix)
IndifferenceT <- read.csv()
head(IndifferenceT)


# LIMITE DE PREFERÊNCIA DE GAUSS
# Esta etapa se aplica independentemente da preferência do tomador de decisão
# em incluir ou não essa função
# Outros critérios que não envolvem essa função assumem o valor 0
# Nota: Se no seu caso particular não envolve esta função de preferência, ela
# deve ser carregada, mas valores 0 podem ser preenchidos em cada critério
# para cada alternativa

casos19 <- rep(0, times = linhas)
difcasos_1 <- rep(0, times = linhas)
difcasos_2 <- rep(0, times = linhas)
difmenor_1 <- rep(0, times = linhas)
difmenor_2 <- rep(0, times = linhas)
difexam_1 <- rep(0, times = linhas)
difexam_2 <- rep(0, times = linhas)
difgif_1 <- rep(0, times = linhas)
difgif_2 <- rep(0, times = linhas)


S_Gauss <- cbind(casos19, 
                 difcasos_1, 
                 difcasos_2,
                 difmenor_1,
                 difmenor_2,
                 difexam_1,
                 difexam_2,
                 difgif_1,
                 difgif_2)

# Carregando a matriz de preferência de Gauss (aninhada em um comando data.matrix)
S_Gauss <- read.csv()
head(S_Gauss)


# PESOS
# Os pesos refletem a importância de cada critério e geralmente variam
# entre 0 e 1 (com a soma de todos os pesos sendo 1)
# Em um cenário que o decisor esteja interessado igualmente em todos
# os critérios, de modo que não os discrimina, eventualmente dando a
# cada critério um peso igual a 1/n (onde n é o número de critérios)
# Dado que sejam quatro critérios, cada um pesa 1/4, por tanto, 25%

casos19 <- rep(0.50, times = linhas)
difcasos_1 <- rep(0.05, times = linhas)
difcasos_2 <- rep(0.05, times = linhas)
difmenor_1 <- rep(0.025, times = linhas)
difmenor_2 <- rep(0.025, times = linhas)
difexam_1 <- rep(0.025, times = linhas)
difexam_2 <- rep(0.025, times = linhas)
difgif_1 <- rep(0.025, times = linhas)
difgif_2 <- rep(0.025, times = linhas)

sum(0.50, 0.20, 0.15, 0.03, 0.03, 0.03, 0.02,0.02, 0.02)


Weights <- cbind(casos19, 
                 difcasos_1, 
                 difcasos_2,
                 difmenor_1,
                 difmenor_2,
                 difexam_1,
                 difexam_2,
                 difgif_1,
                 difgif_2)

# Carregando a matriz de pesos (aninhada em um comando data.matrix)
Weights <- read.csv()
head(Weights)


# DIREÇÃO DE CRITÉRIOS
# isso indica se um critério deve ser minimizado ou maximizado
# Min deve ser usado quando valoers baixos denotam melhor desempenho na avaliação
# Max deve ser usado quando valores altos denotam melhor desempenho

casos19 <- rep("max", times = linhas)
difcasos_1 <- rep("min", times = linhas)
difcasos_2 <- rep("min", times = linhas)
difmenor_1 <- rep("min", times = linhas)
difmenor_2 <- rep("min", times = linhas)
difexam_1 <- rep("min", times = linhas)
difexam_2 <- rep("min", times = linhas)
difgif_1 <- rep("max", times = linhas)
difgif_2 <- rep("max", times = linhas)


Min_Max <- cbind(casos19, 
                 difcasos_1, 
                 difcasos_2,
                 difmenor_1,
                 difmenor_2,
                 difexam_1,
                 difexam_2,
                 difgif_1,
                 difgif_2)

# carregando a matriz de direções (aninhada em um comando as.matrix)
Min_Max <- read.csv()
head(Min_Max)



### --- Etapa 3 ----

# FASE DE AVALIAÇÃO
# Finalizada a etapa anterior deve-se carregar as matrizes na ordem:
# dados, função de preferência, limites de preferência e indiferença.
# pesos, direção dos critérios e preferência de Gauss
PF <- PROMETHEE(dataset, 
                PreferenceF, 
                PreferenceT, 
                IndifferenceT, 
                Weights,
                Min_Max,
                S_Gauss)

# Este comando retorna um objeto PF com os outputs:
# matrizes outranking/non-outranking, fluxos Unicriterion e as pontuações
# PROMETHEE I E PROMETHEE II [fluxos, phi]

# OUTRANKING MATRIX
PF$Outranking


# Non-Outranking matrix
PF$Nonoutranking


# UNI-CRITERION NET FLOWS
PF$UnicriterionNetFlows


# PROMETHEE I (Phi+ e Phi-)
PF$PROMETHEE1


# PROMETHEE II (Phi-net)
PF$PROMETHEE2


promethee <- PF$PROMETHEE2

bd_prom <- cbind(dados, promethee)
bd_prom 

###################
#  FIM do código  #
###################