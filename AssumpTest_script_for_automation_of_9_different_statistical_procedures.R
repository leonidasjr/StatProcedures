## MODELOS QUANTITATIVOS EM FONÉTICA E FONOLOGIA (LL268-HL005)                  ##     
## Script para realizacao de procedimentos estatísticos:                        ##
## testes de pressupostos estatísticos para Teste-T (2 amostras),               ##
## ANOVA (1 fator) e Regressão linear,                                          ##
## reporte de resultados para artigos e plotagem de gráficos.                   ##
## COPYRIGHT (C) SILVA JR., Leônidas; BARBOSA, Plínio A., 2021                  ##
## Grupo de Estudos de Prosódia da Fala - IEL/UNICAMP                           ##   
## E-mail: leonidas.silvajr@gmail.com                                           ##
##################################################################################

## PROCEDIMENTOS ESTATÍSTICOS

##### procedimento 1:update R -------

## verifique se o R está atualizado
install.packages("installr")
library(installr)
updateR()

# ou desta forma
if(!require(installr)) { 
  install.packages("installr"); require(installr)
}
updateR()

##### fim do procedimento 1 -------

##### procedimento 2: carregando  os dados -------
## CONJUNTO 1
dadosLL268HL005 <- read.csv("C:/Users/Leonidas/Dropbox/UNICAMP/FoneticaL2/LL268/Prosodia_Estatistica/dadosLL268HL005.txt", 
                            sep="", stringsAsFactors=TRUE)
#View(dadosLL268HL005)
# ommitindo NAs
#dadosLL268HL005 <- na.omit(dadosLL268HL005)
# anexar os dados
attach(dadosLL268HL005)

## CONJUNTO 2
dataPD <- read.delim2("C:/Users/Leonidas/Dropbox/UNICAMP/PD/Resultados/metricsAmE-BPwithGrade.txt", 
                      stringsAsFactors=TRUE)
#View(dataPD)
# ommitindo NAs
#dataPD <- na.omit(dataPD)
# anexar os dados# anexar os dados
attach(dataPD)
##### fim do procedimento 2 -------

##### procedimento 3: modelando os dados -------

## CONJUNTO 00 - t.test()
ex_ttest <- t.test(f0min ~ audiofile, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
#ex_ttest

ex_ttest_varTRUE <- t.test(f0min ~ audiofile, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F)
#ex_ttest_varTRUE

## CONJUNTO 1 - lm()
fit00 <- lm(df0negmean ~ df0posmean, data = dadosLL268HL005)
#summary(fit00)

fit01 <- lm(df0negmean ~ audiofile, data = dadosLL268HL005)
#summary(fit01)

fit02 <- lm(df0negmean ~ chunk, data = dadosLL268HL005)
#summary(fit02)

fit03 <- lm(f0med ~ chunk, data = dadosLL268HL005)
#summary(fit03)

fit04 <- lm(df0posmean ~ chunk, data = dadosLL268HL005)
#summary(fit04)

fit05 <- lm(f0min ~ chunk, data = dadosLL268HL005)
#summary(fit05)

fit06 <- lm(emph ~ chunk, data = dadosLL268HL005)
#summary(fit06)

## CONJUNTO 2 - lm()
mod00 <- lm(deltaC ~ percV, data = dataPD)
summary(mod00)

mod01 <- lm(deltaV ~ Lang, data = dataPD)
summary(mod01)

mod02 <- lm(varcoS ~ Chunk, data = dataPD)
summary(mod02)

mod03 <- lm(rPVIC ~ Lang, data = dataPD)
summary(mod03)

mod04 <- lm(nPVIV ~ Chunk, data = dataPD)
summary(mod04)

mod05 <- lm(rPVIC ~ Chunk, data = dataPD)
summary(mod05)

mod06 <- lm(rPVIS ~ Chunk, data = dataPD)
summary(mod06)
##### fim do procedimento 3 -------

##### procedimento 4: plotando gráfico no R basics built-in functions -------
  ## BOXPLOTS #----#----####

## boxplots dos parãmetros significativos
# plotaremos na seguinte ordem:

#--------figura figura figura
#--------figura figura figura

# plota as linhas primeiramente
par(mfrow=c(2,3))

# plota as colunas primeiramente
#par(mfcol=c(3,2))

boxplot(f0min ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Mínima - F0",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(f0med ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Mediana - F0",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(f0sd ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "",
        xlab = "Language",
        main = "Desv.Pad. - F0",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(f0SAQ ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "",
        xlab = "Language",
        main = "Semi-amplitude interquartílica - F0",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(df0posmean ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "",
        xlab = "Language",
        main = "Inclinação da derivada - F0 \n (positiva)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(df0negmean ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Inclinação da derivada - F0 \n (negativa)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(df0sdneg ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Desv.Pad. da derivada - F0 \n (negativa)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(hnr ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Proporção sinal-ruído \n (harmonic-to-nise ratio)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(SPI ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Índice de fonação amortecida \n (soft phonation index)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(shimmer ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Shimmer \n (ampF0 - redução de resistência glótica)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)

boxplot(jitter ~ audiofile+chunk,
        col = c("firebrick", "lightblue"), 
        #names = c("1st","2nd")
        #ylab = "", 
        xlab = "Language",
        main = "Jitter \n (F0- redução de controle das pregas vocais)",
        cex.lab=1.5,
        cex.axis=0.9,
        las=1,
        dadosLL268HL005)
par(mfrow=c(1,1))
  
  #----#----#----
  ## GRÁFICOS DE DISPERSÃO (scatterplots) #----#----####

#checando a correlação entre as variáveis selecionadas para o plot:
cor(f0max, f0min)
## [1] 0.6497565

plot(f0max ~ f0min,
     xlim=c(230, 400),
     ylim=c(100, 220),
     ylab = "Mínima de F0", 
     xlab = "Máxima de F0",
     main = "F0 MÀXIMA vs. F0 MÍNIMA",
     cex.lab=1.2,
     cex.axis=1.2,
     pch=16,
     col="blue",
     cex=1.2,
     las=1,
     dadosLL268HL005)
points(dadosLL268HL005$f0max[dadosLL268HL005$audiofile=='AmE'], 
       (dadosLL268HL005$f0min[dadosLL268HL005$audiofile=='AmE']), 
       pch=16, 
       cex=1.2, 
       col="red")
points(dadosLL268HL005$f0max[dadosLL268HL005$audiofile=='BP'], 
       (dadosLL268HL005$f0min[dadosLL268HL005$audiofile=='BP']), 
       pch=16, 
       cex=1.2, 
       col="blue")
abline(lm(f0min ~ f0max, data = dadosLL268HL005),
       col=1,
       lty=1,
       lwd=2)
grid()
legend(x=370, y=130, legend=c("AmE", "BP"), fill=c("red", "blue"))

  #----#----#----
  ## HISTOGRAMAS #----#----####

hist(f0max,
     prob=T,
     #análogo a prob=T, freq=F
     xlim=c(200, 400),
     ylim=c(0, 0.015),
     breaks=seq(from=200, to=400, by=20),
     #breaks representa as "bins"  do hist. Podemos usar simplesmente, breaks="numero desejado"
     #ylab = "Proporção", 
     xlab = "Máxima de F0",
     main = "Histograma da F0 MÀXIMA",
     las=1,
     cex.lab=1.2,
     cex.axis=1.2,
     col=c(2, 4))
lines(density(f0max), 
      col=1,
      lwd=3)
#grid()

# histograma por grupo
par(mfrow=c(1,2))

# AmE
hist(dadosLL268HL005$f0max[dadosLL268HL005$audiofile=='AmE'],
     prob=T,
     xlim=c(200, 400),
     ylim=c(0, 0.025),
     breaks=seq(from=200, to=400, by=20),
     xlab = "Máxima de F0",
     #ylab = "Proporção",
     main = "Histograma da F0 MÀXIMA \n AmE" ,
     las=1,
     cex.lab=1.2,
     cex.axis=1.2,
     col=2)
lines(density(dadosLL268HL005$f0max[dadosLL268HL005$audiofile=='AmE']), 
      col=1,
      lwd=3)
# BP
hist(dadosLL268HL005$f0max[dadosLL268HL005$audiofile=='BP'],
     prob=T,
     xlim=c(200, 400),
     ylim=c(0, 0.025),
     breaks=seq(from=200, to=400, by=20),
     xlab = "Máxima de F0",
     #ylab = "Proporção",
     main = "Histograma da F0 MÀXIMA \n BP" ,
     las=1,
     cex.lab=1.2,
     cex.axis=1.2,
     col=4) 
lines(density(dadosLL268HL005$f0max[dadosLL268HL005$audiofile=='BP']), 
      col=1,
      lwd=3)

#retorna area 1x1
par(mfrow=c(1,1))

  #----#----#----
  ## BARCHARTS ----#----####

# tirando média, desvio-padrão e erro padrão do conjunto de dados com a função "apply"
# parâmetro por parâmetro

f0min_media <- aggregate(f0min~audiofile+chunk, data = dadosLL268HL005, mean)
f0min_sd <- aggregate(f0min~audiofile+chunk, data = dadosLL268HL005, sd)
f0med_media <- aggregate(f0med~audiofile+chunk, data = dadosLL268HL005, mean)
f0med_sd <- aggregate(f0med~audiofile+chunk, data = dadosLL268HL005, sd)

# nosso exemplo com f0max
f0max_media <- aggregate(f0max~audiofile+chunk, data = dadosLL268HL005, mean)
f0max_sd <- aggregate(f0max~audiofile+chunk, data = dadosLL268HL005, sd)
f0max_media_AmE <- f0max_media[which(f0max_media$audiofile=="AmE"),]
f0max_media_BP <- f0max_media[which(f0max_media$audiofile=="BP"),]
f0max_media_AmEBP <- cbind(f0max_media_AmE[,3], f0max_media_BP[,3])

# identificação dos eixos no gráfico
colnames(f0max_media_AmEBP) <- c("AmE", "BP")
rownames(f0max_media_AmEBP) <- f0max_media_AmE$chunk
num_de_chunks <- rownames(f0max_media_AmEBP)

#windows()
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "light grey")
barplot(t(f0max_media_AmEBP), 
        beside=T, 
        ylim=c(0,400),
        las=1,
        xlab="Chunks de fala dos grupos",
        ylab="Máxima de F0",
        main = "F0 MÀXIMA",
        names.arg = num_de_chunks,
        col=c("blue", "yellow"),
        cex.lab=1.2,
        cex.names=1.2,
        cex.axis=1.2)
#box(bty="l")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted") #ou simplesmente grid()
# põe a grade atrás do plot
barplot(t(f0max_media_AmEBP), 
        beside=T, 
        ylim=c(0,400),
        las=1,
        xlab="Chunks de fala dos grupos",
        ylab="Máxima de F0",
        main = "F0 MÀXIMA",
        names.arg=num_de_chunks,
        col=c("blue", "yellow"),
        cex.lab=1.2,
        cex.names=1.2,
        cex.axis=1.2,
        add=T)
legend("topright", legend=c("AmE", "BP"), fill=c("blue", "yellow"), cex = 0.8)

# media
par_sig_media <- aggregate(cbind(f0min, f0med, f0max, f0sd, 
                                 f0SAQ, df0posmean, df0negmean, df0sdneg,
                                 hnr, SPI, shimmer, jitter) ~ audiofile+chunk, data = dadosLL268HL005, mean)

# Desv.Pad.
par_sig_DP <- aggregate(cbind(f0min, f0med, f0max, f0sd,
                              f0SAQ, df0posmean, df0negmean, df0sdneg,
                              hnr, SPI, shimmer, jitter) ~ audiofile+chunk, data = dadosLL268HL005, sd)

barplot(t(par_sig_media$f0max),
        names.arg=par_sig_media$audiofile,
        beside=T,
        ylim=c(0, 350),
        las=1,
        xlab="Língua",
        ylab="Máxima de F0",
        main = "F0 MÀXIMA \n (por chunk)",
        col=c(2, 4),
        cex.lab=1.2,
        cex.names=1.2,
        cex.axis=1.2)
box()

  #----#----#----
  # SCATTER-ERRORBARPLOTS ----#----####

# VERTICAIS 

# media
par_sig_media <- aggregate(cbind(f0min, f0med, f0max, f0sd, 
                                 f0SAQ, df0posmean, df0negmean, df0sdneg,
                                 hnr, SPI, shimmer, jitter) ~ audiofile+chunk, data = dadosLL268HL005, mean)

# Desv.Pad.
par_sig_DP <- aggregate(cbind(f0min, f0med, f0max, f0sd,
                              f0SAQ, df0posmean, df0negmean, df0sdneg,
                              hnr, SPI, shimmer, jitter) ~ audiofile+chunk, data = dadosLL268HL005, sd)

#sem grade
plot(par_sig_media[1:8 ,5],
     #type = "o",
     pch = 19,
     cex = 2.5,
     col = c("red", "blue"),
     lty = 3,
     ylim=c(220, 365),
     xlab = "Língua",
     ylab = "Máxima de F0 (Hz)", 
     main = "F0 MÀXIMA",
     cex.lab = 1.2,
     cex.axis = 1.2,
     las = 1,
     axes = F)
axis(1, at = 1:8, lab = c("CH1", "CH1", "CH2", "CH2", "CH3", "CH3", "CH4", "CH4"),
     cex.axis = 1.5)
axis(2, las=1,
     cex.axis = 1.5)
lines(par_sig_media[1:8 ,5], lty = 2, lwd = 1)
box(bty="l")
legend("bottomright", legend=c("AmE", "BP"), fill=c("red", "blue"), cex = 0.7)
#grid()

#colocando grade atrás
#grid()
#par(new=T)

#replotando com a grade por trás
plot(par_sig_media[1:8 ,5],
     pch = 19,
     cex = 2,
     col = c("red", "blue"),
     lty = 3,
     ylim=c(220, 360),
     xlab = "Chunks das língua",
     ylab = "Máxima de F0 (Hz)", 
     main = "F0 MÀXIMA",
     cex.lab = 1.2,
     cex.axis = 1.2,
     las = 1,
     axes = F)
axis(1, at = 1:8, lab = c("CH1", "CH1", "CH2", "CH2", "CH3", "CH3", "CH4", "CH4"),
     cex.axis = 1.5)
axis(2, las=1,
     cex.axis = 1.5)
lines(par_sig_media[1:8 ,5], lty = 2, lwd = 1)
box(bty="l")
legend("bottomright", legend=c("AmE", "BP"), fill=c("red", "blue"), cex = 0.7)
grid()
#box()
par(new=T)
plot(par_sig_media[1:8 ,5],
     pch = 19,
     cex = 2,
     col = c("red", "blue"),
     lty = 3,
     ylim=c(220, 360),
     xlab = "Chunks das língua",
     ylab = "Máxima de F0 (Hz)", 
     main = "F0 MÀXIMA",
     cex.lab = 1.2,
     cex.axis = 1.2,
     las = 1,
     axes = F)
axis(1, at = 1:8, lab = c("CH1", "CH1", "CH2", "CH2", "CH3", "CH3", "CH4", "CH4"),
     cex.axis = 1.5)
axis(2, las=1,
     cex.axis = 1.5)
lines(par_sig_media[1:8 ,5], lty = 2, lwd = 1)
box(bty="l")
legend("bottomright", legend=c("AmE", "BP"), fill=c("red", "blue"), cex = 0.7)

# acrescentando os bigodes verticais
x <- c(1:8) # [1:2] = Língua -> AmE e BP
y <- par_sig_media[5] # valores da durNorm por unidade VV já na ordem

# tranforme a classe do conjunto de dados (data.frame)
# em valores de classes de caracteres e, sem seguida, numérica para as barras de erro
y <- as.numeric(as.character(y$f0max))
y_sd <- par_sig_DP[5]
y_sd <- as.numeric(as.character(y_sd$f0max))

# plotando as barraas de erro
arrows(x0 = x, 
       y0 = y - y_sd, 
       x1 = x, 
       y1 = y + y_sd, 
       code = 3, 
       angle = 90, 
       length = 0.1)

# DISPERSÃO COM BARRAS DE ERRO DUPLAS (horiz./vert. errorbar plots)

apenas_f0min_f0max <- par_sig_media[ ,-4] # exclui o f0med interposto ao f0min e f0max
apenas_f0min_f0max <- apenas_f0min_f0max[1:8, 1:4] # toma apenas f0max e f0min como var. num.
apenas_f0min_f0max

apenas_f0min_f0max_DP <- par_sig_DP[ ,-4] # exclui o f0med interposto ao f0min e f0max
apenas_f0min_f0max_DP <- apenas_f0min_f0max_DP[1:8, 1:4] # toma apenas f0max e f0min como var. num.
apenas_f0min_f0max_DP

plot(apenas_f0min_f0max[1:8 ,4:3],
     ylim=c(80, 250),
     xlim=c(230, 380),
     ylab="Mínima de F0", 
     xlab="Máxima de F0",
     main="F0 MÀXIMA vs. F0 MÍNIMA",
     cex.lab=1.2,
     cex.axis=1.2,
     pch=19,
     col="blue",
     cex=2,
     las=1)
points(apenas_f0min_f0max$f0max[apenas_f0min_f0max$audiofile=='AmE'], 
       (apenas_f0min_f0max$f0min[apenas_f0min_f0max$audiofile=='AmE']), 
       pch=19, 
       cex=2, 
       col="red")
box(bty="l")
legend("bottomright", legend=c("AmE", "BP"), fill=c("red", "blue"), cex = 0.7)

# tranforme a classe do conjunto de dados (data.frame)
# em valores de classes de caracteres e, sem seguida, numérica para as barras de erro

# horiz. errbar
xx <- apenas_f0min_f0max[4] # f0max
xx <- as.numeric(as.character(xx$f0max))
xx_sd <- apenas_f0min_f0max_DP
xx_sd <- as.numeric(as.character(xx_sd$f0max))

# vert. errbar
yy <- apenas_f0min_f0max[3] # f0min
yy <- as.numeric(as.character(yy$f0min))
yy_sd <- apenas_f0min_f0max_DP[3]
yy_sd <- as.numeric(as.character(yy_sd$f0min))

# plotando as barraas de erro
arrows(x0 = xx, 
       y0 = yy - yy_sd, 
       x1 = xx, 
       y1 = yy + yy_sd, 
       code = 3, 
       angle = 90,
       lty="dotted",
       length = 0.05)
arrows(x0 = xx - xx_sd, 
       y0 = yy, 
       x1 = xx + xx_sd, 
       y1 = yy, 
       code = 3, 
       angle = 90, 
       lty="dotted",
       length = 0.05)
  #----#----#----
##### fim do procedimento 4 -------

##### procedimento 5: instalando/acessando o ggplot2 -------
install.packages("ggpplot2")
library(ggplot2)

# ou desta forma
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}
##### fim do procedimento 5 -------

##### procedimento 6: FUNÇÃO: reportando resultados estatísticos para TESTE-T -------
ggplot.ttest.LL268HL005 <- function (t) {
  
  require(ggplot2)
  ## input parameters
  
  #df and t stats
  df_t.test <- round(t$parameter)
  t_value <- round(t$statistic, digits = 2)
  #p-value
  t_p.value <-format(t$p.value, digits = 2)
  # cat. var. names
  x_group1 <- names(t$estimate[1])
  x_group2 <- names(t$estimate[2])
  x_both <- names(t$estimate[1:2])
  x_vector <- c(x_group1, x_group2)
  x_vector_fct <- as.factor(x_vector)
  # num. var. values
  y_group1 <- t$estimate[[1]]
  y_group2 <- t$estimate[[2]]
  y_both <- c(y_group1, y_group2)
  y_vector <- round(c(y_group1, y_group2))
  # y ~ x dataframe
  xy_df <- data.frame(x_vector, y_vector)
  df <- t$data.name
  x_vector_fct_levels <- nlevels(xy_df$x_vector_fct)
  # conf. int.
  min_ci <- round(t$conf.int[1])
  max_ci <- round(t$conf.int[2])
  # st. error (se)
  se <- t$stderr
  
  print(ggplot(xy_df, aes_string(x = x_vector_fct, y = y_vector, color = x_vector_fct)) +
          theme_bw() +
          geom_point(data = xy_df, size = 5) + 
          #stat_summary(fun = mean, geom = "point", col = "red", size = 4) +
          stat_summary(fun = mean, geom = "line", aes(group = 1), lty = 2, lwd = 0.5, col= "black") +
          labs(title = substitute(paste(italic("t"),
                                        "(",df_t.test,
                                        ")", 
                                        ",",
                                        " = ",t_value, 
                                        ", p = ", t_p.value))) +
          xlab("x") +
          ylab(paste("(y by x)", 
                     t$data)) +
          # error bars enter after the labs for syntax flow of the code
          geom_errorbar(aes(ymax = y_vector + se, ymin = y_vector - se), lty = 1, width = 0.1))
  return(x_both)
}

## ggplot.ttest.LL268HL005()
##### fim do procedimento 6 -------

##### procedimento 7: FUNÇÃO: reportando resultados estatísticos para ANOVA e Regressão linear -------
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}

ggplot.lm.LL268HL005 <- function (fit) {
  
  require(ggplot2)
  #df and f stats
  df_between_groups <- signif(summary(fit)$fstatistic[2], 4)
  df_within_groups <- signif(summary(fit)$fstatistic[3], 4)
  f_value <- signif(summary(fit)$fstatistic[1], 3)
  #different p-values
  f <- summary(fit)$fstatistic
  p_value_model_raw <- pf(f[1],f[2],f[3],lower.tail = F)
  p_value_model <- as.numeric(format(p_value_model_raw, digits=3))
  p_value_coef <- signif(summary(fit)$coef[-1,4], 3)
  r_sq <- signif(summary(fit)$adj.r.squared, 2)
  ci_intercept <- signif(summary(fit)$coef[1,2], 2)
  coef_chr <- as.character(signif(summary(fit)$coef[-1,4], 3))
  coef_values <- toString(coef_chr)
  ci_slope <- signif(summary(fit)$coef[2,2], 2)
  totalSample <- df_within_groups + df_between_groups + 1
  #in the plot
  var_cat <- names(fit$model)[2]
  var_num <- names(fit$model)[1]
  y <- fit$model[1]
  y <- unlist(y, use.names = F)
  x <- fit$model[2]
  x <- unlist(x, use.names = F)
  x_factor <- is.factor(x)
  #x_levels <- nlevels(x)
  
  ifelse(x_factor == FALSE,
         #yes
         print(ggplot(fit$model, aes_string(x = x, y = y)) + 
                 geom_point() +
                 stat_smooth(method = "lm", col = "red") +
                 theme_bw() +
                 labs(title = substitute(paste("[F"[statistic],"(",df_between_groups, 
                                               ",",df_within_groups,")",
                                               " = ",f_value, 
                                               ", p = ", p_value_model,"]",
                                               ", R"^2," = ", r_sq,
                                               ", CI"[95*"%"], "= ", "[",ci_intercept,",", ci_slope,"]",
                                               ", N"[samples], "= ", totalSample)))),
         #no
         print(ggplot(fit$model, aes_string(x = var_cat, y = var_num, col = var_cat)) +
                 geom_boxplot() +
                 labs(title = substitute(paste("[F"[statistic],"(",df_between_groups, 
                                               ",",df_within_groups,")",
                                               " = ",f_value, 
                                               ", p = ", p_value_model,"]",
                                               ", R"^2," = ", r_sq,
                                               ", CI"[95*"%"], "= ", "[",ci_intercept,",", ci_slope,"]",
                                               ", N"[samples], "= ", totalSample)))))
  ifelse(x_factor != FALSE,
         #yes
         ifelse(p_value_coef == p_value_model,
                # yes
                print(ggplot(fit$model, aes_string(x = var_cat, y = var_num, col = var_cat)) +
                        geom_boxplot() +
                        labs(title = substitute(paste("[F", #[statistic],
                                                      "(",df_between_groups,
                                                      ",",df_within_groups,")",
                                                      " = ",f_value, 
                                                      ", p = ", p_value_model,"]",
                                                      ", R"^2," = ", r_sq,
                                                      ", CI"[95*"%"], "= ", "[",ci_intercept,",", ci_slope,"]",
                                                      ", N"[samples], "= ", totalSample)))),
                # no
                print(ggplot(fit$model, aes_string(x = var_cat, y = var_num, col = var_cat)) +
                        geom_boxplot() +
                        labs(title = substitute(paste("[F"[statistic],"(",df_between_groups, 
                                                      ",",df_within_groups,")",
                                                      " = ",f_value, 
                                                      ", p = ", p_value_model,"]",
                                                      ", p"[coef], " = ", coef_values,
                                                      ", R"^2," = ", r_sq,
                                                      ", CI"[95*"%"], "= ", "[",ci_intercept,",", ci_slope,"]",
                                                      ", N"[samples], "= ", totalSample))))),
         #no
         0)
  
  # return used variables in the regression model
  return(names(fit$model))
}
# use the function with your lm's
#ggplot.lm.LL268HL005()

##### fim do procedimento 7 -------

##### procedimento 8: FUNÇÃO: pressupostos estatísticos para TESTE-T -------
assump.ttest.LL268HL005 <- function (fit) {
  
  ## parametros de entrada
  residuos<- resid(fit)
  #alfa <- 5.5e-40
  alfa <- 0.05
  var_cat <- names(fit$model)[2]
  var_num <- names(fit$model)[1]
  var_both <- names(fit$model)[1:2]
  y <- fit$model[1]
  y <- unlist(y, use.names = F)
  x <- fit$model[2]
  x <- unlist(x, use.names = F)
  x_fator <- is.factor(x)
  x_niveis <- nlevels(x)
  
  if(x_fator == TRUE && x_niveis != 2) {
    stop("O fator GRUPO deve ter exatamente 2 níveis")
  } else if(x_fator != TRUE && x_niveis != 2) {
    stop("O fator GRUPO deve ter exatamente 2 níveis")
  }
  ## limpando a area Plots
  #dev.off()
  ## identificando as vriáveis do conjunto de dados
  print("Você está usando as seguintes variáveis:")
  print(var_both)
  print("Sua variável dependente (VD) é:")
  print(var_num)
  print("Sua variável independente (VI) é:")
  print(var_cat)
  readline(prompt="Pressione [Enter] para continuar")
  
  print("Vamos iniciar testando a normalidade dos resíduos")
  readline(prompt="Pressione [Enter] para continuar")
  ## testando a normalidade dos resíduos (teste de shapiro)
  teste_normal_resid <- shapiro.test(residuos)
  ## determinando o p-value do teste de normalidade
  valor_p_shapiro <- teste_normal_resid$p.value
  print(teste_normal_resid)
  readline(prompt="Pressione [Enter] para continuar")
  
  if(valor_p_shapiro < alfa) {
    print("Os dados não estão normalmente distribuídos. Vamos realizar um teste não-paramétrico para o modelo")
    readline(prompt="Pressione [Enter] para continuar")
    
    if(x_fator == TRUE && x_niveis == 2) {
      print(wilcox.test(y ~ x))
      #valor_p_wilcox <- wilcox.test(y ~ x)$p.value
      readline(prompt="Pressione [Enter] para continuar")
      print("A seguir, vejamos a média dos dois grupos")
      readline(prompt="Pressione [Enter] para continuar")
      media <- tapply(y, x, mean)
      print(round(media))
      readline(prompt="Pressione [Enter] para continuar")
      print("Observemos agora o boxplot na área Plots para um panorama de nossos dados")
      readline(prompt="Pressione [Enter] para continuar")
      bplot <- boxplot(y ~ x, xlab = "VI_groupos", ylab = "VD_valores")
      #print(bplot)
      readline(prompt="Pressione [Enter] para continuar")
      return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
    }
    else if(x_fator == TRUE && x_niveis != 2) {
      stop("O grupo não possui dois níveis")
    }
  }
  else {
    print("O modelo passou no TESTE DE NORMALIDADE. Vejamos a seguir, a HOMOCEDASTICIDADE dos dados")
  }
    readline(prompt="Pressione [Enter] para continuar")
    ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
    
    ## atribuindo variáveis
    preditos <- predict(fit)
    teste_F <- var.test(residuos, preditos)
    valor_p_homoced <- teste_F$p.value 
    
    ## testando a homecedasticidade (homogeneidade das variâncias)
    print(teste_F)
    readline(prompt="Pressione [Enter] para continuar")
    
    if(valor_p_homoced < alfa) {
      print("Os dados não são homogêneos. Vamos realizar um TESTE-T de variãncias diferentes")
      readline(prompt="Pressione [Enter] para continuar")
      print(t.test(y ~ x, mu = 0, alt = "two.sided", conf = 0.95, var.eq = F, paired = F))
      readline(prompt="Pressione [Enter] para continuar")
      print("A seguir, vejamos a média dos dois grupos")
      readline(prompt="Pressione [Enter] para continuar")
      media <- tapply(y, x, mean)
      print(round(media))
      readline(prompt="Pressione [Enter] para continuar")
      print("Observemos agora o boxplot na área Plots para um panorama de nossos dados")
      readline(prompt="Pressione [Enter] para continuar")
      bplot <- boxplot(y ~ x, xlab = "VI (groupos)", ylab = "VD (val. num.)", las = 1, col = c("grey", "white"))
      #print(bplot)
      readline(prompt="Pressione [Enter] para continuar")
      return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
    else if(valor_p_homoced >= alfa) {
      print("O modelo é homogêneo. Agora, vamos verificar os valores preditos (fitted) e residuais (residuals) nos gráficos na área Plots")
      }
    readline(prompt="Pressione [Enter] para continuar")
    par(mfrow = c(2, 2))
    plot(fit)
    readline(prompt="Pressione [Enter] para continuar")
    dev.off()
    #readline(prompt="Pressione [Enter] para continuar")
    print("Uma vez que o modelo é normal e homogêneo, vamos realizar um TESTE-T de variâncias iguais:")
    readline(prompt="Pressione [Enter] para continuar")
    print(t.test(y ~ x, mu=0, alt="two.sided", conf=0.95, var.eq=T, paired=F))
    readline(prompt="Pressione [Enter] para continuar")
    print("Agora, vejamos a média dos dois grupos")
    readline(prompt="Pressione [Enter] para continuar")
    media <- tapply(y, x, mean)
    print(round(media, 2), nsmall = 2)
    readline(prompt="Pressione [Enter] para continuar")
    print("Observemos o boxplot na área Plots para um panorama de nossos dados")
    readline(prompt="Pressione [Enter] para continuar")
    bplot <- boxplot(y ~ x, xlab = "VI_groupos", ylab = "VD_valores", las = 1, col = c("grey", "white"))
    #print(bplot)
    readline(prompt="Pressione [Enter] para continuar")
    return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
}

##### fim do procedimento 8 -------

##### procedimento 9: FUNÇÃO: pressupostos estatísticos para ANOVA e Regressão linear -------

assump.lm.LL268HL005 <- function (fit) {
  
  ## atribuindo variáveis
  residuos<- resid(fit)
  #alfa <- 5.5e-40  ##-- "ALFA APENAS PARA TESTE DA FUNÇÃO" --##
  alfa <- 0.05
  var_cat <- names(fit$model)[2]
  var_num <- names(fit$model)[1]
  var_both <- names(fit$model)[1:2]
  y <- fit$model[1]
  y <- unlist(y, use.names = F)
  x <- fit$model[2]
  x <- unlist(x, use.names = F)
  x_fator <- is.factor(x)
  x_niveis <- nlevels(x)
  
  print("Você está usando as seguintes variáveis:")
  print(var_both)
  print("Sua variável dependente (VD) é:")
  print(var_num)
  print("Sua variável independente (VI) é:")
  print(var_cat)
  readline(prompt="Pressione [Enter] para continuar")
  
  print("Vamos iniciar testando a normalidade dos resíduos")
  readline(prompt="Pressione [Enter] para continuar")
  ## testando a normalidade dos resíduos (teste de shapiro)
  teste_normal_resid <- shapiro.test(residuos)
  
  ## determinando o p-value do teste de normalidade
  valor_p_shapiro <- teste_normal_resid$p.value
  
  print(teste_normal_resid)
  readline(prompt="Pressione [Enter] para continuar")
  
  if(valor_p_shapiro < alfa) {
    print("Os dados não estão normalmente distribuídos. Vamos realizar um teste não-paramétrico para o modelo")
    readline(prompt="Pressione [Enter] para continuar")
    
    if(x_fator == TRUE) {
      print(kruskal.test(y ~ x))
      valor_p_kruskal <- kruskal.test(y ~ x)$p.value
      if(valor_p_kruskal < alfa && x_niveis > 2) {
        readline(prompt="Pressione [Enter] para continuar")
        print("Realizaremos agora um teste POST-HOC para verificar que níveis são diferentes")
        readline(prompt="Pressione [Enter] para continuar")
        print(pairwise.wilcox.test(y, x, p.adjust.method = "bonf"))
        readline(prompt="Pressione [Enter] para continuar")
        print("A seguir, vejamos a média do conjunto de dados por nível")
        readline(prompt="Pressione [Enter] para continuar")
        print(tapply(y, x, mean))
        return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
      else if(valor_p_kruskal < alfa && x_niveis == 2) {
        print("A seguir, vejamos a média do conjunto de dados por nível")
        readline(prompt="Pressione [Enter] para continuar")
        print(tapply(y, x, mean))
        return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
      else if(valor_p_kruskal >= alfa) {
        return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
    }
    else if (x_fator != TRUE) {
      print(cor.test(x, y, method = "spearman", exact = F))
      return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
    }
  }
  else {
    print("O modelo passou no TESTE DE NORMALIDADE. Vejamos a seguir, a HOMOCEDASTICIDADE dos dados")
  }
  readline(prompt="Pressione [Enter] para continuar")
  ##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
  
  ## atribuindo variáveis
  preditos <- predict(fit)
  teste_F <- var.test(residuos, preditos)
  valor_p_homoced <- teste_F$p.value 
  
  ## testando a homecedasticidade (homogeneidade das variâncias)
  print(teste_F)
  readline(prompt="Pressione [Enter] para continuar")
  
  if(valor_p_homoced < alfa) {
    print("Os dados não são homogêneos. Vamos realizar um teste não-paramétrico para o modelo")
    readline(prompt="Pressione [Enter] para continuar")
    
    if(x_fator == TRUE) {
      print(kruskal.test(y ~ x))
      valor_p_kruskal <- kruskal.test(y ~ x)$p.value
      
      if(valor_p_kruskal < alfa && x_niveis > 2) {
        readline(prompt="Pressione [Enter] para continuar")
        print("Realizaremos agora um teste POST-HOC para verificar que níveis são diferentes")
        readline(prompt="Pressione [Enter] para continuar")
        print(pairwise.wilcox.test(y, x, p.adjust.method = "bonf"))
        readline(prompt="Pressione [Enter] para continuar")
        print("A seguir, vejamos a média do conjunto de dados por nível")
        readline(prompt="Pressione [Enter] para continuar")
        print(tapply(y, x, mean))
        return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
      else if(valor_p_kruskal < alfa && x_niveis == 2) {
        readline(prompt="Pressione [Enter] para continuar")
        print("A seguir, vejamos a média do conjunto de dados por nível")
        readline(prompt="Pressione [Enter] para continuar")
        print(tapply(y, x, mean))
        return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
      else if(valor_p_kruskal >= alfa) {
        return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
      }
    }  
    else if (x_fator != TRUE) {
      readline(prompt="Pressione [Enter] para continuar")
      print(cor.test(x, y, method = "spearman", exact = F))
      readline(prompt="Pressione [Enter] para continuar")
      return("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS TERMINOU")
    }
  }
  else {
    print("O modelo é homogêneo. Agora, vamos verificar os valores preditos (fitted) e residuais (residuals) nos gráficos na área Plots")
  }
  readline(prompt="Pressione [Enter] para continuar")
  par(mfrow = c(2, 2))
  plot(fit)
  readline(prompt="Pressione [Enter] para continuar")
  #dev.off()
  
  ###---TIMER
  #print("Vejamos em 10 segundo a função de regressão linear...")
  #contag_reg <- 10
  #  while(contag_reg != 0) {
  #    Sys.sleep(1)
  #    contag_reg <- contag_reg - 1
  #    print(contag_reg)
  #  }
  #summary(fit)
  ###---
  
  print("A seguir, vamos examinar seu modelo pela função de regressão linear:")
  readline(prompt="Pressione [Enter] para continuar")
  print(summary(fit))
  readline(prompt="Pressione [Enter] para continuar")
  if(x_niveis > 2) {
    print("Vamos agora realizar um teste POST-HOC para verificar o efeito de cada nível:")
    mod_aov <- aov(fit)
    tukey_test <- TukeyHSD(mod_aov)
    readline(prompt="Pressione [Enter] para continuar")
    print(tukey_test)
    readline(prompt="Pressione [Enter] para continuar")
    print("Vamos verificar uma comparação pareada entre todos os níveis na área Plots")
    readline(prompt="Pressione [Enter] para continuar")
    par(mfrow = c(1, 1))
    plot(tukey_test, las = 1)
    readline(prompt="Pressione [Enter] para continuar")
    print("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS ACABOU")
    #dev.off()
  }
  else {
    print("O TESTE DOS PRESSUPOSTOS ESTATÍSTICOS ACABOU")
  }
}
### assump.lm.LL268HL005()

##### fim do procedimento 9 -------

##### procedimento 10: gganimate(): animações gráficas -------
install.packages("gganimate")
library(gganimate)
library(gifski)
library(png)

g <- ggplot(dadosLL268HL005, aes(x=f0max, y=f0min)) + 
  geom_point(aes(col=audiofile), size=4) + 
  theme_bw()

anim_g <- g +
  transition_states(audiofile,
                    transition_length =2,
                    state_length = 1)
anim_g
##### procedimento 10 em andamento -------
