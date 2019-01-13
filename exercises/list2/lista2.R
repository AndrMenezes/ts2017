# Definições gerais -------------------------------------------------------

rm(list = ls())
bib <- c('readxl', 'tseries', 'ggplot2', 'gridExtra', 'dplyr', 'tidyr')
sapply(bib, require, character.only = T)
setwd('C:/Users/User/Dropbox/4° Série/Séries Temporais/Aulas/Aula2/Dados')
# setwd('C:/Users/André Felipe/Dropbox/4° Série/Séries Temporais/Aulas/Aula2/Dados')

months.br <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

# Exercício 1 -------------------------------------------------------------

atmosfera      <- read_xls(path = 'atmosfera.xls', sheet = 'Plan1', col_names = c('data', 'temperatura', 'umidade'), skip = 1)
atmosfera$data <- as.Date(atmosfera$data, format = '%Y-%m-%d')
atmosfera$mes  <- factor(months(atmosfera$data, abbreviate = T), months.br, ordered = T)
head(atmosfera)

setwd('C:/Users/User/Dropbox/4° Série/Séries Temporais/Listas')

## a)
u <- as.ts(atmosfera$umidade)
plot(temperatura ~ data, data = atmosfera, type = 'l')
plot(umidade ~ data, data = atmosfera, type = 'l')
plot(u); plot(diff(u))

p1 <- ggplot(data = atmosfera, aes(x = data, y = temperatura)) +
  geom_line() +  labs(x = '', y = 'Temperatura') + theme(panel.grid.minor = element_blank(), text = element_text(size = 12)) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b") +
  scale_y_continuous(breaks = round(seq(min(atmosfera$temperatura), max(atmosfera$temperatura), l = 5))) +
  geom_hline(yintercept = mean(atmosfera$temperatura), col = 'red')

p2 <- ggplot(data = atmosfera, aes(x = data, y = umidade)) +
  geom_line() +  labs(x = '', y = 'Umidade') + theme(panel.grid.minor = element_blank(), text = element_text(size = 12)) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b") +
  scale_y_continuous(breaks = round(seq(min(atmosfera$umidade), max(atmosfera$umidade), l = 5))) +
  geom_hline(yintercept = mean(atmosfera$umidade), col = 'red')

ggsave(filename = 'temp-ex1.pdf', plot = p1, width = 10, height = 6)
ggsave(filename = 'umid-ex1.pdf', plot = p2, width = 10, height = 6)
x11()
grid.arrange(p1, p2, ncol = 1)

b1 <- ggplot(data = atmosfera, aes(x = mes, y = temperatura)) +
  geom_boxplot() + labs(x = '', y = 'Temperatura') +
  stat_summary(aes(group = 1), fun.y = mean, geom="line", color = 'red') +
  stat_summary(aes(group = 1), fun.y = mean, geom="point", color = 'red') +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 12))

b2 <- ggplot(data = atmosfera, aes(x = mes, y = umidade)) +
  geom_boxplot() + labs(x = '', y = 'Umidade') +
  stat_summary(aes(group = 1), fun.y = mean, geom="line", color = 'red') +
  stat_summary(aes(group = 1), fun.y = mean, geom="point", color = 'red') +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 12))

ggsave(filename = 'bptemp-ex1.pdf', plot = b1, width = 10, height = 6)
ggsave(filename = 'bpumid-ex1.pdf', plot = b2, width = 10, height = 6)

x11()
grid.arrange(b1, b2, ncol = 1)

## b)
atmosfera2 <- atmosfera %>% 
  mutate(temp_diff1 = c(diff(temperatura), NA), umid_diff1 = c(diff(umidade), NA),
         temp_diff2 = c(diff(temp_diff1), NA), umid_diff2 = c(diff(umid_diff1), NA)) %>% 
  gather(var, value, -c(data, mes))

head(atmosfera2)

medias <- with(atmosfera2, tapply(value, var, mean, na.rm = T))

atmosfera2 %>% filter(var %in% unique(atmosfera2$var)[c(3, 5)]) %>% 
  ggplot(aes(x = data, y = value, col = var)) +
  geom_line() +  labs(x = '', y = 'Temperatura', col = '') + 
  theme(panel.grid.minor = element_blank(), legend.position = 'top', text = element_text(size = 14)) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b-%Y") +
  scale_y_continuous(limits = c(-10, 10)) +
  scale_color_discrete(labels = c('Diff1', 'Diff2')) +
  geom_hline(yintercept = medias[1:2], aes(col = var))

diff.plot <- function(vari, dif) 
{
  if(vari == 'temperatura') yl <- 'Temperatura' else yl <- 'Umidade'
  med <- medias[dif]; tit <- ifelse(substr(dif, 10, 10) == '1', 'Diff1', 'Diff2')
  atmosfera2 %>% filter(var == dif) %>% 
    ggplot(aes(x = data, y = value)) +
    geom_line() +  labs(x = '', y = yl, col = '', title = tit) + 
    theme(panel.grid.minor = element_blank(), text = element_text(size = 14)) +
    scale_x_date(date_breaks = '1 month', date_labels = "%b") +
    geom_hline(yintercept = med, col = 'red') -> pp
  ggsave(filename = paste0(dif, '.pdf'), plot = pp, width = 10, height = 6)
  atmosfera2 %>% filter(var == dif) %>% 
    ggplot(aes(x = mes, y = value)) +
    geom_boxplot() +
    stat_summary(aes(group = 1), fun.y = mean, geom="line", color = 'red') +
    stat_summary(aes(group = 1), fun.y = mean, geom="point", color = 'red') +
    labs(x = '', y = yl, col = '', title = tit) + 
    theme(panel.grid.minor = element_blank(), text = element_text(size = 14)) -> bp
  ggsave(filename = paste0('bp', dif, '.pdf'), plot = bp, width = 10, height = 6)
  return(list(pp, bp))
}
diff.plot(vari = 'temperatura', dif = 'temp_diff1')
diff.plot(vari = 'umidade'    , dif = 'umid_diff1')
diff.plot(vari = 'temperatura', dif = 'temp_diff2')
diff.plot(vari = 'umidade'    , dif = 'umid_diff2')


# Exercício 2 -------------------------------------------------------------
dados <- ts(scan('https://robjhyndman.com/tsdldata/data/abraham4.dat'), frequency = 12,
            start = c(1960, 1), end = c(1968, 12))

plots.all <- function(transf = '', dif = FALSE, ord = 1)
{
  if(transf == 'log')  {x <- log(dados); aux <- 'log-'}
  if(transf == 'sqrt') {x <- sqrt(x); aux <- 'sqrt-'}
  if(transf == '') {x <- dados; aux <- ''}
  pdf(file = paste0(aux, 'vendas.pdf'), width = 9)
  par(mar = c(2.8, 4.0, 0.4, 0.2), cex = 1.4)
  plot(x, ylab = 'Número de vendas de carros', xlab = '')
  graphics.off()
  
  pdf(file = paste0(aux, 'bp-vendas.pdf'), width = 9)
  par(mar = c(2.8, 4.0, 0.4, 0.2), cex = 1.4)
  boxplot(x ~ cycle(dados), names = months.br, cex = 0.8)
  graphics.off()
  
  pdf(file = paste0(aux, 'bpannual-vendas.pdf'), width = 9)
  par(mar = c(2.8, 4.0, 0.4, 0.2), cex = 1.4)
  boxplot(x ~ trunc(time(dados)), cex = 0.8)
  graphics.off()
  
  if(dif == TRUE)
  {
    for(i in 1:ord)
    {
      pdf(file = paste0('diff', i, aux, 'vendas.pdf'), width = 9)
      par(mar = c(2.8, 4.0, 1.0, 0.2), cex = 1.4)
      plot(diff(x), ylab = 'Número de vendas de carros', xlab = '')
      if(ord > 1) mtext(paste0('Diff', i), cex = 1.4, adj = 0)
      graphics.off()
      
      pdf(file = paste0('diff', i, aux, 'bp-vendas.pdf'), width = 9)
      par(mar = c(2.8, 4.0, 1.0, 0.2), cex = 1.4)
      boxplot(diff(x) ~ cycle(d1), names = months.br, cex = 0.8)
      if(ord > 1) mtext(paste0('Diff', i), cex = 1.4, adj = 0)
      graphics.off()
      
      pdf(file = paste0('diff', i, aux, 'bpannual-vendas.pdf'), width = 9)
      par(mar = c(2.8, 4.0, 1.0, 0.2), cex = 1.4)
      boxplot(diff(x) ~ trunc(time(d1)), cex = 0.8)
      if(ord > 1) mtext(paste0('Diff', i), cex = 1.4, adj = 0)
      graphics.off()
    }
  }
}

## a) e b)
plots.all(transf = '', dif = TRUE, ord = 2)

## c) e d)
plots.all(transf = 'log', dif = TRUE, ord = 1)

## e) e f)
plots.all(transf = 'sqrt', dif = TRUE, ord = 1)


# Exercício 3 -------------------------------------------------------------
icv <- read.table('C:/Users/André Felipe/Dropbox/4° Série/Séries Temporais/Aulas/Aula2/Dados/ICV.csv', sep = ';',
                  header = T, dec = ',')
names(icv) <- tolower(names(icv))
icv$data   <- as.Date(paste0('01/', icv$mes.ano), format = '%d/%b/%y')
icv$mes    <- months(icv$data)

icv.ts <- ts(icv$icv, start = c(1970, 1), end = c(1980, 6), frequency = 12)

## a)
pdf(file = 'icv.pdf', width = 9)
par(mar = c(2.8, 4.0, 1.0, 0.2), cex = 1.4)
plot(icv.ts, xlab = '', ylab = 'ICV')
graphics.off()


## b)
icv$data.num <- 1:nrow(icv)
fit.trend <- nls(formula = icv ~ beta1 * exp(beta0 * data.num), data = icv, 
                 start = list(beta0 = 0.03, beta1 = 25))
betas <- coef(fit.trend)

  pdf(file = 'icv-fit.pdf', width = 9)
  par(mar = c(2.8, 4.0, 1.0, 0.2), cex = 1.4)
  plot(icv ~ data.num, data = icv, type = 'l', xlab = '', ylab = 'ICV', xaxt = 'n')
  axis(side = 1, at = seq(0, 120, l = 6), labels = seq(1970, 1980, l = 6))
  fx <- function(x, beta0, beta1) beta1 * exp(beta0 * x)
  x  <- seq(1, nrow(icv), l = 1000)
  lines(x, fx(x, betas[1], betas[2]), col = 2)
  graphics.off()

### ou 
fit.trend2 <- diff(diff(icv.ts))
pdf(file = 'icv-diff.pdf', width = 9)
par(mar = c(2.8, 2.5, 1.0, 0.2), cex = 1.4)
plot(fit.trend2, ylab = '', xlab = '')
mtext('Diff2', cex = 1.4, adj = 0)
graphics.off()

## c) 
icv.no.trend <- residuals(fit.trend, type = 'pearson')
icv.no.trend <- ts(icv.no.trend, start = c(1970, 1), end = c(1980, 6), frequency = 12)
pdf(file = 'icv-nonlinear.pdf', width = 9)
par(mar = c(2.8, 2.5, 1.0, 0.2), cex = 1.4)
plot(icv.no.trend, ylab = '', xlab = '')
mtext('Regressão Não Linear', cex = 1.4, adj = 0)
graphics.off()



# Exercício 4 -------------------------------------------------------------
x <- c(153, 189, 221, 215, 302, 223, 201, 173, 121, 106, 86, 87, 108, 133, 177, 241, 
          228, 283, 255, 238, 164, 128, 108, 87, 74, 95, 145, 200, 187, 201, 292, 220, 
          233, 172, 119, 81, 65, 76, 74, 111, 170, 243, 178, 248, 202, 163, 139, 120, 96,
          95, 53, 94)
x.ts <- ts(data = x, start = c(1967, 1), end = c(1970, 13), frequency = 13)

## a)
pdf(file = 'vendas-ex4.pdf', width = 9)
par(mar = c(3.0, 3.0, 1.5, 1.5), cex = 1.4)
plot(x.ts, xlab = '', ylab = '')
mtext(text = 'N° de vendas', side = 2, line = 2, cex = 1.4)
graphics.off()

## b)
detach(name = 'package:dplyr', unload = T)
q   <- 2; coefs <- rep(1 / (2 * q + 1), 2 * q + 1)
ma2 <- filter(x.ts, filter = coefs, sides=2, method="convolution")
q   <- 4; coefs <- rep(1 / (2 * q + 1), 2 * q + 1)
ma4 <- filter(x.ts, filter = coefs, sides=2, method="convolution")

pdf(file = 'mm24.pdf', width = 9)
par(mar = c(3.0, 3.0, 1.5, 1.5), cex = 1.4)
plot(x.ts, xlab = '', ylab = '')
lines(ma2, col = 'red'); lines(ma4, col = 'blue')
legend('topright', col = c('red', 'blue'), legend = c('q = 2', 'q = 4'), lwd = 1)
mtext(text = 'N° de vendas', side = 2, line = 2, cex = 1.4)
graphics.off()

x.dec <- decompose(x.ts)


## tendencia
pdf(file = 'trend.pdf', width = 9)
par(mar = c(3.0, 3.0, 1.5, 1.5), cex = 1.4)
plot(x.dec$trend, ylab = '', xlab = '')
mtext('Tendência', side = 2, line = 2, cex = 1.4)
graphics.off()

## sazonalidade
pdf(file = 'seasonal.pdf', width = 9)
par(mar = c(3.0, 3.0, 1.5, 1.5), cex = 1.4)
plot(x.dec$seasonal, ylab = '', xlab = '')
mtext('Sazonalidade', side = 2, line = 2, cex = 1.4)
graphics.off()

## random
pdf(file = 'random.pdf', width = 9)
par(mar = c(3.0, 3.0, 1.5, 1.5), cex = 1.4)
plot(x.dec$random, ylab = '', xlab = '')
abline(h = mean(x.dec$random, na.rm = T), lty = 2)
mtext('Resíduo', side = 2, line = 2, cex = 1.4)
graphics.off()

pdf(file = 'decompose.pdf', width = 9)
par(mar = c(3.0, 3.0, 1.5, 1.5), cex = 1.4)
plot(x.dec, xlab = '', main = '')
graphics.off()

mc <- c()
for(i in 1:length(mm))
{
  aux1 <- mm[i]
  aux2 <- mm[i+1]
  mc[i] <- (aux1 + aux2) / 2
}
    
