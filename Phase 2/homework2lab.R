rm(list = ls())

dataDir = './data' 
smokeFile = file.path(dataDir, 'smoke2014.RData') 
if(!file.exists(smokeFile)){  
  download.file(
      'http://pbrown.ca/teaching/astwo/data/smoke2014.RData',
      smokeFile) 
} 
load(smokeFile) 
smoke[1:3,c('Age','ever_cigarettes','Sex','Race', 
        'state','school', 'RuralUrban')]


forInla = smoke[,c('Age','ever_cigarettes','Sex','Race', 
        'state','school', 'RuralUrban')]
forInla = na.omit(forInla)
forInla$y = as.numeric(forInla$ever_cigarettes)
forInla$ageFac = relevel(factor(forInla$Age), '14')

  toPredict = expand.grid(
    ageFac = levels(forInla$ageFac),
    RuralUrban = levels(forInla$RuralUrban),
    Sex = levels(forInla$Sex)
    )
forLincombs = do.call(inla.make.lincombs, 
  as.data.frame(model.matrix( ~ ageFac:RuralUrban + Sex, 
    data=toPredict)))

library("INLA")


fitS2 = inla(y ~ Sex + ageFac:RuralUrban + 
    f(state, model='iid', hyper=list(
        prec=list(prior='pc.prec', param=c(log(1.1), 0.5)))
    ),
  data=forInla, family='binomial',
  lincomb = forLincombs,
  control.inla = list(strategy='laplace', fast=FALSE))

rbind(fitS2$summary.fixed[,c("mean","0.025quant","0.975quant")],
      Pmisc::priorPostSd(fitS2)$summary[,c("mean","0.025quant","0.975quant")])

dim(toPredict)
dim(fitS2$summary.lincomb.derived)
toPredict[1:2,]
fitS2$summary.lincomb.derived[1:2,]

fitS2$summary.hyperpar
theSd= Pmisc::priorPost(fitS2)

plot(fitS2$marginals.hyperpar$'Precision for state', type='l')

plot(theSd$sd$posterior, type='l', xlab='sd', ylab='dens',
  xlim = c(0,1), col='blue')
lines(theSd$sd$prior, col='blue', lty=2)



fitS3 = inla(y ~ Sex + ageFac:RuralUrban + 
    f(state, model='iid', hyper=list(
        prec=list(prior='pc.prec', param=c(99, 0.5)))
    ),
  data=forInla, family='binomial',
  lincomb = forLincombs,
  control.inla = list(strategy='laplace', fast=FALSE))
theSd3= Pmisc::priorPost(fitS3)

lines(theSd3$sd$posterior, col='red')
lines(theSd3$sd$prior, col='red', lty=2)



fitS4 = inla(y ~ Sex + ageFac:RuralUrban + 
    f(state, model='iid', hyper=list(
        prec=list(prior='pc.prec', param=c(log(1.5), 0.5)))
    ),
  data=forInla, family='binomial',
  lincomb = forLincombs,
  control.inla = list(strategy='laplace', fast=FALSE))
theSd4= Pmisc::priorPost(fitS4)

lines(theSd4$sd$posterior, col='grey', lwd=3)
lines(theSd4$sd$prior, col='grey', lwd=1, lty=2)

legend('topright',
  lty=1:2, lwd=1, legend = c('post','prior'),
  bty='n')


rbind(
    fitS2$summary.fixed[, c('mean','0.025quant','0.975quant')],
    Pmisc::priorPostSd(fitS2)$summary[, c('mean','0.025quant','0.975quant')]
  )


theCoef = exp(fitS4$summary.lincomb.derived[,
    c('0.5quant','0.025quant','0.975quant')])
theCoef = theCoef/(1+theCoef)

toPredict$Age = as.numeric(as.character(toPredict$ageFac))

#only plot males
isMale = toPredict$Sex == 'M'
shiftRural = 0.1*(toPredict$RuralUrban == 'Rural')

# plotting symbols big when sd is small
theSd = fitS4$summary.lincomb.derived[,'sd']
theCex = min(theSd)/theSd

plot(toPredict[isMale,'Age'] + shiftRural[isMale], 
  theCoef[isMale,'0.5quant'], 
  xlab='age', ylab='probability', ylim = c(0.015, 1),
  pch = 15, log='y', 
  cex = 2*theCex,
  col = mapmisc::col2html(
    c(Urban = 'red', Rural = 'green')[as.character(toPredict[isMale,'RuralUrban'])],
    0.4)
  )


segments(toPredict[isMale,'Age']+ shiftRural[isMale], 
  theCoef[isMale,'0.025quant'], 
  y1=theCoef[isMale,'0.975quant'],
  col = c(Urban = 'red', Rural = 'green')[as.character(toPredict[isMale,'RuralUrban'])])

legend('bottomright', pch=16, col=c('red','green'), legend = c('Urban','Rural'),
  bty='n')


 do.call(matplot, theSd4$sd$matplot)
 do.call(legend, theSd4$sd$legend)
 mtext(expression(sigma), side=1)
