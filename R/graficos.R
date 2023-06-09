#'Gráficos dos Modelos Matemáticos.
#'
#'Script que engloba todos os gráficos a serem apresentados posteriomente às análises.
#'
#'@param ln_x Contém o vetor com o eixo X do resultado de ln.
#'@param ln_y Contém o vetor com o eixo y do resultado de ln.
#'@param regressao_ln Contém o resultado da regressão de ln.
#'@param regressaoZ Contém o resultado da regressão da mortalidade (Z).
#'@param labelX Contém o rótulo para eixo x.
#'@param labelY Contém o rótulo para eixo y.
#'@param mainA Contém o título do gráfico.
#'@param result Contém a legenda do gráfico.
#'@param sexo Contém um texto referente ao grupo atual.
#'@param entradaX Contém o vetor com o eixo x.
#'@param entradaY Contém o vetor com o eixo y.
#'@param entradaB Contém o vetor com a linha da curva de Bertalanffy.
#'@param entradaG Contém o vetor com a linha da curva de Gompertz.
#'@param entradaL Contém o vetor com a linha da curva de Logística.
#'@param emaB Contém o vetor com a linha da parte maior do IC para Bertalanffy.
#'@param emaG Contém o vetor com a linha da parte maior do IC para Gompertz.
#'@param emaL Contém o vetor com a linha da parte maior do IC para Logistica.
#'@param emeB Contém o vetor com a linha da parte menor do IC para Bertalanffy.
#'@param emeG Contém o vetor com a linha da parte menor do IC para Gompertz.
#'@param emeL Contém o vetor com a linha da parte menor do IC para Logística.
#'@param Lin Contém as classes de comprimento e seu respectivo n agrupado.
#'@param LnNdt Contém vetor de conversão do numero total de peixes de cada classe de comprimento LN(N/dt).
#'@param taspas Contém a idade média de cada classe de comprimento (t').
#'@param agarraX Contém dados de x para calculo da curva de captura.
#'@param agarraY Contém dados de y para calculo da curva de captura.
#'@param labelZX Contém o rótulo para eixo x.
#'@param labelZY Contém o rótulo para eixo y.
#'@param modelo Contém o nome do modelo matemático.
#'@param mainE Contém o título do gráfico.
#'@param labX Contém o rótulo para eixo x.
#'@param labY Contém o rótulo para eixo y.
#'@param pausas Contém o valor para utilizar no break do histograma.
#'
#'
#'@examples
#'graficoR(ln_x, ln_y, regressao_ln, labelX, labelY, mainA, result, sexo=NULL)
#'graficoRLN(ln_x, ln_y, regressao_ln, labelX, labelY, mainA, result,sexo=NULL)
#'graficoN(entradaX, entradaY, entradaB, entradaG, entradaL, labelX, labelY, mainA)
#'graficoA(entradaX, entradaY, entradaB, entradaG, entradaL, emaB, emeB, emaG, emeG, emaL, emeL, labelX, labelY, mainA)
#'graficoZ(LiN, tAspas, lnNdT, agarraX, agarraY, labelZX, labelZY, mainA, modelo, sexo=NULL, mainE, labX, labY, pausas)
#'
#'@export

###########
###########
###########
####################################### FORD-WALFORD
graficoR <- function(ln_x, ln_y, regressao_ln, labelX, labelY, mainA, result, sexo=NULL){

  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
  }
  if(idioma == 1){
  cat("\nDeseja ajustar gráficos do modelo de Ford-Walford? S/N\n")
  }else if(idioma == 2){
    cat("\nDo you want to adjust Ford-Walford model graphics? Y/N\n")
  }
  respostaAjusteGraficoFord <<- toupper(readLines(n=1))
  #condição de exceção
  if(respostaAjusteGraficoFord == ""){
    respostaAjusteGraficoFord <<- "N"
  }
  xlimLn = NULL
  ylimLn = NULL
  if(respostaAjusteGraficoFord == "S" ||respostaAjusteGraficoFord == "Y"){
    if(idioma == 1){
      cat("\nDeseja definir limite inferior e superior",sexo,"\npara os eixos x e y da regressão \ndas médias de comprimento de Ford-Walford? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to define lower and upper bound",sexo,"\nfor the x and y axes of the regression \nof the Ford-Walford length means? Y/N\n")
    }

    respostaLn = readLines(n=1)
    #condição de exceção
    if(respostaLn == ""){
      respostaLn = "N"
    }
    respostaLn <- toupper(respostaLn)

    if(respostaLn=="S"||respostaLn=="Y"){
      if(idioma == 1){
        cat("\nInforme primeiro para o eixo x \no limite inferior e aperte enter.\nEm seguida, o superior do eixo x e aperte enter.\n")
      }else if(idioma == 2){
        cat("\nFirst enter for the x-axis \nthe lower limit and press enter.\nThen the upper limit of the x-axis and press enter.\n")
      }
      xlimLn = scan(n=2)

      if(idioma == 1){
        cat("\nInforme primeiro para o eixo y \no limite inferior e aperte enter.\nEm seguida, o superior do eixo y e aperte enter.\n")
      }else if(idioma == 2){
        cat("\nFirst enter for the y-axis \nthe lower limit and press enter.\nThen the upper limit of the y-axis and press enter.\n")
      }
      ylimLn = scan(n=2)
    }
  }
  xmin = NULL
  ymin = NULL
  xmax = NULL
  ymax = NULL

  if(!is.null(xlimLn) && !is.null(ylimLn)){
    if(!length(xlimLn) == 0 && !length(ylimLn)==0){
      xmin = xlimLn[1]
      ymin = ylimLn[1]
      xmax = xlimLn[2]
      ymax = ylimLn[2]
    }
  }
  #apresenta regressão Ln(Cinf-Ci)
  if(!is.null(xlimLn) && !is.null(ylimLn)){
    if(!length(xlimLn) == 0 && !length(ylimLn)==0){
      windowsFonts(Fonte = windowsFont(letra))
      lnz=plot(ln_x,ln_y,
           family = "Fonte",
           bty = tipoCaixaA,
           col = corPontoA,
           main = mainA,
           xlab = labelX,
           ylab = labelY,
           xlim = c(xmin,xmax),
           ylim = c(ymin,ymax))
    }else{
      windowsFonts(Fonte = windowsFont(letra))
      lnz=plot(ln_x,ln_y,
               family = "Fonte",
               bty = tipoCaixaA,
               col = corPontoA,
               main = mainA,
               xlab = labelX,
               ylab = labelY)
      }
  }else{
    windowsFonts(Fonte = windowsFont(letra))
    lnz=plot(ln_x,ln_y,
             family = "Fonte",
             bty = tipoCaixaA,
             col = corPontoA,
             main = mainA,
             xlab = labelX,
             ylab = labelY)
  }

  par(family = "Fonte")
  legend("bottomright",
         inset=.01,
         bty = tipoCaixaAA,
         legend = result,
         col = c("white"),
         lty=1:2,
         cex=0.8)
  clip(min(ln_x),max(ln_x),min(ln_y), max(ln_y))
  abline(regressao_ln)
}

###########
###########
###########

graficoRLN <- function(ln_x, ln_y, regressao_ln, labelX, labelY, mainA, result,sexo=NULL){

  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
  }

  xlimLn = NULL
  ylimLn = NULL
  if(respostaAjusteGraficoFord == "S"){
    if(idioma == 1){
      cat("\nDeseja definir limite inferior e superior",sexo,"\npara os eixos x e y da regressão \ndo Ln de Ford-Walford? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to define lower and upper bound",sexo,"\nfor the x and y axes of the \nregression of the Ford-Walford Ln? Y/N\n")
    }

    respostaLn = readLines(n=1)
    #condição de exceção
    if(respostaLn == ""){
      respostaLn = "N"
    }
    respostaLn <- toupper(respostaLn)

    if(respostaLn=="S"||respostaLn=="Y"){
      if(idioma == 1){
        cat("\nInforme primeiro para o eixo x \no limite inferior e aperte enter.\nEm seguida, o superior do eixo x e aperte enter.\n")
      }else if(idioma == 2){
        cat("\nFirst enter for the x-axis \nthe lower limit and press enter.\nThen the upper limit of the x-axis and press enter.\n")
      }

      xlimLn = scan(n=2)

      if(idioma == 1){
        cat("\nInforme primeiro para o eixo y \no limite inferior e aperte enter.\nEm seguida, o superior do eixo y e aperte enter.\n")
      }else if(idioma == 2){
        cat("\nFirst enter for the y-axis \nthe lower limit and press enter.\nThen the upper limit of the y-axis and press enter.\n")
      }
      ylimLn = scan(n=2)
    }
  }

  xmin = NULL
  ymin = NULL
  xmax = NULL
  ymax = NULL

  if(!is.null(xlimLn) && !is.null(ylimLn)){
    if(!length(xlimLn) == 0 && !length(ylimLn)==0){
      xmin = xlimLn[1]
      ymin = ylimLn[1]
      xmax = xlimLn[2]
      ymax = ylimLn[2]
    }
  }
  #apresenta regressão Ln(Cinf-Ci)
  if(!is.null(xlimLn) && !is.null(ylimLn)){
    if(!length(xlimLn) == 0 && !length(ylimLn)==0){
     lnz=plot(ln_x,ln_y,
           family = "Fonte",
           bty = tipoCaixaA,
           col = corPontoA,
           main = mainA,
           xlab = labelX,
           ylab = labelY,
           xlim = c(xmin,xmax),
           ylim = c(ymin,ymax))
    }else{
      lnz=plot(ln_x,ln_y,
               family = "Fonte",
               bty = tipoCaixaA,
               col = corPontoA,
               main = mainA,
               xlab = labelX,
               ylab = labelY)
    }
  }else{
    lnz=plot(ln_x,ln_y,
             family = "Fonte",
             bty = tipoCaixaA,
             col = corPontoA,
             main = mainA,
             xlab = labelX,
             ylab = labelY)
  }
  legend("bottomleft",
         inset=.01,
         bty = tipoCaixaAA,
         legend = result,
         col = c("white"),
         lty=1:2,
         cex=0.8)
  clip(min(ln_x),max(ln_x),min(ln_y), max(ln_y))
  abline(regressao_ln)
}

###########
###########
###########

############################## FUNÇÃO SEM AJUSTE DOS MÍNIMOS QUADRADOS
graficoN <- function(entradaX, entradaY, entradaB, entradaG, entradaL, labelX, labelY, mainA){
  linhaB <- entradaB
  linhaG <- entradaG
  linhaL <- entradaL
  curva_x <- entradaX
  curva_y <- entradaY
  limiteX <- 0.1
  limiteXX <- max(curva_x)-0.1

  ####################### TODOS OS MODELOS JUNTOS
  if(!is.null(entradaB)&&!is.null(entradaG)&&!is.null(entradaL)){
    #cria o gráfico
    curvaA=plot(curva_x,curva_y,
                family = "Fonte",
                bty = tipoCaixaB,
                col = corPontoB,
                main = mainA,
                xlab = labelX,
                ylab = labelY,
                ylim = c(mlimY,limY),
                xaxt='n',
                xlim = c(limiteX,limiteXX))
    axis(1, at = seq(0, round(max(curva_x)), by = 1))
    legend("bottomright",
           legend = c(modeloA,modeloB,modeloC),
           bty = tipoCaixaBB,
           lty = c(tipoLinhaA, tipoLinhaB, tipoLinhaC),
           col = c(coresA,coresB,coresC),
           lwd = 1.5,
           cex=0.8)

    #adiciona a linha da curva Bertalanffy
    lines(curva_x, linhaB,
          col = coresA,
          lty = tipoLinhaA,
          lwd = 1.5)

    #adiciona a linha da curva Gompertz
    lines(curva_x, linhaG,
          col = coresB,
          lty = tipoLinhaB,
          lwd = 1.5)

    #adiciona a linha da curva Logística
    lines(curva_x, linhaL,
          col = coresC,
          lty = tipoLinhaC,
          lwd = 1.5)
  }
  if(!is.null(entradaB)&&!is.null(entradaG)&&!is.null(entradaL)){
  if(idioma == 1){
    cat("\nDeseja manter o mesmo padrão de cor e linha
      \ndo gráfico generalizado nos específicos?
      \nDigite (1) para: Manter somente a cor
      \nDigite (2) para: manter somente a linha
      \nDigite (3) para: Manter Ambos
      \nDigite (4) para: Não manter\n")
  }else if(idioma == 2){
    cat("\nDo you want to keep the same color and line pattern
       \nfrom the generalized to the specifics graph?
       \nEnter (1) to: Keep color only
       \nEnter (2) to: keep line only
       \nEnter (3) to: Keep Both
       \nEnter (4) to: Do not keep\n")
  }

   respostaGraficos <<- scan(n=1)
  }else{
    respostaGraficos <<-4
  }
  #controle de exceção
  if(is.null(respostaGraficos)||length(respostaGraficos)==0 || respostaGraficos >=5 || respostaGraficos <= 0){
    respostaGraficos <<- 4
  }

  if(respostaGraficos == 1){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresB
    tipoLinhaBB <<- tipoLinhaA

    #cores das linhas dos gráficos de Logística
    coresF <<- coresC
    tipoLinhaCC <<- tipoLinhaA

  }else if(respostaGraficos == 2){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresA
    tipoLinhaBB <<- tipoLinhaB

    #cores das linhas dos gráficos de Logística
    coresF <<- coresA
    tipoLinhaCC <<- tipoLinhaC

  }else if(respostaGraficos==3){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresB
    tipoLinhaBB <<- tipoLinhaB
    #cores das linhas dos gráficos de Logística
    coresF <<- coresC
    tipoLinhaCC <<- tipoLinhaC

  }else if(respostaGraficos==4){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresA
    tipoLinhaBB <<- tipoLinhaA

    #cores das linhas dos gráficos de Logística
    coresF <<- coresA
    tipoLinhaCC <<- tipoLinhaA

  }
  ################## BERTALANFFY
  if(!is.null(entradaB)){
    curvaB=plot(curva_x,curva_y,
                family = "Fonte",
                bty = tipoCaixaB,
                col = corPontoB,
                main = mainA,
                xlab = labelX,
                ylab = labelY,
                ylim = c(mlimY,limY),
                xaxt='n',
                xlim = c(limiteX,limiteXX))
    axis(1, at = seq(0, round(max(curva_x)), by = 1))
    legend("bottomright",
           legend = c(modeloA),
           bty = tipoCaixaBB,
           lty = c(tipoLinhaAA),
           col = c(coresD),
           lwd = 1.5)

    #adiciona a linha da curva Bertalanffy
    lines(curva_x, linhaB,
          col = coresD,
          lty = tipoLinhaAA,
          lwd = 1.5)
  }
  ################## GOMPERTZ
  if(!is.null(entradaG)){
    curvaC=plot(curva_x,curva_y,
                family = "Fonte",
                bty = tipoCaixaB,
                col = corPontoB,
                main = mainA,
                xlab = labelX,
                ylab = labelY,
                ylim = c(mlimY,limY),
                xaxt='n',
                xlim = c(limiteX,limiteXX))
    axis(1, at = seq(0, round(max(curva_x)), by = 1))
    legend("bottomright",
           legend = c(modeloB),
           bty = tipoCaixaBB,
           lty = c(tipoLinhaBB),
           col = c(coresE),
           lwd = 1.5)

    #adiciona a linha da curva Gompertz
    lines(curva_x, linhaG,
          col = coresE,
          lty = tipoLinhaBB,
          lwd = 1.5)
  }
  ################## LOGISTIC
  if(!is.null(entradaL)){
    curvaD=plot(curva_x,curva_y,
                family = "Fonte",
                bty = tipoCaixaB,
                col = corPontoB,
                main = mainA,
                xlab = labelX,
                ylab = labelY,
                ylim = c(mlimY,limY),
                xaxt='n',
                xlim = c(limiteX,limiteXX))
    axis(1, at = seq(0, round(max(curva_x)), by = 1))
    legend("bottomright",
           legend = c(modeloC),
           bty = tipoCaixaBB,
           lty = c(tipoLinhaCC),
           col = c(coresF),
           lwd = 1.5)

    #adiciona a linha da curva Logística
    lines(curva_x, linhaL,
          col = coresF,
          lty = tipoLinhaCC,
          lwd = 1.5)
  }

  return()
}

###########
###########
###########

############################# FUNÇÃO COM AJUSTE DOS MÍNIMOS QUADRADOS
graficoA <- function(entradaX, entradaY, entradaB, entradaG, entradaL, emaB, emeB, emaG, emeG, emaL, emeL, labelX, labelY, mainA){
  linhaB <- entradaB
  linhaG <- entradaG
  linhaL <- entradaL
  curva_x <- entradaX
  curva_y <- entradaY
  limiteX <- 0.1
  limiteXX <- max(curva_x)-0.1

  ####################### TODOS OS MODELOS JUNTOS
  if(!is.null(entradaB) && !is.null(entradaG) && !is.null(entradaL)){
  #cria o gráfico ajustado com todos os modelos
  curvaE=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaC,
              col = corPontoC,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloA,modeloB,modeloC),
         bty = tipoCaixaCC,
         lty = c(1, 1, 2),
         col = c(coresA,coresB,coresC),
         lwd = 1.5)

  #adiciona a linha da curva Bertalanffy
  lines(curva_x, linhaB,
        col = coresA,
        lty = 1,
        lwd = 1.5)

  #adiciona a linha da curva Gompertz
  lines(curva_x, linhaG,
        col = coresB,
        lty = 1,
        lwd = 1.5)

  #adiciona a linha da curva Logística
  lines(curva_x, linhaL,
        col = coresC,
        lty = 2,
        lwd = 1.5)
  }
  if(!is.null(entradaB) && !is.null(entradaG) && !is.null(entradaL)){
  if(idioma == 1){
    cat("\nDeseja manter o mesmo padrão de cor e linha
      \ndo gráfico generalizado nos específicos?
      \nDigite (1) para: Manter somente a cor
      \nDigite (2) para: manter somente a linha
      \nDigite (3) para: Manter Ambos
      \nDigite (4) para: Não manter\n")
  }else if(idioma == 2){
    cat("\nDo you want to keep the same color and line pattern
       \nfrom the generalized to the specifics graph?
       \nEnter (1) to: Keep color only
       \nEnter (2) to: keep line only
       \nEnter (3) to: Keep Both
       \nEnter (4) to: Do not keep\n")
  }
  respostaGraficos <<- scan(n=1)
  }else{
    respostaGraficos <<- 4
  }

  #controle de exceção
  if(is.null(respostaGraficos)||length(respostaGraficos)==0 || respostaGraficos >=5 || respostaGraficos <= 0){
    respostaGraficos <<- 4
  }

  if(respostaGraficos == 1){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresB
    tipoLinhaBB <<- tipoLinhaA

    #cores das linhas dos gráficos de Logística
    coresF <<- coresC
    tipoLinhaCC <<- tipoLinhaA

  }else if(respostaGraficos == 2){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresA
    tipoLinhaBB <<- tipoLinhaB

    #cores das linhas dos gráficos de Logística
    coresF <<- coresA
    tipoLinhaCC <<- tipoLinhaC

  }else if(respostaGraficos==3){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresB
    tipoLinhaBB <<- tipoLinhaB
    #cores das linhas dos gráficos de Logística
    coresF <<- coresC
    tipoLinhaCC <<- tipoLinhaC

  }else if(respostaGraficos==4){

    #cores das linhas dos gráficos de Bertalanffy
    coresD <<- coresA
    tipoLinhaAA <<- tipoLinhaA

    #cores das linhas dos gráficos de Gompertz
    coresE <<- coresA
    tipoLinhaBB <<- tipoLinhaA

    #cores das linhas dos gráficos de Logística
    coresF <<- coresA
    tipoLinhaCC <<- tipoLinhaA

  }

  if(!is.null(entradaB)){
###################### BERTALANFFY
  curvaF=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaC,
              col = corPontoC,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloA),
         bty = tipoCaixaCC,
         lty = c(tipoLinhaAA),
         col = c(coresD),
         lwd = 1.5)

  #adiciona a linha da curva Bertalanffy
  lines(curva_x, linhaB,
        col = coresD,
        lty = tipoLinhaAA,
        lwd = 1.5)
  }

  if(!is.null(entradaG)){
######################## GOMPERTZ
  curvaG=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaC,
              col = corPontoC,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloB),
         bty = tipoCaixaCC,
         lty = c(tipoLinhaBB),
         col = c(coresE),
         lwd = 1.5)

  #adiciona a linha da curva Gompertz
  lines(curva_x, linhaG,
        col = coresE,
        lty = tipoLinhaBB,
        lwd = 1.5)
  }

  if(!is.null(entradaL)){
######################## LOGÍSTICA
  curvaH=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaC,
              col = corPontoC,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloC),
         bty = tipoCaixaCC,
         lty = c(tipoLinhaCC),
         col = c(coresF),
         lwd = 1.5)

  #adiciona a linha da curva Logística
  lines(curva_x, linhaL,
        col = coresF,
        lty = tipoLinhaCC,
        lwd = 1.5)
  }

  ######################## BERTALANFFY IC
  if(!is.null(entradaB) && !is.null(emaB) && !is.null(emeB)){
  curvaI=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaD,
              col = corPontoD,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloA,modeloD),
         bty = tipoCaixaDD,
         lty = c(1, tipoLinhaD),
         col = c(coresA,coresG),
         lwd = 1.5)

  #adiciona a linha da curva Bertalanffy
  lines(curva_x, linhaB,
        col = coresA,
        lty = 1,
        lwd = 1.5)

  #adiciona a linha da curva do IC
  lines(curva_x, emaB,
        col = coresG,
        lty = tipoLinhaD,
        lwd = 1.5)

  lines(curva_x, emeB,
        col = coresG,
        lty = tipoLinhaD,
        lwd = 1.5)
  }

  if(!is.null(entradaG)&&!is.null(emaG)&&!is.null(emeG)){
######################## GOMPERTZ IC
  curvaJ=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaD,
              col = corPontoD,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloB,modeloD),
         bty = tipoCaixaDD,
         lty = c(1, tipoLinhaD),
         col = c(coresA,coresG),
         lwd = 1.5)

  #adiciona a linha da curva Gompertz
  lines(curva_x, linhaG,
        col = coresA,
        lty = 1,
        lwd = 1.5)

  #adiciona a linha da curva do IC
  lines(curva_x, emaG,
        col = coresG,
        lty = tipoLinhaD,
        lwd = 1.5)

  lines(curva_x, emeG,
        col = coresG,
        lty = tipoLinhaD,
        lwd = 1.5)
  }

  if(!is.null(entradaL)&&!is.null(emaL)&&!is.null(emeL)){
######################## LOGÍSTICA IC
  curvaK=plot(curva_x,curva_y,
              family = "Fonte",
              bty = tipoCaixaD,
              col = corPontoD,
              main = mainA,
              xlab = labelX,
              ylab = labelY,
              ylim = c(mlimY,limY),
              xaxt='n',
              xlim = c(limiteX,limiteXX))
  axis(1, at = seq(0, round(max(curva_x)), by = 1))
  legend("bottomright",
         legend = c(modeloC,modeloD),
         bty = tipoCaixaDD,
         lty = c(1, tipoLinhaD),
         col = c(coresA,coresG),
         lwd = 1.5)

  #adiciona a linha da curva Logística
  lines(curva_x, linhaL,
        col = coresA,
        lty = 1,
        lwd = 1.5)

  #adiciona a linha da curva do IC
  lines(curva_x, emaL,
        col = coresG,
        lty = tipoLinhaD,
        lwd = 1.5)

  lines(curva_x, emeL,
        col = coresG,
        lty = tipoLinhaD,
        lwd = 1.5)
  }

}

###########
###########
###########


################################ MORTALIDADE TOTAL
graficoZ <- function(LiN, tAspas, lnNdT, agarraX, agarraY, labelZX, labelZY, mainA, modelo, sexo=NULL, mainE, labX, labY, pausas){

  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
  }

  #histograma de classes de comprimento
  hist(LiN,
       main=mainE,
       xlab=labX,
       ylab=labY)

  if(modelo=="(Ford-Walford)"){
    if(idioma == 1){
      cat("\nDeseja selecionar os pontos da curva de captura",sexo,"\na partir dos parâmetros de Ford-Walford? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to select the capture curve points",sexo,"\nfrom the Ford-Walford parameters? Y/N\n")
    }

  }else if(modelo=="(Levenberg-Marquardt Bertalanffy)"){
    if(idioma == 1){
      cat("\nDeseja selecionar os pontos da curva de captura",sexo,"\na partir dos parâmetros ajustados de Bertalanffy? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to select the capture curve points",sexo,"\nfrom the adjusted Bertalanffy parameters? Y/N\n")
    }

  }else if(modelo=="(Levenberg-Marquardt Gompertz)"){
    if(idioma == 1){
      cat("\nDeseja selecionar os pontos da curva de captura",sexo,"\na partir dos parâmetros ajustados de Gompertz? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to select the capture curve points",sexo,"\nfrom the adjusted Gompertz parameters? Y/N\n")
    }

  }else if(modelo=="(Levenberg-Marquardt Logística)"){
    if(idioma == 1){
      cat("\nDeseja selecionar os pontos da curva de captura",sexo,"\na partir dos parâmetros ajustados de Logística? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to select the capture curve points",sexo,"\nfrom the adjusted Logistic parameters? Y/N\n")
    }

  }
  resposta <- readLines(n = 1)
  #condição de exceção
  if(resposta == ""){
    resposta = "N"
  }
  if(toupper(resposta) == "S"||toupper(resposta) == "Y"){
    #seleção da reta da curva de captura na janela
    if(idioma == 1){
      text = paste("\nEscolha o ponto mínimo e o máximo no gráfico",sexo,"\npara incluir a linha de regressão!\n")
      textoD = "Clique em dois números distintos."
    }else if(idioma == 2){
      text = paste("\nChoose the min and max point on the graph",sexo,"\nto include the regression line!\n")
      textoD = "Click on two distinct numbers."
    }

    writeLines(text)
    flush.console()
    dev.new()
    op <- par(mfrow = c(1,1),
              c(5, 4, 4, 2) + 0.1,
              oma = c(2, 1, 0, 1) + 0.1)
    plot(x = tAspas,y = lnNdT,
         family = "Fonte",
         type = "n")
    mtext(side = 3, textoD,
          xpd = NA,
          cex = 1.25)
    text(tAspas, lnNdT,
         labels=as.character(order(tAspas)),
         cex= 0.7)
    cutter <- identify(x = tAspas, y = lnNdT,
                       labels = order(tAspas),
                       n=2,
                       col = "red")
    par(op)

    if(is.na(cutter[1]) | is.nan(cutter[1]) |
       is.na(cutter[2]) | is.nan(cutter[2]) ) stop(noquote("Você não escolheu nenhum ponto no gráfico. Por favor, reexecute a função e escolha os pontos no gráfico!"))

    dev.off()

    pegaX <- vector()
    pegaY <- vector()
    indiceCutter <- 1
    for(i in cutter[1]:cutter[2]){
      pegaX[indiceCutter] <- tAspas[i]
      pegaY[indiceCutter] <- lnNdT[i]
      indiceCutter <- indiceCutter+1
    }
    #cálculo do R²
    r2_z <<- cor(pegaX,pegaY)^2

    #calcula regressão
    regressaoZ <<- lm(pegaY~pegaX)

    #coeficiente a
    z_a <<- as.double(regressaoZ$coefficients[1])

    #coeficiente b
    z_b <<- as.double(regressaoZ$coefficients[2])

    result <- paste("Z = ",abs(round(z_b,2))," R² = ",round(r2_z,3))
    Z <<- abs(round(z_b,2))

    xmax <- round(max(tAspas),0)+1
    ymax <- round(max(lnNdT),0)+1

    #apresenta regressão
    curvaCapturaz=plot(tAspas,lnNdT,
                       family = "Fonte",
                       bty = tipoCaixaE,
                       col = corPontoE,
                       pch = 16,
                       main = mainA,
                       xlab = "",
                       ylab = "")
    legend("topright",
           inset=.01,
           bty = tipoCaixaEE,
           legend = result,
           col = c("white"),
           lty=1:2,
           cex=0.8)
    #grid(curvaCapturaz)
    clip(min(pegaX),max(pegaX),min(pegaY), max(pegaY))
    abline(regressaoZ)

  }else if(toupper(resposta) == "N"){
    #seleção da reta da captura automática

    #cálculo do R²
    r2_z <<- cor(agarraX,agarraY)^2

    #calcula regressão
    regressaoZ = lm(agarraY~agarraX)

    #coeficiente a
    z_a <<- as.double(regressaoZ$coefficients[1])

    #coeficiente b
    z_b <<- as.double(regressaoZ$coefficients[2])

    result <- paste("Z = ",abs(round(z_b,2))," R² = ",round(r2_z,3))
    Z <<- abs(round(z_b,2))

    xmaxx <- round(max(agarraX))+1
    ymaxx <- round(max(agarraY))+1
    #apresenta regressão
    curvaCapturaz=plot(tAspas,lnNdT,
                       family = "Fonte",
                       bty = tipoCaixaE,
                       col = corPontoE,
                       pch = 16,
                       main = mainA,
                       xlab = "",
                       ylab = "")
    legend("topright",
           inset=.01,
           bty = tipoCaixaEE,
           legend = result,
           col = c("white"),
           lty=1:2,
           cex=0.8)
    #grid(curvaCapturaz)
    clip(min(agarraX),max(agarraX),min(agarraY), max(agarraY))
    abline(regressaoZ)
  }

}
