#'Intervalo de Confiança das Curvas Ajustadas.
#'
#'Script que calcula o intervalo de confiança sobre os dados ajustados pelo modelo de Levenberg-Marquardt.
#'
#'@param vbl Contém dados completos do ajuste de Levenberg-Marquardt para parâmetros de Bertalanffy.
#'@param gom Contém dados completos do ajuste de Levenberg-Marquardt para parâmetros de Gompertz.
#'@param lgc Contém dados completos do ajuste de Levenberg-Marquardt para parâmetros de Logística.
#'@param curva_linha_ajustadaB Contém dados da curva de Bartalanffy com ajuste de Levenberg-Marquardt.
#'@param curva_linha_ajustadaG Contém dados da curva de Gompertz com ajuste de Levenberg-Marquardt.
#'@param curva_linha_ajustadaL Contém dados da curva de Logística com ajuste de Levenberg-Marquardt.
#'@param real_cont_fw Contém o "n" da amostra.
#'
#'@examples
#'intervaloConfianca(vbl, gom, lgc, curva_linha_ajustadaB, curva_linha_ajustadaG, curva_linha_ajustadaL, real_cont_fw)
#'
#'@export
#'
intervaloConfianca <- function(vbl, gom, lgc, curva_linha_ajustadaB, curva_linha_ajustadaG, curva_linha_ajustadaL, real_cont_fw){

  #função de subtração modular para desvio de média
  subtracao <<- function(x,media){
    if(x<=media){
      return(media-x)
    }else{
      return(x-media)
    }
  }

  #graus de liberdade
  grausliberdade <<- real_cont_fw

  #nível de confiança
  nivel <<- 0.05

  #t alfa/2
  ta <<- nivel/2

  #resultado da tabela t
  talfa <<- -1*qt(ta, df= grausliberdade)

  #raiz de n
  raizN <<- sqrt(real_cont_fw)

  ###### Bertalanffy
  #atribui dados de resumo da função dos mínimos
  sumarioB <<- summary(vbl)

  #atribui erro padrão dos resíduos
  SyxB <<- sumarioB$sigma

  #média de y
  media_yB <<- sum(curva_linha_ajustadaB)/real_cont_fw

  #incrementa y previsto
  yB  <<- data.frame()
  for(i in 1:real_cont_fw){
    yB[i,1] <<- subtracao(curva_linha_ajustadaB[i], media_yB)
  }

  #desvio padrão de Y
  sdYB <<- sqrt(sum(yB^2)/(real_cont_fw-1))

  #erro padrão de Y
  epB <<- sdYB/raizN

  #tranforma y dataframe em vetor para linha do gráfico
  y1B <<- as.vector(yB$V1)

  #IC por indivíduo
  ymeiB <<- vector()
  for(i in 1:real_cont_fw){
    ymeiB[i] <<- curva_linha_ajustadaB[i]-talfa*epB
  }
  ymaiB <<- vector()
  for(i in 1:real_cont_fw){
    ymaiB[i] <<- curva_linha_ajustadaB[i]+talfa*epB
  }

  ######## Gompertz
  #atribui dados de resumo da função dos mínimos
  sumarioG <<- summary(gom)

  #atribui erro padrão dos resíduos
  SyxG <<- sumarioG$sigma

  #média de y
  media_yG <<- sum(curva_linha_ajustadaG)/real_cont_fw

  #incrementa y previsto
  yG  <<- data.frame()
  for(i in 1:real_cont_fw){
    yG[i,1] <<- subtracao(curva_linha_ajustadaG[i], media_yG)
  }

  #desvio padrão de Y
  sdYG <<- sqrt(sum(yG^2)/(real_cont_fw-1))

  #erro padrão de Y
  epG <<- sdYG/raizN

  #tranforma y dataframe em vetor para linha do gráfico
  y1G <<- as.vector(yG$V1)

  #IC por indivíduo
  ymeiG <<- vector()
  for(i in 1:real_cont_fw){
    ymeiG[i] <<- curva_linha_ajustadaG[i]-talfa*epG
  }
  ymaiG <<- vector()
  for(i in 1:real_cont_fw){
    ymaiG[i] <<- curva_linha_ajustadaG[i]+talfa*epG
  }

  ######## Logística
  #atribui dados de resumo da função dos mínimos
  sumarioL <<- summary(lgc)

  #atribui erro padrão dos resíduos
  SyxL <<- sumarioL$sigma

  #média de y
  media_yL <<- sum(curva_linha_ajustadaL)/real_cont_fw

  #incrementa y previsto
  yL  <<- data.frame()
  for(i in 1:real_cont_fw){
    yL[i,1] <<- subtracao(curva_linha_ajustadaL[i], media_yL)
  }

  #desvio padrão de Y
  sdYL <<- sqrt(sum(yL^2)/(real_cont_fw-1))

  #erro padrão de Y
  epL <<- sdYL/raizN

  #tranforma y dataframe em vetor para linha do gráfico
  y1L <<- as.vector(yL$V1)

  #IC por indivíduo
  ymeiL <<- vector()
  for(i in 1:real_cont_fw){
    ymeiL[i] <<- curva_linha_ajustadaL[i]-talfa*epL
  }
  ymaiL <<- vector()
  for(i in 1:real_cont_fw){
    ymaiL[i] <<- curva_linha_ajustadaL[i]+talfa*epL
  }
  return()
}
