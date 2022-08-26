#'Curvas de Crescimento.
#'
#'Script que calcula a curva de crescimento a partir dos parâmetros gerados pelo modelo de Ford-Walford.
#'
#'@param dados Contém os dados para a curva obtidos através do modelo de Ford-Walford.
#'@param c_inf Contém o comprimento assintótico.
#'@param B Contém a taxa de crescimento.
#'@param tzer Contém o tempo no comprimento zero.
#'@param contador Contém o "n" da amostra.
#'@param tempoB Legenda do gráfico eixo x.
#'@param medida Legenda do gráfico eixo y.
#'@param mainNameA Legenda do gráfico título.
#'@param sexo Contém um texto referente ao grupo atual.
#'
#'@examples
#'crescimento(dados, c_inf, B, tzer, contador, tempoB, medida, mainNameA, sexo)
#'
#'@export
crescimento <- function(dados, c_inf, B, tzer, contador, tempoB, medida, mainNameA, sexo=NULL){

  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
    dados_curva <<- dados

    #renomeia dataframe para programação
    names(dados_curva) <<- c("v1","v2")
  }

  #recebe o parâmetro de comprimento assintótico
  c_infinito <<- c_inf

  #recebe o parâmetro de taxa de crescimento
  k <<- B

  #recebe o parâmetro de tempo no tamanho zero
  tzero <<- tzer

  #n amostral
  real_cont_fw <<- contador

  if(!is.null(sexo)){

    names(dados) = c("v1","v2","v3")
    dados_curva <<- select(dados,-c("v3"))
  }


  #cria variável com uma sequência das idades a partir do 0 até a idade máxima
  idades <<- as.vector(dados_curva$v1)
  idades <<- unique(idades)

  #inicializa os vetores da curva de Bertalanffy
  curvayb <<- vector()

  #inicializa os vetores da curva de Gompertz
  curvayg <<- vector()

  #inicializa os vetores da curva de Logística
  curvayl <<- vector()

    #adiciona a coluna com dados do cálculo de Bertalanffy
    for (i in 1:length(idades)) {
      temporaria <<- (c_infinito*(1-exp(-k*(idades[i]-tzero))))
      for(j in 1:real_cont_fw){
        if(dados_curva[j,1]==idades[i]){
          dados_curva[j,3] <<- temporaria
        }
      }
    }
    names(dados_curva) <<- c("v1","v2","v3")
    #adiciona a coluna com dados do cálculo de Gompertz
    for (i in 1:length(idades)) {
      temporaria <<- (c_infinito*exp(-exp(-k*(idades[i]-tzero))))
      for(j in 1:real_cont_fw){
        if(dados_curva[j,1]==idades[i]){
          dados_curva[j,4] <<- temporaria
        }
      }
    }
    names(dados_curva) <<- c("v1","v2","v3","v4")
    #adiciona a coluna com dados do cálculo de Logística
    for (i in 1:length(idades)) {
      temporaria <<- (c_infinito/(1+exp(-k*(idades[i]-tzero))))
      for(j in 1:real_cont_fw){
        if(dados_curva[j,1]==idades[i]){
          dados_curva[j,5] <<- temporaria
        }
      }
    }

  #nomeia a nova coluna
  names(dados_curva) <<- c("idade","ct","bertalanffy","gompertz","logistica")

    #converte em vetor os pontos referentes as linhas do cálculo de Bertalanffy
    curvayb <<- as.vector(dados_curva$bertalanffy)

    #converte em vetor os pontos referentes as linhas do cálculo de Gompertz
    curvayg <<- as.vector(dados_curva$gompertz)

    #converte em vetor os pontos referentes as linhas do cálculo de Logística
    curvayl <<- as.vector(dados_curva$logistica)

    #converte em vetor a idade para posição x
    curva_x <<- as.vector(dados_curva$idade)

    #converte em vetor o comprimento para posição y
    curva_y <<- as.vector(dados_curva$ct)

    if(idioma == 1){
      cat("\nDeseja escolher os gráficos de curva de crescimento? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to choose growth curve charts? Y/N\n")
    }

    respostaCurvas <<- toupper(readLines(n = 1))

    #condição de exceção
    if(respostaCurvas == ""){
      respostaCurvas <<- "N"
    }

    if(respostaCurvas == "S"||respostaCurvas == "Y"){
      if(idioma == 1){
        cat("\nDeseja obter a curva de crescimento",sexo,"\na partir do modelo de Bertalanffy? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to get the growth curve",sexo,"\nfrom the Bertalanffy model? Y/N\n")
      }

      respostaB <<- readLines(n = 1)
      #condição de exceção
      if(respostaB == ""){
        respostaB <<- "N"
      }
      respostaB <<- toupper(respostaB)

      if(idioma == 1){
        cat("\nDeseja obter a curva de crescimento",sexo,"\na partir do modelo de Gompertz? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to get the growth curve",sexo,"\nfrom the Gompertz model? Y/N\n")
      }
      respostaG <<- readLines(n = 1)
      #condição de exceção
      if(respostaG == ""){
        respostaG <<- "N"
      }
      respostaG <<- toupper(respostaG)

      if(idioma == 1){
        cat("\nDeseja obter a curva de crescimento",sexo,"\na partir do modelo de Logística? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to get the growth curve",sexo,"\nfrom the Logistic model? Y/N\n")
      }
      respostaL <<- readLines(n = 1)
      #condição de exceção
      if(respostaL == ""){
        respostaL <<- "N"
      }
      respostaL <<- toupper(respostaL)
    }else if(respostaCurvas == "N"){
      respostaB <<- "S"
      respostaG <<- "S"
      respostaL <<- "S"
    }

    if(respostaB == "Y"){
      respostaB <<- "S"
    }
    if(respostaG == "Y"){
      respostaG <<- "S"
    }
    if(respostaL == "Y"){
      respostaL <<- "S"
    }
    if(respostaB == "S"&& respostaG == "S" && respostaL == "S"){
      graficoN(curva_x, curva_y, curvayb, curvayg, curvayl, tempoB, medida, mainNameA)

      }else if(respostaB == "S" && respostaG == "S" && respostaL == "N"){
      graficoN(curva_x, curva_y, curvayb, curvayg, NULL, tempoB, medida, mainNameA)

        }else if(respostaB == "S" && respostaG == "N" && respostaL == "S"){
      graficoN(curva_x, curva_y, curvayb, NULL, curvayl, tempoB, medida, mainNameA)

          }else if(respostaB == "N" && respostaG == "S" && respostaL == "S"){
      graficoN(curva_x, curva_y, NULL, curvayg, curvayl, tempoB, medida, mainNameA)

            }else if(respostaB == "S" && respostaG == "N" && respostaL == "N"){
      graficoN(curva_x, curva_y, curvayb, NULL, NULL, tempoB, medida, mainNameA)

              }else if(respostaB == "N" && respostaG == "S" && respostaL == "N"){
      graficoN(curva_x, curva_y, NULL, curvayg, NULL, tempoB, medida, mainNameA)

                }else if(respostaB == "N" && respostaG == "N" && respostaL == "S"){
      graficoN(curva_x, curva_y, NULL, NULL, curvayl, tempoB, medida, mainNameA)
                }

  return()
}
