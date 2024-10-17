#'Ajuste de redução dos mínimos quadrados - Levenberg-Marquardt.
#'
#'Script que ajusta as curvas de crescimento de acordo com o modelo de Levenberg-Marquardt.
#'
#'@param c_infinito Contém o comprimento assintótico.
#'@param k Contém a taxa de crescimento.
#'@param tzero Contém o tempo no comprimento zero.
#'@param dados_curva Contém o dataframe com os dados das curvas de crescimento.
#'@param real_cont_fw Contém o "n" da amostra.
#'@param medida Legenda do gráfico eixo y.
#'@param tempoB Legenda do gráfico eixo x.
#'@param mainNameB Legenda do gráfico título.
#'@param sexo Contém um texto referente ao grupo atual.
#'@param genero Contém a legenda do grupo atual.
#'
#'@examples
#'ajusteLevenberg(c_infinito, k, tzero, dados_curva, real_cont_fw, medida, tempoB, mainNameB,sexo, genero)
#'
#'@export

####################################CURVA DE CRESCIMENTO COM AJUSTE
ajusteLevenberg <- function(c_infinito, k, tzero, dados_curva, real_cont_fw, medida, tempoB, mainNameB,sexo=NULL, genero=NULL){

  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
  }

  ######################################## AJUSTE DE REDUÇÃO DOS MÍNIMOS QUADRADOS LEVENBERG-MARQUARDT
  if(is.null(genero)){
    fordd = ford
    if(idioma == 1){
      ## Ocorreu alteração do código neste trecho 02/05/2023
      names(fordd) = c("Cinf","k","t0","ϕ","M")
      cat("\nCinf   k    t0    ϕ    M\n",round(ford,2),"\n\nDeseja travar algum parâmetro utilizado no cálculo \nde ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
      ## fim da alteração 02/05/2023
      }else if(idioma == 2){
      ## Ocorreu alteração do código neste trecho 02/05/2023
      names(fordd) = c("Linf","k","t0","ϕ","M")
      cat("\nLinf   k    t0    ϕ    M\n",round(ford,2),"\n\nDo you want to lock some parameter used in\nthe Levenberg-Marquardt least squares fit calculation \n? Y/N\n")
      ## fim da alteração 02/05/2023
    }

  }else if(genero == "A"){
    if(idioma == 1){
      ## Ocorreu alteração do código neste trecho 02/05/2023
      names(fordA) = c("Cinf","k","t0","ϕ","M")
      cat("\nCinf   k    t0    ϕ    M\n",round(fordA,2),"\n\nDeseja travar algum parâmetro utilizado no cálculo \nde ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
      ## fim da alteração 02/05/2023
    }else if(idioma == 2){
      ## Ocorreu alteração do código neste trecho 02/05/2023
      names(fordA) = c("Linf","k","t0","ϕ","M")
      cat("\nLinf   k    t0    ϕ    M\n",round(fordA,2),"\n\nDo you want to lock some parameter used in\nthe Levenberg-Marquardt least squares fit calculation \n? Y/N\n")
      ## fim da alteração 02/05/2023
    }

  }else if(genero == "B"){
    if(idioma == 1){
      ## Ocorreu alteração do código neste trecho 02/05/2023
      names(fordB) = c("Cinf","k","t0","ϕ", "M")
      cat("\nCinf   k    t0    ϕ    M\n",round(fordB,2),"\n\nDeseja travar algum parâmetro utilizado no cálculo \nde ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
      ## fim da alteração 02/05/2023
   }else if(idioma == 2){
      ## Ocorreu alteração do código neste trecho 02/05/2023
      names(fordB) = c("Linf","k","t0","ϕ", "M")
      cat("\nLinf   k    t0    ϕ    M\n",round(fordB,2),"\n\nDo you want to lock some parameter used in\nthe Levenberg-Marquardt least squares fit calculation \n? Y/N\n")
      ## fim da alteração 02/05/2023
  }

  }
  respostaAjuste <<- toupper(readLines(n=1))
  #condicao de excecao
  if(respostaAjuste == ""){
    respostaAjuste <<- "N"
  }
  if(respostaAjuste == "S"||respostaAjuste == "Y"){
    if(idioma == 1){
      cat("\nDeseja travar o valor do parâmetro Cinf",sexo,"\npara ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to lock the value of the parameter Linf",sexo,"\nfor the Levenberg-Marquardt least squares fit? Y/N\n")
    }

    respostaTravaLi = readLines(n=1)
    #condição de exceção
    if(respostaTravaLi == ""){
      respostaTravaLi = "N"
    }
    respostaTravaLi <- toupper(respostaTravaLi)
    if(idioma == 1){
      cat("\nDeseja travar o valor do parâmetro k",sexo,"\npara ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to lock the value of the parameter k",sexo,"\nfor the Levenberg-Marquardt least squares fit? Y/N\n")
    }

    respostaTravaK = readLines(n=1)
    #condição de exceção
    if(respostaTravaK == ""){
      respostaTravaK = "N"
    }
    respostaTravaK <- toupper(respostaTravaK)
    if(idioma == 1){
      cat("\nDeseja travar o valor do parâmetro t0",sexo,"\npara ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to lock the value of the parameter t0",sexo,"\nfor the Levenberg-Marquardt least squares fit? Y/N\n")
    }

    respostaTravaTz = readLines(n=1)
    #condição de exceção
    if(respostaTravaTz == ""){
      respostaTravaTz = "N"
    }
    respostaTravaTz <- toupper(respostaTravaTz)
  }else if(respostaAjuste == "N"){
    respostaTravaLi = "N"
    respostaTravaK = "N"
    respostaTravaTz = "N"
  }

  if(respostaTravaLi == "Y"){
    respostaTravaLi = "S"
  }
  if(respostaTravaK == "Y"){
    respostaTravaK  = "S"
  }
  if(respostaTravaTz == "Y"){
    respostaTravaTz = "S"
  }
## Ocorreu alteração do código neste trecho 02/05/2023
  if(idioma == 1){
    cat("\nDeseja aplicar base de dados diferente da utilizada no Ford-Walford",sexo,"\npara ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\nSe a escolha for sim, os dados não podem estar agrupados.\n")
  }else if(idioma == 2){
    cat("\nDo you want to apply a different database than the one used in Ford-Walford",sexo,"\nfor the Levenberg-Marquardt least squares fit? Y/N\nIf the choice is yes, the data cannot be grouped.\n")
  }
  respostaTrocaBD = toupper(readLines(n=1))
  if(respostaTrocaBD=="S"||respostaTrocaBD=="Y"){
    #Insere nov base de dados para ajustar a curva sobre os parâmetros obtidos pelo método do Ford-Walford
    if(idioma == 1){
      cat("\nInforme o número referente a extensão do arquivo de dado:\n1-xls ou xlsx (Padrão)\n2-csv\n3-txt\n4-Dataframe\n")
    }else if(idioma == 2){
      cat("\nEnter the extension number of the data file:\n1-xls or xlsx (Default)\n2-csv\n3-txt\n4-Dataframe\n")
    }
    tipo_dadosLM = scan(n=1)
    if(tipo_dadosLM == 1){
      dadoLM <- "xlsx"
    }else if(tipo_dadosLM == 2){
      dadoLM <- "csv"
    }else if(tipo_dadosLM == 3){
      dadoLM <- "txt"
    }else if(tipo_dadosLM == 4){
      dadoLM <- "dataframe"
    }else if(is.null(tipo_dadosLM)){
      dadoLM <- "xlsx"
    }

    if(idioma == 1){
      cat("\nDigite o nome do arquivo com a sua extensão se houver. e.g. arquivo.xlsx\nLembre-se de configurar o local do arquivo como seu diretório de trabalho\n")
    }else if(idioma == 2){
      cat("\nEnter the name of the file with its extension if any. e.g. file.xlsx\nRemember to set the file location to your working directory\n")
    }
    nome_dadosLM = readLines(n=1)

    if(dadoLM=="xlsx"){
      if(idioma == 1){
        cat("\nDigite o nome da planilha que se encontram os dados.\n")
      }else if(idioma == 2){
        cat("\nEnter the name of the worksheet where the data is located.\n")
      }
      planilhaLM = readLines(n=1)
    }

    if(dadoLM=="txt"||dadoLM=="csv"){
      if(idioma == 1){
        cat("\nQual caracter foi utilizado para separação das colunas no arquivo?\n")
      }else if(idioma == 2){
        cat("\nWhat character was used to separate the columns in the file?\n")
      }
      separadorLM = readLines(n=1)
    }else{
      separadorLM = ","
    }

    meus_dadosLM <<- data.frame()
    dadosLM <<- data.frame()

    if(dadoLM == "xlsx"){
      #cria dataframe a partir dos dados da planilha
      if(is.null(planilhaLM)){
        meus_dadosLM <<- read_excel(nome_dadosLM)
      }else{
        meus_dadosLM <<- read_excel(nome_dadosLM, sheet = planilhaLM)
      }
    }else if(dado == "dataframe"){
      meus_dadosLM <<- nome_dadosLM
    }else if(dadoLM == "csv"){
      if(is.null(separadorLM)){
        meus_dadosLM <<- read.csv(file = nome_dadosLM, header = TRUE)
      }else{
        meus_dadosLM <<- read.csv(file = nome_dadosLM, header = TRUE, sep = separadorLM)
      }
    }else if(dadoLM == "txt"){
      if(is.null(separadorLM)){
        if(idioma == 1){
          erro <<- "Necessário informar separador de arquivos em txt."
          erro
        }else{
          erro <<- "Requires txt file separator."
          erro
        }
      }else{
        meus_dadosLM <<- read.delim2(nome_dadosLM, sep=separadorLM)
      }
    }
      #atribui dados sem grupo de sexo da planilha ao dataframe dados
      dadosLM <<- meus_dadosLM[,1]
      dadosLM[,2] <<- meus_dadosLM[,2]
      names(dadosLM) <<- c("idade","ct")
      dados_curva <<- dadosLM

      #captura comprimento geral
      limY <<- max(dadosLM$ct)
      mlimY <<- 0
  }
## Fim da alteração do código 02/05/2023

  #definição dos mínimos e máximos para travar determinados parâmetros
  MAXIMO = as.numeric(9999999)
  MINIMO = as.numeric(0.0001)

  #suprimir alerta de erro
  oldw <- getOption("warn")
  options(warn = -1)

  #definindo controle da fórmula de nlsLM
  control = list(maxiter=10000,minFactor=1/1024,tol=1e-5)

  ############################### BERTALANFFY

  #cria a função de ajuste dos mínimos quadrados Levenberg-Marquardt Bertalanffy
  if(respostaTravaLi=="S" && respostaTravaK=="S"&& respostaTravaTz=="S"){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=k,tzero=tzero), lower=c(c_infinito=c_infinito,k=k,tzero=tzero))

  }else if(respostaTravaLi=="S"  && respostaTravaK=="S"  && respostaTravaTz=="N"){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=k,tzero=MAXIMO), lower=c(c_infinito=c_infinito,k=k,tzero=MINIMO))

  }else if(respostaTravaLi=="S" && respostaTravaK=="N" && respostaTravaTz=="S" ){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=MAXIMO,tzero=tzero), lower=c(c_infinito=c_infinito,k=MINIMO,tzero=tzero))

  }else if(respostaTravaLi=="S"  && respostaTravaK=="N" && respostaTravaTz=="N"){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=MAXIMO,tzero=MAXIMO), lower=c(c_infinito=c_infinito,k=MINIMO,tzero=MINIMO))

  }else if(respostaTravaLi=="N" && respostaTravaK=="S" && respostaTravaTz=="S"){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=k,tzero=tzero), lower=c(c_infinito=MINIMO,k=k,tzero=tzero))

  }else if(respostaTravaLi=="N" && respostaTravaK=="N" && respostaTravaTz=="S" ){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=MAXIMO,tzero=tzero), lower=c(c_infinito=MINIMO,k=MINIMO,tzero=tzero))

  }else if(respostaTravaLi=="N" && respostaTravaK=="S" && respostaTravaTz=="N"){
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=k,tzero=MAXIMO), lower=c(c_infinito=MINIMO,k=k,tzero=MINIMO))
  }else{
    vbl<<-nlsLM(dados_curva$ct~c_infinito*(1-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control)
  }

  #atribui a função a variável
  vbl_extracao <<- vbl$m

  #atribui os parâmetros de Bertalanffy ajustados
  cinfktzeroB <<- vbl_extracao$getAllPars()

  #atribui a curva ajustada
  curva_linha_ajustadaB <<- vbl_extracao$fitted()

  #atribui k ajustado de Bertalanffy
  k_ajustadoB <<- cinfktzeroB[2]

  #atribui t0 ajustado de Bertalanffy
  tzero_ajustadoB <<- cinfktzeroB[3]

  #atribui comprimento infinito de Bertalanffy
  c_infinito_ajustadoB <<- cinfktzeroB[1]

  ## ocorreu alteração do código neste trecho 02/05/2023
  #indice da performance de crescimento
  phi <<- round(log10(k_ajustadoB)+2*log10(c_infinito_ajustadoB),7)
  cinfktzeroB[4] <<- phi
  names(cinfktzeroB) <<- c("c_infinito","k","tzero","ϕ")
  ## fim alteração 02/05/2023

  ################################### GOMPERTZ

  #cria a função de ajuste dos mínimos quadrados Levenberg-Marquardt Gompertz
  if(respostaTravaLi=="S"  && respostaTravaK=="S" && respostaTravaTz=="S"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=k,tzero=tzero), lower=c(c_infinito=c_infinito,k=k,tzero=tzero))

  }else if(respostaTravaLi=="S"  && respostaTravaK=="S" && respostaTravaTz=="N"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=k,tzero=MAXIMO), lower=c(c_infinito=c_infinito,k=k,tzero=MINIMO))

  }else if(respostaTravaLi=="S"  && respostaTravaK=="N" && respostaTravaTz=="S"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=MAXIMO,tzero=tzero), lower=c(c_infinito=c_infinito,k=MINIMO,tzero=tzero))

  }else if(respostaTravaLi=="S" && respostaTravaK=="N" && respostaTravaTz=="N"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=MAXIMO,tzero=MAXIMO), lower=c(c_infinito=c_infinito,k=MINIMO,tzero=MINIMO))

  }else if(respostaTravaLi=="N" && respostaTravaK=="S" || respostaTravaK=="Y" && respostaTravaTz=="S"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=k,tzero=tzero), lower=c(c_infinito=MINIMO,k=k,tzero=tzero))

  }else if(respostaTravaLi=="N" && respostaTravaK=="N" && respostaTravaTz=="S"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=MAXIMO,tzero=tzero), lower=c(c_infinito=MINIMO,k=MINIMO,tzero=tzero))

  }else if(respostaTravaLi=="N" && respostaTravaK=="S"&& respostaTravaTz=="N"){
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=k,tzero=MAXIMO), lower=c(c_infinito=MINIMO,k=k,tzero=MINIMO))
  }else{
    gom<<-nlsLM(dados_curva$ct~c_infinito*exp(-exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control)
  }

  #atribui a função a variável
  gom_extracao <<- gom$m

  #atribui os parâmetros de Gompertz ajustados
  cinfktzeroG <<- gom_extracao$getAllPars()

  #atribui a curva ajustada
  curva_linha_ajustadaG <<- gom_extracao$fitted()

  #atribui k ajustado de Gompertz
  k_ajustadoG <<- cinfktzeroG[2]

  #atribui t0 ajustado de Gompertz
  tzero_ajustadoG <<- cinfktzeroG[3]

  #atribui comprimento infinito de Gompertz
  c_infinito_ajustadoG <<- cinfktzeroG[1]

  ## ocorreu alteração do código neste trecho 02/05/2023
  #indice da performance de crescimento
  phi <<- round(log10(k_ajustadoG)+2*log10(c_infinito_ajustadoG),7)
  cinfktzeroG[4] <<- phi
  names(cinfktzeroG) <<- c("c_infinito","k","tzero","ϕ")
  ## fim alteração 02/05/2023

  ################################### LOGÍSTICA

  #cria a função de ajuste dos mínimos quadrados Levenberg-Marquardt Logística
  if(respostaTravaLi=="S" && respostaTravaK=="S" && respostaTravaTz=="S"){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=k,tzero=tzero), lower=c(c_infinito=c_infinito,k=k,tzero=tzero))

  }else if(respostaTravaLi=="S"  && respostaTravaK=="S"  && respostaTravaTz=="N"){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=k,tzero=MAXIMO), lower=c(c_infinito=c_infinito,k=k,tzero=MINIMO))

  }else if(respostaTravaLi=="S"   && respostaTravaK=="N" && respostaTravaTz=="S" ){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=MAXIMO,tzero=tzero), lower=c(c_infinito=c_infinito,k=MINIMO,tzero=tzero))

  }else if(respostaTravaLi=="S"  && respostaTravaK=="N" && respostaTravaTz=="N"){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=c_infinito,k=MAXIMO,tzero=MAXIMO), lower=c(c_infinito=c_infinito,k=MINIMO,tzero=MINIMO))

  }else if(respostaTravaLi=="N" && respostaTravaK=="S" && respostaTravaTz=="S"){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=k,tzero=tzero), lower=c(c_infinito=MINIMO,k=k,tzero=tzero))

  }else if(respostaTravaLi=="N" && respostaTravaK=="N" && respostaTravaTz=="S"){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=MAXIMO,tzero=tzero), lower=c(c_infinito=MINIMO,k=MINIMO,tzero=tzero))

  }else if(respostaTravaLi=="N" && respostaTravaK=="S" && respostaTravaTz=="N"){
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control,upper=c(c_infinito=MAXIMO,k=k,tzero=MAXIMO), lower=c(c_infinito=MINIMO,k=k,tzero=MINIMO))
  }else{
    lgc<<-nlsLM(dados_curva$ct~c_infinito/(1+exp(-k*(dados_curva$idade-tzero))),data=dados_curva,
                start=list(c_infinito=c_infinito,k=k,tzero=tzero),control=control)
  }

  #atribui a função a variável
  lgc_extracao <<- lgc$m

  #atribui os parâmetros de Logística ajustados
  cinfktzeroL <<- lgc_extracao$getAllPars()

  #atribui a curva ajustada
  curva_linha_ajustadaL <<- lgc_extracao$fitted()

  #atribui k ajustado de Logística
  k_ajustadoL <<- cinfktzeroL[2]

  #atribui t0 ajustado de Logística
  tzero_ajustadoL <<- cinfktzeroL[3]

  #atribui comprimento infinito de Logística
  c_infinito_ajustadoL <<- cinfktzeroL[1]

  ## ocorreu alteração do código neste trecho 02/05/2023
  #indice da performance de crescimento
  phi <<- round(log10(k_ajustadoL)+2*log10(c_infinito_ajustadoL),7)
  cinfktzeroL[4] <<- phi
  names(cinfktzeroL) <<- c("c_infinito","k","tzero","ϕ")
  ## fim alteração 02/05/2023

  #habilita alerta de erro
  options(warn = oldw)
  if(idioma == 1){
    cat("\nDeseja obter o cálculo do Intervalo de Confiança",sexo,"\npara os gráficos com ajuste dos mínimos quadrados? S/N\n")
  }else if(idioma == 2){
    cat("\nDo you want to get the Confidence Interval calculation",sexo,"\nfor the least squares fit graphs? Y/N\n")
  }

  respostaIc = readLines(n=1)
  #condição de exceção
  if(respostaIc == ""){
    respostaIc = "N"
  }

  if(toupper(respostaIc)=="N"){
    ymaiB = NULL
    ymeiB = NULL
    ymaiG = NULL
    ymeiG = NULL
    ymaiL = NULL
    ymeiL = NULL
  }else{
    intervaloConfianca(vbl, gom, lgc, curva_linha_ajustadaB, curva_linha_ajustadaG, curva_linha_ajustadaL, real_cont_fw)
  }
  if(idioma == 1){
    cat("\nDeseja escolher os gráficos de curva de crescimento ajustados? S/N\n")
  }else if(idioma == 2){
    cat("\nDo you want to choose the adjusted growth curve graphs? Y/N\n")
  }

  respostaCurvasAjustadas <<- toupper(readLines(n = 1))
  #condição de exceção
  if(respostaCurvasAjustadas == ""){
    respostaCurvasAjustadas <<- "N"
  }

  if(respostaCurvasAjustadas == "S"||respostaCurvasAjustadas == "Y"){
    if(idioma == 1){
      cat("\nDeseja obter o gráfico do cálculo de Bertalanffy",sexo,"\ncom ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to graph the Bertalanffy calculus",sexo,"\nwith Levenberg-Marquardt least squares fit? Y/N\n")
    }

    respostaBe = readLines(n=1)
    #condição de exceção
    if(respostaBe == ""){
      respostaBe = "N"
    }
    respostaBe <- toupper(respostaBe)
    if(idioma == 1){
      cat("\nDeseja obter o gráfico do cálculo de Gompertz",sexo,"\ncom ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to graph the Gompertz calculus",sexo,"\nwith Levenberg-Marquardt least squares fit? Y/N\n")
    }
    respostaGo = readLines(n=1)
    #condição de exceção
    if(respostaGo == ""){
      respostaGo = "N"
    }
    respostaGo<- toupper(respostaGo)

    if(idioma == 1){
      cat("\nDeseja obter o gráfico do cálculo de Logística",sexo,"\ncom ajuste dos mínimos quadrados de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to graph the Logistic calculus",sexo,"\nwith Levenberg-Marquardt least squares fit? Y/N\n")
    }
    respostaLo = readLines(n=1)
    #condição de exceção
    if(respostaLo == ""){
      respostaLo = "N"
    }
    respostaLo <- toupper(respostaLo)
  }else if(respostaCurvasAjustadas == "N"){
    respostaBe = "S"
    respostaGo = "S"
    respostaLo = "S"
  }

  if(respostaBe == "Y"){
    respostaBe = "S"
  }
  if(respostaGo == "Y"){
    respostaGo = "S"
  }
  if(respostaLo == "Y"){
    respostaLo = "S"
  }
  curva_x = as.vector(dados_curva$idade)
  curva_y = as.vector(dados_curva$ct)

  if(respostaBe == "S" && respostaGo == "S" && respostaLo =="S"){
    graficoA(curva_x, curva_y, curva_linha_ajustadaB, curva_linha_ajustadaG, curva_linha_ajustadaL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

    }else if(respostaBe == "S" && respostaGo == "S" && respostaLo =="N"){
    graficoA(curva_x, curva_y, curva_linha_ajustadaB, curva_linha_ajustadaG, NULL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

      }else if(respostaBe == "S" && respostaGo == "N" && respostaLo =="N"){
    graficoA(curva_x, curva_y, curva_linha_ajustadaB, NULL, NULL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

        }else if(respostaBe == "N" && respostaGo == "S" && respostaLo =="S"){
    graficoA(curva_x, curva_y, NULL, curva_linha_ajustadaG, curva_linha_ajustadaL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

          }else if(respostaBe == "S" && respostaGo == "N" && respostaLo =="S"){
    graficoA(curva_x, curva_y, curva_linha_ajustadaB, NULL, curva_linha_ajustadaL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

            }else if(respostaBe == "N" && respostaGo == "N" && respostaLo =="S"){
    graficoA(curva_x, curva_y, NULL, NULL, curva_linha_ajustadaL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

              }else if(respostaBe == "N" && respostaGo == "S" && respostaLo =="N"){
    graficoA(curva_x, curva_y, NULL, curva_linha_ajustadaG, NULL, ymaiB, ymeiB, ymaiG, ymeiG, ymaiL, ymeiL, tempoB, medida, mainNameB)

  }

  #teste AIC
  #AIC_bertalanffyLM <<- AIC(vbl)
  #AIC_gompertzLM <<- AIC(gom)
  #AIC_logisticaLM <<- AIC(lgc)
  residuos <- data.frame(idade = dados_curva$idade) 
  residuos$ct <- dados_curva$ct 
  residuos$Bertalanffy_Residuos <- dados_curva$ct - curva_linha_ajustadaB
  residuos$Gompertz_Residuos <- dados_curva$ct - curva_linha_ajustadaG
  residuos$Logistica_Residuos <- dados_curva$ct - curva_linha_ajustadaL
      
  # Calcular a soma dos quadrados dos resíduos (SSR)
  SSR_bertalanffy <- sum(residuos$Bertalanffy_Residuos^2)
  SSR_gompertz <- sum(residuos$Gompertz_Residuos^2)
  SSR_logistica <- sum(residuos$Logistica_Residuos^2)

  # Número de parâmetros para cada modelo
  num_parametros <- 3  # Para Bertalanffy, Gompertz e Logística

  # Calcular o AIC
  AIC_bertalanffyLM <<- length(dados_curva$idade) * log(SSR_bertalanffy / length(dados_curva$idade)) + 2 * num_parametros
  AIC_gompertzLM <<- length(dados_curva$idade) * log(SSR_gompertz / length(dados_curva$idade)) + 2 * num_parametros
  AIC_logisticaLM <<- length(dados_curva$idade) * log(SSR_logistica / length(dados_curva$idade)) + 2 * num_parametros
    
  return()
}
