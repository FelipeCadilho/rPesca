#'Interface rPesca.
#'
#'Script que análisa dados amostrais de acordo com modelos de crescimento, mortalidade e verossimilhança.
#'
#'@param cores Determina como será exibido o gráfico, sendo 1 = em escala de cinza ou 2 = colorido. (Por padrão: Escala de cinza)
#'@param idioma Determina o idioma a ser apresentado no resultado, sendo 1 = português ou 2 = inglês.(Por padrão: Português)
#'@param un Determina a unidade de medida, sendo 1 = cm, 2 = mm ou 3 = m.(Por padrão: cm)
#'@param tipoComprimento Determina o tipo do comprimento aplicado, exemplo: se informar "Total" será apresentado como Comprimento Total.
#'@param tempo Determina a medida da idade, sendo 1 = ano, 2 = mês ou 3 = dia.(Por padrão: Ano)
#'@param tipo_dados Informa a extensão do arquivo com os dados, sendo 1 = xls/xlsx, 2 = csv, 3 = txt (primeira linha é cabeçalho) ou 4 = dataframe.(Por padrão: Dataframe)
#'@param nome_dados Campo caracter para informar nome do arquivo que está com os dados incluindo sua extensão. Exemplo: "dados.xls". (Preenchimento obrigatório)
#'@param planilha Campo caracter para informar a pasta com os dados dentro da planilha xls/xlsx.(Por padrão: Primeira pasta)
#'@param separador Campo caracter para informar o separador utilizado caso o arquivo esteja em csv. Exemplo: para espaço " ", para vírgula ",", etc. (Por padrão: Vírgula)
#'@param grupo Informa se deseja analisar em grupos, sendo 1 = sim ou sem preenchimento = não.
#'@param nomeA Informa o nome do grupo A.(Obrigatório para análise de grupos)
#'@param nomeB Informa o nome do grupo B.(Obrigatório para análise de grupos)
#'@param Fonte Informa a família da fonte a ser utilizada nos gráficos, sendo NULL = Times New Roman, 1 = Arial ou 2 = Courier New.(Por padrão: Times New Roman)
#'
#'@examples
#'data("exemplo")
#'rPesca(2, 1, 1, "Total", 1, 4, ex_individual)
#'
#'data("exemplo")
#'rPesca(2, 1, 1, "Total", 1, 4, ex_grupo, NULL, NULL, 1, "ara", "ita")
#'
#'data("example")
#'rPesca(2, 2, 1, "Total", 1, 4, eg_individual)
#'
#'data("example")
#'rPesca(2, 2, 1, "Total", 1, 4, eg_group, NULL, NULL, 1, ara, ita)
#'
#'@export

rPesca <- function(cores=1, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados, planilha=NULL, separador=NULL, grupo=NULL, nomeA=NULL, nomeB=NULL, Fonte=NULL){

  idioma <<- idioma

  if(idioma == 1){
  cat("\nEssa é a primeira execução após instalação deste pacote? S/N\n")
  }else if(idioma == 2){
    cat("\nIs this the first run after installing this package? Y/N\n")
  }
  respostinha = toupper(readLines(n=1))

  if(respostinha == "S"||respostinha == "Y"){

  #chama pacote que lê planilha em excel
  install.packages("readxl")
  library("readxl")

  #chama pacote de controle de dados
  install.packages("dplyr")
  library("dplyr")

  #chama pacote de calculo de logaritmo natural
  install.packages("SciViews")
  library("SciViews")

  #chama pacote de cálculo dos mínimos quadrados  Levenberg
  install.packages("minpack.lm")
  library("minpack.lm")

  #chama pacote de teste de verossimilhança de Kimura (1980)
  install.packages("fishmethods")
  library("fishmethods")

  }else{
    #chama pacote que lê planilha em excel
    library("readxl")

    #chama pacote de controle de dados
    library("dplyr")

    #chama pacote de calculo de logaritmo natural
    library("SciViews")

    #chama pacote de cálculo dos mínimos quadrados  Levenberg
    library("minpack.lm")

    #chama pacote de teste de verossimilhança de Kimura (1980)
    library("fishmethods")
  }
  medida <- ""
  tempoB <- ""
  dado <- ""
  erro <<- NULL

######################################## CLASSIFICAÇÃO DE CORES E TIPOS DE OBJETOS DOS GRÁFICOS

  #cores das linhas dos gráficos
  if(cores==1){
    #cores das linhas dos gráficos de Bertalanffy
    coresA <<- "#000000"

    #cores das linhas dos gráficos de Gompertz
    coresB <<- "#4D4D4D"

    #cores das linhas dos gráficos de Logística
    coresC <<- "#5C5C5C"

    #cores das linhas dos gráficos de Intervalo de Confiança
    coresG <<- "#4D4D4D"

  }else if(cores==2){
    #cores das linhas dos gráficos de Bertalanffy
    coresA <<- "blue"

    #cores das linhas dos gráficos de Gompertz
    coresB <<- "green"

    #cores das linhas dos gráficos de Logística
    coresC <<- "red"

    #cores das linhas dos gráficos de Intervalo de Confiança
    coresG <<- "red"

  }

  ########### COR DO PONTO
  #cor do ponto no gráfico de Ford-Walford
  corPontoA <<- "Black"

  #cor do ponto no gráfico de Curva de Crescimento
  corPontoB <<- "Black"

  #cor do ponto no gráfico de Levenberg-Marquardt
  corPontoC <<- "Black"

  #cor do ponto no gráfico de Intervalo de Confiança
  corPontoD <<- "Black"

  #cor do ponto no gráfico de Mortalidade
  corPontoE <<- "Black"

  ########### TIPO LINHA
  #Tipo das linhas dos gráficos de Bertalanffy
  tipoLinhaA <<- 1

  #Tipo das linhas dos gráficos de Gompertz
  tipoLinhaB <<- 1

  #Tipo das linhas dos gráficos de Logística
  tipoLinhaC <<- 2

  #Tipo das linhas dos gráficos de IC
  tipoLinhaD <<- 2

  ########### TIPO MARGEM

  #Tipo da margem do gráfico de Ford-Walford
  tipoCaixaA <<- "L"

  #Tipo da margem da legenda de Ford-Walford
  tipoCaixaAA <<- "n"

  #Tipo da margem do gráfico de Crescimento
  tipoCaixaB <<- "L"

  #Tipo da margem da legenda de Crescimento
  tipoCaixaBB <<- "n"

  #Tipo da margem do gráfico de Levenberg
  tipoCaixaC <<- "L"

  #Tipo da margem da legenda de Levenberg
  tipoCaixaCC <<- "n"

  #Tipo da margem do gráfico de IC
  tipoCaixaD <<- "L"

  #Tipo da margem da legenda de IC
  tipoCaixaDD <<- "n"

  #Tipo da margem do gráfico de MORTALIDADE
  tipoCaixaE <<- "L"

  #Tipo da margem da legenda de MORTALIDADE
  tipoCaixaEE <<- "n"

  if(is.null(Fonte)){
    letra <<- "Times New Roman"
  }else if(Fonte == 1){
    letra <<- "Arial"
  }else if(Fonte == 2){
    letra <<- "Courier New"
  }
####################################### CLASSIFICAÇÃO DAS LEGENDAS

  #idioma português brasileiro
  if(idioma == 1){

    #definindo a label de y
    if(un == 1){
      medida <- paste("Comprimento", tipoComprimento,"(cm)")
    }else if(un == 2){
      medida <- paste("Comprimento", tipoComprimento,"(mm)")
    }else if(un == 3){
      medida <- paste("Comprimento", tipoComprimento,"(m)")
    }else if(is.null(un)){
      medida <- paste("Comprimento", tipoComprimento,"(cm)")
    }

    if(!is.null(grupo)){
      if(grupo==1){
        if(is.null(nomeA)){
            label2 = "A"
        }else{
            label2 = toupper(nomeA)
        }
        if(is.null(nomeB)){
            label1 = "B"
        }else{
            label1 = toupper(nomeB)
        }
      }
    }

    if(!is.null(grupo)){
      if(grupo == 1){
        #grupo
        grupoA <- paste("do grupo",label2)
        grupoB <- paste("do grupo",label1)
      }
    }

    if(!is.null(grupo)){
      #grupo A
      mainNameAA <- paste("Curva de Crescimento (Ford-Walford)\nGrupo:",label2)
      mainNameBA <- paste("Curva de Crescimento (Levenberg-Marquardt) \nGrupo:",label2)
      mainNameCA <- paste("(Média C",substr(tipoComprimento, 1, 1),"(X), Média C",substr(tipoComprimento, 1, 1),"+1(Y))\nEstimativa do Comprimento Assintótico \nGrupo:",label2)
      mainNameDA <- paste("(Idade(X), LN(Cinf - Média C",substr(tipoComprimento, 1, 1),")(Y))\n Estimativa da Taxa de Crescimento \nGrupo:",label2)
      mainNameEA <- paste("Grupo:",label2)
      #grupo B
      mainNameAB <- paste("Curva de Crescimento (Ford-Walford)\nGrupo:",label1)
      mainNameBB <- paste("Curva de Crescimento (Levenberg-Marquardt) \nGrupo:",label1)
      mainNameCB <- paste("(Média C",substr(tipoComprimento, 1, 1),"(X), Média C",substr(tipoComprimento, 1, 1),"+1(Y))\n Estimativa do Comprimento Assintótico \nGrupo:",label1)
      mainNameDB <- paste("(Idade(X), LN(Cinf - Média C",substr(tipoComprimento, 1, 1),")(Y))\n Estimativa da Taxa de Crescimento \nGrupo:",label1)
      mainNameEB <- paste("Grupo:",label1)
    }else{
      mainNameA <- "Curva de Crescimento (Ford-Walford)"
      mainNameB <- "Curva de Crescimento (Levenberg-Marquardt)"
      mainNameC <- paste("(Média C",substr(tipoComprimento, 1, 1),"(X), Média C",substr(tipoComprimento, 1, 1),"+1(Y))\nEstimativa do Comprimento Assintótico")
      mainNameD <- paste("(Idade(X), LN(Cinf - Média C",substr(tipoComprimento, 1, 1),")(Y))\n Estimativa da Taxa de crescimento")
      mainNameE <- paste("")
    }

    modeloA <<- "Bertalanffy"
    modeloB <<- "Gompertz"
    modeloC <<- "Logística"
    modeloD <<- "IC"

    modeloAA <<- "(Ford-Walford)"
    modeloBB <<- "(Levenberg-Marquardt Bertalanffy)"
    modeloCC <<- "(Levenberg-Marquardt Gompertz)"
    modeloDD <<- "(Levenberg-Marquardt Logística)"

    labelCX <- paste("Média do Comprimento",tipoComprimento,"(X)")
    labelCY <- paste("Média do Comprimento",tipoComprimento,"+1(Y)")
    labelLX <- "Idade(X)"
    labelLY <- paste("ln(Cinf - Média do Comprimento",tipoComprimento,")(Y)")
    labelEY <- paste("Frequência")
    labelEX <- paste("Classes de Comprimento")

    #definindo a label de x
    if(tempo == 1){
      tempoB <- "Idade (ano)"
    }else if(tempo == 2){
      tempoB <- "Idade (mês)"
    }else if(tempo == 3){
      tempoB <- "Idade (dia)"
    }else if(is.null(tempo)){
      tempoB <- "Idade (ano)"
    }
  }else if(idioma == 2){
    #idioma em inglês

    #definindo a label de y
    if(un == 1){
      medida <- paste(tipoComprimento,"Length (cm)")
    }else if(un == 2){
      medida <- paste(tipoComprimento,"Length (mm)")
    }else if(un == 3){
      medida <- paste(tipoComprimento,"Length (m)")
    }else if(is.null(un)){
      medida <- paste(tipoComprimento,"Length (cm)")
    }

    #definindo a label de x
    if(tempo == 1){
      tempoB <- "Age (year)"
    }else if(tempo == 2){
      tempoB <- "Age (month)"
    }else if(tempo == 3){
      tempoB <- "Age (day)"
    }else if(is.null(tempo)){
      tempoB <- "Age (year)"
    }

    if(!is.null(grupo)){
      if(grupo==1){
        if(is.null(nomeA)){
          label2 = "A"
        }else{
          label2 = toupper(nomeA)
        }
        if(is.null(nomeB)){
          label1 = "B"
        }else{
          label1 = toupper(nomeB)
        }
      }
    }

    if(!is.null(grupo)){
      if(grupo == 1){
        #grupo
        grupoA <- paste("of group",label2)
        grupoB <- paste("of group",label1)
      }
    }

    if(!is.null(grupo)){
      #grupo A
      mainNameAA <- paste("Growth Curve \nGroup:",label2)
      mainNameBA <- paste("Growth Curve \n(Levenberg-Marquardt) \nGroup:",label2)
      mainNameCA <- paste("Linear Regression \n(Average ",substr(tipoComprimento, 1, 1),"L(X), Average ",substr(tipoComprimento, 1, 1),"L+1(Y)) \nGroup:",label2)
      mainNameDA <- paste("Linear Regression \n(Age(X), LN(Linf - Average ",substr(tipoComprimento, 1, 1),"L)(Y)) \nGroup:",label2)
      mainNameEA <- paste("Group:",label2)
      #grupo B
      mainNameAB <- paste("Growth Curve \nGroup:",label1)
      mainNameBB <- paste("Growth Curve \n(Levenberg-Marquardt) \nGroup:",label1)
      mainNameCB <- paste("Linear Regression \n(Average ",substr(tipoComprimento, 1, 1),"L(X), Average ",substr(tipoComprimento, 1, 1),"L+1(Y)) \nGroup:",label1)
      mainNameDB <- paste("Linear Regression \n(Age(X), LN(Linf - Average ",substr(tipoComprimento, 1, 1),"L)(Y)) \nGroup:",label1)
      mainNameEB <- paste("Group:",label1)
    }else{
      mainNameA <- "Growth Curve"
      mainNameB <- "Growth Curve \n(Levenberg-Marquardt)"
      mainNameC <- paste("Linear Regression \n(Average ",substr(tipoComprimento, 1, 1),"L(X), Average ",substr(tipoComprimento, 1, 1),"L+1(Y))")
      mainNameD <- paste("Linear Regression \n(Age(X), LN(Linf - Average ",substr(tipoComprimento, 1, 1),"L)(Y))")
      mainNameE <- paste("")
    }

    modeloA <<- "Bertalanffy"
    modeloB <<- "Gompertz"
    modeloC <<- "Logistic"
    modeloD <<- "CI"

    modeloAA <<- "(Ford-Walford)"
    modeloBB <<- "(Levenberg-Marquardt Bertalanffy)"
    modeloCC <<- "(Levenberg-Marquardt Gompertz)"
    modeloDD <<- "(Levenberg-Marquardt Logistic)"

    labelCX <- paste("Average",tipoComprimento,"Length(X)")
    labelCY <- paste("Average",tipoComprimento,"Length+1(Y)")
    labelLX <- "Age(X)"
    labelLY <- paste("ln(Linf - Average",tipoComprimento,"Lenght)(Y)")
    labelEY <- paste("Frequency")
    labelEX <- paste("Length Classes")
  }

################################ CRIAÇÃO DO DATAFRAME DOS DADOS PARA ANÁLISE

  if(tipo_dados == 1){
    dado <- "xlsx"
  }else if(tipo_dados == 2){
    dado <- "csv"
  }else if(tipo_dados == 3){
    dado <- "txt"
  }else if(tipo_dados == 4){
    dado <- "dataframe"
  }else if(is.null(tipo_dados)){
    dado <- "dataframe"
  }

  meus_dados <<- data.frame()
  # if(is.null(grupo)){
  #   dados <<- data.frame(idade=c(0),ct=c(0))
  # }else{
  #   dadosGrupoA <<- data.frame(idade=c(0),ct=c(0),grupos=c(0))
  #   dadosGrupoB <<- data.frame(idade=c(0),ct=c(0),grupos=c(0))
  # }

  if(dado == "xlsx"){
    #cria dataframe a partir dos dados da planilha
    if(is.null(planilha)){
      meus_dados <<- read_excel(nome_dados)
    }else{
      meus_dados <<- read_excel(nome_dados, sheet = planilha)
    }
  }else if(dado == "dataframe"){
    meus_dados <<- nome_dados
  }else if(dado == "csv"){
    if(is.null(separador)){
      meus_dados <<- read.csv(file = nome_dados, header = TRUE)
    }else{
      meus_dados <<- read.csv(file = nome_dados, header = TRUE, sep = separador)
    }
  }else if(dado == "txt"){
    if(is.null(separador)){
      if(idioma == 1){
        erro <<- "Necessário informar separador de arquivos em txt."
        erro
      }else{
        erro <<- "Requires txt file separator."
        erro
      }
    }else{
      meus_dados <<- read.delim2(nome_dados, sep=separador)
    }
  }

  if(is.null(grupo)){

    #Nomeia base de dados principal para filtragem
    names(meus_dados) <<- c("idade","ct")
    
    #atribui dados sem grupo de sexo da planilha ao dataframe dados
    dados <<- data.frame(idade = meus_dados[,1], ct = meus_dados[,2])
    #names(dados) <<- c("idade","ct")


  }else if(grupo == 1){

    #Nomeia base de dados principal para filtragem
    names(meus_dados) <<- c("idade","ct","grupos")

    #atribui dados do grupo A da planilha ao dataframe dados
    dadobrutoA <<- meus_dados %>% group_by(idade)%>% filter (toupper(grupos) == toupper(label2))
    dadosGrupoA <<- data.frame(idade = dadobrutoA[,1], ct = dadobrutoA[,2], grupos = dadobrutoA[,3])
    #names(dadosGrupoA) <<- c("idade","ct","grupos")

    #atribui dados do grupo B da planilha ao dataframe dados
    dadoBrutoB <<- meus_dados %>% group_by(idade)%>% filter (toupper(grupos) == toupper(label1))
    dadosGrupoB <<- data.frame(idade = dadoBrutoB[,1], ct = dadoBrutoB[,2], grupos = dadoBrutoB[,3])
    #names(dadosGrupoB) <<- c("idade","ct","grupos")

  }


  ################################### DEPURAÇÃO DE dados discrepates PELA ESTATÍSTICA

  #pergunta se quer remover dados discrepantes pela análise estatística
  if(idioma == 1){
    cat("\nDeseja remover dados discrepantes através da depuração estatística? S/N\n")
  }else if(idioma == 2){
    cat("\nDo you want to remove discrepant data through statistical debugging? Y/N\n")
  }

  respostaDepuracaoOutliers <<- toupper(readLines(n=1))
  if(respostaDepuracaoOutliers == ""){
    respostaDepuracaoOutliers <<- "N"
  }

  if(is.null(grupo)){

    if(toupper(respostaDepuracaoOutliers) == "S"||toupper(respostaDepuracaoOutliers) == "Y"){

      #remove outliers
      dados <<- removeOutliers(dados)

      #cria base de dados sem outliers
      meus_dados2 <<- dados

      #remove variável repetida
      rm(dadosNovo, envir = .GlobalEnv)

      #captura comprimento geral
      limY <<- max(meus_dados2$ct)
      mlimY <<- 0

      #captura idade geral
      limX <<- as.vector(meus_dados2$idade)

    }else if(toupper(respostaDepuracaoOutliers) == "N"){
      #nada acontece
      meus_dados2 <<- dados

      #captura comprimento geral
      limY <<- max(meus_dados2$ct)
      mlimY <<- 0

      #captura idade geral
      limX <<- as.vector(meus_dados2$idade)
    }

  }else if(grupo == 1){

    if(toupper(respostaDepuracaoOutliers) == "S"||toupper(respostaDepuracaoOutliers) == "Y"){

      #remove outliers do primeiro grupo
      dadosGrupoA <<- removeOutliers(dadosGrupoA,1)

      #recebe outliers
      outliers_A <<- outliers

      #remove variável repetida
      rm(dadosNovo, envir = .GlobalEnv)

      #remove variável repetida
      rm(outliers, envir = .GlobalEnv)

      #remove outliers do segundo grupo
      dadosGrupoB <<- removeOutliers(dadosGrupoB,2)

      #recebe outliers
      outliers_B <<- outliers

      #remove variável repetida
      rm(outliers, envir = .GlobalEnv)

      #remove variável repetida
      rm(dadosNovo, envir = .GlobalEnv)

      #cria base de dados sem outliers
      meus_dados2 <<- rbind(dadosGrupoA, dadosGrupoB)

      #captura comprimento geral
      limY <<- max(meus_dados2$ct)
      mlimY <<- 0

      #captura idade geral
      limX <<- as.vector(meus_dados2$idade)

    }else if(toupper(respostaDepuracaoOutliers) == "N"){
      #nada acontece
      meus_dados2 <<- meus_dados

      #captura comprimento geral
      limY <<- max(meus_dados2$ct)
      mlimY <<- 0

      #captura idade geral
      limX <<- as.vector(meus_dados2$idade)
    }
  }

  ################################### MODELO DE FORD-WALFORD

  if(is.null(grupo)){

    #Sem grupo de sexo
    fordWalford(dados, labelCX, labelCY, labelLX, labelLY, mainNameC, mainNameD)

    #contagem de linhas de ct para curva de crescimento
    contagem_fw <<- count(dados_curva)

    #transforma a contagem em número real
    real_cont_fw <<- as.double(contagem_fw)

    #atribui dados da curva
    dados_curvaS <<- dados_curva

  }else{

      cat("\n[GRUPO: A]\n")

    #Grupo A
    fordWalford(dadosGrupoA, labelCX, labelCY, labelLX, labelLY, mainNameCA, mainNameDA, grupoA)

    #atribui dados da curva para A
    dados_curvaM <<- dados_curva

    #contagem de linhas de ct para curva de crescimento
    contagem_fwA <<- count(dados_curva)

    #transforma a contagem em número real
    real_cont_fwA <<- as.double(sum(contagem_fwA$n))

    #Parâmetros
    c_infinitoM <<- c_infinito
    kM <<- k
    tzeroM <<- tzero

    #Parâmetros dataframe
    fordA <<- ford

    #remove variáveis por segurança
    removedor(1)

      cat("\n[GRUPO: B]\n")

    #grupo B
    fordWalford(dadosGrupoB, labelCX, labelCY, labelLX, labelLY, mainNameCB, mainNameDB, grupoB)

    #atribui dados da curva para B
    dados_curvaF <<- dados_curva

    #contagem de linhas de ct para curva de crescimento
    contagem_fwB <<- count(dados_curva)

    #transforma a contagem em número real
    real_cont_fwB <<- as.double(sum(contagem_fwB$n))

    #Parâmetros
    c_infinitoF <<- c_infinito
    kF <<- k
    tzeroF <<- tzero

    #Parâmetros dataframe
    fordB <<- ford

    #remove variáveis por segurança
    removedor(1)
  }

  if(is.null(erro)){
    #nada acontece
  }else{
    return(erro)
  }
  #################################### CURVA DE CRESCIMENTO SEM AJUSTE

  if(is.null(grupo)){

    #sem grupo
    crescimento(dados_curvaS, c_infinito, k, tzero, real_cont_fw, tempoB, medida, mainNameA)

  }else{

    cat("\n[GRUPO: A]\n")

    crescimento(dados_curvaM, c_infinitoM, kM, tzeroM, real_cont_fwA, tempoB, medida, mainNameAA, grupoA)
    AIC_bertalanffyA <<- AIC_bertalanffy
    AIC_gompertzA <<- AIC_gompertz
    AIC_logisticaA <<- AIC_logistica
    #remove variáveis por segurança
    removedor(2)
    rm(AIC_bertalanffy, envir = .GlobalEnv)
    rm(AIC_gompertz, envir = .GlobalEnv)
    rm(AIC_logistica, envir = .GlobalEnv)
    cat("\n[GRUPO: B]\n")

    crescimento(dados_curvaF, c_infinitoF, kF, tzeroF, real_cont_fwB, tempoB, medida, mainNameAB, grupoB)
    AIC_bertalanffyB <<- AIC_bertalanffy
    AIC_gompertzB <<- AIC_gompertz
    AIC_logisticaB <<- AIC_logistica
    #remove variáveis por segurança
    removedor(2)
    rm(AIC_bertalanffy, envir = .GlobalEnv)
    rm(AIC_gompertz, envir = .GlobalEnv)
    rm(AIC_logistica, envir = .GlobalEnv)
  }

  ######################################## AJUSTE DE MÍNIMO QUADRADO LEVENBERG-MARQUARDT

  if(is.null(grupo)){

    #Sem grupo
    ajusteLevenberg(c_infinito, k, tzero, dados_curva, real_cont_fw, medida, tempoB, mainNameB)

  }else{

    cat("\n[GRUPO: A]\n")

    ajusteLevenberg(c_infinitoM, kM, tzeroM, dados_curvaM, real_cont_fwA, medida, tempoB, mainNameBA,grupoA, "A")

    #Parâmetros Bertalanffy
      cinfktzeroBM <<- cinfktzeroB
      c_infinito_ajustadoBM <<- c_infinito_ajustadoB
      k_ajustadoBM <<- k_ajustadoB
      tzero_ajustadoBM <<- tzero_ajustadoB
    #Parâmetros Gompertz
      cinfktzeroGM <<- cinfktzeroG
      c_infinito_ajustadoGM <<- c_infinito_ajustadoG
      k_ajustadoGM <<- k_ajustadoG
      tzero_ajustadoGM <<- tzero_ajustadoG
    #Parâmetros Logística
      cinfktzeroLM <<- cinfktzeroL
      c_infinito_ajustadoLM <<- c_infinito_ajustadoL
      k_ajustadoLM <<- k_ajustadoL
      tzero_ajustadoLM <<- tzero_ajustadoL

    AIC_bertalanffyLMA <<- AIC_bertalanffyLM
    AIC_gompertzLMA <<- AIC_gompertzLM
    AIC_logisticaLMA <<- AIC_logisticaLM
      removedor(3)
      removedor(4)
    rm(AIC_bertalanffyLM, envir = .GlobalEnv)
    rm(AIC_gompertzLM, envir = .GlobalEnv)
    rm(AIC_logisticaLM, envir = .GlobalEnv)

    cat("\n[GRUPO: B]\n")

    ajusteLevenberg(c_infinitoF, kF, tzeroF, dados_curvaF, real_cont_fwB, medida, tempoB, mainNameBB, grupoB, "B")

    #Parâmetros Bertalanffy
    cinfktzeroBF <<- cinfktzeroB
    c_infinito_ajustadoBF <<- c_infinito_ajustadoB
    k_ajustadoBF <<- k_ajustadoB
    tzero_ajustadoBF <<- tzero_ajustadoB
    #Parâmetros Gompertz
    cinfktzeroGF <<- cinfktzeroG
    c_infinito_ajustadoGF <<- c_infinito_ajustadoG
    k_ajustadoGF <<- k_ajustadoG
    tzero_ajustadoGF <<- tzero_ajustadoG
    #Parâmetros Logística
    cinfktzeroLF <<- cinfktzeroL
    c_infinito_ajustadoLF <<- c_infinito_ajustadoL
    k_ajustadoLF <<- k_ajustadoL
    tzero_ajustadoLF <<- tzero_ajustadoL

    AIC_bertalanffyLMB <<- AIC_bertalanffyLM
    AIC_gompertzLMB <<- AIC_gompertzLM
    AIC_logisticaLMB <<- AIC_logisticaLM
    removedor(3)
    removedor(4)
    rm(AIC_bertalanffyLM, envir = .GlobalEnv)
    rm(AIC_gompertzLM, envir = .GlobalEnv)
    rm(AIC_logisticaLM, envir = .GlobalEnv)
  }

  ####################################### MORTALIDADE ####
  if(idioma == 1){
    cat("\nDeseja calcular a mortalidade total (Z): [Informe a opção]\n(1):Baseado em dados de IDADE\n(2):Baseado em dados de COMPRIMENTO\n(3):Calcular ambos\n")
  }else if(idioma == 2){
    cat("\nDo you wish to calculate the total mortality (Z):[Enter the option]\n(1): Based on AGE data\n(2): Based on LENGTH data\n(3): Calculate both\n")
  }
  respostaMortalidadeCalc <<- scan(n=1)
  #condição de exceção
  if(respostaMortalidadeCalc<1||respostaMortalidadeCalc>3) respostaMortalidadeCalc <<- 3
    
    mortalidadeIdadeford <<- mortalidadeIdadeAford <<- mortalidadeIdadeBford <<- NULL
    mortalidadeIdadeVB <<- mortalidadeIdadeAVB <<- mortalidadeIdadeBVB <<- NULL
    mortalidadeIdadeGP <<- mortalidadeIdadeAGP <<- mortalidadeIdadeBGP <<- NULL
    mortalidadeIdadeLG <<- mortalidadeIdadeALG <<- mortalidadeIdadeBLG <<- NULL
     
   if(respostaMortalidadeCalc==1 || respostaMortalidadeCalc==3){
     
      #### MORTALIDADE Z BASEADA EM DADOS DE IDADE ####
      if(idioma == 1){
       cat("\nDeseja selecionar os parâmetros gerados por um modelo específico\n para calcular a mortalidade total (Z) baseada em idade? S/N (Padrão: Ford-Walford)\n")
      }else if(idioma == 2){
       cat("\nDo you want to select parameters generated by a specific model\n to calculate age-based total mortality (Z)? Y/N (Default: Ford-Walford)\n")
      }
      respostaMorta <- toupper(readLines(n = 1))
      #condição de exceção
      if(respostaMorta == "") respostaMorta = "N"  
      if(respostaMorta == "S" ||respostaMorta == "Y"){
        
        #### FORD-WALFORD ####
        if(idioma == 1){
          cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo de Ford-Walford? S/N\n")
        }else if(idioma == 2){
          cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Ford-Walford model? Y/N\n")
        }
        respostaFord <- toupper(readLines(n = 1))
        if(respostaFord == "") respostaFord = "N"
        if(respostaFord=="S"||respostaFord=="Y"){  
          if(is.null(grupo)){
            mortalidadeIdadeford <<- mortalidadeZ(c_infinito, k, tzero, dados[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }else{
            mortalidadeIdadeAford <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoA[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
            mortalidadeIdadeBford <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoB[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }
          rm(morte, envir = .GlobalEnv)
        }
        #### BERTALANFFY ####
        if(idioma == 1){
          cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo de Bertalanffy? S/N\n")
        }else if(idioma == 2){
          cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Bertalanffy model? Y/N\n")
        }
        respostaFord <- toupper(readLines(n = 1))
        if(respostaFord == "") respostaFord = "N"
        if(respostaFord=="S"||respostaFord=="Y"){  
          if(is.null(grupo)){
            mortalidadeIdadeVB <<- mortalidadeZ(c_infinito, k, tzero, dados[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }else{
            mortalidadeIdadeAVB <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoA[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
            mortalidadeIdadeBVB <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoB[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }
          rm(morte, envir = .GlobalEnv)
        }
        #### GOMPERTZ ####
        if(idioma == 1){
          cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo de Gompertz? S/N\n")
        }else if(idioma == 2){
          cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Gompertz model? Y/N\n")
        }
        respostaFord <- toupper(readLines(n = 1))
        if(respostaFord == "") respostaFord = "N"
        if(respostaFord=="S"||respostaFord=="Y"){  
          if(is.null(grupo)){
            mortalidadeIdadeGP <<- mortalidadeZ(c_infinito, k, tzero, dados[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }else{
            mortalidadeIdadeAGP <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoA[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
            mortalidadeIdadeBGP <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoB[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }
          rm(morte, envir = .GlobalEnv)
        }
        #### LOGISTICA ####
        if(idioma == 1){
          cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo Logístico? S/N\n")
        }else if(idioma == 2){
          cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Logistic model? Y/N\n")
        }
        respostaFord <- toupper(readLines(n = 1))
        if(respostaFord == "") respostaFord = "N"
        if(respostaFord=="S"||respostaFord=="Y"){  
          if(is.null(grupo)){
            mortalidadeIdadeLG <<- mortalidadeZ(c_infinito, k, tzero, dados[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }else{
            mortalidadeIdadeALG <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoA[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
            mortalidadeIdadeBLG <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoB[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          }
          rm(morte, envir = .GlobalEnv)
        }
     }else{
        #### FORD-WALFORD PADRÃO ####        
       
        if(is.null(grupo)){
          mortalidadeIdadeford <<- mortalidadeZ(c_infinito, k, tzero, dados[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
        }else{
          mortalidadeIdadeAford <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoA[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
          mortalidadeIdadeBford <<- mortalidadeZ(c_infinito, k, tzero, dadosGrupoB[,1], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY, adhoc=2)
        }
        rm(morte, envir = .GlobalEnv)
      }
   }
   if(respostaMortalidadeCalc==2 || respostaMortalidadeCalc==3){ 
 
     #### CURVA DE CAPTURA BASEADA EM COMPRIMENTO ####
     if(idioma == 1){
        cat("\nDeseja selecionar os parâmetros gerados por um modelo específico\n para calcular a mortalidade total (Z) baseada em comprimento? S/N (Padrão: Ford-Walford)\n")
     }else if(idioma == 2){
      cat("\nDo you want to select parameters generated by a specific model\n to calculate length-based total mortality (Z)? Y/N (Default: Ford-Walford)\n")
     }
    respostaMorta <- toupper(readLines(n = 1))
    #condição de exceção
    if(respostaMorta == "") respostaMorta = "N"  
    if(respostaMorta == "S" ||respostaMorta == "Y"){

      #### FORD-WALFORD ####
      if(idioma == 1){
        cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo de Ford-Walford? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Ford-Walford model? Y/N\n")
      }  
      respostaFord <- toupper(readLines(n = 1))
      if(respostaFord == "") respostaFord = "N"
      if(respostaFord=="S"||respostaFord=="Y"){
        #ford
        if(is.null(grupo)){  
          #Sem grupo
          ford <<- mortalidadeZ(c_infinito, k, tzero, dados[,2], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY)         
        }else{
          cat("\n[GRUPO: A]\n")  
          fordA <<- mortalidadeZ(c_infinitoM, kM, tzeroM, dadosGrupoA[,2], real_cont_fwA, idioma, modeloAA, mainNameEA, labelEX, labelEY, grupoA, label2)
          removedor(5)  
          
          cat("\n[GRUPO: B]\n")  
          fordB <<- mortalidadeZ(c_infinitoF, kF, tzeroF, dadosGrupoB[,2], real_cont_fwB, idioma, modeloAA, mainNameEB, labelEX, labelEY, grupoB, label1)
          removedor(5)
        }
      }
      #### BERTALANFFY ####
      if(idioma == 1){
        cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo de Bertalanffy? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Bertalanffy model? Y/N\n")
      }  
      respostaBert <- toupper(readLines(n = 1))
      #condicao de excecao
      if(respostaBert == "") respostaBert = "N"
      if(respostaBert=="S"||respostaBert=="Y"){
        #levenberg/Bertalanffy
        if(is.null(grupo)){
          #Sem grupo
          cinfktzeroB <<- mortalidadeZ(c_infinito_ajustadoB, k_ajustadoB, tzero_ajustadoB, dados[,2], real_cont_fw, idioma, modeloBB, mainNameE, labelEX, labelEY)
   
        }else{
          #grupo A
          cat("\n[GRUPO: A]\n")
          cinfktzeroBM <<- mortalidadeZ(c_infinito_ajustadoBM, k_ajustadoBM, tzero_ajustadoBM, dadosGrupoA[,2], real_cont_fwA, idioma, modeloBB, mainNameEA, labelEX, labelEY, grupoA, label2)
          removedor(5)
          
          #grupo B
          cat("\n[GRUPO: B]\n")
          cinfktzeroBF <<- mortalidadeZ(c_infinito_ajustadoBF, k_ajustadoBF, tzero_ajustadoBF, dadosGrupoB[,2], real_cont_fwB, idioma, modeloBB, mainNameEB, labelEX, labelEY, grupoB, label1)
          removedor(5)
        }
      }

      #### GOMPERTZ ####
      if(idioma == 1){
        cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo de Gompertz? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Gompertz model? Y/N\n")
      }  
      respostaGomp <- toupper(readLines(n = 1))
      #condição de exceção
      if(respostaGomp == "") respostaGomp = "N"
      if(respostaGomp=="S"||respostaGomp=="Y"){
        #levenberg/Gompertz
        if(is.null(grupo)){
          #Sem grupo
          cinfktzeroG <<- mortalidadeZ(c_infinito_ajustadoG, k_ajustadoG, tzero_ajustadoG, dados, real_cont_fw, idioma, modeloCC, mainNameE, labelEX, labelEY)

        }else{
          #grupo A
          cat("\n[GRUPO: A]\n")
          cinfktzeroGM <<- mortalidadeZ(c_infinito_ajustadoGM, k_ajustadoGM, tzero_ajustadoGM, dadosGrupoA, real_cont_fwA, idioma, modeloCC, mainNameEA, labelEX, labelEY, grupoA, label2)
          removedor(5)
          
          #grupo B
          cat("\n[GRUPO: B]\n")
          cinfktzeroGF <<- mortalidadeZ(c_infinito_ajustadoGF, k_ajustadoGF, tzero_ajustadoGF, dadosGrupoB, real_cont_fwB, idioma, modeloCC, mainNameEB, labelEX, labelEY, grupoB, label1)
          removedor(5)
        }
      }

      #### LOGISTICA ####
      if(idioma == 1){
        cat("\nDeseja calcular a mortalidade total (Z) usando os parâmetros\ngerados pelo modelo Logístico? S/N\n")
      }else if(idioma == 2){
        cat("\nDo you want to calculate the total mortality (Z) using the parameters\ngenerated by the Logistic model? Y/N\n")
      }  
      respostaLogi <- toupper(readLines(n = 1))
      #condição de exceção
      if(respostaLogi == "") respostaLogi = "N"
      if(respostaLogi=="S"||respostaLogi=="Y"){
        #levenberg/Logística
        if(is.null(grupo)){
          #Sem grupo
          cinfktzeroL <<- mortalidadeZ(c_infinito_ajustadoL, k_ajustadoL, tzero_ajustadoL, dados, real_cont_fw, idioma, modeloDD, mainNameE, labelEX, labelEY)

        }else{
          #grupo A
          cat("\n[GRUPO: A]\n")
          cinfktzeroLM <<- mortalidadeZ(c_infinito_ajustadoLM, k_ajustadoLM, tzero_ajustadoLM, dadosGrupoA, real_cont_fwA, idioma, modeloDD, mainNameEA, labelEX, labelEY, grupoA, label2)
          removedor(5)
          
          #grupo B
          cat("\n[GRUPO: B]\n")
          cinfktzeroLF <<- mortalidadeZ(c_infinito_ajustadoLF, k_ajustadoLF, tzero_ajustadoLF, dadosGrupoB, real_cont_fwB, idioma, modeloDD, mainNameEB, labelEX, labelEY, grupoB, label1)
          removedor(5)
        }
      }
    }else if(respostaMorta == "N"){

      #### ESCOLHA PADRÃO ####
      if(is.null(grupo)){
        #Sem grupo
        ford <<- mortalidadeZ(c_infinito, k, tzero, dados[,2], real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY)
  
      }else{
        #grupo A
        cat("\n[GRUPO: A]\n")
        fordA <<- mortalidadeZ(c_infinitoM, kM, tzeroM, dadosGrupoA[,2], real_cont_fwA, idioma, modeloAA, mainNameEA, labelEX, labelEY, grupoA, label2)
        removedor(5)
        
        #grupo B
        cat("\n[GRUPO: B]\n")
        fordB <<- mortalidadeZ(c_infinitoF, kF, tzeroF, dadosGrupoB[,2], real_cont_fwB, idioma, modeloAA, mainNameEB, labelEX, labelEY, grupoB, label1)
        removedor(5)
      }
    }
  }
     
  ######################################### TESTE DE VEROSSIMILHANÇA KIMURA (1980) ####
  if(!is.null(grupo)){
    if(idioma == 1){
      cat("\nA partir dos parâmetros de Ford-Walford, \ndeseja realizar o teste de verossimilhança de Kimura(1980)\ndisponibilizado pelo pacote fishmethods? S/N\n")
    }else if(idioma == 2){
      cat("\nBased on the Ford-Walford parameters, \ndo you want to perform the Kimura(1980) likelihood test\nprovided by the fishmethods package? Y/N\n")
    }

  respostaVero <- readLines(n = 1)
  #condição de exceção
  if(respostaVero == ""){
    respostaVero = "N"
  }
    if(toupper(respostaVero)=="S"||toupper(respostaVero)=="Y"){
      respostaTeste <- NULL
      if(idioma == 1){
        cat("\nDeseja testar qual dos seguintes modelos, \ndigite o número referente a ele: \n(1) Von Bertalanffy\n(2) Gompertz\n(3) Logistica\n(4) Todos\n")
      }else if(idioma == 2){
        cat("\nWould you like to test which of the following models, \nenter its number: \n(1) Von Bertalanffy\n(2) Gompertz\n(3) Logistics\n(4) All\n")
      }

      respostaTeste <- scan(n = 1)
      #condição de exceção
      if(is.null(respostaTeste)||length(respostaTeste)==0){
        respostaTeste = 4
      }
      Bertalanffy <<- NULL
      Gompertz <<- NULL
      Logistica <<- NULL
      if(respostaTeste == 1 || respostaTeste == 4){
        Bertalanffy <<- growthlrt(meus_dados2$ct, meus_dados2$idade, meus_dados2$grupos, model = 1, error = 2,
                select = 2, Linf = c(c_infinitoM,c_infinitoF), K = c(kM,kF), t0 = c(tzeroM,tzeroF),plottype=1,
                control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))
      }
      if(respostaTeste == 2 || respostaTeste == 4){
        Gompertz <<- growthlrt(meus_dados2$ct, meus_dados2$idade, meus_dados2$grupos, model = 2, error = 2,
                  select = 2, Linf = c(c_infinitoM,c_infinitoF), K = c(kM,kF), t0 = c(tzeroM,tzeroF),plottype=1,
                  control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))
      }
      if(respostaTeste == 3 || respostaTeste == 4){
        Logistica <<- growthlrt(meus_dados2$ct, meus_dados2$idade, meus_dados2$grupos, model = 3, error = 2,
                  select = 2, Linf = c(c_infinitoM,c_infinitoF), K = c(kM,kF), t0 = c(tzeroM,tzeroF),plottype=1,
                  control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))
      }
      if(idioma == 1){
        cat("\n[Verossimilhança com os parâmetros de Ford-Walford]\n")
      }else if(idioma == 2){
        cat("\n[Likelihood with the Ford-Walford parameters]\n")
      }

      if(!is.null(Bertalanffy$results)){
        if(idioma == 1){
          cat("\n[Resultado Verossimilhança com modelo de curva do Bertalanffy]\n")
        }else if(idioma == 2){
          cat("\n[Result Likelihood with Bertalanffy curve model]\n")
        }

        print(Bertalanffy$results)
      }
      if(!is.null(Gompertz$results)){
        if(idioma == 1){
          cat("\n[Resultado Verossimilhança com modelo de curva do Gompertz]\n")
        }else if(idioma == 2){
          cat("\n[Result Likelihood with Gompertz curve model]\n")
        }
        print(Gompertz$results)
      }
      if(!is.null(Logistica$results)){
        if(idioma == 1){
          cat("\n[Resultado Verossimilhança com modelo de curva do Logística]\n")
        }else if(idioma == 2){
          cat("\n[Result Likelihood with Logistic curve model]\n")
        }
        print(Logistica$results)
      }

    }
  }

  if(!is.null(grupo)){
    if(idioma == 1){
      cat("\nA partir dos parâmetros com ajuste de Levenverg-Marquardt, \ndeseja realizar o teste de verossimilhança de Kimura(1980)\ndisponibilizado pelo pacote fishmethods? S/N\n")
    }else if(idioma == 2){
      cat("\nBased on the parameters with Levenverg-Marquardt adjustment, \ndo you want to perform the Kimura(1980) likelihood test\nprovided by the fishmethods package? Y/N\n")
    }

  respostaVero <- readLines(n = 1)
  #condição de exceção
  if(respostaVero == ""){
    respostaVero = "N"
  }
  if(toupper(respostaVero)=="S"||toupper(respostaVero)=="Y"){
    if(idioma == 1){
      cat("\nDeseja testar qual dos seguintes modelos, \ndigite o número referente a ele: \n(1) Von Bertalanffy\n(2) Gompertz\n(3) Logistica\n(4) Todos\n")
    }else if(idioma == 2){
      cat("\nWould you like to test which of the following models, \nenter its number: \n(1) Von Bertalanffy\n(2) Gompertz\n(3) Logistics\n(4) All\n")
    }
    respostaTeste2 <- scan(n = 1)
    #condição de exceção
    if(is.null(respostaTeste2)||length(respostaTeste2)==0){
      respostaTeste2 = 4
    }
    BertalanffyAjustado <<- NULL
    GompertzAjustado <<- NULL
    LogisticaAjustado <<- NULL
    if(idioma == 1){
      cat("\n[Verossimilhança com os parâmetros de Levenberg-Marquardt]\n")
    }else if(idioma == 2){
      cat("\n[Likelihood with Levenberg-Marquardt parameters]\n")
    }

    if(respostaTeste2 == 1 || respostaTeste2 == 4){
      BertalanffyAjustado <<- growthlrt(meus_dados2$ct, meus_dados2$idade, meus_dados2$grupos, model = 1, error = 2,
                select = 2, Linf = c(c_infinito_ajustadoBM,c_infinito_ajustadoBF), K = c(k_ajustadoBM,k_ajustadoBF), t0 = c(tzero_ajustadoBM,tzero_ajustadoBF),plottype=1,
                control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))
    }
    if(respostaTeste2 == 2 || respostaTeste2 == 4){
      GompertzAjustado <<- growthlrt(meus_dados2$ct, meus_dados2$idade, meus_dados2$grupos, model = 2, error = 2,
                select = 2, Linf = c(c_infinito_ajustadoGM,c_infinito_ajustadoGF), K = c(k_ajustadoGM,k_ajustadoGF), t0 = c(tzero_ajustadoGM,tzero_ajustadoGF),plottype=1,
                control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))
    }
    if(respostaTeste2 == 3 || respostaTeste2 == 4){
      LogisticaAjustado <<- growthlrt(meus_dados2$ct, meus_dados2$idade, meus_dados2$grupos, model = 3, error = 2,
                select = 2, Linf = c(c_infinito_ajustadoLM,c_infinito_ajustadoLF), K = c(k_ajustadoLM,k_ajustadoLF), t0 = c(tzero_ajustadoLM,tzero_ajustadoLF),plottype=1,
                control=list(maxiter=10000,minFactor=1/1024,tol=1e-5))
    }

    if(!is.null(BertalanffyAjustado$results)){
      if(idioma == 1){
        cat("\n[Resultado Verossimilhança com modelo de curva do Bertalanffy]\n")
      }else if(idioma == 2){
        cat("\n[Result Likelihood with Bertalanffy curve model]\n")
      }

      print(BertalanffyAjustado$results)
    }
    if(!is.null(GompertzAjustado$results)){
      if(idioma == 1){
        cat("\n[Resultado Verossimilhança com modelo de curva do Gompertz]\n")
      }else if(idioma == 2){
        cat("\n[Result Likelihood with Gompertz curve model]\n")
      }
      print(GompertzAjustado$results)
    }
    if(!is.null(LogisticaAjustado$results)){
      if(idioma == 1){
        cat("\n[Resultado Verossimilhança com modelo de curva do Logística]\n")
      }else if(idioma == 2){
        cat("\n[Result Likelihood with Logistic curve model]\n")
      }
      print(LogisticaAjustado$results)
    }
  }
}
############################## RESULTADOS E FIM DE ALGORITMO ####
  #### AIC ####  
  if(is.null(grupo)){
    if((length(meus_dados2$idade)/3)>=40){
      aic_values <- c(Bertalanffy = AIC_bertalanffy, 
                      Gompertz = AIC_gompertz, 
                      Logistica = AIC_logistica,
                      Bertalanffy_Fitted = AIC_bertalanffyLM, 
                      Gompertz_Fitted = AIC_gompertzLM, 
                      Logistica_Fitted = AIC_logisticaLM)
      akaike <- "AIC"
    }else{
      #AICc
      aic_values <- c(Bertalanffy = AIC_bertalanffy + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                      Gompertz = AIC_gompertz + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                      Logistica = AIC_logistica + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),                      
                      Bertalanffy_Fitted = AIC_bertalanffyLM + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                      Gompertz_Fitted = AIC_gompertzLM + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                      Logistica_Fitted = AIC_logisticaLM + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1))
      akaike <- "AICc"
    }
    # ordenar
    aic_values <<- round(sort(aic_values),2)
    
    # Calcular o menor AIC
    min_aic <- min(aic_values)
    
    # Calcular os ΔAIC para cada modelo
    delta_aic <- round(aic_values - min_aic,2)
    
    # Calcular os weights de AIC
    aic_weights <- round(exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic)),2)
    
    # Exibir os resultados em um data frame
    results <<- data.frame(Values = aic_values,Delta_AIC = delta_aic,AIC_Weight = aic_weights)    
    removedor(31)
  }else{
    if((length(meus_dados2$idade)/3)>=40){
      aic_valuesA <<- c(Bertalanffy_A = AIC_bertalanffyA,Gompertz_A  = AIC_gompertzA,Logistica_A  = AIC_logisticaA,
                       Bertalanffy_Fitted_A = AIC_bertalanffyLMA,Gompertz_Fitted_A = AIC_gompertzLMA,Logistica_Fitted_A = AIC_logisticaLMA)
                       
      aic_valuesB <<- c(Bertalanffy_B = AIC_bertalanffyB,Gompertz_B = AIC_gompertzB,Logistica_B = AIC_logisticaB,                      
                      Bertalanffy_Fitted_B = AIC_bertalanffyLMB,Gompertz_Fitted_B = AIC_gompertzLMB,Logistica_Fitted_B = AIC_logisticaLMB)
      akaike <- "AIC"
    }else{
      #AICc
      aic_valuesA <<- c(Bertalanffy_A = AIC_bertalanffyA + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Gompertz_A = AIC_gompertzA + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Logistica_A = AIC_logisticaA + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),   
                        Bertalanffy_Fitted_A = AIC_bertalanffyLMA + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Gompertz_Fitted_A = AIC_gompertzLMA + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Logistica_Fitted_A = AIC_logisticaLMA + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1))
      
      aic_valuesB <<- c(Bertalanffy_B = AIC_bertalanffyB + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Gompertz_B = AIC_gompertzB + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Logistica_B = AIC_logisticaB + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),  
                        Bertalanffy_Fitted_B = AIC_bertalanffyLMB + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Gompertz_Fitted_B = AIC_gompertzLMB + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1),
                        Logistica_Fitted_B = AIC_logisticaLMB + (2 * 3 * (3 + 1)) / (length(meus_dados2$idade) - 3 - 1))
      akaike <- "AICc"
    }
    # ordenar
    aic_valuesA <<- round(sort(aic_valuesA),2)
    aic_valuesB <<- round(sort(aic_valuesB),2)
    
    # Calcular o menor AIC
    min_aicA <- min(aic_valuesA)
    min_aicB <- min(aic_valuesB)
    
    # Calcular os ΔAIC para cada modelo
    delta_aicA <- round(aic_valuesA - min_aicA,2)
    delta_aicB <- round(aic_valuesB - min_aicB,2)
    
    # Calcular os weights de AIC
    aic_weightsA <- round(exp(-0.5 * delta_aicA) / sum(exp(-0.5 * delta_aicA)),2)
    aic_weightsB <- round(exp(-0.5 * delta_aicB) / sum(exp(-0.5 * delta_aicB)),2)
    
    # Exibir os resultados em um data frame
    resultsA <<- data.frame(Values = aic_valuesA,Delta_AIC = delta_aicA, AIC_Weight = aic_weightsA)    
    resultsB <<- data.frame(Values = aic_valuesB,Delta_AIC = delta_aicB,AIC_Weight = aic_weightsB)    
    removedor(32)
  }
    
  if(idioma==1){
    cat("\n\n############################## Resumo dos resultados #############################\n\n")
    cat("\nTeste",akaike,":\n")
    if(is.null(grupo)){
      print(results)
    }else{
      cat("\nGrupo A\n")
      print(resultsA)
      cat("\nGrupo B\n")
      print(resultsB)
    }
    FIMalgoritmo <<- "############################## Fim do código ##############################"
  }else if(idioma==2){
    cat("\n\n############################## Summary of the results #############################\n\n")
    cat("\n",akaike,"Test:\n")
    if(is.null(grupo)){
      print(results)
    }else{
      cat("\nGroup A\n")
      print(resultsA)
      cat("\nGroup B\n")
      print(resultsB)
    }
    FIMalgoritmo <<- "############################## End of code ##############################"
  }
  if(is.null(grupo)){
    ford  <<- round(ford,2)
    cinfktzeroB <<- round(cinfktzeroB,2)
    cinfktzeroG <<- round(cinfktzeroG,2)
    cinfktzeroL <<- round(cinfktzeroL,2)
  }else{
    fordA <<- round(fordA,2)
    fordB <<- round(fordB,2)
    cinfktzeroBM <<- round(cinfktzeroBM,2)
    cinfktzeroGM <<- round(cinfktzeroGM,2)
    cinfktzeroLM <<- round(cinfktzeroLM,2)
    cinfktzeroBF <<- round(cinfktzeroBF,2)
    cinfktzeroGF <<- round(cinfktzeroGF,2)
    cinfktzeroLF <<- round(cinfktzeroLF,2)
  }
  if(is.null(grupo)){
    #Sem grupo
    removedor(7)
    if(idioma == 1){
      
      #### FORD ####
        cat("\nFord-Walford\nCinf:",ford[[1]],"k:",ford[[2]],"t0:",ford[[3]],"\n")
        if(length(mortalidadeIdadeford)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeford$Z,"ϕ:",mortalidadeIdadeford$ϕ,"M:",mortalidadeIdadeford$M,"F:",mortalidadeIdadeford$F,"E:",mortalidadeIdadeford$E,"Fopt:",mortalidadeIdadeford$Fopt,"Flimit:",mortalidadeIdadeford$Flimit,"\n")
        }
        if(length(ford)>5){
          cat("\nZ baseado em comprimento:",ford$Z,"ϕ:",ford$ϕ,"M:",ford$M,"F:",ford$F,"E:",ford$E,"Fopt:",ford$Fopt,"Flimit:",ford$Flimit,"\n")
        }
        
        #### BERTALANFFY ####
        cat("\nParâmetros Ajustados - Equação de Bertalanffy\nCinf:",cinfktzeroB[[1]],"k:",cinfktzeroB[[2]],"t0:",cinfktzeroB[[3]],"\n")
        if(length(mortalidadeIdadeVB)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeVB$Z,"ϕ:",mortalidadeIdadeVB$ϕ,"M:",mortalidadeIdadeVB$M,"F:",mortalidadeIdadeVB$F,"E:",mortalidadeIdadeVB$E,"Fopt:",mortalidadeIdadeVB$Fopt,"Flimit:",mortalidadeIdadeVB$Flimit,"\n")
        }
        if(length(cinfktzeroB)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroB$Z,"ϕ:",cinfktzeroB$ϕ,"M:",cinfktzeroB$M,"F:",cinfktzeroB$F,"E:",cinfktzeroB$E,"Fopt:",cinfktzeroB$Fopt,"Flimit:",cinfktzeroB$Flimit,"\n")
        }
      
        #### GOMPERTZ ####
        cat("\nParâmetros Ajustados - Equação de Gompertz\nCinf:",cinfktzeroG[[1]],"k:",cinfktzeroG[[2]],"t0:",cinfktzeroG[[3]],"\n")
        if(length(mortalidadeIdadeGP)>5){
            cat("\nZ baseado em idade:",mortalidadeIdadeGP$Z,"ϕ:",mortalidadeIdadeGP$ϕ,"M:",mortalidadeIdadeGP$M,"F:",mortalidadeIdadeGP$F,"E:",mortalidadeIdadeGP$E,"Fopt:",mortalidadeIdadeGP$Fopt,"Flimit:",mortalidadeIdadeGP$Flimit,"\n")
        }
        if(length(cinfktzeroG)>5){
        cat("\nZ baseado em comprimento:",cinfktzeroG$Z,"ϕ:",cinfktzeroG$ϕ,"M:",cinfktzeroG$M,"F:",cinfktzeroG$F,"E:",cinfktzeroG$E,"Fopt:",cinfktzeroG$Fopt,"Flimit:",cinfktzeroG$Flimit,"\n")
        }
        
        #### LOGISTICA ####
        cat("\nParâmetros Ajustados - Equação de Logística\nCinf:",cinfktzeroL[[1]],"k:",cinfktzeroL[[2]],"t0:",cinfktzeroL[[3]],"\n")
        if(length(mortalidadeIdadeLG)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeLG$Z,"ϕ:",mortalidadeIdadeLG$ϕ,"M:",mortalidadeIdadeLG$M,"F:",mortalidadeIdadeLG$F,"E:",mortalidadeIdadeLG$E,"Fopt:",mortalidadeIdadeLG$Fopt,"Flimit:",mortalidadeIdadeLG$Flimit,"\n")
        }
        if(length(cinfktzeroL)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroL$Z,"ϕ:",cinfktzeroL$ϕ,"M:",cinfktzeroL$M,"F:",cinfktzeroL$F,"E:",cinfktzeroL$E,"Fopt:",cinfktzeroL$Fopt,"Flimit:",cinfktzeroL$Flimit,"\n")
        }
      cat("\n\n")
      return(FIMalgoritmo)
      
    }else if(idioma == 2){
     
      #### FORD ####
        cat("\nFord-Walford\nLinf:",ford[[1]],"k:",ford[[2]],"t0:",ford[[3]],"\n")
        if(length(mortalidadeIdadeford)>5){
          cat("\nZ Age-based:",mortalidadeIdadeford$Z,"ϕ:",mortalidadeIdadeford$ϕ,"M:",mortalidadeIdadeford$M,"F:",mortalidadeIdadeford$F,"E:",mortalidadeIdadeford$E,"Fopt:",mortalidadeIdadeford$Fopt,"Flimit:",mortalidadeIdadeford$Flimit,"\n")
        }
        if(length(ford)>5){
          cat("\nZ Length-based:",ford$Z,"ϕ:",ford$ϕ,"M:",ford$M,"F:",ford$F,"E:",ford$E,"Fopt:",ford$Fopt,"Flimit:",ford$Flimit,"\n")
        }
        
        #### BERTALANFFY ####
        cat("\nFitted Parameters - Bertalanffy Equation\nLinf:",cinfktzeroB[[1]],"k:",cinfktzeroB[[2]],"t0:",cinfktzeroB[[3]],"\n")
        if(length(mortalidadeIdadeVB)>5){
          cat("\nZ Age-based:",mortalidadeIdadeVB$Z,"ϕ:",mortalidadeIdadeVB$ϕ,"M:",mortalidadeIdadeVB$M,"F:",mortalidadeIdadeVB$F,"E:",mortalidadeIdadeVB$E,"Fopt:",mortalidadeIdadeVB$Fopt,"Flimit:",mortalidadeIdadeVB$Flimit,"\n")
        }
        if(length(cinfktzeroB)>5){
          cat("\nZ Length-based:",cinfktzeroB$Z,"ϕ:",cinfktzeroB$ϕ,"M:",cinfktzeroB$M,"F:",cinfktzeroB$F,"E:",cinfktzeroB$E,"Fopt:",cinfktzeroB$Fopt,"Flimit:",cinfktzeroB$Flimit,"\n")
        }
      
        #### GOMPERTZ ####
        cat("\nFitted Parameters - Gompertz Equation\nLinf:",cinfktzeroG[[1]],"k:",cinfktzeroG[[2]],"t0:",cinfktzeroG[[3]],"\n")
        if(length(mortalidadeIdadeGP)>5){
            cat("\nZ Age-based:",mortalidadeIdadeGP$Z,"ϕ:",mortalidadeIdadeGP$ϕ,"M:",mortalidadeIdadeGP$M,"F:",mortalidadeIdadeGP$F,"E:",mortalidadeIdadeGP$E,"Fopt:",mortalidadeIdadeGP$Fopt,"Flimit:",mortalidadeIdadeGP$Flimit,"\n")
        }
        if(length(cinfktzeroG)>5){
        cat("\nZ Length-based:",cinfktzeroG$Z,"ϕ:",cinfktzeroG$ϕ,"M:",cinfktzeroG$M,"F:",cinfktzeroG$F,"E:",cinfktzeroG$E,"Fopt:",cinfktzeroG$Fopt,"Flimit:",cinfktzeroG$Flimit,"\n")
        }
        
        #### LOGISTICA ####
        cat("\nFitted Parameters - Logistic Equation\nLinf:",cinfktzeroL[[1]],"k:",cinfktzeroL[[2]],"t0:",cinfktzeroL[[3]],"\n")
        if(length(mortalidadeIdadeLG)>5){
          cat("\nZ Age-based:",mortalidadeIdadeLG$Z,"ϕ:",mortalidadeIdadeLG$ϕ,"M:",mortalidadeIdadeLG$M,"F:",mortalidadeIdadeLG$F,"E:",mortalidadeIdadeLG$E,"Fopt:",mortalidadeIdadeLG$Fopt,"Flimit:",mortalidadeIdadeLG$Flimit,"\n")
        }
        if(length(cinfktzeroL)>5){
          cat("\nZ Length-based:",cinfktzeroL$Z,"ϕ:",cinfktzeroL$ϕ,"M:",cinfktzeroL$M,"F:",cinfktzeroL$F, "E:",cinfktzeroL$E,"Fopt:",cinfktzeroL$Fopt,"Flimit:",cinfktzeroL$Flimit,"\n")
        }
    }
    cat("\n\n")
    return(FIMalgoritmo)
    
    }else{
    #Com grupo
    removedor(8)
      if(idioma == 1){
                
        #GRUPO A
        #### FORD ####
        cat("\nGrupo = A\nFord-Walford\nCinf:",fordA[[1]],"k:",fordA[[2]],"t0:",fordA[[3]],"\n")
        if(length(mortalidadeIdadeAford)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeAford$Z,"ϕ:",mortalidadeIdadeAford$ϕ,"M:",mortalidadeIdadeAford$M,"F:",mortalidadeIdadeAford$F,"E:",mortalidadeIdadeAford$E,"Fopt:",mortalidadeIdadeAford$Fopt,"Flimit:",mortalidadeIdadeAford$Flimit,"\n")
        }
        if(length(fordA)>5){
          cat("\nZ baseado em comprimento:",fordA$Z,"ϕ:",fordA$ϕ,"M:",fordA$M,"F:",fordA$F,"E:",fordA$E,"Fopt:",fordA$Fopt,"Flimit:",fordA$Flimit,"\n")
        }
        
        #### BERTALANFFY ####
        cat("\nGrupo = A\nParâmetros Ajustados - Equação de Bertalanffy\nCinf:",cinfktzeroBM[[1]],"k:",cinfktzeroBM[[2]],"t0:",cinfktzeroBM[[3]],"\n")
        if(length(mortalidadeIdadeAVB)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeAVB$Z,"ϕ:",mortalidadeIdadeAVB$ϕ,"M:",mortalidadeIdadeAVB$M,"F:",mortalidadeIdadeAVB$F,"E:",mortalidadeIdadeAVB$E,"Fopt:",mortalidadeIdadeAVB$Fopt,"Flimit:",mortalidadeIdadeAVB$Flimit,"\n")
        }
        if(length(cinfktzeroBM)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroBM$Z,"ϕ:",cinfktzeroBM$ϕ,"M:",cinfktzeroBM$M,"F:",cinfktzeroBM$F,"E:",cinfktzeroBM$E,"Fopt:",cinfktzeroBM$Fopt,"Flimit:",cinfktzeroBM$Flimit,"\n")
        }
      
        #### GOMPERTZ ####
        cat("\nGrupo = A\nParâmetros Ajustados - Equação de Gompertz\nCinf:",cinfktzeroGM[[1]],"k:",cinfktzeroGM[[2]],"t0:",cinfktzeroGM[[3]],"\n")
        if(length(mortalidadeIdadeAGP)>5){
            cat("\nZ baseado em idade:",mortalidadeIdadeAGP$Z,"ϕ:",mortalidadeIdadeAGP$ϕ,"M:",mortalidadeIdadeAGP$M,"F:",mortalidadeIdadeAGP$F,"E:",mortalidadeIdadeAGP$E,"Fopt:",mortalidadeIdadeAGP$Fopt,"Flimit:",mortalidadeIdadeAGP$Flimit,"\n")
        }
        if(length(cinfktzeroGM)>5){
        cat("\nZ baseado em comprimento:",cinfktzeroGM$Z,"ϕ:",cinfktzeroGM$ϕ,"M:",cinfktzeroGM$M,"F:",cinfktzeroGM$F,"E:",cinfktzeroGM$E,"Fopt:",cinfktzeroGM$Fopt,"Flimit:",cinfktzeroGM$Flimit,"\n")
        }
        
        #### LOGISTICA ####
        cat("\nGrupo = A\nParâmetros Ajustados - Equação de Logística\nCinf:",cinfktzeroLM[[1]],"k:",cinfktzeroLM[[2]],"t0:",cinfktzeroLM[[3]],"\n")
        if(length(mortalidadeIdadeALG)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeALG$Z,"ϕ:",mortalidadeIdadeALG$ϕ,"M:",mortalidadeIdadeALG$M,"F:",mortalidadeIdadeALG$F,"E:",mortalidadeIdadeALG$E,"Fopt:",mortalidadeIdadeALG$Fopt,"Flimit:",mortalidadeIdadeALG$Flimit,"\n")
        }
        if(length(cinfktzeroLM)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroLM$Z,"ϕ:",cinfktzeroLM$ϕ,"M:",cinfktzeroLM$M,"F:",cinfktzeroLM$F,"E:",cinfktzeroLM$E,"Fopt:",cinfktzeroLM$Fopt,"Flimit:",cinfktzeroLM$Flimit,"\n")
        }
        
        #GRUPO B
        #### FORD ####
        cat("\nGrupo = B\nFord-Walford\nCinf:",ford[[1]],"k:",ford[[2]],"t0:",ford[[3]],"\n")
        if(length(mortalidadeIdadeBford)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeBford$Z,"ϕ:",mortalidadeIdadeBford$ϕ,"M:",mortalidadeIdadeBford$M,"F:",mortalidadeIdadeBford$F,"E:",mortalidadeIdadeBford$E,"Fopt:",mortalidadeIdadeBford$Fopt,"Flimit:",mortalidadeIdadeBford$Flimit,"\n")
        }
        if(length(fordB)>5){
          cat("\nZ baseado em comprimento:",fordB$Z,"ϕ:",fordB$ϕ,"M:",fordB$M,"F:",fordB$F,"E:",fordB$E,"Fopt:",fordB$Fopt,"Flimit:",fordB$Flimit,"\n")
        }
        
        #### BERTALANFFY ####
        cat("\nGrupo = B\nParâmetros Ajustados - Equação de Bertalanffy\nCinf:",cinfktzeroBF[[1]],"k:",cinfktzeroBF[[2]],"t0:",cinfktzeroBF[[3]],"\n")
        if(length(mortalidadeIdadeBVB)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeBVB$Z,"ϕ:",mortalidadeIdadeBVB$ϕ,"M:",mortalidadeIdadeBVB$M,"F:",mortalidadeIdadeBVB$F,"E:",mortalidadeIdadeBVB$E,"Fopt:",mortalidadeIdadeBVB$Fopt,"Flimit:",mortalidadeIdadeBVB$Flimit,"\n")
        }
        if(length(cinfktzeroBF)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroBF$Z,"ϕ:",cinfktzeroBF$ϕ,"M:",cinfktzeroBF$M,"F:",cinfktzeroBF$F,"E:",cinfktzeroBF$E,"Fopt:",cinfktzeroBF$Fopt,"Flimit:",cinfktzeroBF$Flimit,"\n")
        }
      
        #### GOMPERTZ ####
        cat("\nGrupo = B\nParâmetros Ajustados - Equação de Gompertz\nCinf:",cinfktzeroGF[[1]],"k:",cinfktzeroGF[[2]],"t0:",cinfktzeroGF[[3]],"\n")
        if(length(mortalidadeIdadeBGP)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeBGP$Z,"ϕ:",mortalidadeIdadeBGP$ϕ,"M:",mortalidadeIdadeBGP$M,"F:",mortalidadeIdadeBGP$F,"E:",mortalidadeIdadeBGP$E,"Fopt:",mortalidadeIdadeBGP$Fopt,"Flimit:",mortalidadeIdadeBGP$Flimit,"\n")
        }
        if(length(cinfktzeroGF)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroGF$Z,"ϕ:",cinfktzeroGF$ϕ,"M:",cinfktzeroGF$M,"F:",cinfktzeroGF$F,"E:",cinfktzeroGF$E,"Fopt:",cinfktzeroGF$Fopt,"Flimit:",cinfktzeroGF$Flimit,"\n")
        }
        
        #### LOGISTICA ####
        cat("\nGrupo = B\nParâmetros Ajustados - Equação de Logística\nCinf:",cinfktzeroLF[[1]],"k:",cinfktzeroLF[[2]],"t0:",cinfktzeroLF[[3]],"\n")
        if(length(mortalidadeIdadeBLG)>5){
          cat("\nZ baseado em idade:",mortalidadeIdadeBLG$Z,"ϕ:",mortalidadeIdadeBLG$ϕ,"M:",mortalidadeIdadeBLG$M,"F:",mortalidadeIdadeBLG$F,"E:",mortalidadeIdadeBLG$E,"Fopt:",mortalidadeIdadeBLG$Fopt,"Flimit:",mortalidadeIdadeBLG$Flimit,"\n")
        }
        if(length(cinfktzeroLF)>5){
          cat("\nZ baseado em comprimento:",cinfktzeroLF$Z,"ϕ:",cinfktzeroLF$ϕ,"M:",cinfktzeroLF$M,"F:",cinfktzeroLF$F,"E:",cinfktzeroLF$E,"Fopt:",cinfktzeroLF$Fopt,"Flimit:",cinfktzeroLF$Flimit,"\n")
        }        
        cat("\n\n")
        return(FIMalgoritmo)
        
      }else if(idioma == 2){        
               
        #GRUPO A
        #### FORD ####
        cat("\nGroup = A\nFord-Walford\nLinf:",fordA[[1]],"k:",fordA[[2]],"t0:",fordA[[3]],"\n")
        if(length(mortalidadeIdadeAford)>5){
          cat("\nZ Age-based:",mortalidadeIdadeAford$Z,"ϕ:",mortalidadeIdadeAford$ϕ,"M:",mortalidadeIdadeAford$M,"F:",mortalidadeIdadeAford$F,"E:",mortalidadeIdadeAford$E,"Fopt:",mortalidadeIdadeAford$Fopt,"Flimit:",mortalidadeIdadeAford$Flimit,"\n")
        }
        if(length(fordA)>5){
          cat("\nZ Length-based:",fordA$Z,"ϕ:",fordA$ϕ,"M:",fordA$M,"F:",fordA$F,"E:",fordA$E,"Fopt:",fordA$Fopt,"Flimit:",fordA$Flimit,"\n")
        }
        
        #### BERTALANFFY ####
        cat("\nGroup = A\nFitted Parameters - Bertalanffy Equation\nLinf:",cinfktzeroBM[[1]],"k:",cinfktzeroBM[[2]],"t0:",cinfktzeroBM[[3]],"\n")
        if(length(mortalidadeIdadeAVB)>5){
          cat("\nZ Age-based:",mortalidadeIdadeAVB$Z,"ϕ:",mortalidadeIdadeAVB$ϕ,"M:",mortalidadeIdadeAVB$M,"F:",mortalidadeIdadeAVB$F,"E:",mortalidadeIdadeAVB$E,"Fopt:",mortalidadeIdadeAVB$Fopt,"Flimit:",mortalidadeIdadeAVB$Flimit,"\n")
        }
        if(length(cinfktzeroBM)>5){
          cat("\nZ Length-based:",cinfktzeroBM$Z,"ϕ:",cinfktzeroBM$ϕ,"M:",cinfktzeroBM$M,"F:",cinfktzeroBM$F,"E:",cinfktzeroBM$E,"Fopt:",cinfktzeroBM$Fopt,"Flimit:",cinfktzeroBM$Flimit,"\n")
        }
      
        #### GOMPERTZ ####
        cat("\nGroup = A\nFitted Parameters - Gompertz Equation\nLinf:",cinfktzeroGM[[1]],"k:",cinfktzeroGM[[2]],"t0:",cinfktzeroGM[[3]],"\n")
        if(length(mortalidadeIdadeAGP)>5){
            cat("\nZ Age-based:",mortalidadeIdadeAGP$Z,"ϕ:",mortalidadeIdadeAGP$ϕ,"M:",mortalidadeIdadeAGP$M,"F:",mortalidadeIdadeAGP$F,"E:",mortalidadeIdadeAGP$E,"Fopt:",mortalidadeIdadeAGP$Fopt,"Flimit:",mortalidadeIdadeAGP$Flimit,"\n")
        }
        if(length(cinfktzeroGM)>5){
        cat("\nZ Length-based:",cinfktzeroGM$Z,"ϕ:",cinfktzeroGM$ϕ,"M:",cinfktzeroGM$M,"F:",cinfktzeroGM$F,"E:",cinfktzeroGM$E,"Fopt:",cinfktzeroGM$Fopt,"Flimit:",cinfktzeroGM$Flimit,"\n")
        }
        
        #### LOGISTICA ####
        cat("\nGroup = A\nFitted Parameters - Logistic Equation\nLinf:",cinfktzeroLM[[1]],"k:",cinfktzeroLM[[2]],"t0:",cinfktzeroLM[[3]],"\n")
        if(length(mortalidadeIdadeALG)>5){
          cat("\nZ Age-based:",mortalidadeIdadeALG$Z,"ϕ:",mortalidadeIdadeALG$ϕ,"M:",mortalidadeIdadeALG$M,"F:",mortalidadeIdadeALG$F,"E:",mortalidadeIdadeALG$E,"Fopt:",mortalidadeIdadeALG$Fopt,"Flimit:",mortalidadeIdadeALG$Flimit,"\n")
        }
        if(length(cinfktzeroLM)>5){
          cat("\nZ Length-based:",cinfktzeroLM$Z,"ϕ:",cinfktzeroLM$ϕ,"M:", cinfktzeroLM$M,"F:",cinfktzeroLM$F,"E:",cinfktzeroLM$E,"Fopt:",cinfktzeroLM$Fopt,"Flimit:",cinfktzeroLM$Flimit,"\n")
        }
        
        #GRUPO B
        #### FORD ####
        cat("\nGroup = B\nFord-Walford\nLinf:",ford[[1]],"k:",ford[[2]],"t0:",ford[[3]],"\n")
        if(length(mortalidadeIdadeBford)>5){
          cat("\nZ Age-based:",mortalidadeIdadeBford$Z,"ϕ:",mortalidadeIdadeBford$ϕ,"M:",mortalidadeIdadeBford$M,"F:",mortalidadeIdadeBford$F,"E:",mortalidadeIdadeBford$E,"Fopt:",mortalidadeIdadeBford$Fopt,"Flimit:",mortalidadeIdadeBford$Flimit,"\n")
        }
        if(length(fordB)>5){
          cat("\nZ Length-based:",fordB$Z,"ϕ:",fordB$ϕ,"M:",fordB$M,"F:",fordB$F,"E:",fordB$E,"Fopt:",fordB$Fopt,"Flimit:",fordB$Flimit,"\n")
        }
        
        #### BERTALANFFY ####
        cat("\nGroup = B\nFitted Parameters - Bertalanffy Equation\nLinf:",cinfktzeroBF[[1]],"k:",cinfktzeroBF[[2]],"t0:",cinfktzeroBF[[3]],"\n")
        if(length(mortalidadeIdadeBVB)>5){
          cat("\nZ Age-based:",mortalidadeIdadeBVB$Z,"ϕ:",mortalidadeIdadeBVB$ϕ,"M:",mortalidadeIdadeBVB$M,"F:",mortalidadeIdadeBVB$F,"E:",mortalidadeIdadeBVB$E,"Fopt:",mortalidadeIdadeBVB$Fopt,"Flimit:",mortalidadeIdadeBVB$Flimit,"\n")
        }
        if(length(cinfktzeroBF)>5){
          cat("\nZ Length-based:",cinfktzeroBF$Z,"ϕ:",cinfktzeroBF$ϕ,"M:",cinfktzeroBF$M,"F:",cinfktzeroBF$F,"E:",cinfktzeroBF$E,"Fopt:",cinfktzeroBF$Fopt,"Flimit:",cinfktzeroBF$Flimit,"\n")
        }
      
        #### GOMPERTZ ####
        cat("\nGroup = B\nFitted Parameters - Gompertz Equation\nLinf:",cinfktzeroGF[[1]],"k:",cinfktzeroGF[[2]],"t0:",cinfktzeroGF[[3]],"\n")
        if(length(mortalidadeIdadeBGP)>5){
            cat("\nZ Age-based:",mortalidadeIdadeBGP$Z,"ϕ:",mortalidadeIdadeBGP$ϕ,"M:",mortalidadeIdadeBGP$M,"F:",mortalidadeIdadeBGP$F,"E:",mortalidadeIdadeBGP$E,"Fopt:",mortalidadeIdadeBGP$Fopt,"Flimit:",mortalidadeIdadeBGP$Flimit,"\n")
        }
        if(length(cinfktzeroGF)>5){
        cat("\nZ Length-based:",cinfktzeroGF$Z,"ϕ:",cinfktzeroGF$ϕ,"M:",cinfktzeroGF$M,"F:",cinfktzeroGF$F,"E:",cinfktzeroGF$E,"Fopt:",cinfktzeroGF$Fopt,"Flimit:",cinfktzeroGF$Flimit,"\n")
        }
        
        #### LOGISTICA ####
        cat("\nGroup = B\nFitted Parameters - Logistic Equation\nLinf:",cinfktzeroLF[[1]],"k:",cinfktzeroLF[[2]],"t0:",cinfktzeroLF[[3]],"\n")
        if(length(mortalidadeIdadeBLG)>5){
          cat("\nZ Age-based:",mortalidadeIdadeBLG$Z,"ϕ:",mortalidadeIdadeBLG$ϕ,"M:",mortalidadeIdadeBLG$M,"F:",mortalidadeIdadeBLG$F,"E:",mortalidadeIdadeBLG$E,"Fopt:",mortalidadeIdadeBLG$Fopt,"Flimit:",mortalidadeIdadeBLG$Flimit,"\n")
        }
        if(length(cinfktzeroLF)>5){
          cat("\nZ Length-based:",cinfktzeroLF$Z,"ϕ:",cinfktzeroLF$ϕ,"M:",cinfktzeroLF$M,"F:",cinfktzeroLF$F,"E:",cinfktzeroLF$E,"Fopt:",cinfktzeroLF$Fopt,"Flimit:",cinfktzeroLF$Flimit,"\n")
        }        
        cat("\n\n")
        return(FIMalgoritmo)        
      }
  }
}
