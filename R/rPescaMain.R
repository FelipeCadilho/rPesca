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
#'data(exemplo)
#'
#'rPesca(2, 1, 1, "Total", 1, 4, "ex_individual")
#'rPesca(2, 1, 1, "Total", 1, 4, "ex_grupo", NULL, 1, "ara", "ita")
#'
#'data(example)
#'rPesca(2, 1, 1, "Total", 1, 4, "ex_individual")
#'rPesca(2, 1, 1, "Total", 1, 4, "ex_grupo", NULL, 1, "ara", "ita")
#'
#'@export

rPesca <- function(cores=1, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=1, nome_dados, planilha=NULL, separador=NULL, grupo=NULL, nomeA=NULL, nomeB=NULL, Fonte=NULL){

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
  if(is.null(grupo)){
    dados <<- data.frame()
  }else{
    dadosGrupoA <<- data.frame()
    dadosGrupoB <<- data.frame()
  }

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

    #atribui dados sem grupo de sexo da planilha ao dataframe dados
    dados <<- meus_dados[,1]
    dados[,2] <<- meus_dados[,2]
    names(dados) <<- c("idade","ct")


  }else if(grupo == 1){

    #Nomeia base de dados principal para filtragem
    names(meus_dados) <<- c("idade","ct","grupos")

    #atribui dados do grupo A da planilha ao dataframe dados
    dadobrutoA <<- meus_dados %>% group_by(idade)%>% filter (toupper(grupos) == toupper(label2))
    dadosGrupoA <<- dadobrutoA[,1]
    dadosGrupoA[,2] <<- dadobrutoA[,2]
    dadosGrupoA[,3] <<- dadobrutoA[,3]
    names(dadosGrupoA) <<- c("idade","ct","grupos")

    #atribui dados do grupo B da planilha ao dataframe dados
    dadoBrutoB <<- meus_dados %>% group_by(idade)%>% filter (toupper(grupos) == toupper(label1))
    dadosGrupoB <<- dadoBrutoB[,1]
    dadosGrupoB[,2] <<- dadoBrutoB[,2]
    dadosGrupoB[,3] <<- dadoBrutoB[,3]
    names(dadosGrupoB) <<- c("idade","ct","grupos")

  }


  ################################### DEPURAÇÃO DE OUTLIERS PELA ESTATÍSTICA

  #pergunta se quer remover outliers pela análise estatística
  if(idioma == 1){
    cat("\nDeseja remover outliers através da depuração estatística? S/N\n")
  }else if(idioma == 2){
    cat("\nDo you want to remove outliers through statistical debugging? Y/N\n")
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
      limY <<- as.vector(meus_dados2$ct)
      limY[1] <<- 0

      #captura idade geral
      limX <<- as.vector(meus_dados2$idade)

    }else if(toupper(respostaDepuracaoOutliers) == "N"){
      #nada acontece
      meus_dados2 <<- dados

      #captura comprimento geral
      limY <<- as.vector(meus_dados2$ct)
      limY[1] <<- 0

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
      limY <<- as.vector(meus_dados2$ct)
      limY[1] <<- 0

      #captura idade geral
      limX <<- as.vector(meus_dados2$idade)

    }else if(toupper(respostaDepuracaoOutliers) == "N"){
      #nada acontece
      meus_dados2 <<- meus_dados

      #captura comprimento geral
      limY <<- as.vector(meus_dados2$ct)
      limY[1] <<- 0

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

    #remove variáveis por segurança
    removedor(2)

    cat("\n[GRUPO: B]\n")

    crescimento(dados_curvaF, c_infinitoF, kF, tzeroF, real_cont_fwB, tempoB, medida, mainNameAB, grupoB)

    #remove variáveis por segurança
    removedor(2)
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
      removedor(3)
      removedor(4)

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
    removedor(3)
    removedor(4)
  }

  ####################################### MORTALIDADE TOTAL
  if(idioma == 1){
    cat("\nDeseja escolher os dados a partir do modelo\n para calcular a mortalidade total (Z)? S/N (Padrão: Ford-Walford)\n")
  }else if(idioma == 2){
    cat("\nDo you want to choose data from the model\n to calculate total mortality (Z)? Y/N (Default: Ford-Walford)\n")
  }

  respostaMorta <- toupper(readLines(n = 1))
  #condição de exceção
  if(respostaMorta == ""){
    respostaMorta = "N"
  }
  if(respostaMorta == "S"){
    if(idioma == 1){
      cat("\nDeseja calcular a mortalidade total (Z) \na partir do modelo de FordWalford? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to calculate the total mortality (Z) \nfrom the FordWalford model? Y/N\n")
    }

    respostaFord <- readLines(n = 1)
    respostaFord <- toupper(respostaFord)
    if(respostaFord == ""){
      respostaFord = "N"
    }
    if(respostaFord=="S"||respostaFord=="Y"){
      #ford
      if(is.null(grupo)){

        #Sem grupo
        mortalidadeZ(c_infinito, k, tzero, dados, real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY)

      }else{
        cat("\n[GRUPO: A]\n")

        mortalidadeZ(c_infinitoM, kM, tzeroM, dadosGrupoA, real_cont_fwA, idioma, modeloAA, mainNameEA, labelEX, labelEY, grupoA, label2)
        removedor(5)

        cat("\n[GRUPO: B]\n")

        mortalidadeZ(c_infinitoF, kF, tzeroF, dadosGrupoB, real_cont_fwB, idioma, modeloAA, mainNameEB, labelEX, labelEY, grupoB, label1)
        removedor(5)
      }
    }

    if(idioma == 1){
      cat("\nDeseja calcular a mortalidade total (Z) a partir do modelo \nde Bertalanffy com redução dos mínimos quadrados \npelo modelo de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to calculate the total mortality (Z) from the \nBertalanffy model with least squares reduction \nby the Levenberg-Marquardt model? Y/N\n")
    }

    respostaBert <- readLines(n = 1)
    respostaBert <- toupper(respostaBert)
    #condicao de excecao
    if(respostaBert == ""){
      respostaBert = "N"
    }
    if(respostaBert=="S"||respostaBert=="Y"){
      #levenberg/Bertalanffy
      if(is.null(grupo)){
        #Sem grupo
        mortalidadeZ(c_infinito_ajustadoB, k_ajustadoB, tzero_ajustadoB, dados, real_cont_fw, idioma, modeloBB, mainNameE, labelEX, labelEY)
      }else{
        #grupo A
        cat("\n[GRUPO: A]\n")
        mortalidadeZ(c_infinito_ajustadoBM, k_ajustadoBM, tzero_ajustadoBM, dadosGrupoA, real_cont_fwA, idioma, modeloBB, mainNameEA, labelEX, labelEY, grupoA, label2)
        removedor(5)
        #grupo B
        cat("\n[GRUPO: B]\n")
        mortalidadeZ(c_infinito_ajustadoBF, k_ajustadoBF, tzero_ajustadoBF, dadosGrupoB, real_cont_fwB, idioma, modeloBB, mainNameEB, labelEX, labelEY, grupoB, label1)
        removedor(5)
      }
    }
    if(idioma == 1){
      cat("\nDeseja calcular a mortalidade total (Z) a partir do modelo \nde Gompertz com redução dos mínimos quadrados \npelo modelo de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to calculate the total mortality (Z) from the \nGompertz model with least squares reduction \nby the Levenberg-Marquardt model? Y/N\n")
    }

    respostaGomp <- readLines(n = 1)
    respostaGomp <- toupper(respostaGomp)
    #condição de exceção
    if(respostaGomp == ""){
      respostaGomp = "N"
    }
    if(respostaGomp=="S"||respostaGomp=="Y"){
      #levenberg/Gompertz
      if(is.null(grupo)){
        #Sem grupo
        mortalidadeZ(c_infinito_ajustadoG, k_ajustadoG, tzero_ajustadoG, dados, real_cont_fw, idioma, modeloCC, mainNameE, labelEX, labelEY)
      }else{
        #grupo A
        cat("\n[GRUPO: A]\n")
        mortalidadeZ(c_infinito_ajustadoGM, k_ajustadoGM, tzero_ajustadoGM, dadosGrupoA, real_cont_fwA, idioma, modeloCC, mainNameEA, labelEX, labelEY, grupoA, label2)
        removedor(5)
        #grupo B
        cat("\n[GRUPO: B]\n")
        mortalidadeZ(c_infinito_ajustadoGF, k_ajustadoGF, tzero_ajustadoGF, dadosGrupoB, real_cont_fwB, idioma, modeloCC, mainNameEB, labelEX, labelEY, grupoB, label1)
        removedor(5)
      }
    }
    if(idioma == 1){
      cat("\nDeseja calcular a mortalidade total (Z) a partir do modelo \nde Logística com redução dos mínimos quadrados \npelo modelo de Levenberg-Marquardt? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to calculate the total mortality (Z) from the \nLogistic model with least squares reduction \nby the Levenberg-Marquardt model? Y/N\n")
    }

    respostaLogi <- readLines(n = 1)
    respostaLogi <- toupper(respostaLogi)
    #condição de exceção
    if(respostaLogi == ""){
      respostaLogi = "N"
    }
    if(respostaLogi=="S"||respostaLogi=="Y"){
      #levenberg/Logística
      if(is.null(grupo)){
        #Sem grupo
        mortalidadeZ(c_infinito_ajustadoL, k_ajustadoL, tzero_ajustadoL, dados, real_cont_fw, idioma, modeloDD, mainNameE, labelEX, labelEY)
      }else{
        #grupo A
        cat("\n[GRUPO: A]\n")
        mortalidadeZ(c_infinito_ajustadoLM, k_ajustadoLM, tzero_ajustadoLM, dadosGrupoA, real_cont_fwA, idioma, modeloDD, mainNameEA, labelEX, labelEY, grupoA, label2)
        removedor(5)
        #grupo B
        cat("\n[GRUPO: B]\n")
        mortalidadeZ(c_infinito_ajustadoLF, k_ajustadoLF, tzero_ajustadoLF, dadosGrupoB, real_cont_fwB, idioma, modeloDD, mainNameEB, labelEX, labelEY, grupoB, label1)
        removedor(5)
      }
    }
  }else if(respostaMorta == "N"){
    if(is.null(grupo)){
      #Sem grupo
      mortalidadeZ(c_infinito, k, tzero, dados, real_cont_fw, idioma, modeloAA, mainNameE, labelEX, labelEY)
    }else{
      #grupo A
      cat("\n[GRUPO: A]\n")
      mortalidadeZ(c_infinitoM, kM, tzeroM, dadosGrupoA, real_cont_fwA, idioma, modeloAA, mainNameEA, labelEX, labelEY, grupoA, label2)
      removedor(5)
      #grupo B
      cat("\n[GRUPO: B]\n")
      mortalidadeZ(c_infinitoF, kF, tzeroF, dadosGrupoB, real_cont_fwB, idioma, modeloAA, mainNameEB, labelEX, labelEY, grupoB, label1)
      removedor(5)
    }
  }
  ######################################### TESTE DE VEROSSIMILHANÇA KIMURA (1980)
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
  cat("\n")
  if(is.null(grupo)){
    #Sem grupo
    removedor(7)
    if(idioma == 1){
      return(list(FordWalford = round(ford,2), Bertalanffy_fitted_Levenberg_Marquardt = round(cinfktzeroB,2), Gompertz_fitted_Levenberg_Marquardt = round(cinfktzeroG,2), Logistica_fitted_Levenberg_Marquardt = round(cinfktzeroL,2)))
    }else if(idioma == 2){
      names(ford) <<- c("Linf","k","t0")
      names(cinfktzeroB) <<- c("Linf","k","t0")
      names(cinfktzeroG) <<- c("Linf","k","t0")
      names(cinfktzeroL) <<- c("Linf","k","t0")
      return(list(FordWalford = round(ford,2), Bertalanffy_fitted_Levenberg_Marquardt = round(cinfktzeroB,2), Gompertz_fitted_Levenberg_Marquardt = round(cinfktzeroG,2), Logistic_fitted_Levenberg_Marquardt = round(cinfktzeroL,2)))
    }

    }else{
    #Com grupo
    removedor(8)
      if(idioma == 1){
        return(list(Grupo = "A",FordWalford = round(fordA,2), Bertalanffy_fitted_Levenberg_Marquardt = round(cinfktzeroBM,2), Gompertz_fitted_Levenberg_Marquardt = round(cinfktzeroGM,2), Logistica_fitted_Levenberg_Marquardt = round(cinfktzeroLM,2),
                    Grupo = "B",FordWalford = round(fordB,2), Bertalanffy_fitted_Levenberg_Marquardt = round(cinfktzeroBF,2), Gompertz_fitted_Levenberg_Marquardt = round(cinfktzeroGF,2), Logistica_fitted_Levenberg_Marquardt = round(cinfktzeroLF,2)))
      }else if(idioma == 2){
        names(fordA) <<- c("Linf","k","t0")
        names(fordB) <<- c("Linf","k","t0")
        names(cinfktzeroBM) <<- c("Linf","k","t0")
        names(cinfktzeroGM) <<- c("Linf","k","t0")
        names(cinfktzeroLM) <<- c("Linf","k","t0")
        names(cinfktzeroBF) <<- c("Linf","k","t0")
        names(cinfktzeroGF) <<- c("Linf","k","t0")
        names(cinfktzeroLF) <<- c("Linf","k","t0")
        return(list(Grupo = "A",FordWalford = round(fordA,2), Bertalanffy_fitted_Levenberg_Marquardt = round(cinfktzeroBM,2), Gompertz_fitted_Levenberg_Marquardt = round(cinfktzeroGM,2), Logistic_fitted_Levenberg_Marquardt = round(cinfktzeroLM,2),
                    Grupo = "B",FordWalford = round(fordB,2), Bertalanffy_fitted_Levenberg_Marquardt = round(cinfktzeroBF,2), Gompertz_fitted_Levenberg_Marquardt = round(cinfktzeroGF,2), Logistic_fitted_Levenberg_Marquardt = round(cinfktzeroLF,2)))
      }

  }
}
