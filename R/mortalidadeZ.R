#'Mortalidade Total (Z).
#'
#'Script que calcula a mortalidade total partindo de parâmetros de Ford-Walford ou de Levenberg-Marquardt.
#'
#'@param c_infinito Contém o comprimento assintótico.
#'@param k Contém a taxa de crescimento.
#'@param tzero Contém o tempo no comprimento zero.
#'@param dados Contém o dataframe com os dados importados.
#'@param real_cont_fw Contém o "n" da amostra.
#'@param idioma Contém o idioma do resultado.
#'@param modelo Contém o modelo usado para cálculos.
#'@param mainE Contém o título do gráfico.
#'@param labX Contém o rótulo para eixo x.
#'@param labY Contém o rótulo para eixo y.
#'@param sexo Contém um texto referente ao grupo atual.
#'@param label Contém a legenda referente ao grupo atual.
#'@param adhoc Determina uma análise fora da rotina do rPesca quando igual 1. (O padrão é NULL) 
#'
#'@examples
#'mortalidadeZ(c_infinito, k, tzero, dados, real_cont_fw, idioma, modelo, mainE, labX, labY, sexo, label)
#'mortalidadeZ(c_infinito = 35.5, k=0.10, tzero=-1.10, dados=dados, idioma=1, adhoc=1)
#'
#'@export
mortalidadeZ <- function(c_infinito, k, tzero, dados, real_cont_fw=NULL, idioma, modelo=NULL, mainE=NULL, labX=NULL, labY=NULL, sexo=NULL, label=NULL, adhoc=NULL){

  #atualizada 02/10/2024 para utilização fora da rotina rPesca
  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
  }
  
  #determina espaço vazio na pergunta se estiver fora da rotina rPesca
  if(is.null(modelo)){
    modelo <<-""
  }
  
  #determina os nomes das labels e chama pacotes
  if(!is.null(adhoc)){
    #chama pacote que lê planilha em excel
    library("readxl")
    
    #chama pacote de controle de dados
    library("dplyr")
    
    #chama pacote de calculo de logaritmo natural
    library("SciViews")
    
    #chama pacote de cálculo dos mínimos quadrados Levenberg
    library("minpack.lm")
    
    #chama pacote de teste de verossimilhança de Kimura (1980)
    library("fishmethods")
    
    if(idioma==1){
      mainE="Curva de Captura Linearizada Baseada no Comprimento" 
      labX="t(anos)" 
      labY="Ln(N/dt)"
    }else if(idioma==2){
      mainE="Length-Based Linearized Catch Curve" 
      labX="t(years)" 
      labY="Ln(N/dt)"
    }
    real_cont_fw <<- nrow(dados[,2])
    real_cont_fww <<- nrow(dados[,2])
  }
  
  #inicializa ok
  ok <<- 0
  
  #começa a repetição de escolhas ####
  while(ok==0){
    modelOld <<- modelo
    
    #perguntar se quer informar limite mínimo e máximo do comprimento ####
    if(idioma == 1){
      cat("\nDeseja informar o limite\nmínimo e máximo do comprimento",sexo,"? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to inform\nthe minimum and maximum limit of the Length",sexo,"? Y/N\n")
    }
    
    respostaLimCt <- readLines(n=1)
    #condição de exceção
    if(respostaLimCt == ""){
      respostaLimCt = "N"
    }
    #capturar o limite mínimo e máximo do comprimento ####
    respostaLimMin <<- 0
    respostaLimMin <<- 0
    if(toupper(respostaLimCt)=="S"||toupper(respostaLimCt)=="Y"){
      
      if(idioma == 1){
        cat("\nInforme o limite mínimo:\n")
      }else if(idioma == 2){
        cat("\nEnter the minimum limit:\n")
      }
      respostaLimMin <<- scan(n=1)
      #condição de exceção
      if(length(respostaLimMin)==0){
        respostaLimCt = "N"
      }
      if(idioma == 1){
        cat("\nInforme o limite máximo:\n")
      }else if(idioma == 2){
        cat("\nEnter the maximum limit:\n")
      }
      
      respostaLimMax <<- scan(n=1)
      #condição de exceção
      if(length(respostaLimMax)==0){
        respostaLimCt = "N"
      }
    }
    
    #selecionar os dados de comprimento da planilha matriz que estão entre o limite mínimo e máximo do comprimento definido pelo usuário ####
    classe_ct_bruto <<- dados
    classess_ct <<- data.frame(idade=c(0), ct=c(0))
    indiceClasseZ <<- 1
    if(is.null(adhoc){
    real_cont_fww <<- real_cont_fw
    }
    if(toupper(respostaLimCt)=="S"){
      for(i in 1:real_cont_fw){
        if(classe_ct_bruto[i,2]>=respostaLimMin && classe_ct_bruto[i,2]<= respostaLimMax){
          classess_ct[indiceClasseZ,2] <<- classe_ct_bruto[i,2]
          indiceClasseZ <<- indiceClasseZ+1
        }
        real_cont_fww <<- indiceClasseZ-1
      }
    }else if(toupper(respostaLimCt)=="N"){
      classess_ct <<- classe_ct_bruto
    }
    
    #suprimir alerta de erro ####
    oldw <- getOption("warn")
    options(warn = -1)
    
    #inicia variável do maior e menor comprimento ####
    if(!is.null(adhoc)){
      if(idioma==1){
        cat("\nDeseja calcular a classe de comprimento?(S/N)\n")
      }else if(idioma==2){
        cat("\nWould you like to calculate the length class?(Y/N)\n")
      }
    }
      resposta <- toupper(readLines(n = 1))
      if(resposta == "S" || resposta=="Y" || is.null(adhoc)){
        limSup <<- 0
        limInf <<- 9999999999
        
        #procura o maior e o menor comprimento dos dados
        for(i in 1:real_cont_fww){
          if(classess_ct[i,2]>limSup){
            limSup <<- classess_ct[i,2]
          }
          if(classess_ct[i,2]<limInf){
            limInf <<- classess_ct[i,2]
          }
        }
        
        #obtém a amplitude do comprimento
        tamClasse <<- round((limSup - limInf),0)
        
        #calcula de acordo com a classe com frequência mais próxima de 5 a 20 ####
        classe <<- round(tamClasse/20,0)
        if(classe == 0){
          classe <<- classe + 2
        }else{
          classe <<- classe + 1
        }
        
        #inicia o dataframe que obterá os dados das classes de comprimento ####
        classes_ct <<- data.frame(ct=c(0))
        
        #informar classe encontrada e se vai colocá-la manualmente
        if(idioma == 1){
          cat("\nO algoritmo encontrou a classe:",as.integer(classe),"\ncomo a melhor opção.\n\nDeseja alterar esse valor? S/N\n")
        }else if(idioma == 2){
          cat("\nAlgorithm found class:",as.integer(classe),"\nas the best choice.\n\nDo you want to change this value? Y/N\n")
        }
        
        respostaClasse <- readLines(n = 1)
        #condição de exceção
        if(respostaClasse == ""){
          respostaClasse = "N"
        }
        if(toupper(respostaClasse)=="S"||toupper(respostaClasse)=="Y"){
          if(idioma == 1){
            cat("\nDigite o valor desejado para a classe:\n")
          }else if(idioma == 2){
            cat("\nEnter the desired value for the class:\n")
          }
          
          
          #salva classe anterior para caso usuário não coloque valor algum
          classeLog = classe
          
          #obtém dados do usuário
          classe <<- scan(n=1)
          
          #condição de exceção
          if(is.null(classe)||length(classe)==0){
            classe <<- classeLog
          }
        }
        
        #calcula as classes de comprimento
        for(i in 1:real_cont_fww){
          classes_ct[i,1] <<- round((classess_ct[i,2]/classe),0)*classe
        }
      }else{
        #nao calcula a classe (dado já distribuido em classes)
        #ordena conjunto
        classess_ct <<- classess_ct %>% arrange(classess_ct[[2]])
        dados_unicos <<- classess_ct[,2] %>% distinct()
        #identifica a classe usada
        classe <<- dados_unicos[2,]-dados_unicos[1,]
        #inicia novo conjunto do ct
        classes_ct <<- data.frame(ct=c(0))
        #atribui os ct ao novo conjunto
        for(i in 1:real_cont_fww){
          classes_ct[i,1] <<- classess_ct[i,2]
        }
      }

    #redução das classes e contagem do n ####
    LiN <<- group_by(classes_ct,ct)%>%summarise(n = sum(ct)/ct)
    
    #atribui classe de comprimento em um vetor
    lin <<- as.vector(LiN$ct)
    
    #remove duplicados
    lin <<- unique(lin)
    
    #remove classes duplicadas
    LiN <<- LiN %>% group_by(ct) %>% filter (! duplicated(ct))
    
    
    #variável com n das classes resumidas
    contReal <<- length(which(!is.na(lin)))
    
    #contador-1
    conteReal <<- contReal-1
    
    #inicia dataframe de idade média da classe de comprimento i (t(i)) ####
    Ti <<- data.frame()
    
    #calcula t(i) 
    for(i in 1:contReal){
      Ti[i,1] <<- tzero-(1/k)*ln(1-lin[i]/c_infinito)
    }
    
    #inicia dataframe de duração de tempo de cada classe de comprimento (dt) ####
    dT <<- data.frame()
    
    #calcula dt
    for(i in 1:conteReal){
      indiceB <- i+1
      dT[i,1] <<- Ti[indiceB,1] - Ti[i,1]
    }
    
    #inicia dataframe de idade média de cada classe de comprimento (t') ####
    tAspas <<- data.frame()
    
    #calcula t'
    for(i in 1:conteReal){
      indiceC = i+1
      tAspas[i,1] <<- tzero-(1/k)*ln(1-((1/2*(lin[i]+lin[indiceC]))/c_infinito))
    }
    names(tAspas) <<- c("ct")
    
    #inicia vetor de conversão do número total de peixes de cada classe de comprimento LN(N/dt) ####
    lnNdT <<- vector()
    
    #calcula LN(N/dt)
    for(i in 1:conteReal){
      lnNdT[i] <<- ln(LiN[i,2]/dT[i,1])
    }
    
    if(!is.null(sexo)){
      labelA =paste("Curva de Captura Z", modelo,"\nGrupo:",label)
      labelB = "x"
      labelC = "y"
      #tradução para inglês
      if(idioma == 2){
        labelA =paste("Catch Curve Z\n", modelo,"\nGroup:",label)
        labelB = "x"
        labelC = "y"
      }
    }else{
      labelA =paste("Curva de Captura Z\n", modelo)
      labelB = "x"
      labelC = "y"
      
      #tradução para inglês
      if(idioma == 2){
        labelA =paste("Catch Curve Z\n", modelo)
        labelB = "x"
        labelC = "y"
      }
    }
    
    #cria vetor do eixo x
    z_x <<- na.omit(as.vector(tAspas$ct))
    
    #cria vetor do eixo y
    z_y <<- na.omit(as.numeric(lnNdT))
    
    #variável com n das classes resumidas
    contesReal <<- 0
    contRealX <<- 0
    contRealY <<- 0
    
    #variavel que receberá posição mais alta do vetor com NA
    ocultado <<- vector()
    
    #se existir NA ocultado recebe posição
    ocultado <<- c(which(is.na(z_x)),which(is.na(z_y)))
    
    #remove posições duplicadas
    ocultado <<- unique(ocultado)
    
    #ordena posições em ordem crescente
    ocultado <<- ocultado[order(ocultado)]
    
    #se não exisir NA atribui aos vetores
    contRealX <<- length(which(!is.na(z_x)))
    contRealY <<- length(which(!is.na(z_y)))
    
    #novos vetores de x e y organizados
    z_xx <<- vector()
    z_yy <<- vector()
    
    #indice para não pular posição no novo vetor
    indiceD <<- 0
    
    #organizar vetor alinhando as mesmas posições sem NA
    for(i in 1:conteReal){
      if(!isTRUE(ocultado[ocultado==i]==i)){
        indiceD <<- indiceD+1
        z_xx[indiceD] <<- z_x[i]
        z_yy[indiceD] <<- z_y[i]
      }
    }
    
    #recebe o n do vetor organizado
    contesReal <<- length(z_xx)
    
    #habilita alerta de erro
    options(warn = oldw)
    
    #tentativa de remoção dos outliers da regressão linear
    agarraX <<- vector()
    agarraY <<- vector()
    indiceF = 0
    indiceG = contesReal
    acum <<- 0
    repeat{
      indiceF = indiceF+1
      #verifica se é NA
      if(is.na(z_x[indiceG]) || is.na(z_y[indiceG])){
        #reseta índice
        indiceF = 0
        #verifica se vetores estão vazios
      }else if(is.na(agarraX[indiceF]) && is.na(agarraY[indiceF])){
        #iniciar vetores com os últimos valores
        agarraX[indiceF] <<- z_x[indiceG]
        agarraY[indiceF] <<- z_y[indiceG]
        #reseta índice
        indiceF = 0
        acum <<- acum +1
        #se a primeira posição de x for maior que a última
      }else if(z_xx[indiceG]<agarraX[indiceF]){
        #e se a primeira posição de y for menor que a última
        if(z_yy[indiceG]>agarraY[indiceF]){
          indiceH=indiceF+1
          #atribui valor atual ao campo seguinte do indiceF
          agarraX[indiceH] <<- z_xx[indiceG]
          agarraY[indiceH] <<- z_yy[indiceG]
          acum <<- acum + 1
        }else{
          break
        }
      }else{
        break
      }
      #controle de repetição
      if(indiceG == 1){
        break
      }
      indiceG = indiceG-1
    }
    indiceI <<- contesReal
    indiceJ <<- 0
    #se na sequência, a segunda posição não existir entra nessa tratativa
    if(is.na(agarraX[2]) || is.na(agarraY[2])){
      indiceIII = indiceI
      indiceII = 0
      #procura sequência para regressão
      for(i in indiceIII:1){
        indiceII = i-1
        #verifica se existe dados na posição anterior diferente do tamanho na posição atual
        if(z_yy[indiceII]>z_yy[i] && z_xx[indiceII]<z_xx[i]){
          indiceJ <<- indiceIII
        }
        if(indiceJ != 0){
          i = 1
          break
        }
      }
      
      indiceK <<- indiceJ
      acum <<- 0
      #zera porição 1 dos vetores
      agarraX[1] <<- NaN
      agarraY[1] <<- NaN
      repeat{
        indiceF = indiceF+1
        #verifica se vetores estão com a primeira posição vazia
        if(is.na(agarraX[indiceF]) && is.na(agarraY[indiceF])){
          #iniciar vetores com os últimos valores
          agarraX[indiceF] <<- z_xx[indiceK]
          agarraY[indiceF] <<- z_yy[indiceK]
          #reseta índice
          indiceF = 0
          acum <<- acum +1
          #se a primeira posição de x for maior que a última
        }else if(z_xx[indiceK]<agarraX[indiceF]){
          #e se a primeira posição de y for menor que a última
          if(z_yy[indiceK]>agarraY[indiceF]){
            indiceH=indiceF+1
            #atribui valor atual ao campo seguinte do indiceF
            agarraX[indiceH] <<- z_xx[indiceK]
            agarraY[indiceH] <<- z_yy[indiceK]
            acum <<- acum + 1
          }else{
            break
          }
        }else{
          break
        }
        #controle de repetição
        if(indiceK == 1){
          break
        }
        indiceK <<- indiceK-1
      }
    }
    
    #chama o gráfico ####
    graficoZ(lin, z_xx, z_yy, agarraX, agarraY, labelB, labelC, labelA, modelOld, sexo, mainE, labX, labY,classe)
    
    if(idioma == 1){
      cat("\nDeseja refazer esta etapa? S/N\n")
    }else if(idioma == 2){
      cat("\nDo you want to redo this step? Y/N\n")
    }
    ok <<- readLines(n=1)
    #se usuário está satisfeito encerra
    if(ok == 0 || toupper(ok) == "N"){
      ok <<- 1
      if(!is.null(adhoc)){
        #indice da performance de crescimento Pauly and Munro (1984)
        phi <<- round(log10(k)+2*log10(c_infinito),7)
        
        #mortalidade natural Then et al. (2015) Pauly_NLS-T equation
        M = 4.118*(k^(0.73))*(c_infinito^(-0.33))
        
        morte[1] <<- c_infinito
        morte[2] <<- k
        morte[3] <<- tzero
        morte[4] <<- phi
        morte[5] <<- M
        morte[6] <<- Z
        morte[7] <<- Z-M
        morte[8] <<- morte[7]/Z
        #Fopt Patterson (1992)
        morte[9] <<- 0.5*M
        #Flimit Patterson (1992)
        morte[10] <<- 2/3*M
        names(morte) <<- c("c_infinito","k","tzero","ϕ","M","Z","F","E","Fopt","Flimit")
        return(morte)
      }
    }else if(toupper(ok) == "S"||toupper(ok) == "Y"){
      #se o usuário não estiver satisfeito continua
      ok <<- 0
      rm(modelOld, envir = .GlobalEnv)
      removedor(51)
    }
  }
}
