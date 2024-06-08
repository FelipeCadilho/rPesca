#'Remoção de Outliers.
#'
#'Script que analisa e remove estatisticamente outliers.
#'
#'@param dados Contém um dataframe com os dados importados.
#'@param grupo Contém indicador de dados com grupos.
#'
#'@examples
#'removeOutliers(dados)
#'
#'@export

removeOutliers <- function(Dados,grupo=NULL){

  #recebe dataframe com a base de dados
  dadosVelho <<- Dados

  #inicia dataframe do novo conjunto de dados apurado
  dadosNovo <<- data.frame()

  #inicia dataframe dos dados outliers
  outliers  <<- data.frame()

  #indice interno para o for
  x         <<-  1

  #indice interno para o for
  y         <<-  1

  #atribui a coluna idade em um dataframe denominado dados
  dadosIdade <<- Dados[,1]

  #ordena e atribui os dados da coluna idade no dataframe ano sem dados duplicados
  contagemIdade <<- Dados %>% group_by(idade) %>% filter (! duplicated(idade)) %>% select(idade) %>% count(idade)

  #calcula somatório da contagem do dataframe de idade
  Nidade <<- as.double(sum(contagemIdade[,2]))

  for(v in 1:Nidade){
  #filtra comprimento por idade
  filtro    <<- dadosVelho %>% group_by(idade)%>% filter (idade == idadeOrdenada[v,])

  #calcula quartis e média
  analisar  <<- as.vector(filtro$ct)

  #média do conjunto de comprimento por idade
  media     <<- mean(analisar)

  #quartis
  quartil   <<- quantile(analisar)

  #quartil 1 do conjunto de comprimento por idade
  quartil1  <<- quartil[[2]]

  #quartil 3 do conjunto de comprimento por idade
  quartil3  <<- quartil[[4]]

  #amplitude dos quartis do conjunto de comprimento por idade
  i_qr      <<- quartil3 - quartil1

  #limite superior do conjunto de comprimento por idade
  L_sup     <<- media + 1.5 * i_qr

  #limite inferior do conjunto de comprimento por idade
  L_inf     <<- media - 1.5 * i_qr

  #conta linhas do dataframe
  contagem  <<- count(filtro[,2])

  #contador do conjunto de comprimento por idade
  ene        <<- as.double(sum(contagem))

    #atribui aos dataframes os dados analisados
    for(i in 1:ene){
      #condição se tem outlier
      if(filtro[i,2] > L_sup || filtro[i,2] < L_inf){

        #insere os outliers removidos
        outliers[x,1]   <<- filtro[i,1]

        #insere os outliers removidos
        outliers[x,2]   <<- filtro[i,2]

        #indice da linha do dataframe outlier
        x <<- x + 1

      #condiçao se não tiver outlier
      }else{

        #insere os dados que serão utilizados
        dadosNovo[y,1]  <<- filtro[i,1]

        #insere os dados que serão utilizados
        dadosNovo[y,2]  <<- filtro[i,2]

        if(!is.null(grupo)){
        #insere os dados que serão utilizados
        dadosNovo[y,3]  <<- filtro[i,3]
        }

        #indice da linha do dataframe dadosNovo
        y <<- y + 1

      }
    }
  }
  #apaga as variáveis desnecessárias
  removedor(6)

  #retorna dados alterados
  return(dadosNovo)

}
