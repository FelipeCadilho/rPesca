#'Modelo de Ford-Walford.
#'
#'Esta função gera os primeiros parâmetros da história de vida dos peixes.
#'
#'@param dados Contém um dataframe com os dados importados.
#'@param labelCX Contém o rótulo para eixo x sobre média de comprimento.
#'@param labelCY Contém o rótulo para eixo y sobre média de comprimento.
#'@param labelLX Contém o rótulo para eixo x sobre o ln.
#'@param labelLY Contém o rótulo para eixo y sobre o ln.
#'@param mainNameC Contém o título do gráfico para regressão do comprimento.
#'@param mainNameD Contém o título do gráfico para regressão do ln.
#'@param sexo Contém um texto referente ao grupo atual.
#'
#'@examples
#'fordWalford(dados, labelCX, labelCY, labelLX, labelLY, mainNameC, mainNameD, sexo)
#'
#'@export

fordWalford <- function(dados, labelCX, labelCY, labelLX, labelLY, mainNameC, mainNameD, sexo=NULL){

  #determina espaço vazio na pergunta se não houver grupo
  if(is.null(sexo)){
    sexo <<- ""
  }

  #atribui dados da planilha ao dataframe de curva de crescimento
  dados_curva <<- dados

  #atribui a coluna idade em um dataframe denominado dados
  dadosDaIdade <<- as.vector(dados_curva$idade)

  #ordena e atribui os dados da coluna idade no dataframe ano sem dados duplicados
  iidade <<- unique(dadosDaIdade)

  #conta quantidade de idades
  contagem_idade <<- length(iidade)

  #transforma em real a quantidade de idades
  real_som_idade <<- as.double(contagem_idade)

  #atribui a coluna comprimento em um vetor
  dados_ct <<- dados[,2]

  #conta a quantidade de individuos total
  contagem_total <<- length(dados_ct)

  #transforma em real os dados do dataframe
  real_cont_total <<- as.double(contagem_total)

  #cria dataframe para o comprimento
  ct <<- data.frame(ct = c(0))

  #inicializa dataframe
  conjunto_ct <<- data.frame()

  #separa os comprimentos por ano calculando a média
  for(i in 1:real_som_idade){
    #ordena os comprimentos de acordo com os anos
    conjunto_ct <<- dados %>% group_by(idade)%>% filter (idade == iidade[i])

    #calcula média dentro do seu grupamento anual
    ct[i,1] <<- as.numeric(apply(conjunto_ct[,2],2,mean))
  }

  #cria dataframe para mpedia do comprimento
  media_c <<- data.frame()

  #cria dataframe para idade em cálculo de log
  idade_log <<- data.frame()

  #ordena c(x)
  for(i in 1:real_som_idade){
    if(i < real_som_idade){
      media_c[i,1] <<- ct[i,1]
      idade_log[i,1] <<- iidade[i]
    }
  }

  #cria dataframe c+1
  c_mais_um <<- data.frame()

  #ordena c+1(y)
  for(i in 2:real_som_idade){
    reducao <- i-1
    c_mais_um[reducao,1] <<- ct[i,1]
  }

  #conta quantidade de linhas de c+1
  contagem_c_mais_um <- count(c_mais_um)

  #transforma em real a contagem de c+1
  real_contagem_c_mais_um <- as.double(contagem_c_mais_um)

  #inicia variável do maior comprimento
  maior <- 0

  #transforma em vetor os comprimentos
  maior = as.vector(dados$ct)

  #seleciona o maior comprimento
  maior = max(maior)

  #cria vetor de comprimento
  c_x <- as.vector(media_c$V1)

  #cria vetor de comprimento subsequente
  c_y <- as.vector(c_mais_um$V1)

  #cálculo do R²
  r2_c <<- cor(c_x,c_y)^2

  #calcula regressão
  regressao_c = lm(c_y~c_x)

  #coeficiente a
  c_a <<- as.double(regressao_c$coefficients[1])

  #coeficiente b
  c_b <<- as.double(regressao_c$coefficients[2])

  resultadoC <- paste("y = ",round(c_b,3),"x + ",round(c_a,4)," R² = ",round(r2_c,3))
  graficoR(c_x, c_y, regressao_c, labelCX, labelCY, mainNameC, resultadoC, sexo)

  #calcula o comprimento assintótico empírico
  c_infinito_empirico <<- maior/0.95

  #calcula o comprimento assintótico ajustado
  c_infinito <<- (round(c_a,3))/(1-(round(c_b,4)))

  #cria dataframe para log de comprimento assintótico menos comprimento
  log_c_infinito_menos_med_ct <<- data.frame()

  #calcula ln
  for(i in 1 :real_contagem_c_mais_um){
    log_c_infinito_menos_med_ct[i,1] <<- ln(c_infinito - ct[i,1])
  }

  #cria vetor de idade
  ln_x <<- as.vector(idade_log$V1)

  #cria vetor de Ln(Cinf-Ci)
  ln_y <<- as.vector(log_c_infinito_menos_med_ct$V1)

  #procura se existe número inválido
  buscaErro <<- which(is.na(ln_y))

  #condição de exceção
  if(length(buscaErro)==0){
    #nada acontece
  }else{
    #tratamento de erro
    if(idioma == 1){
      erro <<- cat("\n\nO cálculo do logaritmo neperiano resultou em erro. \nVerifique sua base de dados e tente novamente.\n\n")
      return(erro)
    }else if(idioma == 2){
      erro <<- cat("\n\nThe calculation of the Neperian logarithm resulted in an error. \nCheck your database and try again.\n\n")
      return(erro)
    }

  }

  #cálculo do R² do log
  r2_ln <<- cor(ln_x,ln_y)^2

  #calcula regressão do log
  regressao_ln = lm(ln_y~ln_x)

  #coeficiente a
  ln_a <<- as.double(regressao_ln$coefficients[1])

  #coeficiente b
  k <<- abs(as.double(regressao_ln$coefficients[2]))
  ln_b <<- as.double(regressao_ln$coefficients[2])

  resultadoLn <- paste("y = ",round(ln_b,3),"x + ",round(ln_a,4)," R² = ",round(r2_ln,3))
  graficoRLN(ln_x, ln_y, regressao_ln, labelLX, labelLY, mainNameD, resultadoLn, sexo)

  #t0
  tzero <<- ((ln_a-ln(c_infinito))/k)

  ford <<- c(c_infinito, k, tzero)
  names(ford) <<- c("c_infinito","k","tzero")

  return()
}
