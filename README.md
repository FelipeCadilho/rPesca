# rPesca: An Approach for Estimating Growth and Mortality in Fisheries Studies with Limited Data.
The centralization of mathematical methods in a single computational platform is essential for fisheries research. 
The rPesca package in R language gathers routines for age, growth, and mortality analysis of fishery resources, using 
limited data such as age and length. This information is crucial for the management of artisanal and industrial 
fisheries, enabling the integration of results and enhancing decision-making processes.

## Package Installation Instructions
### Prerequisites for Installing the Package
* It is recommended to use **RStudio** preferably.
* Make sure you have the `devtools` or `remotes` packages properly installed.
### How to Install
* If you have the `devtools` package installed, use the following command:
>devtools::install_github("FelipeCadilho/rPesca")

* If you have the `remotes` package installed, use the following command:
>remotes::install_github("FelipeCadilho/rPesca")

## Prerequisites to Run the Package
### Database
* The data file should contain age information in the first column, followed by length in the second column,
and finally, the categorical classification of gender, for example, in the third column.
* The file can have the extensions `.xls`, `.xlsx`, `.csv`, `.txt`, or be of type `dataframe` in R.
* It is necessary to inform the column separator character for data files with extensions `.csv` and `.txt`.

## Step-by-step to Execute the Package
1. Set the working directory in RStudio to the directory where the data file is located.
  * Set the directory using the `setwd()` command or through the Files panel in the `Files` tab.
>Example 1: setwd("C:/User/UserPC/Documents")

>Example 2: https://youtu.be/QzSV8wvA1Do

2. To load the package functions, execute the command `library(rPesca)` in RStudio.
3. Execute the `rPesca()` function by providing the parameters according to your data:
* `cores` Numeric field to determine how the graph will be displayed, where `1` represents grayscale and `2` represents colored. (Default: Grayscale)
* `idioma` Numeric field to define the language to be used in the questionnaire and the result, where `1` is Portuguese and `2` is English. (Default: Portuguese)
* `un` Numeric field to define the unit of measurement, where `1` is cm, `2` is mm, or `3` is m. (Default: cm)
* `tipoComprimento` Character field to indicate the type of applied length, for example, when `"Total"` is informed, it will be presented as Total Length.
* `tempo` Numeric field to define the unit of age measurement, where `1` is year, `2` is month, or `3` is day. (Default: Year)
* `tipo_dados` Numeric field to indicate the extension of the data file, where `1` is `.xls`/`.xlsx`, `2` is `.csv`, `3` is `.txt` (with header), or `4` is `dataframe`. (Default: Dataframe)
* `nome_dados` Character field to indicate the name of the data file, including its extension when necessary. Example: "data.xls" (Required)
* `planilha` Character field to indicate the sheet with the data within the `.xls`/`.xlsx` file. (Default: First sheet)
* `separador` Character field to indicate the separator used if the file is in `.csv` or `.txt`, for example, space `" "`, comma `","`, etc. (Default: Comma)
* `grupo` Numeric field to determine analysis in groups, where `1` is yes, and the absence indicates no.
* `nomeA` Character field to indicate the name of group A. (Required for group analysis)
* `nomeB` Character field to indicate the name of group B. (Required for group analysis)
* `Fonte` Character field to indicate the font family to be used in the graphs, where `NULL` is Times New Roman, `1` is Arial, or `2` is Courier New. (Default: Times New Roman)
* Note1 : When indicating a `dataframe`, there is no need to use quotation marks around the name.

* Examples of fillings with and without the name of each parameter:
>   rPesca(cores=2, idioma=2, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=eg_individual)

>   rPesca(2, 2, 1, "Total", 1, 4, eg_individual)

>   rPesca(cores=2, idioma=2, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=eg_group, grupo=1, nomeA="ara", nomeB="ita")

>   rPesca(2, 2, 1, "Total", 1, 4, eg_group, NULL, NULL, 1, "ara", "ita")
* Note 2: When providing values without specifying the parameter names in the `rPesca()` function, it is essential
to insert them according to the order in the parameter list. This ensures that the values are correctly assigned to the
parameters in the sequence they were defined in the function.

* Note 3: In cases where it is necessary to indicate `NULL` or there is no need to select a specific option, simply omit
the value and the corresponding parameter name in the `rPesca()` function, as long as you provide the names of the other parameters.
4. Answer the program's questions to customize the analysis according to your needs.

# rPesca: Uma Abordagem para a Estimativa de Crescimento e Mortalidade em Estudos Pesqueiros com Dados Limitados.
A centralização de métodos matemáticos em uma única plataforma computacional é essencial para pesquisas de recursos 
pesqueiros. O pacote rPesca em linguagem R reúne rotinas de análise de idade, crescimento e mortalidade de recursos 
pesqueiros, utilizando dados limitados como idade e comprimento. Essas informações são fundamentais para a gestão 
de pescarias artesanais e industriais, permitindo a integralização dos resultados e aprimorando a tomada de decisões.

## Instruções de instalação do pacote
### Pré-requisitos para instalar o pacote
* É recomendado utilizar o **RStudio** preferencialmente.
* Certifique-se de ter os pacotes `devtools` ou `remotes` devidamente instalados.
### Como instalar
* Caso tenha o pacote `devtools` instalado, utilize o seguinte comando:
>devtools::install_github("FelipeCadilho/rPesca")

* Caso tenha o pacote `remotes` instalado, utilize o seguinte comando:
>remotes::install_github("FelipeCadilho/rPesca")

## Pré-requisitos para executar o pacote
### Base de dados
* O arquivo de dados deve apresentar, na primeira coluna, as informações de idade, seguidas pelo comprimento na segunda coluna e, finalmente, a classificação categórica de gênero sexual, por exemplo, na terceira coluna.
* O arquivo pode ter as extensões `.xls`, `.xlsx`, `.csv`, `.txt` ou ser do tipo objeto `dataframe` do R.
* É necessário informar o caractere separador de colunas do arquivo de dados com as extensões `.csv` e `.txt`.

## Passo a passo para executar o pacote
1. Configure a pasta de trabalho no RStudio para o diretório onde o arquivo de dados está localizado.
    - Configure o diretório usando o comando `setwd()` ou pelo painel de visualização na aba `Files`.
>Exemplo 1: setwd("C:/User/Usuário da Máquina/Documents")

>Exemplo 2: https://youtu.be/lnoQcMe63oA?t=156
2. Para carregar as funções do pacote, execute o comando `library(rPesca)` no console do RStudio.
3. Realize a execução da função `rPesca()` informando os parâmetros de acordo com os seus dados.
    - `cores` Campo numérico para determinar como o gráfico será exibido, onde `1` representa escala de cinza e `2` representa colorido. (Padrão: Escala de cinza)
    - `idioma` Campo numérico para definir o idioma a ser utilizado no questionário e no resultado, sendo `1` para português e `2` para inglês. (Padrão: Português)
    - `un` Campo numérico para definir a unidade de medida, sendo `1` para cm, `2` para mm ou `3` para m. (Padrão: cm)
    - `tipoComprimento` Campo de caractere para indicar o tipo de comprimento aplicado, por exemplo, ao informar `"Total"`, será apresentado como Comprimento Total.
    - `tempo` Campo numérico para definir a unidade de medida da idade, sendo `1` para ano, `2` para mês ou `3` para dia. (Padrão: Ano)
    - `tipo_dados` Campo numérico para indicar a extensão do arquivo com os dados, sendo `1` para `.xls`/`.xlsx`, `2` para `.csv`, `3` para `.txt` (com cabeçalho) ou `4` para `dataframe`. (Padrão: Dataframe)
    - `nome_dados` Campo de caractere para indicar o nome do arquivo com os dados, incluindo sua extensão quando devido. Exemplo: "dados.xls" (Preenchimento obrigatório)
    - `planilha` Campo de caractere para indicar a pasta com os dados dentro da planilha `.xls`/`.xlsx`. (Padrão: Primeira pasta)
    - `separador` Campo de caractere para indicar o separador utilizado caso o arquivo esteja em `.csv` ou `.txt`, por exemplo, espaço `" "`, vírgula `","`, etc. (Padrão: Vírgula)
    - `grupo` Campo numérico para determinar analise em grupos, onde `1` é sim e o não preenchimento indica não.
    - `nomeA` Campo de caractere para indicar do grupo A. (Obrigatório para análise de grupos)
    - `nomeB` Campo de caractere para indicar o nome do grupo B. (Obrigatório para análise de grupos)
    - `Fonte` Campo de caractere para indicar a família da fonte a ser utilizada nos gráficos, onde `NULL` é Times New Roman, `1` é Arial ou `2` é Courier New. (Padrão: Times New Roman)
*  Observação 1: Quando for necessário indicar um arquivo `dataframe`, não é preciso colocar o nome entre aspas. 
*  Exemplos de preenchimentos com e sem o nome de cada parâmetro:
>   rPesca(cores=2, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=ex_individual)

>   rPesca(2, 1, 1, "Total", 1, 4, ex_individual)

>   rPesca(cores=2, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=ex_grupo, grupo=1, nomeA="ara", nomeB="ita")

>   rPesca(2, 1, 1, "Total", 1, 4, ex_grupo, NULL, NULL, 1, "ara", "ita")
*  Observação 2: Em preenchimentos sem o nome de cada parâmetro, é necessário inserir os valores de acordo com a ordem da lista dos parâmetros na função `rPesca()`. Isso garante que os valores sejam atribuídos corretamente aos parâmetros na sequência em que foram definidos na função.
*  Observação 3: Em casos em que for necessário indicar `NULL` ou não houver necessidade de selecionar uma opção específica, basta não incluir o valor e o nome do parâmetro correspondente na função `rPesca()`, desde que esteja fornecendo os nomes dos demais parâmetros.

4.  Responda às perguntas do programa para personalizar a análise de acordo com suas necessidades.
