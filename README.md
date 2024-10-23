# rPesca
The rPesca package was developed to facilitate the analysis of growth and mortality data in fish populations. It provides an interactive interface that guides users through questionnaires, allowing for a dynamic and accessible workflow.

Growth analysis is fundamental to understanding the biology of species, while mortality calculations are essential for the sustainable management of fisheries. The rPesca package combines these aspects, enabling researchers and fishery managers to conduct complex analyses simply and effectively.

## Description
The `rPesca` package follows a dynamic routine where the script interacts with the user through questionnaires. The steps in the routine include:

Removal of discrepant data (non-outliers)
Calculation of growth parameters (Linf, k, t0) using the Ford-Walford method
Calculation of growth curves (von Bertalanffy, Gompertz, Logistic)
Adjustment of curves and parameters via the Levenberg-Marquardt method
Mortality calculations (Z, M, F, E, Fopt, Flimit) – total mortality calculated from age or length
Likelihood test (Kimura 1980) for two groups (female and male)
Generation of plots for each step, with some plots adjustable by the user

---
## Installation Prerequisites

It is recommended to use RStudio. Make sure to have one of the packages below installed:

### Installation with `devtools`

```r
devtools::install_github("FelipeCadilho/rPesca")
```
#### If `remotes` is installed:

```r
remotes::install_github("FelipeCadilho/rPesca")
```

## Database

The data file must contain the following columns:

- **Column 1**: Age
- **Column 2**: Length
- **Column 3**: Gender (categorical classification) (Optional)

### Supported Formats:

- `.xls`, `.xlsx`, `.csv`, `.txt` or **dataframe** in R

**Important**: For `.csv` and `.txt` files, it is necessary to specify the column separator character.

## Step by Step to Run the Package

1. Set the working directory in RStudio to the location where the data file is stored.

    **Example:**

    ```r
    setwd("C:/User/User_name/Documents")
    ```

2. Load the package functions with the command:

    ```r
    library(rPesca)
    ```

3. run the `rPesca()` function, providing the parameters appropriate for your data.

## Function Parameters `rPesca`

The `rPesca()` function has the following parameters:

- **cores**: Numeric field to determine how the chart will be displayed (1 for grayscale, 2 for color). **(Default: Grayscale)**
- **idioma**: Numeric field to set the language for the questionnaire and results (1 for Portuguese, 2 for English). **(Default: Portuguese)**
- **un**: Numeric field to set the unit of measurement (1 for cm, 2 for mm, 3 for m). **(Default: cm)**
- **tipoComprimento**: Character field to indicate the type of length (e.g., "Total").
- **tempo**: Numeric field to set the unit of age measurement (1 for year, 2 for month, 3 for day). **(Default: Year)**
- **tipo_dados**: Numeric field to indicate the file extension for the data (1 for .xls/.xlsx, 2 for .csv, 3 for .txt (with header), 4 for dataframe).  **(Default: Dataframe)**
- **nome_dados**: Character field to indicate the name of the file with the data, including the extension.  **(Required)**
- **planilha**: Character field to indicate the sheet with the data inside the .xls/.xlsx file. **(Default: First sheet)**
- **separador**: Character field to indicate the separator used if the file is in .csv or .txt format (e.g., space " ", comma ",", etc.). **(Default: Comma)**
- **grupo**: Numeric field to determine if group analysis will be performed (1 for yes, leave blank for no).
- **nomeA**: Character field to indicate the name of group A. **(Required for group analysis)**
- **nomeB**: Character field to indicate the name of group B. **(Required for group analysis)**
- **Fonte**: Character field to indicate the font family to be used in the charts (NULL for Times New Roman, 1 for Arial, 2 for Courier New). **(Default: Times New Roman)**

### Notes

1. When using a dataframe, it is not necessary to place the name in quotes.
2. If parameters are entered without specifying the name of each, the values must be entered in the order of the parameter list in the `rPesca()` function.
3. When indicating `NULL` or when it is unnecessary to select a specific option, simply omit the corresponding parameter and its value, provided that the names of the other parameters are supplied.

## Usage Examples

### Example 1: Usage with Parameter Names

```r
rPesca(cores=2, idioma=2, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=ex_individual)
```
### Example 2: Usage without Parameter Names

```r
rPesca(2, 2, 1, "Total", 1, 4, ex_individual)
```
Example 3: Group Analysis with Parameter Names

```r
rPesca(cores=2, idioma=2, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=ex_grupo, grupo=1, nomeA="ara", nomeB="ita")
```
Example 4: Group Analysis without Parameter Names
```r
rPesca(2, 2, 1, "Total", 1, 4, ex_grupo, NULL, NULL, 1, "ara", "ita")
```
### Notes

- When using parameters without names, the values must be entered in the order of the parameter list in the `rPesca()`function. This ensures that values are correctly assigned to the corresponding parameters as defined in the function.
- When it is necessary to indicate `NULL` ou não houver necessidade de selecionar uma opção específica, basta não incluir o valor e o nome do parâmetro correspondente na função.

## Complete Usage Example

Here is a step-by-step example of how to use the **rPesca** package:

1. **Prepare the data**: Create a file named `fisheries_data.xlsx` with the following columns: age, length, and gender. An example of the file's content:

age | length | gender 
------|-------------|--------
1| 2.30 | 3.35|M

2. **Set the working directory**:
```r
setwd("C:/User/User_name/Documents")
```
3. **Load the package**:

```r
library(rPesca)
```
4. **Run the rPesca function**:

```r
rPesca(cores=2, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=1, nome_dados="dados_pesca.xlsx")
```
**Interpret the results**: After execution, the package will generate graphs and reports based on the provided data, allowing you to analyze growth and mortality results.

## Common Errors and Solutions

- **Error: "File not found"**  
  **Solution:** Check if the working directory is set correctly and ensure that the file name is correct, including the extension.

- **Error: "Inconsistent data"**  
  **Solution:** Make sure your data file has the columns in the correct order and with appropriate data types (age as numeric, length as numeric, and gender as categorical).

- **Erro: "Função não encontrada"**  
  **Solution:** Check if you have loaded the package using `library(rPesca)` before attempting to run the function.

## Frequently Asked Questions (FAQs)

- **Can I use the package with data from other fish species?**  
  Yes, **rPesca** is designed to be flexible and can be used with different species, as long as the data is correctly formatted.

- **What should I do if the chart does not appear?**  
  Check if your R graphical environment is functioning correctly. You can try using `dev.off()` to reset the graphical device.

- **How can I adjust the chart parameters?**  
  You can customize the appearance of the charts through the parameters provided in the `rPesca()` function, such as the choice of colors and font types.

## Glossary

- **c_infinito** or **Linf**: Asymptotic length, the maximum size a fish can reach.
- **k**: Growth rate, indicates how fast a fish reaches its maximum length.
- **tzero** or **t0**: The time at which the fish's length would be zero if the growth curve were extended into the past.

## Best Practices Tips

- **Data Organization**: Always keep your data organized and name your files clearly. Use consistent naming conventions.
- **Documentation**: Document your code and analyses. This will make it easier to reuse and understand in the future.
- **Test with Small Data**: Before running analyses on large datasets, test your workflow with a smaller dataset to ensure everything works properly.

## Plot Customization

The **rPesca** package allows for chart customization through the following parameters:

- **cores**: Choose between grayscale (1) or color (2).
- **Fonte**: Select the font family for the charts. You can choose between Times New Roman (NULL), Arial (1), or Courier New (2).

Customization example:
```r
rPesca(cores=2, Fonte=1, ...)
```

# Mortality Calculation Outside the Main Routine
It is possible to independently calculate total mortality using:

### By Age:
```r
mortalidadeZ(c_infinito=60.44, k=0.19, tzero=-0.74, dataframe, idioma=2, adhoc=2)
```
### By Length:
```r
mortalidadeZ(c_infinito=60.44, k=0.19, tzero=-0.74, dataframe, idioma=2, adhoc=1)
```
The asymptotic length (c_infinito), growth rate (k), and age at zero length (t0) data are optional when calculating by age. However, for length-based calculations, they are mandatory.

# rPesca

O pacote **rPesca** foi desenvolvido para facilitar a análise de dados de crescimento e mortalidade em populações de peixes. Ele oferece uma interface interativa que guia os usuários através de questionários, permitindo um fluxo de trabalho dinâmico e acessível.

A análise de crescimento é fundamental para entender a biologia das espécies, enquanto os cálculos de mortalidade são essenciais para a gestão sustentável da pesca. O **rPesca** combina esses aspectos, permitindo aos pesquisadores e gestores de pesca realizar análises complexas de maneira simples e eficaz.

## Descrição

O pacote rPesca segue uma rotina dinâmica onde o script interage com o usuário a partir de questionários. As etapas da rotina incluem:

1. Remoção de dados discrepantes (não outliers)
2. Cálculo dos parâmetros de crescimento (linf, k, t0) utilizando o método Ford-Walford
3. Cálculo das curvas de crescimento (von Bertalanffy, Gompertz, Logística)
4. Ajuste das curvas e parâmetros via método Levenberg-Marquardt
5. Cálculos de mortalidade (Z, M, F, E, Fopt, Flimit) – mortalidade total calculada a partir de idade ou comprimento
6. Teste de verossimilhança (Kimura 1980) para dois grupos (feminino e masculino)
7. Geração de gráficos para cada etapa, com alguns gráficos ajustáveis pelo usuário

---

## Pré-requisitos de Instalação

Recomenda-se o uso do RStudio. Certifique-se de ter um dos pacotes abaixo instalados:

### Instalação com `devtools`

```r
devtools::install_github("FelipeCadilho/rPesca")
```
#### Se `remotes` estiver instalado:

```r
remotes::install_github("FelipeCadilho/rPesca")
```
## Base de Dados

O arquivo de dados deve conter as seguintes colunas:

- **Coluna 1**: Idade
- **Coluna 2**: Comprimento
- **Coluna 3**: Gênero (classificação categórica)(Opcional)

### Formatos Suportados:

- `.xls`, `.xlsx`, `.csv`, `.txt` ou **dataframe** no R

**Importante**: Para arquivos `.csv` e `.txt`, é necessário informar o caractere separador de colunas.

## Passo a Passo para Executar o Pacote

1. Defina o diretório de trabalho no RStudio para o local onde o arquivo de dados está armazenado.

    **Exemplo:**

    ```r
    setwd("C:/User/Usuario/Documents")
    ```

2. Carregue as funções do pacote com o comando:

    ```r
    library(rPesca)
    ```

3. Execute a função `rPesca()` informando os parâmetros adequados aos seus dados.

## Parâmetros da Função `rPesca`

A função `rPesca()` possui os seguintes parâmetros:

- **cores**: Campo numérico para determinar como o gráfico será exibido (1 para escala de cinza, 2 para colorido). **(Padrão: Escala de cinza)**
- **idioma**: Campo numérico para definir o idioma do questionário e dos resultados (1 para português, 2 para inglês). **(Padrão: Português)**
- **un**: Campo numérico para definir a unidade de medida (1 para cm, 2 para mm, 3 para m). **(Padrão: cm)**
- **tipoComprimento**: Campo de caractere para indicar o tipo de comprimento (exemplo: "Total").
- **tempo**: Campo numérico para definir a unidade de medida da idade (1 para ano, 2 para mês, 3 para dia). **(Padrão: Ano)**
- **tipo_dados**: Campo numérico para indicar a extensão do arquivo com os dados (1 para .xls/.xlsx, 2 para .csv, 3 para .txt (com cabeçalho), 4 para dataframe). **(Padrão: Dataframe)**
- **nome_dados**: Campo de caractere para indicar o nome do arquivo com os dados, incluindo a extensão. **(Preenchimento obrigatório)**
- **planilha**: Campo de caractere para indicar a pasta com os dados dentro da planilha .xls/.xlsx. **(Padrão: Primeira pasta)**
- **separador**: Campo de caractere para indicar o separador utilizado caso o arquivo esteja em .csv ou .txt (exemplo: espaço " ", vírgula ",", etc.). **(Padrão: Vírgula)**
- **grupo**: Campo numérico para determinar análise em grupos (1 para sim, preenchimento vazio para não).
- **nomeA**: Campo de caractere para indicar o nome do grupo A. **(Obrigatório para análise de grupos)**
- **nomeB**: Campo de caractere para indicar o nome do grupo B. **(Obrigatório para análise de grupos)**
- **Fonte**: Campo de caractere para indicar a família da fonte a ser utilizada nos gráficos (NULL para Times New Roman, 1 para Arial, 2 para Courier New). **(Padrão: Times New Roman)**

### Observações

1. Quando for necessário indicar um arquivo dataframe, não é preciso colocar o nome entre aspas.
2. Nos preenchimentos sem o nome de cada parâmetro, é necessário inserir os valores na ordem da lista de parâmetros na função `rPesca()`.
3. Em casos em que for necessário indicar `NULL` ou não houver necessidade de selecionar uma opção específica, basta não incluir o valor e o nome do parâmetro correspondente na função `rPesca()`, desde que esteja fornecendo os nomes dos demais parâmetros.

## Exemplos de Uso

### Exemplo 1: Uso com Nomes de Parâmetros

```r
rPesca(cores=2, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=ex_individual)
```
### Exemplo 2: Uso sem Nomes de Parâmetros

```r
rPesca(2, 1, 1, "Total", 1, 4, ex_individual)
```
### Exemplo 3: Análise em Grupos com Nomes de Parâmetros

```r
rPesca(cores=2, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=4, nome_dados=ex_grupo, grupo=1, nomeA="ara", nomeB="ita")
```
### Exemplo 4: Análise em Grupos sem Nomes de Parâmetros
```r
rPesca(2, 1, 1, "Total", 1, 4, ex_grupo, NULL, NULL, 1, "ara", "ita")
```
### Observações

- Em preenchimentos sem o nome de cada parâmetro, é necessário inserir os valores de acordo com a ordem da lista dos parâmetros na função `rPesca()`. Isso garante que os valores sejam atribuídos corretamente aos parâmetros na sequência em que foram definidos na função.
- Em casos em que for necessário indicar `NULL` ou não houver necessidade de selecionar uma opção específica, basta não incluir o valor e o nome do parâmetro correspondente na função `rPesca()`, desde que esteja fornecendo os nomes dos demais parâmetros.


## Exemplo Completo de Uso

Aqui está um exemplo passo a passo de como utilizar o pacote **rPesca**:

1. **Preparar os dados**: Crie um arquivo chamado `dados_pesca.xlsx` com as seguintes colunas: idade, comprimento e gênero. Um exemplo de conteúdo do arquivo:

idade | comprimento | genero 
------|-------------|--------
1| 2.30 | 3.35|M

2. **Configurar o diretório de trabalho**:
```r
setwd("C:/User/SeuUsuario/Documents")
```
3. **Carregar o pacote**:

```r
library(rPesca)
```
4. **Executar a função rPesca**:

```r
rPesca(cores=2, idioma=1, un=1, tipoComprimento="Total", tempo=1, tipo_dados=1, nome_dados="dados_pesca.xlsx")
```
**Interpretar os resultados**: Após a execução, o pacote gerará gráficos e relatórios baseados nos dados fornecidos, permitindo que você analise os resultados de crescimento e mortalidade.

## Erros Comuns e Soluções

- **Erro: "Arquivo não encontrado"**  
  **Solução:** Verifique se o diretório de trabalho foi configurado corretamente e se o nome do arquivo está correto, incluindo a extensão.

- **Erro: "Dados inconsistentes"**  
  **Solução:** Certifique-se de que seu arquivo de dados possui as colunas na ordem correta e com os tipos de dados apropriados (idade como numérico, comprimento como numérico e gênero como categórico).

- **Erro: "Função não encontrada"**  
  **Solução:** Verifique se você carregou o pacote usando `library(rPesca)` antes de tentar executar a função.

## Perguntas Frequentes (FAQs)

- **Posso usar o pacote com dados de outras espécies de peixes?**  
  Sim, o **rPesca** foi projetado para ser flexível e pode ser utilizado com diferentes espécies, desde que os dados estejam formatados corretamente.

- **O que devo fazer se o gráfico não aparecer?**  
  Verifique se o seu ambiente gráfico do R está funcionando corretamente. Você pode tentar usar `dev.off()` para resetar o dispositivo gráfico.

- **Como posso ajustar os parâmetros do gráfico?**  
  Você pode personalizar a aparência dos gráficos através dos parâmetros fornecidos na função `rPesca()`, como a escolha de cores e tipos de fonte.

## Glossário

- **c_infinito** ou **Linf**: Comprimento assintótico, é o tamanho máximo que um peixe pode atingir.
- **k**: Taxa de crescimento, indica a rapidez com que um peixe atinge seu comprimento máximo.
- **tzero** ou **t0**: O tempo no qual o comprimento do peixe seria zero se a curva de crescimento fosse estendida ao passado.

## Dicas de Melhores Práticas

- **Organização dos Dados**: Sempre mantenha seus dados organizados e nomeie seus arquivos de forma clara. Use convenções de nomenclatura consistentes.
- **Documentação**: Documente seu código e análises. Isso facilitará a reutilização e compreensão futura.
- **Teste com Dados Pequenos**: Antes de executar análises em grandes conjuntos de dados, teste seu fluxo de trabalho com um conjunto de dados menor para verificar se tudo funciona

## Personalização do Gráfico

O pacote **rPesca** permite a personalização dos gráficos através dos seguintes parâmetros:

- **cores**: Escolha entre escala de cinza (1) ou colorido (2).
- **Fonte**: Selecione a família da fonte para os gráficos. Você pode escolher entre Times New Roman (NULL), Arial (1) ou Courier New (2).

Exemplo de personalização:
```r
rPesca(cores=2, Fonte=1, ...)
```
# Cálculo de Mortalidade Fora da Rotina Principal
É possível calcular a mortalidade total de forma independente usando:

### Por Idade:
```r
mortalidadeZ(c_infinito=60.44, k=0.19, tzero=-0.74, dataframe, idioma=1, adhoc=2)
```
### Por Comprimento:
```r
mortalidadeZ(c_infinito=60.44, k=0.19, tzero=-0.74, dataframe, idioma=1, adhoc=1)
```
Os dados de comprimento assintótico(c_infinito), taxa de crescimento(k) e idade no tamanho zero(t0) são opcionais quando for calcular por idade. No entanto, para calcular pelo comprimento são obrigatórios.
