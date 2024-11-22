TidyModels - Bootstrap resampling and tidy regression models
================
Philipe Dias
2024-11-21

# Replicação de um dos exemplos de ensino do site tidymodels.org, que demonstra a execução do método de *bootstraping* em um modelo de regressão não linear

## 1. Modelos de Regressão

Utilizando a base inclusa no R base *mtcars*, primeiro analisamos a
distribuição de pontos entre peso (*wt*) e quilômetros rodados (*mpg*)
dos carros presentes nos dados, para então utilizar um modelo de
**mínimos quadrados não lineares** para analisar o efeito do peso sobre
o número de quilômetros rodados:

``` r
ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  theme_bw()
```

![](TidyModels---Replicação-2---Bootstrap-resampling-and-tidy-regression-models_files/figure-gfm/Gráfico%20e%20Regressão-1.png)<!-- -->

``` r
regnl <- nls(mpg ~ k / wt + b, mtcars, start = list(k = 1, b = 0))
summary(regnl)
```

    ## 
    ## Formula: mpg ~ k/wt + b
    ## 
    ## Parameters:
    ##   Estimate Std. Error t value Pr(>|t|)    
    ## k   45.829      4.249  10.786 7.64e-12 ***
    ## b    4.386      1.536   2.855  0.00774 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.774 on 30 degrees of freedom
    ## 
    ## Number of iterations to convergence: 1 
    ## Achieved convergence tolerance: 6.813e-09

``` r
tidy(regnl)
```

    ## # A tibble: 2 × 5
    ##   term  estimate std.error statistic  p.value
    ##   <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 k        45.8       4.25     10.8  7.64e-12
    ## 2 b         4.39      1.54      2.85 7.74e- 3

O método de MQNL é semelhante ao mais utilizado para regressões
lineares, **mínimos quadrados ordinários (MQO)**. Os dois buscam
minimizar a soma dos quadrados do resíduo, porém, enquanto MQO é
utilizado para combinações lineares, o MQNL serve para parâmetros não
lineares, como aparenta ser a relação entre *mpg* e *wt*. Observando
pelo p-valor encontrado, tanto o parâmetro *k* quanto *b* são
estatísticamente relevantes.

Para melhor visualizar, plotamos a relação predita pelo modelo junto com
os dados, como no gráfico anterior:

``` r
ggplot(mtcars, aes(wt,mpg)) +
  geom_point() +
  geom_line(aes(y = predict(regnl))) +
  theme_bw()
```

![](TidyModels---Replicação-2---Bootstrap-resampling-and-tidy-regression-models_files/figure-gfm/Modelo%20+%20Dados%20em%20Gráfico-1.png)<!-- -->

Pelo gráfico, vemos que o modelo estimado se aproxima da distribuição
dos pontos da base de dados. Porém, dado que ele foi construído e
testado com somente um conjunto de dados, é possível que, se aplicado à
outra base semelhante, o modelo não se ajuste bem, tornando-o
ineficiente. É aí que entra o **Bootstrap**

## 2. Modelos de *Bootstrap*

A fim de tornar os modelos criados eficientes para analisar diversas
bases de dados com informações diferentes, o *bootstrap* **seleciona,
aleatoriamente, amostras de uma base de dados (com reposição)**, de
forma que a variação dos resultados estimados por cada amostra se
aproxima da variância da própria estimativa inicial. Dessa forma, é
possível criar um **intervalo de confiança** da estimativa do modelo
utilizado.

Para fazer isso com a regressão não linear criada anteriormente,
primeiro precisamos dividir a base em réplicas aleatórias com reposição.
Para isso, podemos utilizar o comando *bootstraps()*:

``` r
set.seed(27)

boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)
boots
```

    ## # Bootstrap sampling with apparent sample 
    ## # A tibble: 2,001 × 2
    ##    splits          id           
    ##    <list>          <chr>        
    ##  1 <split [32/13]> Bootstrap0001
    ##  2 <split [32/10]> Bootstrap0002
    ##  3 <split [32/13]> Bootstrap0003
    ##  4 <split [32/11]> Bootstrap0004
    ##  5 <split [32/9]>  Bootstrap0005
    ##  6 <split [32/10]> Bootstrap0006
    ##  7 <split [32/11]> Bootstrap0007
    ##  8 <split [32/13]> Bootstrap0008
    ##  9 <split [32/11]> Bootstrap0009
    ## 10 <split [32/11]> Bootstrap0010
    ## # ℹ 1,991 more rows

Agora, realizamos 2000 iterações de *bootstrapping* com a base *mtcars*,
dividindo as amostras entre os **dados de treinamento**, os quais serão
utilizados para construir a regressão, e os **dados de teste**, que
serão responsáveis por mostrar e o modelo se encaixa bem a um novo
conjunto de dados.

Para aplicar a regressão em cada amostra, podemos criar uma função que,
repetidamente, passe por cada coluna e devolva os parâmetros encontrados
por base de dados, o que pode ser facilitado pelo comando *map()*, que
será responsável pela iteração da regressão. A fim de observar melhor os
dados, é possível utilizar o comando *tidy()* em conjunto com *map()*,
que organizará os parâmetros. Por fim, para observar essas informações,
precisaremos utilizar a função *unnest()*, que tornará a coluna de
informações da regressão, inicialmente uma lista, em diversas colunas,
uma com cada informação relevante:

``` r
reg_nls_para_bootstrap <- function(split) {
  nls(mpg ~ k / wt + b, analysis(split), start = list(k = 1, b = 0))
}

boots_com_modelo <- boots |> 
  mutate(
    modelo = map(splits, reg_nls_para_bootstrap),
    tidied = map(modelo, tidy)
  )

boot_resultados <- boots_com_modelo |> 
  unnest(tidied)

boot_resultados
```

    ## # A tibble: 4,002 × 8
    ##    splits          id         modelo term  estimate std.error statistic  p.value
    ##    <list>          <chr>      <list> <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 <split [32/13]> Bootstrap… <nls>  k        42.1       4.05     10.4  1.91e-11
    ##  2 <split [32/13]> Bootstrap… <nls>  b         5.39      1.43      3.78 6.93e- 4
    ##  3 <split [32/10]> Bootstrap… <nls>  k        49.9       5.66      8.82 7.82e-10
    ##  4 <split [32/10]> Bootstrap… <nls>  b         3.73      1.92      1.94 6.13e- 2
    ##  5 <split [32/13]> Bootstrap… <nls>  k        37.8       2.68     14.1  9.01e-15
    ##  6 <split [32/13]> Bootstrap… <nls>  b         6.73      1.17      5.75 2.78e- 6
    ##  7 <split [32/11]> Bootstrap… <nls>  k        45.6       4.45     10.2  2.70e-11
    ##  8 <split [32/11]> Bootstrap… <nls>  b         4.75      1.62      2.93 6.38e- 3
    ##  9 <split [32/9]>  Bootstrap… <nls>  k        43.6       4.63      9.41 1.85e-10
    ## 10 <split [32/9]>  Bootstrap… <nls>  b         5.89      1.68      3.51 1.44e- 3
    ## # ℹ 3,992 more rows

Assim, com os coeficientes da regressão de cada uma das amostras, é
possível criar um **intervalo de confiança** para os valores dos
parâmetros, utilizando o **método dos percentis**, comumente utilizado
para intervalos criados a partir de amostras de *bootstrap*, tanto a
partir do comando *int_pctl()*, quanto visualmente a partir de
histogramas:

``` r
ic_percentis <- int_pctl(boots_com_modelo, tidied)
ic_percentis
```

    ## # A tibble: 2 × 6
    ##   term   .lower .estimate .upper .alpha .method   
    ##   <chr>   <dbl>     <dbl>  <dbl>  <dbl> <chr>     
    ## 1 b      0.0475      4.12   7.31   0.05 percentile
    ## 2 k     37.6        46.7   59.8    0.05 percentile

``` r
ggplot(boot_resultados, aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap( ~ term, scales = 'free') +
  geom_vline(aes(xintercept = .lower), data = ic_percentis, col = 'darkblue') +
  geom_vline(aes(xintercept = .upper), data = ic_percentis, col = 'darkblue') +
  theme_bw()
```

![](TidyModels---Replicação-2---Bootstrap-resampling-and-tidy-regression-models_files/figure-gfm/Intervalo%20de%20Confiança%20-%20Infos%20+%20Histograma-1.png)<!-- -->

No código do histograma, o parâmetro *bins* dentro de *geom_histogram()*
controla o número de **faixas** de cada histograma, enquanto o comando
*facet_wrap()* possibilita a criação de *subplots*, ou seja, de mais de
um gráfico no mesmo espaço, subdivididos pelo *term*, ou seja, pelos
parâmetros do modelo de regressão, $b$ e $k$, e com possibilidade de
terem diferentes escalas no eixo x, dado pelo parâmetro *scales*. Ele
também cria duas linhas verticais em cada histograma, baseadas nos
limites inferior e superior, definidos pelo comando *int_pctl*, a partir
do comando *geom_vline()*.

## 3. Visualização do *fit* do Modelo

Para analisar melhor como os modelos se encaixaram nos dados de cada
*bootstrap*, podemos utilizar o comando *augment()*. Como são muitas
amostras, pegaremos somente 200 como exemplos, tanto a partir de seus
valores quanto a partir do gráfico que mostre todas as curvas sobre os
pontos da base:

``` r
boot_augmented <- boots_com_modelo |> 
  sample_n(200) |> 
  mutate(
    augmented = map(modelo, augment)
  ) |> 
  unnest(augmented)
boot_augmented
```

    ## # A tibble: 6,400 × 8
    ##    splits          id            modelo tidied     mpg    wt .fitted .resid
    ##    <list>          <chr>         <list> <list>   <dbl> <dbl>   <dbl>  <dbl>
    ##  1 <split [32/11]> Bootstrap1644 <nls>  <tibble>  16.4  4.07    15.6  0.829
    ##  2 <split [32/11]> Bootstrap1644 <nls>  <tibble>  19.7  2.77    21.9 -2.21 
    ##  3 <split [32/11]> Bootstrap1644 <nls>  <tibble>  19.2  3.84    16.4  2.84 
    ##  4 <split [32/11]> Bootstrap1644 <nls>  <tibble>  21.4  2.78    21.8 -0.437
    ##  5 <split [32/11]> Bootstrap1644 <nls>  <tibble>  26    2.14    27.8 -1.75 
    ##  6 <split [32/11]> Bootstrap1644 <nls>  <tibble>  33.9  1.84    32.0  1.88 
    ##  7 <split [32/11]> Bootstrap1644 <nls>  <tibble>  32.4  2.2     27.0  5.35 
    ##  8 <split [32/11]> Bootstrap1644 <nls>  <tibble>  30.4  1.62    36.1 -5.70 
    ##  9 <split [32/11]> Bootstrap1644 <nls>  <tibble>  21.5  2.46    24.4 -2.86 
    ## 10 <split [32/11]> Bootstrap1644 <nls>  <tibble>  26    2.14    27.8 -1.75 
    ## # ℹ 6,390 more rows

``` r
ggplot(boot_augmented, aes(wt, mpg)) +
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = 'red') +
  geom_point() +
  theme_bw()
```

![](TidyModels---Replicação-2---Bootstrap-resampling-and-tidy-regression-models_files/figure-gfm/Fit%20dos%20Modelos%20-%20Valores%20+%20Gráfico-1.png)<!-- -->

Para criar o gráfico, dentro de *geom_line()*, explicitamos que a
variável que deverá ser utilizada para formar as curvas é *.fitted*, com
os valores de ajuste de cada modelo, e que, para cada *id*, ou seja,
cada amostra de *bootstrap*, uma nova curva deve ser criada.

## 4. Extrapolação para mais Modelos

Utilizamos a visualização do ajuste do modelo para regressões não
lineares, porém também podemos generalizar tal processo. Por exemplo, se
fosse escolhido utilizar o método de **spline cúbico suavizado**, também
muito utilizado para relações não lineares, a partir do comando
*smooth.spline()*, o processo completo ficaria parecido:

``` r
ajuste_spline = function(split) {
  dados = analysis(split)
  smooth.spline(dados$wt, dados$mpg, df = 4)
}

boot_spline <- boots |> 
  sample_n(200) |> 
  mutate(
    spline = map(splits, ajuste_spline),
    aug_spline = map(spline, augment)
  )

splines_aug <- 
  boot_spline |> 
  unnest(aug_spline)

splines_aug
```

    ## # A tibble: 6,400 × 8
    ##    splits          id            spline         x     y     w .fitted  .resid
    ##    <list>          <chr>         <list>     <dbl> <dbl> <dbl>   <dbl>   <dbl>
    ##  1 <split [32/10]> Bootstrap0208 <smth.spl>  3.17  15.8     1    18.0 -2.22  
    ##  2 <split [32/10]> Bootstrap0208 <smth.spl>  3.15  22.8     1    18.1  4.67  
    ##  3 <split [32/10]> Bootstrap0208 <smth.spl>  3.22  21.4     1    17.8  3.63  
    ##  4 <split [32/10]> Bootstrap0208 <smth.spl>  5.34  14.7     1    13.9  0.799 
    ##  5 <split [32/10]> Bootstrap0208 <smth.spl>  3.17  15.8     1    18.0 -2.22  
    ##  6 <split [32/10]> Bootstrap0208 <smth.spl>  3.57  14.3     1    16.0 -1.69  
    ##  7 <split [32/10]> Bootstrap0208 <smth.spl>  3.78  15.2     1    15.2  0.0335
    ##  8 <split [32/10]> Bootstrap0208 <smth.spl>  1.84  33.9     1    30.0  3.93  
    ##  9 <split [32/10]> Bootstrap0208 <smth.spl>  3.57  15       1    16.0 -0.986 
    ## 10 <split [32/10]> Bootstrap0208 <smth.spl>  2.62  21       1    21.9 -0.862 
    ## # ℹ 6,390 more rows

``` r
ggplot(splines_aug, aes(x, y)) +
  geom_line(aes(y = .fitted, group = id), alpha = 0.2, col = 'blue') +
  geom_point() +
  labs(x = 'wt', y = 'mpg') +
  theme_bw()
```

![](TidyModels---Replicação-2---Bootstrap-resampling-and-tidy-regression-models_files/figure-gfm/Workflow%20com%20Modelo%20Diferente-1.png)<!-- -->

Diferente da regressão, o spline suavizado **cria uma curva mais
ajustada ao longo do tempo**, ao invés de forçar uma relação específica,
como o método de mínimos quadrados não lineares.
