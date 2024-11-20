TidyModels - Correlation and regression fundamentals with tidy data
principles
================
Philipe Dias
2024-11-19

# Replicação de um dos exemplos de ensino do site tidymodels.org, mostrando os fundamentos de análises de correlação e de regressão

## 1. Análise de Correlação

Primeiro, fazendo uma análise prévia da base Orange, tornando-a uma base
*tibble*:

``` r
data(Orange)

Orange <- as_tibble(Orange)

head(Orange)
```

    ## # A tibble: 6 × 3
    ##   Tree    age circumference
    ##   <ord> <dbl>         <dbl>
    ## 1 1       118            30
    ## 2 1       484            58
    ## 3 1       664            87
    ## 4 1      1004           115
    ## 5 1      1231           120
    ## 6 1      1372           142

A base, portanto, possui informações de diferentes árvores em diferentes
idades e, com cada idade, uma circunferência diferente. Sabendo que,
conforme a árvore cresce, sua circunferência também, pode-se fazer uma
**análise da correlação** entre os dois:

``` r
corr <- cor(Orange$age, Orange$circumference)
paste('Correlação entre a idade da árvore e sua circunferência é de', corr)
```

    ## [1] "Correlação entre a idade da árvore e sua circunferência é de 0.913518852891591"

Para evidenciar ainda mais, é possível fazer um gráfico da trajetória da
circunferência das árvores ao longo do tempo, usando a biblioteca
*ggplot*:

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](TidyModels---Replicação-1---Correlation-and-regression-fundamentals-with-tidy-data-principles_files/figure-gfm/Gráfico%20da%20Correlação-1.png)<!-- -->

Assim, vemos que, assim como esperado, a circunferência das árvores
aumentou conforme sua idade aumentou. Além do gráfico, podemos também
usar uma tabela para ver a correlação da circunferência de cada árvore
com sua idade:

``` r
Orange |> 
  group_by(Tree) |> 
  summarize(Correlação = cor(age, circumference))
```

    ## # A tibble: 5 × 2
    ##   Tree  Correlação
    ##   <ord>      <dbl>
    ## 1 3          0.988
    ## 2 1          0.985
    ## 3 5          0.988
    ## 4 2          0.987
    ## 5 4          0.984

Como é possível ver, a correlação de cada árvore é maior do que a
correlação de todos os valores agregados.

### Teste de Hipótese Geral

Além de só estimar a correlação, também podemos fazer um teste de
hipótese, em que a hipótese nula é de que *a correlação é igual a 0* e a
hipótese alternativa indica que ela é *diferente de 0*, a partir do
comando *cor.test()*. O teste utilizado é o **teste de hipótese para o
coeficiente de correlação de Pearson**, que mede a correlação linear de
duas variáveis a partir dos valores observados e das médias das duas
variáveis em questão, que, no caso, são a idade e circunferência da
árvore. Para organizar o resultado, também foi utilizado o comando
*tidy()*, que apresenta-os de forma mais limpa e visual:

``` r
teste <- cor.test(Orange$age, Orange$circumference)

tidy(teste)
```

    ## # A tibble: 1 × 8
    ##   estimate statistic  p.value parameter conf.low conf.high method    alternative
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
    ## 1    0.914      12.9 1.93e-14        33    0.834     0.956 Pearson'… two.sided

### Teste de Hipótese para cada árvore + *Nest-Map-Unnest Workflow*

Com o objetivo de fazer mais de um teste de hipótese, sendo um para cada
uma das 5 árvores, podemos, primeiro, compilar as informações de cada
árvore em uma base *nested*:

``` r
nested <- Orange |> 
  nest(dados = c(age, circumference))

print(nested)
```

    ## # A tibble: 5 × 2
    ##   Tree  dados           
    ##   <ord> <list>          
    ## 1 1     <tibble [7 × 2]>
    ## 2 2     <tibble [7 × 2]>
    ## 3 3     <tibble [7 × 2]>
    ## 4 4     <tibble [7 × 2]>
    ## 5 5     <tibble [7 × 2]>

Agora, todas as informações de idade e circunferência de cada árvore
estão coletadas na coluna *dados*, separadas pelo número de cada uma.
Podemos, então realizar o teste de hipótese com cada conjunto de dados,
a partir do comando *map()*:

``` r
nested_teste <-  nested |> 
  mutate(teste = map(dados, ~ cor.test(.x$age, .x$circumference)))

print(nested_teste)
```

    ## # A tibble: 5 × 3
    ##   Tree  dados            teste  
    ##   <ord> <list>           <list> 
    ## 1 1     <tibble [7 × 2]> <htest>
    ## 2 2     <tibble [7 × 2]> <htest>
    ## 3 3     <tibble [7 × 2]> <htest>
    ## 4 4     <tibble [7 × 2]> <htest>
    ## 5 5     <tibble [7 × 2]> <htest>

O comando *map()* serve para **aplicar uma função, de forma iterada, a
cada elemento de uma lista ou coluna**. Os *.x\$* utilizados antes de
chamar as informações de idade e circunferência são utilizados uma vez
que, para o comando, é necessário especificar, dentro da lista de *data
frames* que é a coluna *dados*, quais que serão utilizados para executar
a função.

Além disso, podemos também tornar os dados mais visualizáveis utilizando
o *tidy()*, que também pode ser iterado para cada teste usando o
*map()*:

``` r
nested_teste_tidy <- nested_teste |>  
  mutate(
    tidied = map(teste, tidy)
  )

print(nested_teste_tidy)
```

    ## # A tibble: 5 × 4
    ##   Tree  dados            teste   tidied          
    ##   <ord> <list>           <list>  <list>          
    ## 1 1     <tibble [7 × 2]> <htest> <tibble [1 × 8]>
    ## 2 2     <tibble [7 × 2]> <htest> <tibble [1 × 8]>
    ## 3 3     <tibble [7 × 2]> <htest> <tibble [1 × 8]>
    ## 4 4     <tibble [7 × 2]> <htest> <tibble [1 × 8]>
    ## 5 5     <tibble [7 × 2]> <htest> <tibble [1 × 8]>

Dessa forma, com o teste feito para todas as árvores e visualizável, é
possível então realizar o processo de *unnesting* e, assim, expandir as
colunas para que as informações contidas dentro delas se tornem de fato
visualizáveis, a do comando *unnest()*, e selecionando as informações
que queremos a partir do *select()*:

``` r
nested_teste_tidy |> 
  unnest(cols = tidied) |> 
  select(-dados, -teste)
```

    ## # A tibble: 5 × 9
    ##   Tree  estimate statistic   p.value parameter conf.low conf.high method        
    ##   <ord>    <dbl>     <dbl>     <dbl>     <int>    <dbl>     <dbl> <chr>         
    ## 1 1        0.985      13.0 0.0000485         5    0.901     0.998 Pearson's pro…
    ## 2 2        0.987      13.9 0.0000343         5    0.914     0.998 Pearson's pro…
    ## 3 3        0.988      14.4 0.0000290         5    0.919     0.998 Pearson's pro…
    ## 4 4        0.984      12.5 0.0000573         5    0.895     0.998 Pearson's pro…
    ## 5 5        0.988      14.1 0.0000318         5    0.916     0.998 Pearson's pro…
    ## # ℹ 1 more variable: alternative <chr>

O comum é realizar todos estes passos em uma única sequência de código,
devido à facilidade do *pipe*, \|\>. **O código completo, portanto,
ficaria assim**:

``` r
Orange |> 
  nest(dados = c(age, circumference)) |> 
  mutate(
    teste = map(dados, ~ cor.test(.x$age, .x$circumference)),
    tidied = map(teste, tidy)
  ) |> 
  unnest(cols = tidied) |> 
  select(-dados, -teste)
```

    ## # A tibble: 5 × 9
    ##   Tree  estimate statistic   p.value parameter conf.low conf.high method        
    ##   <ord>    <dbl>     <dbl>     <dbl>     <int>    <dbl>     <dbl> <chr>         
    ## 1 1        0.985      13.0 0.0000485         5    0.901     0.998 Pearson's pro…
    ## 2 2        0.987      13.9 0.0000343         5    0.914     0.998 Pearson's pro…
    ## 3 3        0.988      14.4 0.0000290         5    0.919     0.998 Pearson's pro…
    ## 4 4        0.984      12.5 0.0000573         5    0.895     0.998 Pearson's pro…
    ## 5 5        0.988      14.1 0.0000318         5    0.916     0.998 Pearson's pro…
    ## # ℹ 1 more variable: alternative <chr>

## 2. Modelos de Regressão

### 2.1. Regressão Simples

O *workflow* utilizado para os testes de hipótese individuais também
pode ser replicado para regressões. Fazendo a regressão linear da idade
da árvore em relação ao seu comprimento e depois arrumando os resultados
com o comando *tidy()*, temos:

``` r
reg <- lm(age ~ circumference, data = Orange)
summary(reg)
```

    ## 
    ## Call:
    ## lm(formula = age ~ circumference, data = Orange)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -317.88 -140.90  -17.20   96.54  471.16 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    16.6036    78.1406   0.212    0.833    
    ## circumference   7.8160     0.6059  12.900 1.93e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 203.1 on 33 degrees of freedom
    ## Multiple R-squared:  0.8345, Adjusted R-squared:  0.8295 
    ## F-statistic: 166.4 on 1 and 33 DF,  p-value: 1.931e-14

``` r
tidy(reg)
```

    ## # A tibble: 2 × 5
    ##   term          estimate std.error statistic  p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)      16.6     78.1       0.212 8.33e- 1
    ## 2 circumference     7.82     0.606    12.9   1.93e-14

Agora sim, pensando em fazer regressões individuais para cada árvore,
podemos utilizar o mesmo processo de antes:

``` r
Orange |> 
  nest(data = c(-Tree)) |> 
  mutate(
    regs = map(data, ~ lm(age ~ circumference, data = .x)),
    tidied = map(regs, tidy)
  ) |> 
  unnest(tidied) |> 
  select(-data, -regs)
```

    ## # A tibble: 10 × 6
    ##    Tree  term          estimate std.error statistic   p.value
    ##    <ord> <chr>            <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 1     (Intercept)    -265.      98.6      -2.68  0.0436   
    ##  2 1     circumference    11.9      0.919    13.0   0.0000485
    ##  3 2     (Intercept)    -132.      83.1      -1.59  0.172    
    ##  4 2     circumference     7.80     0.560    13.9   0.0000343
    ##  5 3     (Intercept)    -210.      85.3      -2.46  0.0574   
    ##  6 3     circumference    12.0      0.835    14.4   0.0000290
    ##  7 4     (Intercept)     -76.5     88.3      -0.867 0.426    
    ##  8 4     circumference     7.17     0.572    12.5   0.0000573
    ##  9 5     (Intercept)     -54.5     76.9      -0.709 0.510    
    ## 10 5     circumference     8.79     0.621    14.1   0.0000318

### 2.2. Regressão Múltipla

É possível utilizar a mesma metodologia para regressões múltiplas.
Utilizando outra base criada já dentro do R, *mtcars*, vamos
transformá-la em *tibble*, criar dados *nested* diferenciando carros
mecânicos e manuais, e então fazer a regressão múltipla com a coluna
*wt* como variável resposta, definidos pela coluna *am*:

``` r
data(mtcars)
mtcars <- as_tibble(mtcars)


mtcars |> 
  nest(data = c(-am)) |> 
  mutate(
    regs = map(data, ~ lm(wt ~ mpg + qsec + gear, data = .x)),
    tidied = map(regs, tidy)
  ) |> 
  unnest(tidied) |> 
  select(-data, -regs)
```

    ## # A tibble: 8 × 6
    ##      am term        estimate std.error statistic  p.value
    ##   <dbl> <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1     1 (Intercept)   4.28      3.46      1.24   0.247   
    ## 2     1 mpg          -0.101     0.0294   -3.43   0.00750 
    ## 3     1 qsec          0.0398    0.151     0.264  0.798   
    ## 4     1 gear         -0.0229    0.349    -0.0656 0.949   
    ## 5     0 (Intercept)   4.92      1.40      3.52   0.00309 
    ## 6     0 mpg          -0.192     0.0443   -4.33   0.000591
    ## 7     0 qsec          0.0919    0.0983    0.935  0.365   
    ## 8     0 gear          0.147     0.368     0.398  0.696

Além do comando *tidy()*, também existem os comandos *glanced()* e
*augmented()* para melhor organizar os resultados de modelos
estatísticos, cada um com suas particularidades. Enquanto o primeiro dos
três foca em tornar fácil a análise de significância, providenciando os
parâmetros do modelo, o segundo busca ajudar na análise da qualidade
geral da regressão, entregando informações mais específicas como $R^2$ e
a estatística F. Por fim, o terceiro, além de entregar as informações do
modelo, adiciona-as aos dados que a base original já possuía, a fim de
facilitar a realização de gráficos e outras análises visuais. Podemos
utilizar os três pelo mesmo método realizado acima:

``` r
regs_organizadas <- mtcars |> 
  nest(dados = c(-am)) |> 
  mutate(
    regs = map(dados, ~ lm(wt ~ mpg + qsec + gear, data = .x)),
    tidied = map(regs, tidy),
    glanced = map(regs, glance),
    augmented = map(regs, augment)
  )
  
regs_organizadas |> 
  select(tidied) |> 
  unnest(tidied)
```

    ## # A tibble: 8 × 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   4.28      3.46      1.24   0.247   
    ## 2 mpg          -0.101     0.0294   -3.43   0.00750 
    ## 3 qsec          0.0398    0.151     0.264  0.798   
    ## 4 gear         -0.0229    0.349    -0.0656 0.949   
    ## 5 (Intercept)   4.92      1.40      3.52   0.00309 
    ## 6 mpg          -0.192     0.0443   -4.33   0.000591
    ## 7 qsec          0.0919    0.0983    0.935  0.365   
    ## 8 gear          0.147     0.368     0.398  0.696

``` r
regs_organizadas |> 
  select(glanced) |> 
  unnest(glanced)
```

    ## # A tibble: 2 × 12
    ##   r.squared adj.r.squared sigma statistic  p.value    df    logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <dbl>     <dbl> <dbl> <dbl>
    ## 1     0.833         0.778 0.291     15.0  0.000759     3  -0.00580  10.0  12.8
    ## 2     0.625         0.550 0.522      8.32 0.00170      3 -12.4      34.7  39.4
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
regs_organizadas |> 
  select(augmented) |> 
  unnest(augmented)
```

    ## # A tibble: 32 × 10
    ##       wt   mpg  qsec  gear .fitted  .resid  .hat .sigma  .cooksd .std.resid
    ##    <dbl> <dbl> <dbl> <dbl>   <dbl>   <dbl> <dbl>  <dbl>    <dbl>      <dbl>
    ##  1  2.62  21    16.5     4    2.73 -0.107  0.517  0.304 0.0744      -0.527 
    ##  2  2.88  21    17.0     4    2.75  0.126  0.273  0.304 0.0243       0.509 
    ##  3  2.32  22.8  18.6     4    2.63 -0.310  0.312  0.279 0.188       -1.29  
    ##  4  2.2   32.4  19.5     4    1.70  0.505  0.223  0.233 0.278        1.97  
    ##  5  1.62  30.4  18.5     4    1.86 -0.244  0.269  0.292 0.0889      -0.982 
    ##  6  1.84  33.9  19.9     4    1.56  0.274  0.286  0.286 0.125        1.12  
    ##  7  1.94  27.3  18.9     4    2.19 -0.253  0.151  0.293 0.0394      -0.942 
    ##  8  2.14  26    16.7     5    2.21 -0.0683 0.277  0.307 0.00732     -0.276 
    ##  9  1.51  30.4  16.9     5    1.77 -0.259  0.430  0.284 0.263       -1.18  
    ## 10  3.17  15.8  14.5     5    3.15  0.0193 0.292  0.308 0.000644     0.0789
    ## # ℹ 22 more rows
