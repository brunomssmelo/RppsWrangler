## Pacote RppsWrangler
Este pacote está em desenvolvimento e o objetivo é que o mesmo disponibilize um conjunto de funções que auxilie na fiscalização dos RPPS, mediante a obtenção de dados na web, cálculo e monitoramento de indicadores. No que diz respeito à obtenção de dados, objetiva-se que o pacote disponibilize funções para:

* Importação de dados dos relatórios DIPR, DAIR e DRAA disponiblizados pelo Ministério do Trabalho no [CADPREV](http://cadprev.previdencia.gov.br/Cadprev/faces/pages/index.xhtml) no formato `.pdf`   
* Importação de dados dos relatórios DIPR, DAIR e DRAA no formato `.xml`   
* Importação dos mesmos dados no formato `.xls` ou `.xlsx`   

Além da importação dos dados acima referidos, pretende-se que o pacote também consiga importar os dados necessários a construção de indicadores para monitoramento dos RPPS. O monitoramento será feito mediante um *dashboard*. Fontes de dados, além do Ministério do Trabalho inclui o [SICONFI](https://siconfi.tesouro.gov.br/siconfi/index.jsf)

Outras funcionalidades planejadas para o pacote incluem:
* Verificação de enquadramento das aplicações dos RPPS na [Resolução CMN 3922/2010](http://www.mtps.gov.br/images/RPPS/MaisInformacoes/legislacaoaplicada/Resoluo-CMN-3922-2010.pdf)
* xxx 

## Instalação do Pacote 
Para a instalação do pacote deve-se proceder da seguinte forma:

```
install.packages("devtools")
devtools::install_github("brunomssmelo/RppsWrangler")
```
