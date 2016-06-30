#' Importacao de dados contidos no DAIR disponiblizado no site do Min. do Trabalho e Previdencia Social.
#'
#' Esta funcao extrai os dados contidos no Demonstrativo de Aplicacoes e Investimentos dos Recursos (DAIR)
#' dos Regimes Proprios de Previdencia Social disponibilizado, em formato pdf, no site  do Min. do Trabalho e
#' Previdencia Social.
#'
#' @param arqNome uma string contendo o nome do arquivo correspondente ao "DAIR".
#' @param arqTipo uma string contendo o tipo do arquivo de origem (pdf ou xml)
#' @return um objeto do tipo data.frame contendo a relacao das aplicacoes financeiras do RPPS.
#' @author Bruno M. S. S. Melo
#' @details
#' Na presente versao apenas as aplicacoes financeiras realizadas em fundos de investimento sao extraidas.
#' OBS.: Por enquanto a função so extrai de arquivos do tipo "pdf".
#' @examples
#' \dontrun{
#' dfDair <- mtpsImportaDair("DAIR.pdf")
#' }
#' @seealso \code{tabulizer::extract_tables}
#' @export
mtpsImportaDair <- function(arqNome) {

  if (arqTipo == "pdf") {
    lsExtractedDair <- lapply(tabulizer::extract_tables(arqNome), `Encoding<-`, 'UTF-8')
  } else {
    stop("Apenas arquivos do tipo pdf podem ser extraidos na presente versao.")
  }

  # Relaciona os nomes dos campos constantes das fichas de aplicações do DAIR
  tokens <- c("Aplicacao No:",
              "Segmento:",
              "Tipo de Ativo:",
              "Data da Posicao Atual:",
              "Instituicao Financeira:",
              "CNPJ da Instituicao Financeira:",
              "Fundo:",
              "CNPJ do Fundo:",
              "Quantidade de Cotas:",
              "Valor Atual da Cota:",
              "Valor Total Atual:",
              "Indice de Referencia:",
              "Patrimonio Liquido do Fundo:",
              "% dos Recursos do RPPS:",
              "% do Patrimonio Liquido do Fundo:",
              "Taxa de Performance:",               # IGNORADO
              "Nivel de Risco:",                    # IGNORADO
              "Agencia de Risco")                   # IGNORADO

  numPdfPages <- length(lsExtractedDair)

  fullText <- ""
  for (p in 2:numPdfPages) {

    page  <- lsExtractedDair[[p]]
    fullText <- stringr::str_c(fullText,
                               stringr::str_c(apply(page, 1, stringr::str_c, collapse=''), collapse = " "))
  }
  fullText <- iconv(enc2native(fullText), to = "ASCII//TRANSLIT")

  # Localiza a posição dos tokens em todo o texto
  posTokens <- stringr::str_locate_all(fullText,tokens)

  posAplicacoes <- posTokens[[1]]
  numAplicacoes <- nrow(posAplicacoes)

  dfDair <- NULL
  for ( a in 1:numAplicacoes ) {
    suppressWarnings({
      try({

        text <- ifelse(a < numAplicacoes,
                       stringr::str_sub(fullText, posAplicacoes[a,1], posAplicacoes[a+1,1]-1),
                       stringr::str_sub(fullText, posAplicacoes[a,1], nchar(fullText)))


        # Localiza a posição dos tokens
        posTokens <- stringr::str_locate_all(text,tokens)

        # Determinados nomes de campos são substrings de outros campos, devemos corrigir isso

        # Retirar falsas ocorrencias do campo "Instituição Financeira"
        if (length(posTokens[[5]])>0) {
          posTokens[[5]] <- matrix(posTokens[[5]][seq.int(from = 1, to = nrow(posTokens[[5]]), by = 2),], ncol = 2)
        }

        # Retirar falsas ocorrencias do campo "Fundo"
        if (length(posTokens[[7]])>0) {
          posTokens[[7]] <- matrix(posTokens[[7]][seq.int(from = 1, to = nrow(posTokens[[7]]), by = 4),], ncol = 2)
        }

        # Retirar falsas ocorrencias do campo "Patrimônio Líquido"
        if (length(posTokens[[13]])>0) {
          posTokens[[13]] <- matrix(posTokens[[13]][seq.int(from = 1, to = nrow(posTokens[[13]]), by = 2),], ncol = 2)
        }

        # Cria vetor com "flags" indicando se determinado token foi encontrado
        hasToken <- sapply(posTokens, 'length')>0

        for (t in 1:(length(tokens)-1)) {
          if (length(posTokens[[t]]) == 0 ) {
            posTokens[[t]] <- posTokens[[t+1]]
          }
        }

        dfDair <- rbind(
          dfDair,
          data.frame(
            ID = ifelse(hasToken[1],
                        as.integer(stringr::str_trim(stringr::str_sub(text, posTokens[[1]][1,2]+1,posTokens[[2]][1,1]-1))),
                        NA),
            SEGMENTO = ifelse(hasToken[2],
                              stringr::str_trim(stringr::str_sub(text, posTokens[[2]][1,2]+1,posTokens[[3]][1,1]-1)),
                              ""),
            TIPO_ATIVO = ifelse(hasToken[3],
                                stringr::str_trim(stringr::str_sub(text, posTokens[[3]][1,2]+1,posTokens[[4]][1,1]-1)),
                                ""),
            DATA_POSICAO = ifelse(hasToken[4],
                                  stringr::str_trim(stringr::str_sub(text, posTokens[[4]][1,2]+1,posTokens[[5]][1,1]-1)),
                                  ""),
            INSTITUICAO_FINANCEIRA = ifelse(hasToken[5],
                                            stringr::str_trim(stringr::str_sub(text, posTokens[[5]][1,2]+1,posTokens[[6]][1,1]-1)),
                                            ""),
            CNPJ_INSTITUICAO_FINANCEIRA = ifelse(hasToken[6],
                                                 stringr::str_trim(stringr::str_sub(text, posTokens[[6]][1,2]+1,posTokens[[7]][1,1]-1)),
                                                 ""),
            FUNDO = ifelse(hasToken[7],
                           stringr::str_trim(stringr::str_sub(text, posTokens[[7]][1,2]+1,posTokens[[8]][1,1]-1)),
                           ""),
            CNPJ_FUNDO = ifelse(hasToken[8],
                                stringr::str_trim(stringr::str_sub(text, posTokens[[8]][1,2]+1,posTokens[[9]][1,1]-1)),
                                ""),
            QTD_COTAS = ifelse(hasToken[9],
                               as.numeric(
                                 stringr::str_replace(
                                   stringr::str_replace_all(
                                     string = stringr::str_trim(stringr::str_sub(text, posTokens[[9]][1,2]+1,posTokens[[10]][1,1]-1)),
                                     pattern = "[.]", replacement = "")
                                   ,",",".")),
                               0),
            VALOR_ATUAL_COTA = ifelse(hasToken[10],
                                      as.numeric(
                                        stringr::str_replace(
                                          stringr::str_replace_all(
                                            string = stringr::str_trim(stringr::str_sub(text, posTokens[[10]][1,2]+1,posTokens[[11]][1,1]-1)),
                                            pattern = "[.]", replacement = "")
                                          ,",",".")),
                                      0),
            VALOR_TOTAL_ATUAL = ifelse(hasToken[11],
                                       as.numeric(
                                         stringr::str_replace(
                                           stringr::str_replace_all(
                                             string = stringr::str_trim(stringr::str_sub(text, posTokens[[11]][1,2]+1,posTokens[[12]][1,1]-1)),
                                             pattern = "[.]", replacement = "")
                                           ,",",".")),
                                       0),
            INDICE_REFERENCIA = ifelse(hasToken[12],
                                       stringr::str_trim(stringr::str_sub(text, posTokens[[12]][1,2]+1,posTokens[[13]][1,1]-1)),
                                       ""),
            PL_FUNDO = ifelse(hasToken[13],
                              as.numeric(
                                stringr::str_replace(
                                  stringr::str_replace_all(
                                    string = stringr::str_trim(stringr::str_sub(text, posTokens[[13]][1,2]+1,posTokens[[14]][1,1]-1)),
                                    pattern = "[.]", replacement = "")
                                  ,",",".")),
                              0),
            PERC_RECURSOS_RPPS = ifelse(hasToken[14],
                                        stringr::str_trim(stringr::str_sub(text, posTokens[[14]][1,2]+1,posTokens[[15]][1,1]-1)),
                                        0),
            PERC_PL_FUNDO = ifelse(hasToken[15],
                                   stringr::str_trim(
                                     stringr::str_sub(text,
                                                      posTokens[[15]][1,2]+1,
                                                      posTokens[[15]][1,2]+1+stringr::str_locate(stringr::str_sub(text, posTokens[[15]][1,2]+1,nchar(text)), "%")[1])),
                                   "")
          )
        )
      }, silent = T)}) #try
  }
  return(dfDair[complete.cases(dfDair),])
}
