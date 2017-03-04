#' Importacao de dados contidos no DAIR disponiblizado no site do Min. do Trabalho e Previdencia Social.
#'
#' Esta funcao extrai os dados contidos no Demonstrativo de Aplicacoes e Investimentos dos Recursos (DAIR)
#' dos Regimes Proprios de Previdencia Social disponibilizado, em formato pdf, no site  do Min. do Trabalho e
#' Previdencia Social.
#'
#' @param caminho uma string contendo o nome do arquivo correspondente ao "DAIR".
#' @return um objeto do tipo data.frame contendo a relacao das aplicacoes financeiras do RPPS.
#' @author Bruno M. S. S. Melo
#' @details
#' Obs.: Na presente versao as aplicacoes em acoes ainda nao sao extraidas.
#' @examples
#' \dontrun{
#' dfDair <- mtpsImportaDair(caminho = "DAIR.pdf")
#' }
#' @seealso \code{tm::readPDF}
#' @export
mtpsImportaDairPdf <- function(caminho) {

  if (!file.exists(caminho)) {
    warning(paste( c("Arquivo ", caminho, " nao encontrado."), collapse = ""))

    return(NULL)
  }

  # Relaciona os nomes dos campos constantes das fichas de aplicacoes do DAIR
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



  #lsExtractedDair <- lapply(tabulizer::extract_tables(caminho), `Encoding<-`, 'UTF-8')
  fullText <- utilExtractPdfText(uri = caminho, enconding = 'UTF-8')

  # BACALHAU --> melhorar isso
  fullText <- stringr::str_replace(fullText, pattern = "Data da Negociacao:\\s+\\d{2}/\\d{2}/\\d{4}", replacement = "Data da Posicao Atual: ")
  fullText <- stringr::str_replace(fullText, pattern = "Valor Atual do Ativo:", replacement = "Valor Total Atual:")
  fullText <- stringr::str_replace(fullText, pattern = "Saldo:", replacement = "Valor Total Atual:")
  fullText <- stringr::str_replace(fullText, pattern = "do Ativo:", replacement = "do Fundo:")
  fullText <- stringr::str_replace(fullText, pattern = "(\\d{4}-\\d{2} )Ativo:", replacement = "\\1Fundo:")
  fullText <- stringr::str_replace(fullText, pattern = "Segmento de Mercado:", replacement = "Segmento:")
  fullText <- stringr::str_replace(fullText, pattern = "Data de Vencimento:\\s+\\d{2}/\\d{2}/\\d{4}", replacement = "")
  fullText <- stringr::str_replace(fullText, pattern = "Quantidade:", replacement = "Quantidade de Cotas:")

  # Extrai o nome do Ente
  ente <- gsub('^.*(1. ENTE Nome: )\\s*|\\s*( /).*$', '', fullText)
  ente <- iconv(enc2native(ente), to = "ASCII//TRANSLIT")

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

        dfNovaAplicacao <- data.frame(
          ORDEM_DAIR = NA_integer_,
          SEGMENTO = NA_character_,
          TIPO_ATIVO = NA_character_,
          DATA_POSICAO = NA_character_,
          INSTITUICAO_FINANCEIRA = NA_character_,
          CNPJ_INSTITUICAO_FINANCEIRA = NA_character_,
          FUNDO = NA_character_,
          CNPJ_FUNDO = NA_character_,
          QTD_COTAS = NA_real_,
          VALOR_ATUAL_COTA = NA_real_,
          VALOR_TOTAL_ATUAL = NA_real_,
          INDICE_REFERENCIA = NA_character_,
          PL_FUNDO = NA_real_,
          PERC_RECURSOS_RPPS = NA_real_,
          PERC_PL_FUNDO = NA_real_,
          stringsAsFactors = F)

        if(hasToken[1]){
          pos <- 1
          dfNovaAplicacao$ORDEM_DAIR <- as.integer(
            stringr::str_trim(
              stringr::str_sub(text,
                               posTokens[[pos]][1,2]+1,
                               min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1)))
        }

        if(hasToken[2]){
          pos <- 2
          dfNovaAplicacao$SEGMENTO <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[3]){
          pos <- 3
          dfNovaAplicacao$TIPO_ATIVO <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[4]){
          pos <- 4
          dfNovaAplicacao$DATA_POSICAO <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[5]){
          pos <- 5
          dfNovaAplicacao$INSTITUICAO_FINANCEIRA <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[6]){
          pos <- 6
          dfNovaAplicacao$CNPJ_INSTITUICAO_FINANCEIRA <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[7]){
          pos <- 7
          dfNovaAplicacao$FUNDO <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[8]){
          pos <- 8
          dfNovaAplicacao$CNPJ_FUNDO <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[9]){
          pos <- 9
          dfNovaAplicacao$QTD_COTAS <- as.numeric(
            stringr::str_replace(
              stringr::str_replace_all(
                string = stringr::str_trim(
                  stringr::str_sub(text,
                                   posTokens[[pos]][1,2]+1,
                                   min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1)),
                pattern = "[.]", replacement = "")
              ,",","."))
        }

        if(hasToken[10]){
          pos <- 10
          dfNovaAplicacao$VALOR_ATUAL_COTA <- as.numeric(
            stringr::str_replace(
              stringr::str_replace_all(
                string = stringr::str_trim(
                  stringr::str_sub(text,
                                   posTokens[[pos]][1,2]+1,
                                   min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1)),
                pattern = "[.]", replacement = "")
              ,",","."))
        }

        if(hasToken[11]){
          pos <- 11
          # dfNovaAplicacao$VALOR_TOTAL_ATUAL <- as.numeric(
          #   stringr::str_replace(
          #     stringr::str_replace_all(
          #       string = stringr::str_trim(
          #         stringr::str_sub(text,
          #                          posTokens[[pos]][1,2]+1,
          #                          min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1)),
          #       pattern = "[.]", replacement = "")
          #     ,",","."))

          dfNovaAplicacao$VALOR_TOTAL_ATUAL <- as.numeric(
            stringr::str_replace(
              stringr::str_replace_all(
                string = stringr::str_trim(
                  stringr::str_extract(
                    string = stringr::str_sub(text,
                                              posTokens[[pos]][1,2]+1,
                                              posTokens[[pos]][1,2]+20),
                    pattern = "[\\d{1,3}.]*,\\d{2,}")),
                  pattern = "[.]", replacement = "")
                ,",","."))
        }

        if(hasToken[12]){
          pos <- 12
          dfNovaAplicacao$INDICE_REFERENCIA <- stringr::str_trim(
            stringr::str_sub(text,
                             posTokens[[pos]][1,2]+1,
                             min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))
        }

        if(hasToken[13]){
          pos <- 13
          dfNovaAplicacao$PL_FUNDO <- as.numeric(
            stringr::str_replace(
              stringr::str_replace_all(
                string = stringr::str_trim(
                  stringr::str_sub(text,
                                   posTokens[[pos]][1,2]+1,
                                   min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1)),
                pattern = "[.]", replacement = "")
              ,",","."))
        }

        if(hasToken[14]){
          pos <- 14
          # dfNovaAplicacao$PERC_RECURSOS_RPPS <- stringr::str_trim(
          #   stringr::str_sub(text,
          #                    posTokens[[pos]][1,2]+1,
          #                    min(unlist(posTokens)[unlist(posTokens)>posTokens[[pos]][1,2]])-1))

          dfNovaAplicacao$PERC_RECURSOS_RPPS <- as.numeric(
            stringr::str_replace(
              stringr::str_replace_all(
                string = stringr::str_trim(
                  stringr::str_extract(
                    string = stringr::str_sub(text,
                                              posTokens[[pos]][1,2]+1,
                                              posTokens[[pos]][1,2]+6),
                    pattern = "[\\d{1,3}.]*,\\d{2,}")),
                pattern = "[.]", replacement = "")
              ,",","."))
        }

        if(hasToken[15]){
          pos <- 15
          # dfNovaAplicacao$PERC_PL_FUNDO <- stringr::str_trim(
          #   stringr::str_sub(text,
          #                    posTokens[[pos]][1,2]+1,
          #                    posTokens[[pos]][1,2]+1+stringr::str_locate(stringr::str_sub(text, posTokens[[pos]][1,2]+1,nchar(text)), "%")[1]))

          dfNovaAplicacao$PERC_PL_FUNDO <- as.numeric(
            stringr::str_replace(
              stringr::str_replace_all(
                string = stringr::str_trim(
                  stringr::str_extract(
                    string = stringr::str_sub(text,
                                              posTokens[[pos]][1,2]+1,
                                              posTokens[[pos]][1,2]+6),
                    pattern = "[\\d{1,3}.]*,\\d{2,}")),
                pattern = "[.]", replacement = "")
              ,",","."))
        }

        # browser()

        dfDair <- rbind(
          dfDair,
          dfNovaAplicacao
        )
      }, silent = T)}) #try
  }

  dfDair <- data.frame(
    ENTE = ente,
    dfDair,
    stringsAsFactors = F
  )

  dfDair$CNPJ_FUNDO <- stringr::str_pad(
    string = dfDair$CNPJ_FUNDO,
    width = 14,
    side = "left",
    pad = "0"
  )

  dfDair$CNPJ_INSTITUICAO_FINANCEIRA <- stringr::str_pad(
    string = dfDair$CNPJ_INSTITUICAO_FINANCEIRA,
    width = 14,
    side = "left",
    pad = "0"
  )

  #return(dfDair[complete.cases(dfDair),])
  return(dfDair)
}
