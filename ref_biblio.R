# UI ----

ref_biblio_ui <- function(id) {
  
  tagList(
    tags$head(
      tags$style(HTML("
        .ref_biblio-text {
          font-size: 14px;
          line-height: 1.6;
          text-align: left;
        }
      "))
    ),
    
    fluidRow(
      column(12, 
             h3(strong("Referências"))
      )
    ),
    fluidRow(
      column(12, 
             div(class = "ref_biblio-text",
                
                 tags$p(HTML('S. R. Aurélio, K. F. Genaro, and E. D. Macedo Filho. “Análise comparativa dos padrões
             de deglutição de crianças com paralisia cerebral e crianças normais”. Pt. In: <em>Revista
             Brasileira de Otorrinolaringologia</em> 68 (mar. 2002). Publisher: ABORL-CCF Associação
             Brasileira de Otorrinolaringologia e Cirurgia Cérvico-Facial, pp. 167-173. ISSN:
             0034-7299. DOI: 10/fk7ccn. 
             <a href="https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt" target="_blank">https://www.scielo.br/j/rboto/a/hvQv9C6JY9h7MbrrSY98WpB/abstract/?lang=pt</a> (visited on
             07/26/2024).'
                 )),
                
                 tags$p(HTML('Brasil. <em>Diretrizes de atenção à pessoa com paralisia cerebral</em>. Tech. rep. Brasília:
             Ministério da Saúde, 2013.
             <a href="https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf" target="_blank">https://bvsms.saude.gov.br/bvs/publicacoes/diretrizes_atencao_paralisia_cerebral.pdf</a>.'
                 )),
                
                 tags$p(HTML('M. Fernandes and T. C. Morata. “Estudo dos efeitos auditivos e extra-auditivos da
             exposição ocupacional a ruído e vibração”. Pt. In: <em>Revista Brasileira de
             Otorrinolaringologia</em> 68 (out. 2002). Publisher: ABORL-CCF Associação Brasileira de
             Otorrinolaringologia e Cirurgia Cérvico-Facial, pp. 705-713. ISSN: 0034-7299. DOI:
             10/cxqmh6. <a href="https://www.scielo.br/j/rboto/a/xwtrdfpnFfGDKvsnKVXFHVF/" target="_blank">https://www.scielo.br/j/rboto/a/xwtrdfpnFfGDKvsnKVXFHVF/</a> (visited on
             07/26/2024).'
                 )),
                
                 tags$p(HTML('J. M. Marques. <em>Bioestatística: ênfase em fonoaudiologia: introdução ao uso do
             computador</em>. Pt. 1st ed. <a href="https://www.jurua.com.br/shop_item.asp?id=12491" target="_blank">https://www.jurua.com.br/shop_item.asp?id=12491</a>. Curitiba:
             Juruá, 2008. ISBN: 85-362-0287-4.'
                 )),
                
                 tags$p(HTML('P. A. Morettin and W. d. O. Bussab. <em>Estatística básica</em>. 9th ed. São Paulo: Saraiva,
             2017. ISBN: 978-85-472-2022-8.'
                 )),
                
                 tags$p(HTML('D. Team and S. Batistia. <em>1. 001 Problemas de Estatística para Leigos</em>. Pt-br. Trans.
             by D. Team and S. Batistia. Rio de Janeiro: Alta Books, 2016. ISBN: 85-508-0011-2.
             (Visited on 07/29/2024).'
                 ))
             )
      )
    )
  )
}


# server ----
ref_biblio_server <- function(id) {
  
}