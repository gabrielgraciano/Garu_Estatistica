# UI ----

inicio_equipe_ui <- function(id) {
 
   ns <- NS(id)
   
  tagList(
    tags$head(
      tags$style(HTML("
        .participant-container {
          display: flex;
          flex-wrap: wrap;
          justify-content: center;
        }
        .participant {
          margin: 15px;
          text-align: center;
        }
        .participant img {
          width: 150px;
          height: 150px;
          border-radius: 50%;
          object-fit: cover;
        }
        .participant-name {
          margin-top: 10px;
          font-weight: bold;
        }
      "))
    ),
    fluidRow(
      column(12, 
             h3(strong("Equipe"))
             )
      ),
    fluidRow(
      column(12, 
             div(
               div(class = "participant-container", 
                   
                   div(class = "participant", 
                      img(src = "images/alessandra.jpg"),
                       div(class = "participant-name",
                           HTML("Alessandra A. S. Menezes<br>
                           Epidemiologia e Bioestatística<br>
                           Medicina Preventiva<br>
                              UNIFESP"))
                   ),
                   div(class = "participant", 
                       img(src = "images/camila.jpg"),
                       div(class = "participant-name",
                           HTML("Camila Bertini Martins<br>
                                Epidemiologia e Bioestatística<br>
                           Medicina Preventiva<br>
                               UNIFESP"))
                   ),
                   div(class = "participant", 
                       img(src = "images/flavia.jpg"),
                       div(class = "participant-name", 
                           HTML("Flávia Cristina Martins Queiroz Mariano<br>
                                Instituto de Ciência e Tecnologia<br>
                               ICT-UNIFESP"))
                       )
                   )
               )
             )
      ),
    fluidRow(
      column(12, 
             div(
               div(class = "participant-container", 
                   
                       div(class = "participant", 
                           img(src = "images/gabriel.jpg"),
                           div(class = "participant-name", 
                               HTML("Gabriel Graciano Dias<br>
                                Graduação em Biomedicina<br>
                               UNIFESP"))
                       ),
                       div(class = "participant", 
                           img(src = "images/joao.jpg"),
                           div(class = "participant-name", 
                               HTML("Joao Henrique de Araujo Morais<br>
                           Graduação em Ciência e Tecnologia <br> 
                          Graduação em Ciência da Computação <br>
                               ICT-UNIFESP"))
                       ),
                       div(class = "participant", 
                           img(src = "images/paulopaiva.jpg"),
                           div(class = "participant-name", 
                               HTML("Paulo Bandiera Paiva<br>
                                Informática em Saúde<br>
                               UNIFESP"))
                       )
                   )
               )
             )
      )
    )
    
}


# server ----

inicio_equipe_server <- function(input, output, session) {
  
}