       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.
      ******************************************************************
      * OBJETIVO: SISTEMA DE GESTAO DE CLIENTES
      * AUTHOR: RENILSON BINHO
      ******************************************************************
       ENVIRONMENT DIVISION.


      * CRIANDO REFERENCIA PARA UM ARQUIVO FISICO
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * REFERENCIA DO ARQUIVO
           SELECT CLIENTES ASSIGN TO 'C:\COBOL\CLIENTES.DAT'
               ORGANIZATION IS INDEXED
      * ACESSO DIRETO ATRAVES DE UMA CHAVE
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS CLIENTES-STATUS
               RECORD KEY IS CLIENTES-CHAVE.

       DATA DIVISION.
      * CRIANDO VARIAVEIS PARA O REGISTRO DO ARQUIVO
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTES-REG.
           02 CLIENTES-CHAVE.
               03 CLIENTES-FONE PIC 9(09).
           02 CLIENTE-NOME      PIC X(30).
           02 CLIENTES-EMAIL    PIC X(40).


       WORKING-STORAGE SECTION.

       77 WRK-OPCAO PIC X(1).
       77 WRK-MODULO PIC X(25) VALUE SPACES.
       77 WRK-TECLA PIC X(1).
       77 WRK-OPCAO-RELATORIO PIC X(1).
       77 CLIENTES-STATUS PIC 99.
       77 WRK-MSGERRO PIC X(35).

      * VARIAVEIS PARA LIMPAR TELA
       SCREEN SECTION.
       01 TELA.
           05 LIMPA-TELA.
      * LIMPA A TELA
               10 BLANK SCREEN.
      * DEIXA UMA LINHA COM A COR DIFERENTE
               10 LINE 01 COLUMN 1 ERASE EOL BACKGROUND-COLOR 2.
               10 LINE 01 COLUMN 45 PIC X(25) ERASE EOL
                  BACKGROUND-COLOR 2 FOREGROUND-COLOR 0
                  FROM 'SISTEMA DE CLIENTES'.
               10 LINE 02 COLUMN 01 PIC X(25) ERASE EOL
                  BACKGROUND-COLOR 1 FOREGROUND-COLOR 0 FROM WRK-MODULO.

       01 MENU.
           02 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
           02 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
           02 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
           02 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
           02 LINE 11 COLUMN 15 VALUE '5 - RELATORIO'.
           02 LINE 12 COLUMN 15 VALUE 'X - SAIR'.
           02 LINE 13 COLUMN 15 VALUE 'OPCAO.......: '.
           02 LINE 13 COLUMN 29 USING WRK-OPCAO.

       01 MENU-RELATORIO.
           02 LINE 12 COLUMN 55 VALUE '1 - EM TELA'.
           02 LINE 13 COLUMN 55 VALUE '2 - EM DISCO'.
           02 LINE 14 COLUMN 55 VALUE 'OPCAO......: '.
           02 LINE 14 COLUMN 68 USING WRK-OPCAO-RELATORIO.

       01 TELA-REGISTRO.
           02 CHAVE FOREGROUND-COLOR 2.
               03 LINE 10 COLUMN 10 VALUE 'TELEFONE: '.
               03 COLUMN PLUS 2 PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.
           02 SS-DADOS.
               03 LINE 11 COLUMN 10 VALUE 'NOME....: '.
               03 COLUMN PLUS 2 PIC X(30) USING CLIENTE-NOME.
               03 LINE 12 COLUMN 10 VALUE 'EMAIL...: '.
               03 COLUMN PLUS 2 PIC X(40) USING CLIENTES-EMAIL.

       01 MOSTRAR-ERRO.
           02 MSG-ERRO.
               03 LINE 16 COLUMN 1 ERASE EOL BACKGROUND-COLOR 3.
               03 LINE 16 COLUMN 10 PIC X(40)
                   BACKGROUND-COLOR 3 FROM WRK-MSGERRO.
               03 LINE 16 COLUMN PLUS 2 PIC X(01)
                   BACKGROUND-COLOR 3 USING WRK-TECLA.


       PROCEDURE DIVISION.
       0001-PRINCIPAL SECTION.
      * ABRINDO ARQUIVO PARA E/S DE DADOS
           OPEN I-O CLIENTES
      * VERIFICANDO SE O ARQUIVO EXISTE
           IF CLIENTES-STATUS = 35 THEN
      * CRIANDO ARQUIVO
               OPEN OUTPUT CLIENTES
      * FECHANDO ARQUIVO
               CLOSE CLIENTES
      * ABRINDO NOVAMENTE PARA E/S
               OPEN I-O CLIENTES
           END-IF.

           PERFORM 0100-INICIAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.

       0100-INICIAR.
           DISPLAY TELA.
           ACCEPT MENU.

       0200-PROCESSAR.
           MOVE SPACES TO WRK-MSGERRO.
           MOVE SPACES TO CLIENTE-NOME.
           MOVE SPACES TO CLIENTES-EMAIL.
           EVALUATE WRK-OPCAO
               WHEN 1
                   PERFORM 0500-INCLUIR
               WHEN 2
                   PERFORM 0600-CONSULTAR
               WHEN 3
                   PERFORM 0700-ALTERAR
               WHEN 4
                  PERFORM 0800-EXCLUIR
               WHEN 5
                   DISPLAY TELA
                   ACCEPT MENU-RELATORIO
                   EVALUATE WRK-OPCAO-RELATORIO
                       WHEN 1
                           PERFORM 0900-RELATORIO-TELA
                       WHEN 2
                           PERFORM 0901-RELATORIO-DISCO
                       WHEN OTHER
                           DISPLAY 'ENTRE COM A OPCAO CORRETA!'
                   END-EVALUATE
               WHEN 'X'
                   PERFORM 0300-FINALIZAR
               WHEN OTHER
                   PERFORM 0100-INICIAR
                   PERFORM 0200-PROCESSAR
           END-EVALUATE.

       0300-FINALIZAR.
      * FECHANDO ARQUIVO
           CLOSE CLIENTES
           STOP RUN.

       0500-INCLUIR.
           MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
           DISPLAY TELA.
           ACCEPT TELA-REGISTRO.
      * ESCREVENDO NO ARQUIVO E VERIFICANDO SE JÁ EXISTE O REGISTRO
               WRITE CLIENTES-REG
                   INVALID KEY
                   MOVE 'JA EXISTE ! (N)OVO REGISTRO ?' TO WRK-MSGERRO
                   ACCEPT MOSTRAR-ERRO
                   IF WRK-TECLA = 'N' OR WRK-TECLA = 'n'
                   MOVE ZEROS TO CLIENTES-FONE
                   PERFORM 0500-INCLUIR
               END-WRITE.
      * VERIFICANDO SE REGISTO JÁ EXISTE
      *         IF CLIENTES-STATUS = 22
      *             DISPLAY 'REGISTO JÁ EXISTE!'
      *             ACCEPT WRK-OPCAO
      *         END-IF.
                   PERFORM 0100-INICIAR.
                   PERFORM 0200-PROCESSAR.

       0600-CONSULTAR.
           MOVE 'MODULO - CONSULTA' TO WRK-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
           READ CLIENTES
               INVALID KEY
                   MOVE 'NAO ENCONTRADO' TO WRK-MSGERRO
               NOT INVALID KEY
                   DISPLAY SS-DADOS
           END-READ.
           ACCEPT MOSTRAR-ERRO.
           PERFORM 0100-INICIAR.
           PERFORM 0200-PROCESSAR.

       0700-ALTERAR.
           MOVE 'MODULO - ALTERACAO ' TO WRK-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
           READ CLIENTES
               IF CLIENTES-STATUS = 0
                   ACCEPT SS-DADOS
                   REWRITE CLIENTES-REG
                   IF CLIENTES-STATUS = 0
                       MOVE 'REGISTRO ALTERADO COM SUCESSO'
                                       TO WRK-MSGERRO
                   ELSE
                       MOVE 'REGISTRO NAO ALTERADO' TO WRK-MSGERRO
                   END-IF
               ELSE
                   MOVE 'REGISTRO NAO ENCONTRADO ' TO WRK-MSGERRO
               END-IF.
               ACCEPT MOSTRAR-ERRO.
               PERFORM 0100-INICIAR.
               PERFORM 0200-PROCESSAR.


       0800-EXCLUIR.
           MOVE 'MODULO - EXCLUSAO ' TO WRK-MODULO.
           DISPLAY TELA.
           DISPLAY TELA-REGISTRO.
           ACCEPT CHAVE.
           READ CLIENTES
               INVALID KEY
                   MOVE 'NAO ENCONTRADO ' TO WRK-MSGERRO
               NOT INVALID KEY
                   MOVE 'ENCONTRADO, DESEJA EXCLUIR (S/N)?'
                       TO WRK-MSGERRO
                   DISPLAY SS-DADOS
           END-READ.
           ACCEPT MOSTRAR-ERRO.
           IF (WRK-TECLA = 'S' OR WRK-TECLA = 's')
                       AND CLIENTES-STATUS = 0
               DELETE CLIENTES
                   INVALID KEY
                       MOVE 'NAO EXCLUIDO ' TO WRK-MSGERRO
                       ACCEPT MOSTRAR-ERRO
               END-DELETE
           END-IF.
           PERFORM 0100-INICIAR.
           PERFORM 0200-PROCESSAR.

      * CRIA O RELATORIO APARTIR DE UM CLIENTE
       0900-RELATORIO-TELA.
           MOVE 'MODULO - RELATORIO ' TO WRK-MODULO.
           DISPLAY TELA.
           ACCEPT CHAVE.
           START CLIENTES KEY EQUAL CLIENTES-FONE.
           READ CLIENTES
               INVALID KEY
                   MOVE 'NENHUM REGISTRO ENCONTRADO' TO WRK-MSGERRO
               NOT INVALID KEY
                   DISPLAY TELA
                   DISPLAY '   RELATORIO DE CLIENTES   '
                   DISPLAY '---------------------------'
                   PERFORM UNTIL CLIENTES-STATUS = 10
                       DISPLAY CLIENTES-FONE ' '
                               CLIENTE-NOME ' '
                               CLIENTES-EMAIL
                       READ CLIENTES NEXT
                   END-PERFORM
           END-READ.
               MOVE 'REGISTROS LIDOS' TO WRK-MSGERRO.
           ACCEPT MOSTRAR-ERRO.

       0901-RELATORIO-DISCO.
           CONTINUE.
