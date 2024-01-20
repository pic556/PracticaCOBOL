      ******************************************************************
      * Author:Pedro Burgos
      * Date:9/1/24
      * Purpose:Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA2.


       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 Num1 PIC 9(3).
       01 Num2 PIC S9(3).
      /LA S involucra el signo
       01 Res_Resto PIC 9(3).

       01 Nota1 PIC 9(3).
       01 Nota2 PIC 9(3).
       01 Nota3 PIC 9(3).
       01 CNota PIC 9(3) VALUE 3.
       01 SNota PIC 9(3).
       01 Prome PIC 9(3).

       01 BOOLEAN-VARIABLE   PIC 9 VALUE 0.
      /Un BOOLEANO QUE YA LE INDICO SI ES T O F




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Ingrese su NU1: ".
           ACCEPT Num1.
           DISPLAY SPACE.
           DISPLAY "Ingrese su NU2: ".
           ACCEPT Num2.

           DISPLAY SPACE.
           DISPLAY "Ingrese su Nota1: ".
           ACCEPT Nota1.
           DISPLAY SPACE.
           DISPLAY "Ingrese su Nota2: ".
           ACCEPT Nota2.
           DISPLAY SPACE.
           DISPLAY "Ingrese su Nota3: ".
           ACCEPT Nota3.


           PERFORM EsPar.
           PERFORM EsPositiva.
           PERFORM ElPromedio.

           STOP RUN.


       EsPar.
              DIVIDE Num1 BY 2 GIVING Res_Resto REMAINDER Res_Resto.
              IF Res_Resto  EQUAL TO 0
                  THEN
                      MOVE 1 TO BOOLEAN-VARIABLE
              ELSE
                      MOVE 0 TO BOOLEAN-VARIABLE
              END-IF.

              DISPLAY "EsPar: " BOOLEAN-VARIABLE.

       EsPositiva.
              IF Num2 > 0 THEN
                     MOVE 1 TO BOOLEAN-VARIABLE
              ELSE
                     MOVE 0 TO BOOLEAN-VARIABLE
              END-IF.
              DISPLAY "EsPositiva: " BOOLEAN-VARIABLE.

       ElPromedio.
              COMPUTE SNota = Nota1 + Nota2 + Nota3.
              DIVIDE SNota BY CNota GIVING Prome.
              IF prome > 7 THEN
                     DISPLAY "PROMOCIONA"
              ELSE
                     IF Prome > 3 THEN
                            DISPLAY "APROBADO"
                     ELSE
                            DISPLAY "DESAPROBADO"
                     END-IF
              END-IF.


       END PROGRAM MATEMATICA2.
