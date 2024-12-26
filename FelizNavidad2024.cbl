      ******************************************************************
      * Author:Admi_Argentina
      * Date:24/12/24
      * Purpose:Celebrar-La-NAVIDAD-en-Linkedin
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Navidad2024.


       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ALTURA       PIC 99 VALUE 5.
       01 I            PIC 99 VALUE 0.
       01 J            PIC 99 VALUE 0.
       01 ESPACIOS     PIC 99 VALUE 0.
       01 ESTRELLAS    PIC 99 VALUE 0.
       01 LINEA        PIC X(50) VALUE SPACES.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Ingrese la altura del arbol: ".
           ACCEPT ALTURA.

           PERFORM ArmandoArbolito.
           DISPLAY "Feliz navidad y año nuevo 2025".
           DISPLAY "Pedro Burgos - linkedIn/Pburgos123"
           STOP RUN.

       ArmandoArbolito.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > ALTURA
             COMPUTE I = J
             COMPUTE ESPACIOS = ALTURA - I
             COMPUTE ESTRELLAS = 2 * I - 1
             MOVE SPACES TO LINEA
               PERFORM RELLENAR-ESPACIOS
               PERFORM RELLENAR-ESTRELLAS
               DISPLAY LINEA
           END-PERFORM.


       RELLENAR-ESPACIOS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ESPACIOS
               MOVE " " TO LINEA(I:1)
           END-PERFORM.

       RELLENAR-ESTRELLAS.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ESTRELLAS
               MOVE "*" TO LINEA(ESPACIOS + I:1)
           END-PERFORM.

       END PROGRAM Navidad2024.
