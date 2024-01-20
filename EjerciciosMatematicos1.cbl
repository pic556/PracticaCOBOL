      ******************************************************************
      * Author: Pedro Burgos
      * Date: 6/1/24
      * Purpose: Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA1.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 Num1      PIC 9(3).
      / Define un campo numérico de tres dígitos.
      / La notación 9 en COBOL se utiliza para representar dígitos numéricos.
      / El (3) indica que el campo tendrá tres dígitos en total.

       01 Num2      PIC 9(3).

       01 Resultado PIC 9(3)V9(2).
      / Variable que guardará el resultado.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Ingrese su NU1: ".
           ACCEPT Num1.
           DISPLAY SPACE.
           DISPLAY "Ingrese su NU2: ".
           ACCEPT Num2.

           DISPLAY "Programa de funciones simples en COBOL".
           DISPLAY "Resultados:".

           PERFORM Doble.
           PERFORM Suma.
           PERFORM Distancia.
      / PERFORM se utiliza para ejecutar una sección de código que ha sido identificada
      / mediante un nombre específico.



           STOP RUN.

       Doble.
           MULTIPLY Num1 BY 2 GIVING Resultado.
           DISPLAY "Doble: " Resultado.

      /Multiplica Num1 por 2 y almacena el resultado en Resultado.

       Suma.
           ADD Num1 TO Num2 GIVING Resultado.
           DISPLAY "Suma: " Resultado.
      /resultado = num1 + num2
      /Suma Num1 y Num2 y almacena el resultado en Resultado.

       Distancia.
           COMPUTE Resultado = Num1**2 + Num2**2.
           COMPUTE Resultado = FUNCTION SQRT(Resultado).
           DISPLAY "Distancia: " Resultado.

      /Calcula el cuadrado de Num1 y Num2, los suma y almacena en Resultado.
      /Calcula la raíz cuadrada del resultado anterior y almacena en Resultado.

       END PROGRAM MATEMATICA1.
