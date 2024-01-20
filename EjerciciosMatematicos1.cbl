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
      / Define un campo num�rico de tres d�gitos.
      / La notaci�n 9 en COBOL se utiliza para representar d�gitos num�ricos.
      / El (3) indica que el campo tendr� tres d�gitos en total.

       01 Num2      PIC 9(3).

       01 Resultado PIC 9(3)V9(2).
      / Variable que guardar� el resultado.

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
      / PERFORM se utiliza para ejecutar una secci�n de c�digo que ha sido identificada
      / mediante un nombre espec�fico.



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
      /Calcula la ra�z cuadrada del resultado anterior y almacena en Resultado.

       END PROGRAM MATEMATICA1.
