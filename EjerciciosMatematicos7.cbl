      ******************************************************************
      * Author:Pedro Burgos
      * Date:9/2/2024
      * Purpose:Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA7.


       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 Num1 Pic S9(3).
       01 Num2 Pic 9(3).
       01 Num3 Pic 9(3).


       01 D1 PIC 9(3).
       01 D3 PIC 9(3).


       01 SUMA Pic 9(3) VALUE 0.
       01 C1 PIC 9(2) VALUE 1.
       01 C2 PIC 9(2) VALUE 1.

       01 Res_Resto PIC 9(3).
       01 BOOLEAN-VARIABLE   PIC 9 VALUE 0.

       01 MyArray1 PIC 9(3) OCCURS 10 TIMES VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY "Ingrese su NU1: ".
            ACCEPT Num1.

            PERFORM NumPerf.
            STOP RUN.


       EsDivisorSinNum.
              DIVIDE Num3 BY D3 GIVING Res_Resto REMAINDER Res_Resto.
              IF Res_Resto  EQUAL TO 0
                  THEN
                      MOVE 1 TO BOOLEAN-VARIABLE
              ELSE
                      MOVE 0 TO BOOLEAN-VARIABLE
              END-IF.





       NumPerf.
           COMPUTE Num2 = Num1 - 1.
           DISPLAY "Armaremos nuestro  array con divisores sin Num1 "
              PERFORM VARYING D1 FROM 1 BY 1 UNTIL D1 > Num2
                     MOVE Num1 TO Num3
                     MOVE D1 TO D3
                     PERFORM EsDivisorSinNum
                     IF BOOLEAN-VARIABLE  EQUAL TO 1 THEN
                            MOVE D1 TO MyArray1(C1)
                            DISPLAY "Posicion ", C1, ": ", MyArray1(C1)
                            COMPUTE C1 =  C1 + 1
                     END-IF
              END-PERFORM.
             PERFORM VARYING C2 FROM 1 BY 1 UNTIL C2 > 10
             COMPUTE SUMA = MyArray1(C2) + SUMA
             END-PERFORM.
             IF SUMA EQUAL TO Num1 THEN
                    DISPLAY "EL ", Num1
                    DISPLAY "ES UN NUMERO PERFECTO"
             ELSE
                    DISPLAY "NO ES UN NUMERO PERFECTO"
             END-IF.

       END PROGRAM MATEMATICA7.
