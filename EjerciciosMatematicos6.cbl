      ******************************************************************
      * Author:Pedro Burgos
      * Date:31/1/24
      * Purpose:Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA6.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 Num1 Pic 9(3).
       01 Num2 Pic 9(3).
       01 Num3 Pic 9(3).
       01 NumRe PIC 9(3).
       01 ContR PIC 9(3) VALUE 0.
       01 MCD Pic 9(3).


       01 D1 PIC 9(3).
       01 D2 PIC 9(3).
       01 D3 PIC 9(3).

       01 C1 PIC 9(2) VALUE 1.
       01 C2 PIC 9(2) VALUE 1.
       01 K1 PIC 9(2) VALUE 1.
       01 K2 PIC 9(2) VALUE 1.
       01 R1 PIC 9(2) VALUE 1.
       01 R2 PIC 9(2) VALUE 1.
       01 R3 PIC 9(2) VALUE 1.
       01 indi PIC 9(2) VALUE 1.


       01 Res_Resto PIC 9(3).
       01 BOOLEAN-VARIABLE   PIC 9 VALUE 0.

       01 MyArray1 PIC 9(3) OCCURS 10 TIMES VALUE 0.
       01 MyArray2 PIC 9(3) OCCURS 10 TIMES VALUE 0.
       01 MyArray3 PIC 9(3) OCCURS 10 TIMES VALUE 0.
       01 MyArrayUn PIC 9(3) OCCURS 20 TIMES VALUE 0.
       01 MyArrayR PIC 9(3) OCCURS 20 .

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Ingrese su NU1: ".
           ACCEPT Num1.
           DISPLAY "Ingrese su NU2: ".
           ACCEPT Num2.

           PERFORM MaxCD.

           STOP RUN.


       EsDivisor.
              DIVIDE Num3 BY D3 GIVING Res_Resto REMAINDER Res_Resto.
              IF Res_Resto  EQUAL TO 0
                  THEN
                      MOVE 1 TO BOOLEAN-VARIABLE
              ELSE
                      MOVE 0 TO BOOLEAN-VARIABLE
              END-IF.


       Union.
              PERFORM VARYING K1 FROM 1 BY 1 UNTIL k1 > 10
                   MOVE MyArray1(k1) TO MyArrayUn(k1)
              END-PERFORM
              PERFORM VARYING k1 FROM 1 BY 1 UNTIL k1 > 10
                    COMPUTE k2 = k1 + 10
                   MOVE MyArray2(k1) TO MyArrayUn(k2)
              END-PERFORM
              DISPLAY "Union de array1 y array2"
              PERFORM VARYING k1 FROM 1 BY 1 UNTIL k1 > 20
               DISPLAY "Posicion ", k1, ": ", MyArrayUn(k1)
              END-PERFORM.

       CantRep.
              PERFORM VARYING R1 FROM 1 BY 1 UNTIL R1 > 20
              IF NumRe EQUAL TO MyArrayUn(R1) THEN
                     COMPUTE ContR = ContR + 1
              END-IF
              END-PERFORM.



       Repetidos.
           PERFORM VARYING R2 FROM 1 BY 1 UNTIL R2 > 20
                     MOVE MyArrayUn(R2) TO NumRe
                     PERFORM CantRep
                     IF ContR > 1 AND MyArrayUn(2) NOT EQUAL TO 0 THEN
                            MOVE MyArrayUn(R2) to MyArrayR(R3)
                            DISPLAY "R tiene: " MyArrayR(R3)
                            COMPUTE R3 = R3 + 1
                            MOVE 0 TO ContR
                     END-IF
                     MOVE 0 TO ContR
           END-PERFORM
           DISPLAY "Armaremos Repetidos ", LENGTH OF MyArrayR
           PERFORM VARYING k1 FROM 1 BY 1 UNTIL k1 > 20
           DISPLAY "Posicion ", k1, ": ", MyArrayR(k1)
           END-PERFORM.

       Maximo.
        PERFORM VARYING Indi FROM 1 BY 1 UNTIL Indi > 20
                   IF MyArrayR(Indi) > MCD
                      MOVE MyArrayR(Indi) TO MCD
                   END-IF
         END-PERFORM.

       MaxCD.
           DISPLAY "Armaremos nuestor primer array "
              PERFORM VARYING D1 FROM 1 BY 1 UNTIL D1 > Num1
                     MOVE Num1 TO Num3
                     MOVE D1 TO D3
                     PERFORM EsDivisor
                     IF BOOLEAN-VARIABLE  EQUAL TO 1 THEN
                            MOVE D1 TO MyArray1(C1)
                            DISPLAY "Posicion ", C1, ": ", MyArray1(C1)
                            COMPUTE C1 =  C1 + 1
                     END-IF
              END-PERFORM.
           DISPLAY "Armaremos nuestor segundo array "
               PERFORM VARYING D2 FROM 1 BY 1 UNTIL D2 > Num2
                     MOVE Num2 TO Num3
                     MOVE D2 TO D3
                     PERFORM EsDivisor
                     IF BOOLEAN-VARIABLE  EQUAL TO 1 THEN
                            MOVE D2 TO MyArray2(C2)
                            DISPLAY "Posicion ", C2, ": ", MyArray2(C2)
                            COMPUTE C2 =  C2 + 1
                     END-IF
              END-PERFORM.
           PERFORM Union.
           PERFORM Repetidos.
           PERFORM Maximo.
           DISPLAY "el MCD es : ", MCD.

       END PROGRAM MATEMATICA6.
      // FUNIONA SOLO CON (N:K) != (1:1) Y CON K   MAYOR O IGUAL A N
