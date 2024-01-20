      ******************************************************************
      * Author:Pedro Burgos
      * Date:18/1/24
      * Purpose:Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA5.



       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 MyArray1 PIC 9(3) OCCURS 10 TIMES.
       01 MyArray2 PIC 9(3) OCCURS 10 TIMES.
       01 MyArray3 PIC 9(3) OCCURS 10 TIMES.
       01 MyArrayUn PIC 9(3) OCCURS 20 TIMES.
       01 MyArrayIn PIC 9(3) OCCURS 10 TIMES.
       01 MyArrayM PIC 9(3) OCCURS 10 TIMES.


       01 Contador PIC 9(2) VALUE 1.
       01 Contador1 PIC 9(2) VALUE 1.
       01 Cont2 PIC 9(2) VALUE 1.
       01 Cont3 PIC 9(2) VALUE 1.
       01 Cont4 PIC 9(2) VALUE 1.


       01 indi PIC 9(2) VALUE 1.
       01 indi2 PIC 9(2) VALUE 1.
       01 IpN PIC 9(3).
       01 IpN2 PIC 9(3).
       01 May Pic 9(3).


       01 Num1 Pic 9(3).
       01 Num2 Pic 9(3).

       01 Per1 PIC 9 VALUE 0.
       01 Per2 PIC 9 VALUE 0.





       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            PERFORM Arrays.
            DISPLAY SPACE.
            PERFORM Union.
            DISPLAY SPACE.
            PERFORM Inter.
            DISPLAY SPACE.
            PERFORM MayorQue.


            STOP RUN.

       Arrays.
               DISPLAY "Armaremos nuestor primer array "
               PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
                   DISPLAY "Ingrese numero para lista1: "
                   ACCEPT IpN
                   MOVE IpN TO MyArray1(Contador)
               END-PERFORM
               DISPLAY SPACE.
               DISPLAY "Armaremos nuestor segundo array "
               PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
                   DISPLAY "Ingrese numero para lista2: "
                   ACCEPT IpN2
                   MOVE IpN2 TO MyArray2(Contador)
               END-PERFORM.
               DISPLAY SPACE.
               DISPLAY "Elementos en la array1:".
               PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
               DISPLAY "Posicion ", Contador, ": ", MyArray1(Contador)
               END-PERFORM
               DISPLAY SPACE.
               DISPLAY "Elementos en la array2:".
               PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
               DISPLAY "Posicion ", Contador, ": ", MyArray2(Contador)
               END-PERFORM.


       Union.
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
                   MOVE MyArray1(Contador) TO MyArrayUn(Contador)
              END-PERFORM
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
                    COMPUTE Cont2 = Contador + 10
                   MOVE MyArray2(Contador) TO MyArrayUn(Cont2)
              END-PERFORM
              DISPLAY "Union de array1 y array2"
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 20
               DISPLAY "Posicion ", Contador, ": ", MyArrayUn(Contador)
              END-PERFORM.




       Inter.
             PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
              MOVE MyArray1(Contador) TO Num1
              PERFORM VARYING Contador1 FROM 1 BY 1 UNTIL Contador1 > 10
               IF MyArray2(Contador1) EQUAL TO Num1 THEN
                 MOVE Num2 TO MyArrayIn(indi)
                 COMPUTE indi = indi + 1
                 EXIT PERFORM
                 END-IF
              END-PERFORM
             END-PERFORM.


           DISPLAY "Elementos en la intersección:"
           PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
            IF MyArrayIn(Contador) NOT = ZEROES
            DISPLAY "int ", Contador, ": ", MyArrayIn(Contador)
            END-IF
            END-PERFORM.

       MayorQue.
              DISPLAY "Ingrese numero que sea menor a lista1: "
                   ACCEPT May
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
              if  May < MyArray1(Contador) THEN
                  MOVE MyArray1(Contador) TO MyArrayM(indi2)
                  COMPUTE indi2 = indi2 + 1
              END-IF
              END-PERFORM.

           DISPLAY "Elementos Mayores a: ", May
           PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
            IF MyArrayM(Contador) NOT = ZEROES
            DISPLAY "int ", Contador, ": ", MyArrayM(Contador)
            END-IF
            END-PERFORM.

       END PROGRAM MATEMATICA5.
