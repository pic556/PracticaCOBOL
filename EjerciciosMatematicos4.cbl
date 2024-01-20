      ******************************************************************
      * Author:Pedro Burgos
      * Date:12/1/24
      * Purpose:Practica-Matematica
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATEMATICA4.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 MyArray PIC 9(3) OCCURS 10 TIMES.
       01 MyArray2 PIC 9(3) OCCURS 10 TIMES.
       01 MyArray3 PIC 9(3) OCCURS 10 TIMES.
       01 MyArrayC PIC 9(3) OCCURS 10 TIMES.

       01 Contador PIC 9(2) VALUE 1.
       01 Cord2 PIC 9(2) VALUE 1.
       01 IpN PIC 9(3).

      /Creo una especie de array usando tables
      /Una tabla es una sección de almacenamiento
      /que almacena una colección de elementos, y cada elemento
      /se puede acceder mediante un índice

       01 Num1 PIC 9(3).
       01 NumRe PIC 9(3).
       01 Per PIC 9 VALUE 0.
       01 Rep PIC 9 VALUE 0.
       01 ContR PIC 9(3) VALUE 0.
       01 Num2 Pic 9(3).

       01 Maxi Pic 9(3) VALUE 0.
       01 Imax Pic 9(3).




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Ingrese su NU1: ".
            ACCEPT Num1.



            PERFORM Array.
            PERFORM MosArr.
            PERFORM pert.
            DISPLAY SPACE.
            DISPLAY "¿Pertenece NU1 al array? "Per.
            DISPLAY SPACE.
            PERFORM HayRep.
            DISPLAY "¿Hay repetidos en el array? "Rep.
            DISPLAY SPACE.
            PERFORM Max.
            DISPLAY"el maximo de array1 es: ", maxi.
            DISPLAY SPACE.
            PERFORM ordenar.
            DISPLAY SPACE.
            DISPLAY "Ingrese su NU2: ".
            ACCEPT Num2.
            DISPLAY "Quitamos a NU2: ".
            PERFORM Qui.




            STOP RUN.

       Array.
               PERFORM UNTIL Contador > 10
                   DISPLAY "Ingrese numero para lista: "
                   ACCEPT IpN
                   MOVE IpN TO MyArray(Contador)
                   MOVE IpN TO MyArrayC(Contador)
                   ADD 1 TO Contador
               END-PERFORM.


       MosArr.
               DISPLAY "Elementos en la tabla:".
               PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
               DISPLAY "Posicion ", Contador, ": ", MyArray(Contador)
              END-PERFORM.
       pert.
               PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
               IF MyArray(Contador) EQUAL TO Num1 THEN
                      MOVE 1 TO Per
               END-IF
               END-PERFORM.


       CantRep.
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
              IF NumRe EQUAL TO MyArray(Contador) THEN
                     COMPUTE ContR = ContR + 1
              END-IF
              END-PERFORM.

       HayRep.
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
              COMPUTE NumRe = MyArray(Contador)
              PERFORM CantRep
              IF ContR > 1 THEN
                     MOVE 1 TO Rep
              END-IF
              END-PERFORM.

       Max.
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
              IF Maxi LESS OR EQUAL TO MyArrayC(Contador) THEN
                     COMPUTE Maxi = MyArrayC(Contador)
                     COMPUTE Imax = Contador
              END-IF
              END-PERFORM.


       ordenar.
               PERFORM VARYING Cord2 FROM 1 BY 1 UNTIL Cord2 > 10
                 MOVE Maxi to MyArray3(Cord2)
                 DISPLAY "Posicion ", Cord2, ": ",MyArray3(Cord2)
                 MOVE 0 TO MyArrayC(Imax)
                 MOVE 0 TO Maxi
                 PERFORM Max
               END-PERFORM.





       Qui.
              PERFORM VARYING Contador FROM 1 BY 1 UNTIL Contador > 10
              IF Num2 NOT EQUAL TO MyArray(Contador) THEN
                MOVE MyArray(Contador) to MyArray2(Contador)
                DISPLAY "Posicion ", Contador, ": ", MyArray2(Contador)
              END-IF

              END-PERFORM.



       END PROGRAM MATEMATICA4.
